# This file the copy of https://raw.githubusercontent.com/dylex/postgresql-typed/master/nix/utilities.nix
let
  # bash magic so we can set several traps without removin any existing one.
  # Brought to you by https://stackoverflow.com/questions/16115144/save-and-restore-trap-state-easy-way-to-manage-multiple-handlers-for-traps/16115145
  trapMagic = ''
    trap_stack_name() {
      local sig=''${1//[^a-zA-Z0-9]/_}
      echo "__trap_stack_$sig"
    }

    extract_trap() {
      echo ''${@:3:$(($#-3))}
    }

    get_trap() {
      eval echo $(extract_trap `trap -p $1`)
    }

    trap_push() {
      local new_trap=$1
      shift
      local sigs=$*
      for sig in $sigs; do
        local stack_name=`trap_stack_name "$sig"`
        local old_trap=$(get_trap $sig)
        eval "''${stack_name}"'[''${#'"''${stack_name}"'[@]}]=$old_trap'
        trap "''${new_trap}" "$sig"
      done
    }

    trap_prepend() {
      local new_trap=$1
      shift
      local sigs=$*
      for sig in $sigs; do
        if [[ -z $(get_trap $sig) ]]; then
          trap_push "$new_trap" "$sig"
        else
          trap_push "$new_trap ; $(get_trap $sig)" "$sig"
        fi
      done
    }
    '';

in self: super: {

  lib = super.lib //
    {

      # This function filters out stuff we don't want to consider part of the source
      # when building with nix. Any change in one of these files would cause a
      # re-build otherwise
      cleanSource =
        let
          fldSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
            # Filter out Subversion and CVS directories.
            (type == "directory" &&
              ( baseName == ".git" ||
                baseName == ".circleci" ||
                baseName == ".nix-cache" ||
                baseName == ".cache" ||
                baseName == "nix" ||
                baseName == "dist" ||
                baseName == "dist-newstyle"
              )
            ) ||
            # Filter out editor backup / swap files.
            self.lib.hasSuffix "~" baseName ||
            builtins.match "^\\.sw[a-z]$" baseName != null ||
            builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

            # filter out .ghc.environment
            builtins.match "^\\.ghc.environment.*" baseName != null ||

            # Filter out nix-build result symlinks
            (type == "symlink" && self.lib.hasPrefix "result" baseName) ||

            # Filter other random crap we have lying around for development
            # which we don't need to properly build
            (baseName == "develop.sh") ||
            (baseName == "Setup") ||
            (baseName == "Setup.o") ||
            (baseName == "Setup.hi") ||
            (baseName == ".bash_history") ||
            (baseName == "README.md")
          );
        in builtins.filterSource fldSourceFilter;
    };

  haskell = super.haskell // {
    lib = super.haskell.lib // {
      # This function provides an ephemeral postgresql instance for development in
      # the shellHook and at build/test time of the package it wraps
      withPostgres = pg: drv:
        let functions = ''
            ${trapMagic}

            function initPG() {
              ${super.lib.optionalString super.stdenv.isDarwin "export TMPDIR=/tmp"}
              ${super.lib.optionalString (!super.stdenv.isDarwin) "export LANG=C.UTF-8"}
              ${super.lib.optionalString (!super.stdenv.isDarwin) "export LC_ALL=C.UTF-8"}
              ${super.lib.optionalString (!super.stdenv.isDarwin) "export LC_CTYPE=C.UTF-8"}
              export TZ='UTC'
              export PGHOST=$(mktemp -d)
              export PGDATA=$PGHOST/db
              export PGPORT=5433
              export PGSOCK=$PGHOST/.s.PGSQL.$PGPORT
              export PGDATABASE=templatepg
              export PGUSER=templatepg
              # We set these environment variables so postgresql-typed knows how
              # to connect to the database at compile-time to make sure all SQL
              # queries are well typed and well formed
              export TPG_SOCK=$PGSOCK
              export TPG_DB=$PGDATABASE
              export TPG_USER=$PGUSER
              #
              ${pg}/bin/initdb -E UTF8 $PGDATA
              # avoid conflicts on travis and elsewhere
              echo "port = $PGPORT" >> $PGDATA/postgresql.conf
              ${pg}/bin/postgres -D $PGDATA -k $PGHOST  &
              echo -n "Waiting for database to start up..."
              while [[ ! -e $PGSOCK ]]; do sleep 0.1; done
              ${pg}/bin/createuser -h $PGHOST -U $(id -u --name) -s $PGUSER
              ${pg}/bin/createdb -h $PGHOST -O $PGUSER $PGDATABASE

              echo "Created database PGDATABASE=$PGDATABASE at PGHOST=$PGHOST."
              echo "Call killPG to stop and delete it. Call initPG to re-create it"
            }
            function killPG() {
              echo "Killing postgres database at $PGHOST"
              pg_ctl stop || true
              echo "Waiting for postgres database to die ..."
              while [[ -e $PGSOCK ]]; do sleep 0.1; done
              echo "Postgres is dead, deleting its data dir"
              rm -rf $PGHOST
            }
            function reinitPG {
              killPG && initPG
            }
            # export the functions so they're available in the development nix-shell
            # so the database can be re-created easly
            export -f initPG
            export -f killPG
            export -f reinitPG

            trap_prepend "killPG" EXIT
            '';

        in super.haskell.lib.overrideCabal drv (old: {
          buildDepends = (old.buildDepends or []) ++ [ pg ];
          preBuild = ''
            ${old.preBuild or ""}
            ${functions}
            initPG
          '';
          shellHook = ''
            ${old.shellHook or ""}
            ${functions}
            initPG
            '';
          postInstall = ''
            killPG
            ${old.postInstall or ""}
            '';
        });
    };
  };
}
