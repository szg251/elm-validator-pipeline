module Test.Validator.String exposing (..)

import Expect
import Test exposing (..)
import Validator.String as String


isEmailTest : Test
isEmailTest =
    describe "isEmail"
        [ test "me@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "me@example.com") (Ok "me@example.com"))
        , test "a.nonymous@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "a.nonymous@example.com") (Ok "a.nonymous@example.com"))
        , test "name+tag@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "name+tag@example.com") (Ok "name+tag@example.com"))
        , test "a.name+tag@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "a.name+tag@example.com") (Ok "a.name+tag@example.com"))
        , test "\"spaces must be quoted\"@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "\"spaces must be quoted\"@example.com") (Ok "\"spaces must be quoted\"@example.com"))
        , test "!#$%&'*+-/=.?^_`{|}~@[1.0.0.127]"
            (\_ -> Expect.equal (String.isEmail "error" "!#$%&'*+-/=.?^_`{|}~@[1.0.0.127]") (Ok "!#$%&'*+-/=.?^_`{|}~@[1.0.0.127]"))
        , test "fails on me@"
            (\_ -> Expect.equal (String.isEmail "error" "me@") (Err [ "error" ]))
        , test "fails on @example.com"
            (\_ -> Expect.equal (String.isEmail "error" "@example.com") (Err [ "error" ]))
        , test "fails on me.@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "me.@example.com") (Err [ "error" ]))
        , test "fails on .me@example.com"
            (\_ -> Expect.equal (String.isEmail "error" ".me@example.com") (Err [ "error" ]))
        , test "fails on me@example..com"
            (\_ -> Expect.equal (String.isEmail "error" "me@example..com") (Err [ "error" ]))
        , test "fails on me\\@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "me\\@example.com") (Err [ "error" ]))
        , test "fails on spaces\\ must\\ be\\ within\\ quotes\\ even\\ when\\ escaped@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "spaces\\ must\\ be\\ within\\ quotes\\ even\\ when\\ escaped@example.com") (Err [ "error" ]))
        , test "fails on a\\@mustbeinquotes@example.com"
            (\_ -> Expect.equal (String.isEmail "error" "a\\@mustbeinquotes@example.com") (Err [ "error" ]))
        ]


isPhoneJpTest : Test
isPhoneJpTest =
    describe "isPhoneJp"
        [ test "geographic number with one digit area code"
            (\_ -> Expect.equal (String.isPhoneJp "error" "06-1234-5678") (Ok "06-1234-5678"))
        , test "geographic number with two digit area code"
            (\_ -> Expect.equal (String.isPhoneJp "error" "075-123-4567") (Ok "075-123-4567"))
        , test "geographic number with three digit area code"
            (\_ -> Expect.equal (String.isPhoneJp "error" "0742-12-3456") (Ok "0742-12-3456"))
        , test "geographic number with four digit area code"
            (\_ -> Expect.equal (String.isPhoneJp "error" "04992-1-2345") (Ok "04992-1-2345"))
        , test "geographic number with five digit area code"
            (\_ -> Expect.equal (String.isPhoneJp "error" "082486-1234") (Ok "082486-1234"))
        , test "10 digit 0x0 non geographic number"
            (\_ -> Expect.equal (String.isPhoneJp "error" "080-1234-5678") (Ok "080-1234-5678"))
        , test "without separator"
            (\_ -> Expect.equal (String.isPhoneJp "error" "06-12345678") (Ok "06-12345678"))

        -- These two cases are not covered by the current regex, but should be fixed later
        -- , test "10 digit 0xx0 non geographic number"
        --     (\_ -> Expect.equal (String.isPhoneJp "error" "0120-123-456") (Ok "0120-123-456"))
        -- , test "11 digit non geographic number"
        --     (\_ -> Expect.equal (String.isPhoneJp "error" "090-1234-56789") (Ok "090-1234-56789"))
        , test "fails when starting with a non zero digit"
            (\_ -> Expect.equal (String.isPhoneJp "error" "16-1234-5678") (Err [ "error" ]))
        , test "fails when digit count is invalid"
            (\_ -> Expect.equal (String.isPhoneJp "error" "06-1234-56789") (Err [ "error" ]))
        ]


isUrlTest : Test
isUrlTest =
    describe "isUrl"
        [ test "http://www.example.com"
            (\_ -> Expect.equal (String.isUrl "error" "http://www.example.com") (Ok "http://www.example.com"))
        , test "https://www.example.com"
            (\_ -> Expect.equal (String.isUrl "error" "https://www.example.com") (Ok "https://www.example.com"))
        , test "https://www.example.com/page"
            (\_ -> Expect.equal (String.isUrl "error" "https://www.example.com/page") (Ok "https://www.example.com/page"))
        , test "https://www.example.com/page?a=5"
            (\_ -> Expect.equal (String.isUrl "error" "https://www.example.com/page?a=5") (Ok "https://www.example.com/page?a=5"))
        , test "https://www.example.com/page?a=5&b=str"
            (\_ -> Expect.equal (String.isUrl "error" "https://www.example.com/page?a=5&b=str") (Ok "https://www.example.com/page?a=5&b=str"))
        , test "fails on 123"
            (\_ -> Expect.equal (String.isUrl "error" "123") (Err [ "error" ]))
        , test "fails on htp://facebook.com"
            (\_ -> Expect.equal (String.isUrl "error" "htp://facebook.com") (Err [ "error" ]))
        , test "fails on http://facebook"
            (\_ -> Expect.equal (String.isUrl "error" "http://facebook?a=") (Err [ "error" ]))
        ]


notEmptyTest : Test
notEmptyTest =
    describe "notEmpty"
        [ test "succeeds"
            (\_ -> Expect.equal (String.notEmpty "error" " a") (Ok " a"))
        , test "suceeds on line break only"
            (\_ -> Expect.equal (String.notEmpty "error" "\n") (Ok "\n"))
        , test "suceeds on space only"
            (\_ -> Expect.equal (String.notEmpty "error" " ") (Ok " "))
        , test "fails"
            (\_ -> Expect.equal (String.notEmpty "error" "") (Err [ "error" ]))
        ]


notBlankTest : Test
notBlankTest =
    describe "notBlank"
        [ test "succeeds"
            (\_ -> Expect.equal (String.notBlank "error" " a") (Ok " a"))
        , test "fails on empty"
            (\_ -> Expect.equal (String.notBlank "error" "") (Err [ "error" ]))
        , test "fails on line break only"
            (\_ -> Expect.equal (String.notBlank "error" "\n") (Err [ "error" ]))
        , test "fails on space only"
            (\_ -> Expect.equal (String.notBlank "error" " ") (Err [ "error" ]))
        ]


letterOnlyTest : Test
letterOnlyTest =
    describe "letterOnly"
        [ test "succeeds"
            (\_ -> Expect.equal (String.letterOnly "error" "abc") (Ok "abc"))
        , test "fails"
            (\_ -> Expect.equal (String.letterOnly "error" "ab1c") (Err [ "error" ]))
        ]


hasLetterTest : Test
hasLetterTest =
    describe "hasLetter"
        [ test "succeeds"
            (\_ -> Expect.equal (String.hasLetter "error" "1a23") (Ok "1a23"))
        , test "fails"
            (\_ -> Expect.equal (String.hasLetter "error" "123") (Err [ "error" ]))
        ]


numberOnlyTest : Test
numberOnlyTest =
    describe "numberOnly"
        [ test "succeeds"
            (\_ -> Expect.equal (String.numberOnly "error" "123") (Ok "123"))
        , test "fails"
            (\_ -> Expect.equal (String.numberOnly "error" "123a") (Err [ "error" ]))
        ]


hasNumberTest : Test
hasNumberTest =
    describe "hasNumber"
        [ test "succeeds"
            (\_ -> Expect.equal (String.hasNumber "error" "a1bc") (Ok "a1bc"))
        , test "fails"
            (\_ -> Expect.equal (String.hasNumber "error" "abc") (Err [ "error" ]))
        ]


minLengthTest : Test
minLengthTest =
    describe "minLength"
        [ test "succeeds"
            (\_ -> Expect.equal (String.minLength "error" 2 "ab") (Ok "ab"))
        , test "fails"
            (\_ -> Expect.equal (String.minLength "error" 2 "a") (Err [ "error" ]))
        ]


maxLengthTest : Test
maxLengthTest =
    describe "maxLength"
        [ test "succeeds"
            (\_ -> Expect.equal (String.maxLength "error" 2 "ab") (Ok "ab"))
        , test "fails"
            (\_ -> Expect.equal (String.maxLength "error" 2 "abc") (Err [ "error" ]))
        ]


isIntTest : Test
isIntTest =
    describe "isInt"
        [ test "succeeds"
            (\_ -> Expect.equal (String.isInt "error" "1") (Ok 1))
        , test "fails"
            (\_ -> Expect.equal (String.isInt "error" "1.5") (Err [ "error" ]))
        ]


isFloatTest : Test
isFloatTest =
    describe "isFloat"
        [ test "succeeds on float"
            (\_ -> Expect.equal (String.isFloat "error" "1.5") (Ok 1.5))
        , test "succeeds on integer"
            (\_ -> Expect.equal (String.isFloat "error" "1") (Ok 1.0))
        , test "fails"
            (\_ -> Expect.equal (String.isFloat "error" "a1") (Err [ "error" ]))
        ]
