module Test.Validator.String exposing (..)

import Expect
import Test exposing (..)
import Validator.String exposing (hasLetter, hasNumber, isUrl)


hasNumberTest : Test
hasNumberTest =
    describe "hasNumber"
        [ test "succeeds"
            (\_ -> Expect.equal (hasNumber "error" "a1bc") (Ok "a1bc"))
        , test "fails"
            (\_ -> Expect.equal (hasNumber "error" "abc") (Err [ "error" ]))
        ]


hasLetterTest : Test
hasLetterTest =
    describe "hasLetter"
        [ test "succeeds"
            (\_ -> Expect.equal (hasLetter "error" "1a23") (Ok "1a23"))
        , test "fails"
            (\_ -> Expect.equal (hasLetter "error" "123") (Err [ "error" ]))
        ]


isUrlTest : Test
isUrlTest =
    describe "isUrl"
        [ test "http://www.example.com"
            (\_ -> Expect.equal (isUrl "error" "http://www.example.com") (Ok "http://www.example.com"))
        , test "https://www.example.com"
            (\_ -> Expect.equal (isUrl "error" "https://www.example.com") (Ok "https://www.example.com"))
        , test "https://www.example.com/page"
            (\_ -> Expect.equal (isUrl "error" "https://www.example.com/page") (Ok "https://www.example.com/page"))
        , test "https://www.example.com/page?a=5"
            (\_ -> Expect.equal (isUrl "error" "https://www.example.com/page?a=5") (Ok "https://www.example.com/page?a=5"))
        , test "https://www.example.com/page?a=5&b=str"
            (\_ -> Expect.equal (isUrl "error" "https://www.example.com/page?a=5&b=str") (Ok "https://www.example.com/page?a=5&b=str"))
        , test "fails on 123"
            (\_ -> Expect.equal (isUrl "error" "123") (Err [ "error" ]))
        , test "fails on htp://facebook.com"
            (\_ -> Expect.equal (isUrl "error" "htp://facebook.com") (Err [ "error" ]))
        , test "fails on http://facebook"
            (\_ -> Expect.equal (isUrl "error" "http://facebook?a=") (Err [ "error" ]))
        ]
