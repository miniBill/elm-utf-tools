module UTF16Test exposing (roundtrips, specific)

import Bytes
import Expect
import Fuzz
import Hex.Convert
import String.UTF16
import Test exposing (Test, describe, fuzz, test)


roundtrips : Test
roundtrips =
    describe "Roundtrip tests"
        [ fuzz Fuzz.string "UTF16 roundtrips [LE]" <|
            \input ->
                input
                    |> String.UTF16.fromString Bytes.LE
                    |> String.UTF16.toString Bytes.LE
                    |> Expect.equal (Just input)
        , fuzz Fuzz.string "UTF16 roundtrips [BE]" <|
            \input ->
                input
                    |> String.UTF16.fromString Bytes.BE
                    |> String.UTF16.toString Bytes.BE
                    |> Expect.equal (Just input)
        ]


pairs : List ( String, String, String )
pairs =
    -- Examples from https://en.wikipedia.org/wiki/UTF-16
    [ ( "$", "0024", "2400" )
    , ( "€", "20AC", "AC20" )
    , ( "𐐷", "D801DC37", "01D837DC" )
    , ( "𤭢", "D852DF62", "52D862DF" )
    ]


specific : Test
specific =
    pairs
        |> List.concatMap
            (\( input, be, le ) ->
                [ test (input ++ " [BE]") <|
                    \_ ->
                        input
                            |> String.UTF16.fromString Bytes.BE
                            |> Hex.Convert.toString
                            |> Expect.equal be
                , test (input ++ " [LE]") <|
                    \_ ->
                        input
                            |> String.UTF16.fromString Bytes.LE
                            |> Hex.Convert.toString
                            |> Expect.equal le
                ]
            )
        |> describe "Specific test strings"
