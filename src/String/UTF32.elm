module String.UTF32 exposing (length, fromList, toList)

{-| Need to work in unicode codepoints? Tired of UTF-8's silliness? You came to
the right place!

@docs length, fromList, toList

-}

import Char


{-| Build a `String` from a list of unicode codepoints.

    import String.UTF32 as UTF32

    UTF32.fromList [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ]
    --> "hello"

    UTF32.fromList [ 0x0001F4A9 ]
    --> "💩"

-}
fromList : List Int -> String
fromList bytes =
    bytes
        |> List.map Char.fromCode
        |> String.fromList


{-| Calculates the number UTF-32 characters in a `String`.

    import String.UTF32 as UTF32

    String.length "💩"
    --> 2

    UTF32.length "💩"
    --> 1

-}
length : String -> Int
length input =
    String.foldl (\_ acc -> acc + 1) 0 input


{-| Converts a String to a list of unicode codepoints. The inverse of `toString`

    import String.UTF32 as UTF32

    UTF32.toList "hello"
    --> [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ]

    UTF32.toList "💩"
    --> [ 0x0001F4A9 ]

-}
toList : String -> List Int
toList string =
    string |> foldl (::) [] |> List.reverse


{-| Fold over a string, left to right, accumulating unicode codepoints.
-}
foldl : (Int -> a -> a) -> a -> String -> a
foldl op initial input =
    String.foldl (\c acc -> op (Char.toCode c) acc) initial input


byteToString : Int -> String
byteToString int =
    Char.fromCode int |> String.fromChar
