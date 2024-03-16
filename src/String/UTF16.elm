module String.UTF16 exposing (length, fromString, toString, fromList, toList, foldr)

{-| For some reason want to reason about strings as if they were UTF-16
sequences?

Technically, that's what they are, but Elm allows treating them as Unicode
codepoints, which is super-convenient. Occasionally, however, you may find
yourself in need of some old fashioned UTF-16 bytes, so here you go!

@docs length, fromString, toString, fromList, toList, foldr

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode


{-| Like `String.foldr`, but working on utf16 bytes as opposed to `Chars`.
-}
foldr : (Int -> acc -> acc) -> acc -> String -> acc
foldr op initialAcc string =
    String.foldr (utf32ToUtf16Bytes op) initialAcc string


utf32ToUtf16Bytes : (Int -> acc -> acc) -> Char -> acc -> acc
utf32ToUtf16Bytes op char acc =
    let
        int : Int
        int =
            Char.toCode char
    in
    if int <= 0x00010000 then
        op int acc

    else
        let
            c : Int
            c =
                int - 0x00010000
        in
        op
            (shiftRightZfBy 10 c |> or 0xD800)
            (op (and 0x03FF c |> or 0xDC00) acc)


{-| Count the number of UTF-16 bytes in a `String`.

Note that this should always equal `String.length` in Elm.

-}
length : String -> Int
length =
    String.length


{-| Turn a `String` into a the corresponding UTF-16 bytes.
-}
fromString : Bytes.Endianness -> String -> Bytes
fromString endianess s =
    foldr (\e acc -> Bytes.Encode.unsignedInt16 endianess e :: acc) [] s
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


{-| Turn a `String` into a list of UTF-16 bytes.
-}
toList : String -> List Int
toList s =
    foldr (::) [] s


{-| Turn a list of UTF-16 bytes into a `String`.
-}
fromList : List Int -> String
fromList bytes =
    bytes
        |> List.foldl utf16ToUtf32Int ( [], Nothing )
        |> Tuple.first
        |> List.reverse
        |> String.fromList


{-| Turn UTF-16 bytes into a `String`.
-}
toString : Bytes.Endianness -> Bytes -> Maybe String
toString endianess bytes =
    let
        width : Int
        width =
            Bytes.width bytes

        decoder : Bytes.Decode.Decoder String
        decoder =
            Bytes.Decode.loop ( width, [], Nothing ) (utf16ToUtf32 endianess)
    in
    Bytes.Decode.decode decoder bytes


type alias UTF16Acc =
    ( Int, List Char, Maybe Int )


utf16ToUtf32 : Bytes.Endianness -> UTF16Acc -> Bytes.Decode.Decoder (Bytes.Decode.Step UTF16Acc String)
utf16ToUtf32 endianess ( left, acc, combine ) =
    if left <= 0 then
        case combine of
            Just _ ->
                Bytes.Decode.fail

            Nothing ->
                List.reverse acc
                    |> String.fromList
                    |> Bytes.Decode.Done
                    |> Bytes.Decode.succeed

    else
        Bytes.Decode.unsignedInt16 endianess
            |> Bytes.Decode.map
                (\char ->
                    Bytes.Decode.Loop <|
                        case combine of
                            Nothing ->
                                if char >= 0xD800 && char < 0xE000 then
                                    ( left - 2, acc, Just char )

                                else
                                    ( left - 2, Char.fromCode char :: acc, Nothing )

                            Just prev ->
                                let
                                    x : Char
                                    x =
                                        prev
                                            |> and 0x03FF
                                            |> shiftLeftBy 10
                                            |> or (and 0x03FF char)
                                            |> (+) 0x00010000
                                            |> Char.fromCode
                                in
                                ( left - 2
                                , x :: acc
                                , Nothing
                                )
                )


utf16ToUtf32Int : Int -> ( List Char, Maybe Int ) -> ( List Char, Maybe Int )
utf16ToUtf32Int char ( acc, combine ) =
    case combine of
        Nothing ->
            if char >= 0xD800 && char < 0xE000 then
                ( acc, Just char )

            else
                ( Char.fromCode char :: acc, Nothing )

        Just prev ->
            ( prev
                |> and 0x03FF
                |> shiftLeftBy 10
                |> or (and 0x03FF char)
                |> (+) 0x00010000
                |> (\x -> Char.fromCode x :: acc)
            , Nothing
            )
