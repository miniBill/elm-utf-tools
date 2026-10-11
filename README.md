# Elm UTF tools
This is a fork of [zwilias/elm-utf-tools](https://github.com/zwilias/elm-utf-tools) that adds support for converting UTF-16 to and from Bytes.

Tools for working with strings and UTF-8, UTF-16 and UTF-32 codepoints. Some
algorithms need to consider a string as a series of codepoints with a certain
encoding. Elm itself allows operating on a string as if it were a series of
unicode codepoints, except for some operations (like `String.length`).
