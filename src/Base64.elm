module Base64 exposing (Data, SixBit, decode, encode)

-- A retarded Base64 url-ready implementation.
-- Encodes 62nd character as - and 63rd as _
-- Expects input in six-bit chunks

import Array
import Bitwise
import Dict


type alias BoolInt =
    Int


type alias SixBit =
    -- These should be Bools but the `elm-bloom` library uses 1 and 0
    -- so we'll go with that to prevent conversions there-and-back.
    { a : BoolInt
    , b : BoolInt
    , c : BoolInt
    , d : BoolInt
    , e : BoolInt
    , f : BoolInt
    }


type alias Data =
    List SixBit


encodeSixBit : SixBit -> Char
encodeSixBit sb =
    let
        pairs : List ( BoolInt, Int )
        pairs =
            [ ( sb.a, 5 )
            , ( sb.b, 4 )
            , ( sb.c, 3 )
            , ( sb.d, 2 )
            , ( sb.e, 1 )
            , ( sb.f, 0 )
            ]

        foldPair : ( BoolInt, Int ) -> Int -> Int
        foldPair ( bit, shiftBy ) acc =
            Bitwise.shiftLeftBy shiftBy bit
                |> Bitwise.or acc
    in
    List.foldl foldPair 0 pairs |> intToChar


encode : Data -> String
encode =
    List.map encodeSixBit >> String.fromList


decodeSixBit : Char -> SixBit
decodeSixBit c =
    let
        int : Int
        int =
            charToInt c

        getBit : Int -> BoolInt
        getBit shift =
            Bitwise.and 1 (Bitwise.shiftRightBy shift int)
    in
    SixBit
        (getBit 5)
        (getBit 4)
        (getBit 3)
        (getBit 2)
        (getBit 1)
        (getBit 0)


decode : String -> Data
decode =
    String.toList >> List.map decodeSixBit



-- convert between chars and ints


chars =
    -- regularly  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    String.toList "ijlIftrsvabcdeghknopquxyzmw0123456789ABCDEFGHJKLMNOPQRSTUVWXYZ-_"


charray : Array.Array Char
charray =
    Array.fromList chars


intToChar : Int -> Char
intToChar i =
    Array.get i charray |> Maybe.withDefault '_'


charDict : Dict.Dict Char Int
charDict =
    List.indexedMap (\x y -> ( y, x )) chars |> Dict.fromList


charToInt : Char -> Int
charToInt char =
    Dict.get char charDict |> Maybe.withDefault 63
