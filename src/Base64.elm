module Base64 exposing (Data, SixBit, decode, encode)

-- A retarded Base64 url-ready implementation.
-- Encodes 62nd character as - and 63rd as _
-- Expects input in six-bit chunks

import Bitwise


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


intToChar : Int -> Char
intToChar i =
    case i of
        0 ->
            'A'

        1 ->
            'B'

        2 ->
            'C'

        3 ->
            'D'

        4 ->
            'E'

        5 ->
            'F'

        6 ->
            'G'

        7 ->
            'H'

        8 ->
            'I'

        9 ->
            'J'

        10 ->
            'K'

        11 ->
            'L'

        12 ->
            'M'

        13 ->
            'N'

        14 ->
            'O'

        15 ->
            'P'

        16 ->
            'Q'

        17 ->
            'R'

        18 ->
            'S'

        19 ->
            'T'

        20 ->
            'U'

        21 ->
            'V'

        22 ->
            'W'

        23 ->
            'X'

        24 ->
            'Y'

        25 ->
            'Z'

        26 ->
            'a'

        27 ->
            'b'

        28 ->
            'c'

        29 ->
            'd'

        30 ->
            'e'

        31 ->
            'f'

        32 ->
            'g'

        33 ->
            'h'

        34 ->
            'i'

        35 ->
            'j'

        36 ->
            'k'

        37 ->
            'l'

        38 ->
            'm'

        39 ->
            'n'

        40 ->
            'o'

        41 ->
            'p'

        42 ->
            'q'

        43 ->
            'r'

        44 ->
            's'

        45 ->
            't'

        46 ->
            'u'

        47 ->
            'v'

        48 ->
            'w'

        49 ->
            'x'

        50 ->
            'y'

        51 ->
            'z'

        52 ->
            '0'

        53 ->
            '1'

        54 ->
            '2'

        55 ->
            '3'

        56 ->
            '4'

        57 ->
            '5'

        58 ->
            '6'

        59 ->
            '7'

        60 ->
            '8'

        61 ->
            '9'

        62 ->
            '-'

        _ ->
            -- ideally 63
            '_'


charToInt : Char -> Int
charToInt char =
    case char of
        'A' ->
            0

        'B' ->
            1

        'C' ->
            2

        'D' ->
            3

        'E' ->
            4

        'F' ->
            5

        'G' ->
            6

        'H' ->
            7

        'I' ->
            8

        'J' ->
            9

        'K' ->
            10

        'L' ->
            11

        'M' ->
            12

        'N' ->
            13

        'O' ->
            14

        'P' ->
            15

        'Q' ->
            16

        'R' ->
            17

        'S' ->
            18

        'T' ->
            19

        'U' ->
            20

        'V' ->
            21

        'W' ->
            22

        'X' ->
            23

        'Y' ->
            24

        'Z' ->
            25

        'a' ->
            26

        'b' ->
            27

        'c' ->
            28

        'd' ->
            29

        'e' ->
            30

        'f' ->
            31

        'g' ->
            32

        'h' ->
            33

        'i' ->
            34

        'j' ->
            35

        'k' ->
            36

        'l' ->
            37

        'm' ->
            38

        'n' ->
            39

        'o' ->
            40

        'p' ->
            41

        'q' ->
            42

        'r' ->
            43

        's' ->
            44

        't' ->
            45

        'u' ->
            46

        'v' ->
            47

        'w' ->
            48

        'x' ->
            49

        'y' ->
            50

        'z' ->
            51

        '0' ->
            52

        '1' ->
            53

        '2' ->
            54

        '3' ->
            55

        '4' ->
            56

        '5' ->
            57

        '6' ->
            58

        '7' ->
            59

        '8' ->
            60

        '9' ->
            61

        '-' ->
            62

        _ ->
            -- ideally literal _
            63
