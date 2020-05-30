module Base64Tests exposing (..)

import Base64 exposing (Data, SixBit, decode, encode)
import Debug exposing (toString)
import Expect
import Test exposing (..)


cases : List ( Data, String )
cases =
    [ ( [], "" )
    , ( [ SixBit 0 0 0 0 0 0 ], "A" )
    , ( [ SixBit 0 0 0 1 1 1 ], "H" )
    , ( [ SixBit 0 1 0 1 0 1 ], "V" )
    , ( [ SixBit 1 0 1 0 1 0 ], "q" )
    , ( [ SixBit 1 1 1 0 0 0 ], "4" )
    , ( [ SixBit 1 1 1 1 1 1 ], "_" )
    , ( [ SixBit 0 0 0 0 0 0
        , SixBit 1 0 0 0 0 1
        , SixBit 1 0 1 0 0 0
        , SixBit 1 0 0 0 1 1
        ]
      , "Ahoj"
      )
    ]


canEncode : ( Data, String ) -> Test
canEncode inout =
    case inout of
        ( bits, str ) ->
            test ("Can encode " ++ toString bits) <|
                \_ -> Expect.equal (encode bits) str


canDecode : ( Data, String ) -> Test
canDecode inout =
    case inout of
        ( bits, str ) ->
            test ("Can decode " ++ str) <|
                \_ -> Expect.equal (decode str) bits


encodeTest =
    cases
        |> List.map canEncode
        |> describe "encode"


decodeTest =
    cases
        |> List.map canDecode
        |> describe "decode"
