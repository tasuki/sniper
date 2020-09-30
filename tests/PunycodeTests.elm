module PunycodeTests exposing (..)

import Expect
import Punycode exposing (..)
import Test exposing (..)


type alias Case =
    ( String, String, String )



-- core functionality tests


cases : List Case
cases =
    -- taken from https://en.wikipedia.org/wiki/Punycode, believed to be correct
    [ ( "", "", "The empty string." )
    , ( "a", "a-", "Only ASCII characters, one, lowercase." )
    , ( "A", "A-", "Only ASCII characters, one, uppercase." )
    , ( "3", "3-", "Only ASCII characters, one, a digit." )
    , ( "-", "--", "Only ASCII characters, one, a hyphen." )
    , ( "--", "---", "Only ASCII characters, two hyphens." )
    , ( "London", "London-", "Only ASCII characters, more than one, no hyphens." )
    , ( "Lloyd-Atkinson", "Lloyd-Atkinson-", "Only ASCII characters, one hyphen." )
    , ( "This has spaces", "This has spaces-", "Only ASCII characters, with spaces." )
    , ( "-> $1.00 <-", "-> $1.00 <--", "Only ASCII characters, mixed symbols." )
    , ( "ü", "tda", "No ASCII characters, one Latin-1 Supplement character." )
    , ( "α", "mxa", "No ASCII characters, one Greek character." )
    , ( "例", "fsq", "No ASCII characters, one CJK character." )
    , ( "😉", "n28h", "No ASCII characters, one emoji character." )
    , ( "αβγ", "mxacd", "No ASCII characters, more than one character." )
    , ( "München", "Mnchen-3ya", "Mixed string, with one character that is not an ASCII character." )
    , ( "Mnchen-3ya", "Mnchen-3ya-", "Only ASCII characters, equal to the Punycode of München (effectively encoding München twice)." )
    , ( "München-Ost", "Mnchen-Ost-9db", "Mixed string, with one character that is not ASCII, and a hyphen." )
    , ( "Bahnhof München-Ost", "Bahnhof Mnchen-Ost-u6b", "Mixed string, with one space, one hyphen, and one character that is not ASCII." )
    , ( "abæcdöef", "abcdef-qua4k", "Mixed string, two non-ASCII characters." )
    , ( "правда", "80aafi6cg", "Russian, without ASCII." )
    , ( "ยจฆฟคฏข", "22cdfh1b8fsa", "Thai, without ASCII." )
    , ( "도메인", "hq1bm8jm9l", "Korean, without ASCII." )
    , ( "ドメイン名例", "eckwd4c7cu47r2wf", "Japanese, without ASCII." )
    , ( "MajiでKoiする5秒前", "MajiKoi5-783gue6qz075azm5e", "Japanese with ASCII." )
    , ( "「bücher」", "bcher-kva8445foa", "Mixed non-ASCII scripts (Latin-1 Supplement and CJK)." )
    ]


canDecode : Case -> Test
canDecode testCase =
    case testCase of
        ( decoded, encoded, comment ) ->
            test ("Can decode " ++ encoded) <|
                \_ -> Expect.equal (decode encoded) decoded


decodeTest =
    cases |> List.map canDecode |> describe "decode"



-- adjacent tests


decodeNumberCases =
    [ ( ( "9DB", 0, 72 ), Ok ( 3, 1365 ) )
    , ( ( "ECKWD4C7CU47R2WF", 0, 72 ), Ok ( 3, 12324 ) )
    , ( ( "ECKWD4C7CU47R2WF", 3, 17 ), Ok ( 5, 73 ) )
    , ( ( "ECKWD4C7CU47R2WF", 5, 21 ), Ok ( 7, 72 ) )
    , ( ( "ECKWD4C7CU47R2WF", 7, 20 ), Ok ( 9, 73 ) )
    , ( ( "ECKWD4C7CU47R2WF", 9, 19 ), Ok ( 13, 39160 ) )
    , ( ( "ECKWD4C7CU47R2WF", 13, 84 ), Ok ( 16, 6923 ) )
    ]


canDecodeNumber : ( ( String, Int, Int ), GeneralizedNumberResult ) -> Test
canDecodeNumber testCase =
    case testCase of
        ( ( extended, extpos, bias ), expected ) ->
            test ("Decodes number " ++ extended ++ " " ++ String.fromInt extpos) <|
                \_ -> Expect.equal (decodeGeneralizedNumber (String.toList extended) extpos bias) expected


decodeNumberTest =
    decodeNumberCases |> List.map canDecodeNumber |> describe "decode number"
