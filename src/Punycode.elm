module Punycode exposing (..)

import String.Extra


tmin =
    1


tmax =
    26


baseConstant =
    36


skew =
    38


damp =
    700


initial_n =
    128


initial_bias =
    72


getT : Int -> Int -> Int
getT j bias =
    let
        res =
            baseConstant * (j + 1) - bias
    in
    if res < tmin then
        tmin

    else if res > tmax then
        tmax

    else
        res


type alias GeneralizedNumberResult =
    Result String ( Int, Int )


decodeGeneralizedNumberHelper : Int -> Int -> Int -> List Char -> Int -> Int -> GeneralizedNumberResult
decodeGeneralizedNumberHelper result w j extended extpos bias =
    let
        char : Maybe Char
        char =
            List.head <| List.drop extpos extended

        t : Int
        t =
            getT j bias

        toOrd : Int -> Maybe Int
        toOrd c =
            if 0x41 <= c && c <= 0x5A then
                -- A-Z
                Just (c - 0x41)

            else if 0x30 <= c && c <= 0x39 then
                -- 0-9
                Just (c - 0x16)

            else
                Nothing

        digit : Maybe Int
        digit =
            Maybe.andThen toOrd <| Maybe.map Char.toCode char

        getResult : Int -> Int -> Int -> GeneralizedNumberResult
        getResult d newPos newResult =
            if d < t then
                Ok ( newPos, newResult )

            else
                decodeGeneralizedNumberHelper
                    newResult
                    (w * (baseConstant - t))
                    (j + 1)
                    extended
                    newPos
                    bias
    in
    case digit of
        Just d ->
            getResult d (extpos + 1) (result + d * w)

        Nothing ->
            Err "borked"


decodeGeneralizedNumber : List Char -> Int -> Int -> GeneralizedNumberResult
decodeGeneralizedNumber extended extpos bias =
    decodeGeneralizedNumberHelper 0 1 0 extended extpos bias


reduceDelta : Int -> Int -> ( Int, Int )
reduceDelta delta divisions =
    if delta > ((baseConstant - tmin) * tmax) // 2 then
        reduceDelta (delta // (baseConstant - tmin)) (divisions + baseConstant)

    else
        ( delta, divisions )


adapt : Int -> Bool -> Int -> Int
adapt delta first numchars =
    let
        divisor =
            if first then
                damp

            else
                2

        intermediateDelta =
            delta // divisor + delta // divisor // numchars

        ( newDelta, divisions ) =
            reduceDelta intermediateDelta 0
    in
    divisions + ((baseConstant - tmin + 1) * newDelta // (newDelta + skew))


insertionSortHelper : List Char -> List Char -> Int -> Int -> Int -> Int -> List Char
insertionSortHelper base extended char pos bias extpos =
    let
        recurse nextpos delta =
            let
                intermediatePos =
                    pos + delta + 1

                onePlus =
                    1 + List.length base

                newChar =
                    char + (intermediatePos // onePlus)

                newPos =
                    modBy onePlus intermediatePos

                newBase =
                    List.take newPos base ++ [ Char.fromCode newChar ] ++ List.drop newPos base

                newBias =
                    adapt delta (extpos == 0) (List.length newBase)
            in
            insertionSortHelper newBase extended newChar newPos newBias nextpos
    in
    if extpos < List.length extended then
        case decodeGeneralizedNumber extended extpos bias of
            Ok ( nextpos, delta ) ->
                recurse nextpos delta

            Err _ ->
                base

    else
        base


insertionSort : String -> String -> String
insertionSort base extended =
    String.fromList <|
        insertionSortHelper
            (String.toList base)
            (String.toList extended)
            0x80
            -1
            72
            0


decode : String -> String
decode inputStr =
    let
        base =
            String.Extra.leftOfBack "-" inputStr

        extensionStart =
            if String.contains "-" inputStr then
                (+) 1 <| String.length base

            else
                0

        extended =
            String.dropLeft extensionStart <| String.toUpper inputStr
    in
    insertionSort base extended
