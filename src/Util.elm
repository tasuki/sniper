module Util exposing
    ( divWrap
    , divWrapClass
    , msgToCommand
    , msgToCommandAfter
    , splitOut
    , takeHigher
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List.Extra
import Process
import Task


splitOut : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
splitOut predicate =
    let
        helper : ( List a, List a ) -> Maybe ( List a, a, List a )
        helper ( a, b ) =
            case ( a, b ) of
                ( la, n :: lb ) ->
                    Just ( la, n, lb )

                _ ->
                    Nothing
    in
    List.Extra.splitWhen predicate >> Maybe.andThen helper


takeHigher : (a -> Maybe comparable) -> a -> a -> Maybe comparable
takeHigher field old new =
    case ( field old, field new ) of
        ( Just fOld, Just fNew ) ->
            Just (max fOld fNew)

        ( Just f, Nothing ) ->
            Just f

        ( _, f ) ->
            f


msgToCommandAfter : Float -> msg -> Cmd msg
msgToCommandAfter seconds m =
    Process.sleep (1000 * seconds) |> Task.perform (always m)


msgToCommand : msg -> Cmd msg
msgToCommand =
    Task.succeed >> Task.perform identity


divWrap : Html msg -> Html msg
divWrap html =
    div [] <| [ html ]


divWrapClass : String -> Html msg -> Html msg
divWrapClass cls html =
    div [ class <| cls ] <| [ html ]
