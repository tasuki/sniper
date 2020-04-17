module Main exposing (Domain, Model(..), Msg(..), baseUrl, getEndingSoon, init, main, subscriptions, update, view, viewModel)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


baseUrl =
    -- "https://www.namebase.io/api" sends bad cors headers
    "http://nb-proxy.tasuki.org/api"


type alias Domain =
    { name : String
    , bids : Int
    , reveal : Int
    , highestBid : Maybe Int
    }


type alias EndingSoon =
    { lastBlock : Int, domains : List Domain }


type Model
    = Failure
    | Loading
    | Success EndingSoon


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getEndingSoon )



-- UPDATE


type Msg
    = MorePlease
    | GotEndingSoon (Result Http.Error EndingSoon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getEndingSoon )

        GotEndingSoon result ->
            case result of
                Ok endingSoon ->
                    ( Success endingSoon, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Hello Happy Handshake Snipers" ]
        , viewModel model
        ]


viewModel : Model -> Html Msg
viewModel model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random cat for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success endingSoon ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , div [] [ text (String.fromInt endingSoon.lastBlock) ]
                ]



-- HTTP


getEndingSoon : Cmd Msg
getEndingSoon =
    Http.get
        { url = baseUrl ++ "/domains/ending-soon/0"
        , expect = Http.expectJson GotEndingSoon endingSoonDecoder
        }


endingSoonDecoder : D.Decoder EndingSoon
endingSoonDecoder =
    D.map2 EndingSoon
        (D.field "height" D.int)
        (D.field "domains" domainsDecoder)


domainsDecoder : D.Decoder (List Domain)
domainsDecoder =
    D.list <|
        D.map4 Domain
            (D.field "name" D.string)
            (D.field "total_number_bids" D.int)
            (D.field "reveal_block" D.int)
            (D.succeed Nothing)
