module Main exposing (Domain, Model(..), init, main, subscriptions, update, view, viewModel)

import ApiClient as AC
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- SETTINGS


blocksToDisplay =
    20



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Domain =
    { name : String
    , reveal : Int
    , bids : Int
    , highestBid : Maybe Int
    }


type alias DomainsAtBlock =
    { block : Int, domains : List Domain }


type alias State =
    { lastBlock : Int
    , domainsAtBlock : List DomainsAtBlock
    }


type Model
    = Failure
    | Loading
    | Success State


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, AC.getEndingSoon GotEndingSoon )


currentBlock : { a | lastBlock : Int } -> Int
currentBlock s =
    s.lastBlock + 1


nextBlock : { a | lastBlock : Int } -> Int
nextBlock s =
    currentBlock s + 1


timeLeft : State -> Int -> Int
timeLeft state block =
    (block - currentBlock state) * 10



-- UPDATE


type Msg
    = Refresh
    | GotEndingSoon AC.EndingSoonResult
    | GotDomainDetails AC.DomainDetailsResult


initiateState : AC.EndingSoon -> State
initiateState endingSoon =
    let
        toDomain : AC.EndingSoonDomain -> Domain
        toDomain esd =
            Domain esd.name esd.revealAt esd.bids Nothing

        getBlock : List AC.EndingSoonDomain -> Int -> DomainsAtBlock
        getBlock endingSoonDomains block =
            endingSoonDomains
                |> List.map toDomain
                |> List.filter (\d -> d.reveal == block)
                |> DomainsAtBlock block

        blocks : List Int
        blocks =
            List.range (nextBlock endingSoon) (nextBlock endingSoon + blocksToDisplay)

        domainsAtBlocks : List DomainsAtBlock
        domainsAtBlocks =
            List.map (getBlock endingSoon.domains) blocks
    in
    State endingSoon.lastBlock domainsAtBlocks


updateState : State -> AC.EndingSoon -> State
updateState state endingSoon =
    state


updateModelWithDomains : Model -> AC.EndingSoon -> Model
updateModelWithDomains model endingSoon =
    let
        state =
            case model of
                Loading ->
                    initiateState endingSoon

                Failure ->
                    initiateState endingSoon

                Success oldState ->
                    updateState oldState endingSoon
    in
    Success state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( Loading, AC.getEndingSoon GotEndingSoon )

        GotEndingSoon result ->
            case result of
                Ok endingSoon ->
                    ( updateModelWithDomains model endingSoon, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotDomainDetails result ->
            case result of
                Ok domainDetails ->
                    ( Loading, Cmd.none )

                Err _ ->
                    ( Loading, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "sniper" ]
        (viewModel model)


divWrap : Html Msg -> Html Msg
divWrap html =
    div [] <| [ html ]


viewDomain : Domain -> Html Msg
viewDomain d =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1-2 name" ]
            [ a [ href ("https://www.namebase.io/domains/" ++ d.name) ] [ text d.name ]
            ]
        , div [ class "pure-u-1-4 bids" ] [ text <| String.fromInt d.bids ]
        , div [ class "pure-u-1-4 highest" ]
            [ text <| Maybe.withDefault "" <| Maybe.map String.fromInt d.highestBid
            ]
        ]


viewSection : State -> DomainsAtBlock -> Html Msg
viewSection state dab =
    div [ class "pure-g section" ]
        [ div [ class "pure-u-5-24 block it gray" ]
            [ divWrap <| text <| "<" ++ (String.fromInt <| timeLeft state dab.block) ++ " min left" ]
        , div [ class "pure-u-3-24 block it gray" ] [ divWrap <| text <| String.fromInt <| dab.block ]
        , div [ class "pure-u-16-24 domains" ] (List.map viewDomain dab.domains)
        ]


viewModel : Model -> List (Html Msg)
viewModel model =
    case model of
        Loading ->
            [ h2 [] [ text "Hello Happy Handshake Snipers" ]
            , p [] [ text "Loading, this shouldn't take long..." ]
            ]

        Failure ->
            [ h2 [] [ text "Could not load the list of auctioned domains. Something's wrong :(" ]
            , button [ onClick Refresh ] [ text "Try Again!" ]
            ]

        Success state ->
            [ div [ class "pure-g topbar" ]
                [ div [ class "pure-u-1-3 block it gray" ]
                    [ divWrap <| text <| String.fromInt (currentBlock state)
                    ]
                , div [ class "pure-u-2-3 gray" ] [ text "<- currently mined block" ]
                ]
            , div [ class "pure-g header" ]
                [ div [ class "pure-u-1-3 block" ] [ divWrap <| text "Block" ]
                , div [ class "pure-u-1-3 name" ] [ text "Domain name" ]
                , div [ class "pure-u-1-6 bids" ] [ text "Bids" ]
                , div [ class "pure-u-1-6 highest" ] [ text "High bid" ]
                ]
            , div [] <| List.map (viewSection state) state.domainsAtBlock
            ]
