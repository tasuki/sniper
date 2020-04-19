module Main exposing
    ( Domain
    , Model(..)
    , init
    , main
    , subscriptions
    , update
    , view
    , viewModel
    )

import ApiClient as AC
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)
import Time



-- SETTINGS


blocksToDisplay =
    20


pagesToFetch =
    5



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
    ( Loading, getEnding )


emptyDomain =
    Domain "" 0 0 Nothing


updateDomain : Domain -> Domain -> Domain
updateDomain old new =
    let
        highest =
            case ( old.highestBid, new.highestBid ) of
                ( Just bid, Nothing ) ->
                    Just bid

                ( _, bid ) ->
                    bid
    in
    Domain new.name new.reveal new.bids highest


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
    = Fetch
    | Refetch Time.Posix
    | GotEndingSoon AC.EndingSoonResult
    | GotDomainDetails AC.DomainDetailsResult


getEndingSoon : Int -> Cmd Msg
getEndingSoon page =
    AC.getEndingSoon page GotEndingSoon


getEnding : Cmd Msg
getEnding =
    Cmd.batch <|
        List.map getEndingSoon (List.range 0 pagesToFetch |> List.reverse)


toDomains : AC.EndingSoon -> List Domain
toDomains es =
    let
        toDomain : AC.EndingSoonDomain -> Domain
        toDomain esd =
            Domain esd.name esd.revealAt esd.bids Nothing
    in
    List.map toDomain es.domains


showBlocks : AC.EndingSoon -> List Int
showBlocks es =
    List.range (nextBlock es) (nextBlock es + blocksToDisplay)


getBlock : List Domain -> Int -> DomainsAtBlock
getBlock domains block =
    domains |> List.filter (\d -> d.reveal == block) |> DomainsAtBlock block


getDomainsAtBlocks : List Domain -> List Int -> List DomainsAtBlock
getDomainsAtBlocks domains blocks =
    List.map (domains |> getBlock) blocks


initiateState : AC.EndingSoon -> State
initiateState endingSoon =
    getDomainsAtBlocks (toDomains endingSoon) (showBlocks endingSoon)
        |> State endingSoon.lastBlock


domainsToDict : List Domain -> Dict String Domain
domainsToDict domains =
    List.map (\d -> ( d.name, d )) domains |> Dict.fromList


updateState : State -> AC.EndingSoon -> State
updateState state endingSoon =
    let
        oldDomains : List Domain
        oldDomains =
            List.concatMap .domains state.domainsAtBlock

        newDomains : List Domain
        newDomains =
            toDomains endingSoon

        oldDomainsDict : Dict String Domain
        oldDomainsDict =
            domainsToDict oldDomains

        newDomainsDict : Dict String Domain
        newDomainsDict =
            domainsToDict newDomains

        allDomainNames : Set String
        allDomainNames =
            oldDomains ++ newDomains |> List.map (\d -> d.name) |> Set.fromList

        maybeUpdateDomain : String -> Domain
        maybeUpdateDomain name =
            case ( Dict.get name oldDomainsDict, Dict.get name newDomainsDict ) of
                ( Just old, Just new ) ->
                    updateDomain old new

                ( Just old, _ ) ->
                    old

                ( _, Just new ) ->
                    new

                ( _, _ ) ->
                    emptyDomain

        allDomains : List Domain
        allDomains =
            Set.toList allDomainNames |> List.map maybeUpdateDomain
    in
    getDomainsAtBlocks allDomains (showBlocks endingSoon)
        |> State endingSoon.lastBlock


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
        Fetch ->
            ( Loading, getEnding )

        Refetch _ ->
            ( model, getEnding )

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
    Sub.batch
        -- every three minutes
        [ Time.every (3 * 60 * 1000) Refetch
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "sniper" ]
        (viewModel model)


divWrap : Html Msg -> Html Msg
divWrap html =
    div [] <| [ html ]


nameClass =
    "pure-u-14-24 name"


bidsClass =
    "pure-u-4-24 bids"


highestClass =
    "pure-u-6-24 highest"


viewDomain : Domain -> Html Msg
viewDomain d =
    div [ class "pure-g" ]
        [ div [ class nameClass ]
            [ a [ href ("https://www.namebase.io/domains/" ++ d.name) ] [ text d.name ]
            ]
        , div [ class bidsClass ] [ text <| String.fromInt d.bids ]
        , div [ class highestClass ]
            [ text <| Maybe.withDefault "" <| Maybe.map String.fromInt d.highestBid
            ]
        ]


viewSection : State -> DomainsAtBlock -> Html Msg
viewSection state dab =
    div [ class "pure-g section" ]
        [ div [ class "pure-u-6-24 block it gray" ]
            [ divWrap <| text <| "<" ++ (String.fromInt <| timeLeft state dab.block) ++ " min left" ]
        , div [ class "pure-u-4-24 block it gray" ] [ divWrap <| text <| String.fromInt <| dab.block ]
        , div [ class "pure-u-14-24 domains" ] (List.map viewDomain dab.domains)
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
            , button [ onClick Fetch ] [ text "Try Again!" ]
            ]

        Success state ->
            [ div [ class "pure-g topbar" ]
                [ div [ class "pure-u-10-24 block it gray" ]
                    [ divWrap <| text <| String.fromInt (currentBlock state)
                    ]
                , div [ class "pure-u-14-24 gray" ] [ text "<- currently mined block" ]
                ]
            , div [ class "pure-g header" ]
                [ div [ class "pure-u-10-24 block" ] [ divWrap <| text "Block" ]
                , div [ class "pure-u-14-24" ]
                    [ div [ class nameClass ] [ text "Domain name" ]
                    , div [ class bidsClass ] [ text "Bids" ]
                    , div [ class highestClass ] [ text "High bid" ]
                    ]
                ]
            , div [] <| List.map (viewSection state) state.domainsAtBlock
            ]
