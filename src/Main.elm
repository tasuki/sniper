module Main exposing (main)

import ApiClient as AC
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Set exposing (Set)
import Time
import Util



-- SETTINGS


blocksToDisplay =
    15



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
    , bids : Maybe Int
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


initialState =
    State 0 []


emptyDomain =
    Domain "" 0 Nothing Nothing


updateDomain : Domain -> Domain -> Domain
updateDomain old new =
    let
        bids =
            Util.maybeUpdateField .bids old new

        highestBid =
            Util.maybeUpdateField .highestBid old new
    in
    Domain new.name new.reveal bids highestBid


currentBlock : { a | lastBlock : Int } -> Int
currentBlock s =
    s.lastBlock + 1


nextBlock : Int -> Int
nextBlock lastBlock =
    lastBlock + 2


timeLeft : State -> Int -> Int
timeLeft state block =
    (block - currentBlock state) * 10



-- UPDATE


type Msg
    = FetchEndingSoon
    | RefetchEndingSoon Time.Posix
    | GotEndingSoon Int AC.EndingSoonResult
    | FetchDomainDetails Time.Posix
    | GotDomainDetails AC.DomainDetailsResult


getEnding : Cmd Msg
getEnding =
    getEndingPage 0


getEndingPage : Int -> Cmd Msg
getEndingPage page =
    AC.getEndingSoon (GotEndingSoon page) page


fetchDomains : List Domain -> Cmd Msg
fetchDomains domains =
    List.map .name domains
        |> List.map (AC.getDomainDetails GotDomainDetails)
        |> Cmd.batch


shouldGetDomain : Int -> ( Int, Domain ) -> Maybe Domain
shouldGetDomain mins ( seq, domain ) =
    if Basics.modBy seq mins == 0 then
        Just domain

    else
        Nothing


getDomainsWhoseTimeIsRipe : State -> Int -> Cmd Msg
getDomainsWhoseTimeIsRipe state mins =
    List.concatMap (\dab -> dab.domains) state.domainsAtBlock
        |> List.map (\d -> ( d.reveal - currentBlock state, d ))
        |> List.filterMap (shouldGetDomain mins)
        |> fetchDomains


initiateState : AC.EndingSoon -> State
initiateState endingSoon =
    updateStateEndingSoon initialState endingSoon


chooseLastBlock : State -> Int -> Int
chooseLastBlock state new =
    Basics.max state.lastBlock new


endingSoonToDomains : AC.EndingSoon -> List Domain
endingSoonToDomains es =
    let
        toDomain : AC.EndingSoonDomain -> Domain
        toDomain esd =
            Domain esd.name esd.revealAt (Just esd.bids) Nothing
    in
    List.map toDomain es.domains


updateStateEndingSoon : State -> AC.EndingSoon -> State
updateStateEndingSoon state endingSoon =
    let
        newLastBlock =
            chooseLastBlock state endingSoon.lastBlock

        showBlocks : Int -> List Int
        showBlocks block =
            List.range (nextBlock block) (nextBlock block + blocksToDisplay - 1)

        getBlock : List Domain -> Int -> DomainsAtBlock
        getBlock domains block =
            domains |> List.filter (\d -> d.reveal == block) |> DomainsAtBlock block

        getDomainsAtBlocks : List Domain -> List Int -> List DomainsAtBlock
        getDomainsAtBlocks domains blocks =
            List.map (getBlock domains) blocks

        domainsToDict : List Domain -> Dict String Domain
        domainsToDict domains =
            List.map (\d -> ( d.name, d )) domains |> Dict.fromList

        oldDomains : List Domain
        oldDomains =
            List.concatMap .domains state.domainsAtBlock

        newDomains : List Domain
        newDomains =
            endingSoonToDomains endingSoon

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
            Set.toList allDomainNames |> List.map maybeUpdateDomain |> List.sortBy .name
    in
    getDomainsAtBlocks allDomains (showBlocks newLastBlock)
        |> State newLastBlock


updateModelEndingSoon : Model -> AC.EndingSoon -> State
updateModelEndingSoon model endingSoon =
    case model of
        Loading ->
            initiateState endingSoon

        Failure ->
            initiateState endingSoon

        Success oldState ->
            updateStateEndingSoon oldState endingSoon


replaceDomain : DomainsAtBlock -> Domain -> DomainsAtBlock
replaceDomain dab newDomain =
    case Util.splitOut (\d -> d.name == newDomain.name) dab.domains of
        Nothing ->
            dab

        Just ( before, oldDomain, after ) ->
            DomainsAtBlock dab.block (before ++ updateDomain oldDomain newDomain :: after)


updateStateDomainDetails : State -> Int -> Domain -> State
updateStateDomainDetails state lastBlock domain =
    let
        newLastBlock =
            chooseLastBlock state lastBlock

        newDabs : List DomainsAtBlock
        newDabs =
            case Util.splitOut (\dab -> dab.block == domain.reveal) state.domainsAtBlock of
                Nothing ->
                    state.domainsAtBlock

                Just ( before, found, after ) ->
                    before ++ replaceDomain found domain :: after
    in
    State newLastBlock <|
        List.Extra.dropWhile (\dab -> dab.block < nextBlock newLastBlock) newDabs


updateModelDomainDetails : Model -> AC.DomainDetails -> Model
updateModelDomainDetails model domainDetails =
    let
        toDomain : AC.DomainDetails -> Domain
        toDomain d =
            Domain d.name d.revealAt (Just <| List.length d.bids) (Just d.highestBid)
    in
    case model of
        Success state ->
            Success <|
                updateStateDomainDetails
                    state
                    domainDetails.lastBlock
                    (toDomain domainDetails)

        _ ->
            -- shouldn't be calling this when no state
            Failure


updateEndingSoon : State -> Int -> AC.EndingSoon -> ( Model, Cmd Msg )
updateEndingSoon state page endingSoon =
    let
        domainsWithoutHighest : Set String
        domainsWithoutHighest =
            List.concatMap .domains state.domainsAtBlock
                |> List.filter (\d -> d.highestBid == Nothing)
                |> List.map .name
                |> Set.fromList

        domainsToFetch : List Domain
        domainsToFetch =
            endingSoonToDomains endingSoon
                |> List.filter (\d -> Set.member d.name domainsWithoutHighest)

        maxFetched : Int
        maxFetched =
            endingSoon.domains
                |> List.map (\d -> d.revealAt)
                |> List.maximum
                |> Maybe.withDefault 0

        fetchNext : Cmd Msg
        fetchNext =
            if maxFetched <= currentBlock state + blocksToDisplay then
                getEndingPage (page + 1)

            else
                Cmd.none
    in
    ( Success <| state
    , Cmd.batch
        [ fetchDomains domainsToFetch, fetchNext ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchEndingSoon ->
            ( Loading, getEnding )

        RefetchEndingSoon _ ->
            ( model, getEnding )

        GotEndingSoon page result ->
            case result of
                Ok endingSoon ->
                    updateEndingSoon (updateModelEndingSoon model endingSoon) page endingSoon

                Err _ ->
                    ( Failure, Cmd.none )

        FetchDomainDetails time ->
            case model of
                Success state ->
                    -- add 1 to have minutes 1-60 instead of 0-59
                    ( model, getDomainsWhoseTimeIsRipe state (1 + Time.toMinute Time.utc time) )

                _ ->
                    ( model, Cmd.none )

        GotDomainDetails result ->
            case result of
                Ok domainDetails ->
                    ( updateModelDomainDetails model domainDetails, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


minutes : number -> number
minutes min =
    min * 60 * 1000


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (minutes 10) RefetchEndingSoon
        , Time.every (minutes 1) FetchDomainDetails
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


displayMaybeNumber : Maybe Int -> String
displayMaybeNumber maybeN =
    Maybe.withDefault "" <| Maybe.map String.fromInt maybeN


displayBid : Maybe Int -> String
displayBid maybeBid =
    let
        bid =
            Maybe.map (\n -> n // 10000) maybeBid
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        integer =
            String.slice 0 -2 bid

        decimal =
            String.right 2 bid
    in
    if String.length decimal == 2 then
        integer ++ "." ++ decimal

    else
        ""


viewDomain : Domain -> Html Msg
viewDomain d =
    div [ class "pure-g" ]
        [ div [ class nameClass ]
            [ a [ href ("https://www.namebase.io/domains/" ++ d.name) ] [ text d.name ]
            ]
        , div [ class bidsClass ] [ text <| displayMaybeNumber d.bids ]
        , div [ class highestClass ] [ text <| displayBid d.highestBid ]
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
            , button [ onClick FetchEndingSoon ] [ text "Try Again!" ]
            ]

        Success state ->
            [ div [ class "pure-g topbar" ]
                [ div [ class "pure-u-10-24 block it gray" ]
                    [ divWrap <| text <| String.fromInt state.lastBlock
                    ]
                , div [ class "pure-u-14-24 gray" ] [ text "<- blockchain height" ]
                , div [ class "pure-u-10-24 block it gray" ]
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
