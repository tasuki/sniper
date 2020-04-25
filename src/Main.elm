module Main exposing (main)

import ApiClient as AC
import Browser
import Domains
    exposing
        ( Domain
        , DomainUpdate
        , DomainsAtBlock
        , ElementState(..)
        , domainsWithoutHighestBid
        , getDomainsAtBlocks
        , mergeDomainLists
        , replaceDabs
        , setDomainState
        , updateDomain
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Set exposing (Set)
import Task
import Time



-- SETTINGS


blocksToDisplay =
    15



-- SUBSCRIPTIONS


minutes : number -> number
minutes min =
    min * 60 * 1000


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (minutes 10) RefetchEndingSoon
        , Time.every (minutes 1) ChooseDomainsToFetch
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


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


currentBlock : { a | lastBlock : Int } -> Int
currentBlock s =
    s.lastBlock + 1


nextBlock : Int -> Int
nextBlock lastBlock =
    lastBlock + 2


chooseLastBlock : State -> Int -> Int
chooseLastBlock state =
    Basics.max state.lastBlock


timeLeft : State -> Int -> Int
timeLeft state block =
    (block - currentBlock state) * 10



-- UPDATE


type Msg
    = FetchEndingSoon
    | RefetchEndingSoon Time.Posix
    | GotEndingSoon Int AC.EndingSoonResult
    | ChooseDomainsToFetch Time.Posix
    | FetchDomains (List Domain)
    | GotDomainDetails AC.DomainDetailsResult



-- commands


getEnding : Cmd Msg
getEnding =
    getEndingPage 0


getEndingPage : Int -> Cmd Msg
getEndingPage page =
    AC.getEndingSoon (GotEndingSoon page) page


fetchDomains : List Domain -> Cmd Msg
fetchDomains =
    List.map .name
        >> List.map (AC.getDomainDetails GotDomainDetails)
        >> Cmd.batch


updateEndingSoon : State -> Int -> List Domain -> Cmd Msg
updateEndingSoon state page domains =
    let
        domainsWithoutHighest : Set String
        domainsWithoutHighest =
            domainsWithoutHighestBid state.domainsAtBlock
                |> List.map .name
                |> Set.fromList

        domainsToFetch : List Domain
        domainsToFetch =
            List.filter (\d -> Set.member d.name domainsWithoutHighest) domains

        maxFetched : Int
        maxFetched =
            domains
                |> List.map (\d -> d.reveal)
                |> List.maximum
                |> Maybe.withDefault 0

        fetchNext : Cmd Msg
        fetchNext =
            if maxFetched <= currentBlock state + blocksToDisplay then
                getEndingPage (page + 1)

            else
                Cmd.none
    in
    Cmd.batch [ fetchDomains domainsToFetch, fetchNext ]



-- convert from api to domain


endingSoonToDomains : AC.EndingSoon -> List Domain
endingSoonToDomains es =
    let
        toDomain : AC.EndingSoonDomain -> Domain
        toDomain esd =
            Domain esd.name esd.revealAt (Just esd.bids) Nothing New
    in
    List.map toDomain es.domains


detailsToDomain : AC.DomainDetails -> Domain
detailsToDomain d =
    Domain
        d.name
        d.revealAt
        (Just <| List.length d.bids)
        (Just d.highestBid)
        Refreshed



-- state operations


domainsWhoseTimeIsRipe : State -> Int -> List Domain
domainsWhoseTimeIsRipe state mins =
    let
        shouldGetDomain : ( Int, Domain ) -> Maybe Domain
        shouldGetDomain ( seq, domain ) =
            if Basics.modBy seq mins == 0 then
                Just domain

            else
                Nothing
    in
    List.concatMap (\dab -> dab.domains) state.domainsAtBlock
        |> List.map (\d -> ( d.reveal - currentBlock state, d ))
        |> List.filterMap shouldGetDomain


updateStateEndingSoon : State -> Int -> List Domain -> State
updateStateEndingSoon state lastBlock newDomains =
    let
        newLastBlock =
            chooseLastBlock state lastBlock

        showBlocks : Int -> List Int
        showBlocks block =
            List.range (nextBlock block) (nextBlock block + blocksToDisplay - 1)

        oldDomains : List Domain
        oldDomains =
            List.concatMap .domains state.domainsAtBlock

        allDomains : List Domain
        allDomains =
            mergeDomainLists oldDomains newDomains
    in
    getDomainsAtBlocks allDomains (showBlocks newLastBlock)
        |> State newLastBlock


updateStateDomainDetails : DomainUpdate -> Int -> Domain -> State -> State
updateStateDomainDetails updateFun lastBlock domain state =
    State lastBlock <|
        List.Extra.dropWhile
            (\dab -> dab.block < nextBlock lastBlock)
            (replaceDabs state.domainsAtBlock domain updateFun)


setRefreshing : List Domain -> State -> State
setRefreshing domains state =
    let
        setState : Domain -> List DomainsAtBlock -> List DomainsAtBlock
        setState d dab =
            replaceDabs dab d (setDomainState Refreshing)

        newDabs : List DomainsAtBlock
        newDabs =
            List.foldl setState state.domainsAtBlock domains
    in
    { state | domainsAtBlock = newDabs }



-- model operations


getNewState : Model -> Int -> List Domain -> State
getNewState model =
    case model of
        Success oldState ->
            updateStateEndingSoon oldState

        _ ->
            updateStateEndingSoon initialState


mapModel : Model -> (State -> State) -> Model
mapModel model fun =
    case model of
        Success state ->
            Success <| fun state

        _ ->
            Failure


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
                    let
                        domains : List Domain
                        domains =
                            endingSoonToDomains endingSoon

                        newState : State
                        newState =
                            getNewState model endingSoon.lastBlock domains

                        cmd : Cmd Msg
                        cmd =
                            updateEndingSoon newState page domains
                    in
                    ( Success newState, cmd )

                Err _ ->
                    ( Failure, Cmd.none )

        ChooseDomainsToFetch time ->
            case model of
                Success state ->
                    let
                        refreshDomains : List Domain
                        refreshDomains =
                            -- add 1 to have minutes 1-60 instead of 0-59
                            domainsWhoseTimeIsRipe state
                                (1 + Time.toMinute Time.utc time)
                    in
                    ( model
                    , Task.perform
                        (always (FetchDomains refreshDomains))
                        (Task.succeed ())
                    )

                _ ->
                    ( model, Cmd.none )

        FetchDomains refreshDomains ->
            case model of
                Success state ->
                    ( Success <| setRefreshing refreshDomains state, fetchDomains refreshDomains )

                _ ->
                    ( model, Cmd.none )

        GotDomainDetails result ->
            case result of
                Ok domainDetails ->
                    ( mapModel model
                        (\state ->
                            updateStateDomainDetails
                                (updateDomain Refreshed)
                                (chooseLastBlock state domainDetails.lastBlock)
                                (detailsToDomain domainDetails)
                                state
                        )
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



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
displayMaybeNumber =
    Maybe.map String.fromInt >> Maybe.withDefault ""


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


domainState : Domain -> String
domainState d =
    case d.state of
        New ->
            ""

        Refreshing ->
            ""

        Refreshed ->
            "shake"

        Hidden ->
            ""


viewDomain : Domain -> Html Msg
viewDomain d =
    div [ class ("pure-g domain " ++ domainState d) ]
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
            [ divWrap <|
                text <|
                    "<"
                        ++ (String.fromInt <| timeLeft state dab.block)
                        ++ " min left"
            ]
        , div [ class "pure-u-4-24 block it gray" ]
            [ divWrap <| text <| String.fromInt <| dab.block ]
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
            [ h2 []
                [ text "Could not load the list of auctioned domains. Something's wrong :("
                ]
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
