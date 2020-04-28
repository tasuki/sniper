module Main exposing (main)

import ApiClient as AC
import Browser
import Domains exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Set exposing (Set)
import Task
import Time
import Util



-- SETTINGS


blocksToDisplay =
    15



-- SUBSCRIPTIONS


seconds : number -> number
seconds secs =
    secs * 1000


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
    { lastBlock : Height
    , domainsAtBlock : List Block
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


chooseLastBlock : State -> Height -> Height
chooseLastBlock state =
    Basics.max state.lastBlock



-- UPDATE


type Msg
    = FetchEndingSoon
    | RefetchEndingSoon Time.Posix
    | GotEndingSoon Int AC.EndingSoonResult
    | ChooseDomainsToFetch Time.Posix
    | FetchDomains (List Domain)
    | GotDomainDetails AC.DomainDetailsResult
    | RemoveEndedBlocks



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
                |> List.reverse

        maxFetched : Int
        maxFetched =
            domains
                |> List.map (\d -> d.reveal)
                |> List.maximum
                |> Maybe.withDefault 0

        fetchNext : Cmd Msg
        fetchNext =
            if maxFetched <= currentBlock state.lastBlock + blocksToDisplay then
                getEndingPage (page + 1)

            else
                Cmd.none
    in
    Cmd.batch [ fetchDomains domainsToFetch, fetchNext ]


chooseDomainsToRefresh : Time.Posix -> State -> Cmd Msg
chooseDomainsToRefresh time state =
    let
        shouldGetDomain : Int -> ( Int, Domain ) -> Maybe Domain
        shouldGetDomain mins ( blocksLeft, domain ) =
            if Basics.modBy blocksLeft mins == 0 then
                Just domain

            else
                Nothing

        domainsWhoseTimeIsRipe : Int -> List Domain
        domainsWhoseTimeIsRipe mins =
            state.domainsAtBlock
                |> List.concatMap (\block -> block.domains)
                |> List.map (\d -> ( d.reveal - currentBlock state.lastBlock, d ))
                |> List.filterMap (shouldGetDomain mins)

        refreshDomains : List Domain
        refreshDomains =
            -- add 1 to have minutes 1-60 instead of 0-59
            domainsWhoseTimeIsRipe (1 + Time.toMinute Time.utc time)
    in
    Util.msgToCommand (FetchDomains refreshDomains)


removeEndedBlocks : State -> Cmd Msg
removeEndedBlocks state =
    if oldestBlockState state.domainsAtBlock == Hidden then
        Process.sleep (seconds 2) |> Task.perform (always RemoveEndedBlocks)

    else
        Cmd.none



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


updateStateEndingSoon : State -> Height -> List Domain -> State
updateStateEndingSoon state lastBlock newDomains =
    let
        firstBlock : Height
        firstBlock =
            List.head state.domainsAtBlock
                |> Maybe.map .height
                |> Maybe.withDefault (nextBlock lastBlock)

        newLastBlock : Height
        newLastBlock =
            chooseLastBlock state lastBlock

        oldDomains : List Domain
        oldDomains =
            List.concatMap .domains state.domainsAtBlock

        showBlocks : Height -> List Height
        showBlocks height =
            List.range firstBlock (currentBlock height + blocksToDisplay)

        allDomains : List Domain
        allDomains =
            mergeDomainLists oldDomains newDomains
    in
    State newLastBlock <|
        hideBlocks newLastBlock <|
            getDomainsAtBlocks allDomains (showBlocks newLastBlock)


updateStateDomainDetails : Height -> Domain -> State -> State
updateStateDomainDetails lastBlock domain state =
    let
        newLastBlock =
            chooseLastBlock state lastBlock
    in
    State newLastBlock <|
        hideBlocks newLastBlock <|
            replaceBlocks state.domainsAtBlock domain (updateDomain Refreshed)


setRefreshing : List Domain -> State -> State
setRefreshing domains state =
    let
        setState : Domain -> List Block -> List Block
        setState d blocks =
            replaceBlocks blocks d (setDomainState Refreshing)

        newDabs : List Block
        newDabs =
            List.foldl setState state.domainsAtBlock domains
    in
    { state | domainsAtBlock = newDabs }



-- model operations


getNewState : Model -> Height -> List Domain -> State
getNewState model =
    case model of
        Success oldState ->
            updateStateEndingSoon oldState

        _ ->
            updateStateEndingSoon initialState


mapSuccessfulModel : Model -> (State -> ( State, Cmd msg )) -> ( Model, Cmd msg )
mapSuccessfulModel model stateUpdate =
    case model of
        Success state ->
            let
                ( newState, cmd ) =
                    stateUpdate state
            in
            ( Success <| newState, cmd )

        _ ->
            ( Failure, Cmd.none )


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
                            Cmd.batch
                                [ updateEndingSoon newState page domains
                                , removeEndedBlocks newState
                                ]
                    in
                    ( Success newState, cmd )

                Err _ ->
                    ( Failure, Cmd.none )

        ChooseDomainsToFetch time ->
            mapSuccessfulModel model
                (\state ->
                    ( state
                    , chooseDomainsToRefresh time state
                    )
                )

        FetchDomains refreshDomains ->
            mapSuccessfulModel model
                (\state ->
                    ( setRefreshing refreshDomains state
                    , fetchDomains refreshDomains
                    )
                )

        GotDomainDetails result ->
            case result of
                Ok domainDetails ->
                    mapSuccessfulModel model
                        (\state ->
                            let
                                newState =
                                    updateStateDomainDetails
                                        domainDetails.lastBlock
                                        (detailsToDomain domainDetails)
                                        state
                            in
                            ( newState, removeEndedBlocks newState )
                        )

                Err _ ->
                    ( model, Cmd.none )

        RemoveEndedBlocks ->
            mapSuccessfulModel model
                (\state ->
                    ( { state
                        | domainsAtBlock =
                            removeHidden state.domainsAtBlock
                      }
                    , Cmd.none
                    )
                )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "sniper" ]
        (viewModel model)


viewModel : Model -> List (Html Msg)
viewModel model =
    case model of
        Loading ->
            [ h2 [] [ text "Hello Happy Handshake Snipers" ]
            , p [] [ text "Loading, this shouldn't take long..." ]
            ]

        Failure ->
            [ h2 [] [ text " Something's wrong :(" ]
            , p [] [ text "Could not load the list of auctioned domains." ]
            , p [] [ button [ onClick FetchEndingSoon ] [ text "Try Again!" ] ]
            ]

        Success state ->
            viewState state


viewState : State -> List (Html Msg)
viewState state =
    [ div [ class "pure-g topbar" ]
        [ div [ class <| classBlock ++ " it gray" ]
            [ Util.divWrap <| text <| String.fromInt state.lastBlock ]
        , div [ class <| classDomains ]
            [ Util.divWrapClass "domain-header gray" <| text "<- blockchain height" ]
        , div [ class <| classBlock ++ " it gray" ]
            [ Util.divWrap <| text <| String.fromInt (currentBlock state.lastBlock) ]
        , div [ class <| classDomains ]
            [ Util.divWrapClass "domain-header gray" <| text "<- currently mined block" ]
        ]
    , div [ class "pure-g header" ]
        [ div [ class classBlock ]
            [ Util.divWrap <| text "Block" ]
        , div [ class classDomains ]
            [ div [ class "pure-g domain-header" ]
                [ div [ class className ] [ text "Domain name" ]
                , div [ class classH ] [ Util.divWrap <| text "❤" ]
                , div [ class classG ] [ Util.divWrap <| text "G" ]
                , div [ class classBids ] [ text "Bids" ]
                , div [ class classHighest ] [ text "High bid" ]
                ]
            ]
        ]
    , div [] <| List.map (viewBlock state.lastBlock) state.domainsAtBlock
    ]
