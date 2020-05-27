module Main exposing (main)

import ApiClient as AC
import Browser
import Domains exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)
import Task
import Time
import Util



-- SETTINGS


blocksToDisplay =
    15



-- SUBSCRIPTIONS


intervalFromSeconds : number -> number
intervalFromSeconds secs =
    secs * 1000


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (intervalFromSeconds 3) Tick
        , Time.every (intervalFromSeconds 60) ChooseDomainsToFetch
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
    , blocks : List Block
    }


type Status
    = Initial
    | Loading
    | Failure
    | Success


type alias Model =
    { status : Status
    , state : State
    , refreshed : Time.Posix
    }


initialState : State
initialState =
    State 0 []


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Initial initialState (Time.millisToPosix 0), Task.perform Tick Time.now )


chooseLastBlock : State -> Height -> Height
chooseLastBlock state =
    Basics.max state.lastBlock



-- UPDATE


type Msg
    = FetchEndingSoon
    | Tick Time.Posix
    | GotEndingSoon Int AC.EndingSoonResult
    | ChooseDomainsToFetch Time.Posix
    | FetchDomains (List Domain)
    | GotDomainDetails AC.DomainDetailsResult
    | RemoveRefreshing Domain
    | RemoveEndedBlocks
    | FlipFave Domain
    | ShowFaves Block
    | HideFaves Block



-- commands


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
            domainsWithoutHighestBid state.blocks
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
            state.blocks
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
    if oldestBlockState state.blocks == Hidden then
        Util.msgToCommandAfter 2 RemoveEndedBlocks

    else
        Cmd.none



-- convert from api to domain


endingSoonToDomains : AC.EndingSoon -> List Domain
endingSoonToDomains es =
    let
        toDomain : AC.EndingSoonDomain -> Domain
        toDomain esd =
            Domain
                esd.name
                esd.revealAt
                (Just esd.bids)
                Nothing
                Regular
                False
    in
    List.map toDomain es.domains


detailsToDomain : AC.DomainDetails -> Domain
detailsToDomain d =
    let
        maxFromBids : Int
        maxFromBids =
            List.map .stakeAmount d.bids
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Domain
        d.name
        d.revealAt
        (Just <| List.length d.bids)
        (Just <| Basics.max maxFromBids d.highestBid)
        Refreshed
        False



-- state operations


updateStateEndingSoon : State -> Height -> List Domain -> State
updateStateEndingSoon state lastBlock newDomains =
    let
        firstBlock : Height
        firstBlock =
            List.head state.blocks
                |> Maybe.map .height
                |> Maybe.withDefault (nextBlock lastBlock)

        newLastBlock : Height
        newLastBlock =
            chooseLastBlock state lastBlock

        oldDomains : List Domain
        oldDomains =
            List.concatMap .domains state.blocks

        showBlocks : Height -> List Height
        showBlocks height =
            List.range firstBlock (currentBlock height + blocksToDisplay)

        allDomains : List Domain
        allDomains =
            mergeDomainLists oldDomains newDomains
    in
    State newLastBlock <|
        hideBlocks newLastBlock <|
            getDomainsAtBlocks state.blocks allDomains (showBlocks newLastBlock)


updateStateDomainDetails : Height -> Domain -> State -> State
updateStateDomainDetails lastBlock domain state =
    let
        newLastBlock =
            chooseLastBlock state lastBlock
    in
    State newLastBlock <|
        hideBlocks newLastBlock <|
            replaceBlocks state.blocks domain (updateDomain Refreshed)


setRefreshing : List Domain -> State -> State
setRefreshing domains state =
    let
        setState : Domain -> List Block -> List Block
        setState d blocks =
            replaceBlocks blocks d (updateDomain Refreshing)

        newBlocks : List Block
        newBlocks =
            List.foldl setState state.blocks domains
    in
    { state | blocks = newBlocks }


flipFave : Domain -> State -> State
flipFave domain state =
    let
        newBlocks : List Block
        newBlocks =
            replaceSortedBlocks state.blocks domain flipFaveFlag
    in
    { state | blocks = newBlocks }


showHideFaved : Bool -> Block -> State -> State
showHideFaved favedOnly blockToUpdate state =
    let
        maybeUpdateBlock : Block -> Block
        maybeUpdateBlock block =
            if block.height == blockToUpdate.height then
                { block | favedOnly = favedOnly }

            else
                block

        newBlocks =
            List.map maybeUpdateBlock state.blocks
    in
    { state | blocks = newBlocks }



-- model operations


updateOnTick : Time.Posix -> Model -> ( Model, Cmd Msg )
updateOnTick now model =
    let
        timeInSeconds : Time.Posix -> Int
        timeInSeconds posix =
            Time.posixToMillis posix // 1000

        -- refresh if more than ten minutes out of date
        isOutdated : Bool
        isOutdated =
            timeInSeconds now - timeInSeconds model.refreshed > 600

        fetch : Cmd Msg
        fetch =
            Util.msgToCommand FetchEndingSoon
    in
    case ( model.status, isOutdated ) of
        ( Loading, _ ) ->
            -- already loading - don't do anything
            ( model, Cmd.none )

        ( Success, True ) ->
            -- fetch ending and remove old blocks
            ( { model | refreshed = now }, Cmd.batch [ fetch, removeEndedBlocks model.state ] )

        ( Success, False ) ->
            -- remove old blocks
            ( model, removeEndedBlocks model.state )

        ( _, _ ) ->
            -- initial or failed, fetch ending!
            ( { model | status = Loading, refreshed = now }, fetch )


gotEndingSoon : Model -> Int -> AC.EndingSoon -> ( Model, Cmd Msg )
gotEndingSoon model page endingSoon =
    let
        domains : List Domain
        domains =
            endingSoonToDomains endingSoon

        newState : State
        newState =
            updateStateEndingSoon
                model.state
                endingSoon.lastBlock
                domains
    in
    ( { model | status = Success, state = newState }
    , updateEndingSoon newState page domains
    )


gotDomainDetails : Model -> AC.DomainDetails -> ( Model, Cmd Msg )
gotDomainDetails model domainDetails =
    let
        domain =
            detailsToDomain domainDetails

        newState =
            updateStateDomainDetails
                domainDetails.lastBlock
                domain
                model.state
    in
    ( { model | state = newState }
    , Util.msgToCommandAfter 1 (RemoveRefreshing domain)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            updateOnTick now model

        FetchEndingSoon ->
            ( model, getEndingPage 0 )

        GotEndingSoon page result ->
            case result of
                Ok endingSoon ->
                    gotEndingSoon model page endingSoon

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        ChooseDomainsToFetch time ->
            ( model, chooseDomainsToRefresh time model.state )

        FetchDomains refreshDomains ->
            ( { model | state = setRefreshing refreshDomains model.state }
            , fetchDomains refreshDomains
            )

        GotDomainDetails result ->
            case result of
                Ok domainDetails ->
                    gotDomainDetails model domainDetails

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        RemoveRefreshing domain ->
            let
                makeRegular state =
                    { state
                        | blocks =
                            replaceBlocks state.blocks domain (updateDomain Regular)
                    }
            in
            ( { model | state = makeRegular model.state }
            , Cmd.none
            )

        RemoveEndedBlocks ->
            let
                removeHiddenBlocks state =
                    { state | blocks = removeHidden state.blocks }
            in
            ( { model | state = removeHiddenBlocks model.state }
            , Cmd.none
            )

        FlipFave domain ->
            ( { model | state = flipFave domain model.state }
            , Cmd.none
            )

        ShowFaves block ->
            ( { model | state = showHideFaved False block model.state }
            , Cmd.none
            )

        HideFaves block ->
            ( { model | state = showHideFaved True block model.state }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "sniper" ]
        (viewHeader model ++ viewState model.state)


viewHeader : Model -> List (Html Msg)
viewHeader model =
    case model.status of
        Failure ->
            [ h2 [] [ text "Something's wrong :(" ]
            , p [ class "error" ]
                [ text "Could not load auctioned names. Will try again in a couple of seconds!" ]
            ]

        Success ->
            []

        _ ->
            [ h2 [] [ text "Hello Happy Handshake Snipers" ]
            , p [] [ text "Loading, this shouldn't take long..." ]
            ]


viewState : State -> List (Html Msg)
viewState state =
    if state == initialState then
        []

    else
        displayState state


displayState : State -> List (Html Msg)
displayState state =
    [ div [ class "pure-g topbar" ]
        [ div [ class <| classBlock ++ " it gray" ]
            [ Util.divWrap <| text <| String.fromInt state.lastBlock ]
        , div [ class <| classDomains ]
            [ Util.divWrapClass "domain-header gray" <| text "← blockchain height" ]
        , div [ class <| classBlock ++ " it gray" ]
            [ Util.divWrap <| text <| String.fromInt (currentBlock state.lastBlock) ]
        , div [ class <| classDomains ]
            [ div [ class "domain-header gray" ]
                [ text "← "
                , abbr [ title "You just missed it!" ] [ text "currently mined block" ]
                ]
            ]
        ]
    , div [ class "pure-g header" ]
        [ div [ class classBlock ]
            [ Util.divWrap <| text "Block" ]
        , div [ class classDomains ]
            [ div [ class "pure-g domain-header" ]
                [ div [ class className ] [ text "Auctioned name" ]
                , div [ class classH ] [ Util.divWrap <| abbr [ title "Watch this name" ] [ text "❤" ] ]
                , div [ class classG ] [ Util.divWrap <| abbr [ title "Search name on Google" ] [ text "G" ] ]
                , div [ class classBids ] [ abbr [ title "Number of bids on the name" ] [ text "Bids" ] ]
                , div [ class classHighest ] [ abbr [ title "Highest bid on the name" ] [ text "High bid" ] ]
                ]
            ]
        ]
    , div [] <| List.map (viewBlock FlipFave ShowFaves HideFaves state.lastBlock) state.blocks
    ]
