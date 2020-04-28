module Domains exposing
    ( Block
    , Domain
    , DomainUpdate
    , ElementState(..)
    , Height
    , classBids
    , classBlock
    , classDomains
    , classG
    , classH
    , classHighest
    , className
    , currentBlock
    , domainsWithoutHighestBid
    , flipFaveFlag
    , getDomainsAtBlocks
    , hideBlocks
    , mergeDomainLists
    , nextBlock
    , oldestBlockState
    , removeHidden
    , replaceBlocks
    , setDomainState
    , updateDomain
    , viewBlock
    )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Set exposing (Set)
import Util



-- MODEL


type alias Domain =
    { name : String
    , reveal : Height
    , bids : Maybe Int
    , highestBid : Maybe Int
    , state : ElementState
    , faved : Bool
    }


type alias Block =
    { height : Height
    , domains : List Domain
    , state : ElementState
    }


type alias Height =
    Int


type alias DomainUpdate =
    Domain -> Domain -> Domain


type ElementState
    = New
    | Refreshing
    | Refreshed
    | Hidden



-- domains


emptyDomain : Domain
emptyDomain =
    Domain "" 0 Nothing Nothing Hidden False


updateDomain : ElementState -> DomainUpdate
updateDomain state old new =
    let
        bids =
            Util.maybeUpdateField .bids old new

        highestBid =
            Util.maybeUpdateField .highestBid old new
    in
    Domain new.name new.reveal bids highestBid state old.faved


setDomainState : ElementState -> DomainUpdate
setDomainState state old _ =
    { old | state = state }


flipFaveFlag : DomainUpdate
flipFaveFlag old _ =
    { old | faved = not old.faved }


domainsToDict : List Domain -> Dict String Domain
domainsToDict =
    List.map (\d -> ( d.name, d )) >> Dict.fromList


mergeDomainLists : List Domain -> List Domain -> List Domain
mergeDomainLists oldDomains newDomains =
    let
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
            case
                ( Dict.get name oldDomainsDict
                , Dict.get name newDomainsDict
                )
            of
                ( Just old, Just new ) ->
                    updateDomain old.state old new

                ( Just old, _ ) ->
                    old

                ( _, Just new ) ->
                    new

                ( _, _ ) ->
                    emptyDomain
    in
    Set.toList allDomainNames |> List.map maybeUpdateDomain |> List.sortBy .name



-- domains at block


replaceDomain : Block -> Domain -> DomainUpdate -> Block
replaceDomain block newDomain updateFun =
    case Util.splitOut (\d -> d.name == newDomain.name) block.domains of
        Nothing ->
            block

        Just ( before, oldDomain, after ) ->
            Block block.height
                (before ++ updateFun oldDomain newDomain :: after)
                block.state


replaceBlocks : List Block -> Domain -> DomainUpdate -> List Block
replaceBlocks blocks domain updateFun =
    case Util.splitOut (\block -> block.height == domain.reveal) blocks of
        Nothing ->
            blocks

        Just ( before, block, after ) ->
            before ++ replaceDomain block domain updateFun :: after


domainsWithoutHighestBid : List Block -> List Domain
domainsWithoutHighestBid =
    List.concatMap .domains >> List.filter (\d -> d.highestBid == Nothing)


getDomainsAtBlocks : List Domain -> List Height -> List Block
getDomainsAtBlocks domains =
    let
        getBlock : List Domain -> Height -> Block
        getBlock ds height =
            List.filter (\d -> d.reveal == height) ds
                |> (\block -> Block height block New)
    in
    List.map (getBlock domains)


oldestBlockState : List Block -> ElementState
oldestBlockState blocks =
    List.head blocks
        |> Maybe.map .state
        |> Maybe.withDefault New


hideBlocks : Height -> List Block -> List Block
hideBlocks lastBlock =
    let
        hideEarlierThan : Height -> Block -> Block
        hideEarlierThan height block =
            if block.height < height then
                { block | state = Hidden }

            else
                block
    in
    List.map (hideEarlierThan (lastBlock + 2))


removeHidden : List Block -> List Block
removeHidden =
    List.Extra.dropWhile (\block -> block.state == Hidden)



-- VIEW


classBlock =
    "pure-u-7-24 block"


classDomains =
    "pure-u-17-24"


classG =
    "pure-u-2-24 g"


classH =
    "pure-u-2-24 h"


className =
    "pure-u-13-24 name"


classBids =
    "pure-u-2-24 bids"


classHighest =
    "pure-u-5-24 highest"


currentBlock : Height -> Height
currentBlock lastBlock =
    lastBlock + 1


nextBlock : Height -> Height
nextBlock lastBlock =
    lastBlock + 2


timeLeft : Height -> Height -> Height
timeLeft chainHeight blockEndsAt =
    (blockEndsAt - currentBlock chainHeight) * 10


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
        Refreshed ->
            "shake"

        _ ->
            ""


classFaved : Domain -> String
classFaved d =
    if d.faved then
        " faved"

    else
        ""


viewDomain : (Domain -> msg) -> Domain -> Html msg
viewDomain faveAction d =
    div [ class ("pure-g domain " ++ domainState d) ]
        [ div [ class className ]
            [ a [ href <| "https://www.namebase.io/domains/" ++ d.name ] [ text d.name ] ]
        , div [ class (classH ++ classFaved d) ]
            [ a [ onClick (faveAction d) ] [ text "❤" ] ]
        , div [ class classG ]
            [ a [ href <| "https://www.google.com/search?q=" ++ d.name ] [ text "G" ] ]
        , div [ class classBids ]
            [ text <| displayMaybeNumber d.bids ]
        , div [ class classHighest ]
            [ text <| displayBid d.highestBid ]
        ]


blockState : Block -> String
blockState block =
    case block.state of
        Hidden ->
            "hide"

        _ ->
            ""


minutesLeft : Height -> Height -> String
minutesLeft chainHeight blockHeight =
    "<" ++ (String.fromInt <| timeLeft chainHeight blockHeight) ++ " min left \u{00A0} "


minutesLeftDiv : Height -> Height -> Html msg
minutesLeftDiv chainHeight blockHeight =
    Util.divWrap <|
        text <|
            minutesLeft chainHeight blockHeight
                ++ (String.fromInt <| blockHeight)


viewBlock : (Domain -> msg) -> Height -> Block -> Html msg
viewBlock faveAction chainHeight block =
    div [ class ("pure-g section " ++ blockState block) ]
        [ div [ class <| classBlock ++ " it gray" ]
            [ minutesLeftDiv chainHeight block.height ]
        , div [ class <| classDomains ]
            (List.map (viewDomain faveAction) block.domains)
        ]
