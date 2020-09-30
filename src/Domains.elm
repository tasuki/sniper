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
    , favedDomains
    , flipFaveFlag
    , hideBlocks
    , mergeDomainLists
    , nextBlock
    , oldestBlockState
    , putDomainsInBlocks
    , removeHidden
    , replaceBlocks
    , replaceSortedBlocks
    , updateDomain
    , viewBlock
    )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Punycode
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
    , favedOnly : Bool
    }


type alias Height =
    Int


type alias DomainUpdate =
    Domain -> Domain -> Domain


type ElementState
    = Regular
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
            Util.takeHigher .bids old new

        highestBid =
            Util.takeHigher .highestBid old new
    in
    Domain new.name new.reveal bids highestBid state old.faved


flipFaveFlag : DomainUpdate
flipFaveFlag old _ =
    { old | faved = not old.faved }


domainsToDict : List Domain -> Dict String Domain
domainsToDict =
    List.map (\d -> ( d.name, d )) >> Dict.fromList


by : (a -> comparable) -> (a -> a -> Order)
by toCmp a b =
    compare (toCmp a) (toCmp b)


andThen : (a -> comparable) -> (a -> a -> Order) -> (a -> a -> Order)
andThen toCmp primary a b =
    case primary a b of
        EQ ->
            by toCmp a b

        ineq ->
            ineq


compareByFaved : Domain -> number
compareByFaved domain =
    case domain.faved of
        True ->
            0

        False ->
            1


sortDomains : List Domain -> List Domain
sortDomains =
    List.sortWith (by compareByFaved |> andThen .name)


mergeDomainLists : (Domain -> Bool) -> List Domain -> List Domain -> List Domain
mergeDomainLists isFave newDomains oldDomains =
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
                    updateDomain Regular old new

                ( Just old, _ ) ->
                    old

                ( _, Just new ) ->
                    { new | faved = isFave new }

                ( _, _ ) ->
                    emptyDomain
    in
    Set.toList allDomainNames
        |> List.map maybeUpdateDomain
        |> sortDomains



-- blocks


sortBlock : Block -> Block
sortBlock block =
    { block | domains = sortDomains block.domains }


replaceDomain : Block -> Domain -> DomainUpdate -> Block
replaceDomain block newDomain updateFun =
    case Util.splitOut (\d -> d.name == newDomain.name) block.domains of
        Nothing ->
            block

        Just ( before, oldDomain, after ) ->
            { block | domains = before ++ updateFun oldDomain newDomain :: after }


replaceBlocksWithTransform : (Block -> Block) -> List Block -> Domain -> DomainUpdate -> List Block
replaceBlocksWithTransform transform blocks domain updateFun =
    case Util.splitOut (\block -> block.height == domain.reveal) blocks of
        Nothing ->
            blocks

        Just ( before, block, after ) ->
            before ++ transform (replaceDomain block domain updateFun) :: after


replaceSortedBlocks : List Block -> Domain -> DomainUpdate -> List Block
replaceSortedBlocks =
    replaceBlocksWithTransform sortBlock


replaceBlocks : List Block -> Domain -> DomainUpdate -> List Block
replaceBlocks =
    replaceBlocksWithTransform identity


domainsWithoutHighestBid : List Block -> List Domain
domainsWithoutHighestBid =
    List.concatMap .domains >> List.filter (\d -> d.highestBid == Nothing)


favedDomains : List Block -> List Domain
favedDomains =
    List.concatMap .domains >> List.filter .faved


putDomainsInBlocks : (Height -> Bool) -> List Height -> List Domain -> List Block -> List Block
putDomainsInBlocks showFavedOnly blocksToShow domains blocks =
    let
        updateBlock : Height -> List Domain -> Block
        updateBlock height domainList =
            case List.Extra.find (\b -> b.height == height) blocks of
                Nothing ->
                    Block height domainList Regular (showFavedOnly height)

                Just b ->
                    { b | domains = domainList }

        getBlock : List Domain -> Height -> Block
        getBlock ds height =
            List.filter (\d -> d.reveal == height) ds |> updateBlock height
    in
    List.map (getBlock domains) blocksToShow


oldestBlockState : List Block -> ElementState
oldestBlockState blocks =
    List.head blocks
        |> Maybe.map .state
        |> Maybe.withDefault Regular


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
    "pure-u-1-6 pure-u-md-1-4 block"


classDomains =
    "pure-u-5-6 pure-u-md-3-4"


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


viewName : String -> String
viewName domainName =
    if String.startsWith "xn--" domainName then
        let
            punyPart =
                String.dropLeft 4 domainName
        in
        Punycode.decode punyPart

    else
        domainName


viewDomain : (Domain -> msg) -> Domain -> Html msg
viewDomain faveAction d =
    let
        displayName =
            viewName d.name
    in
    div [ class ("pure-g domain " ++ domainState d) ]
        [ div [ class className ]
            [ a [ href <| "https://www.namebase.io/domains/" ++ d.name ] [ text displayName ] ]
        , div [ class (classH ++ classFaved d) ]
            [ a [ onClick (faveAction d) ] [ text "❤" ] ]
        , div [ class classG ]
            [ a [ href <| "https://www.google.com/search?q=" ++ displayName ] [ text "G" ] ]
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
    "<" ++ (String.fromInt <| timeLeft chainHeight blockHeight) ++ " min"


blockInfo : Height -> Height -> Html msg
blockInfo chainHeight blockHeight =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1-1 pure-u-md-2-3" ]
            [ div [ class "block-detail" ] [ text <| minutesLeft chainHeight blockHeight ] ]
        , div [ class "pure-u-1-1 pure-u-md-1-3" ]
            [ div [ class "block-detail" ] [ text <| String.fromInt <| blockHeight ] ]
        ]


showHideDomains : (Block -> msg) -> (Block -> msg) -> Block -> List (Html msg)
showHideDomains showFaves hideFaves block =
    let
        domainCount =
            List.length block.domains

        favedCount =
            List.filter (\d -> d.faved) block.domains
                |> List.length

        unfavedCount =
            domainCount - favedCount

        showHideUnfavedTxt : msg -> String -> List (Html msg)
        showHideUnfavedTxt action txt =
            [ div [ class "show-hide-none" ]
                [ a [ onClick action ] [ text txt ] ]
            ]

        showHideUnfaved : List (Html msg)
        showHideUnfaved =
            if block.favedOnly then
                showHideUnfavedTxt
                    (showFaves block)
                    ("↶ show " ++ String.fromInt unfavedCount ++ " more names ↷")

            else
                showHideUnfavedTxt
                    (hideFaves block)
                    ("↺ hide " ++ String.fromInt unfavedCount ++ " names ↻")
    in
    if unfavedCount > 0 then
        showHideUnfaved

    else
        []


viewBlock : (Domain -> msg) -> (Block -> msg) -> (Block -> msg) -> Height -> Block -> Html msg
viewBlock faveAction showFaves hideFaves chainHeight block =
    let
        domainList =
            if List.isEmpty block.domains then
                [ div [ class "show-hide-none" ] [ text "⨯ no names here ⨯" ] ]

            else
                List.filter (\d -> not block.favedOnly || d.faved) block.domains
                    |> List.map (viewDomain faveAction)
    in
    div [ class ("pure-g section " ++ blockState block) ]
        [ div [ class <| classBlock ++ " it gray" ]
            [ blockInfo chainHeight block.height ]
        , div [ class <| classDomains ]
            (domainList ++ showHideDomains showFaves hideFaves block)
        ]
