module Domains exposing
    ( Block
    , Domain
    , DomainUpdate
    , ElementState(..)
    , domainsWithoutHighestBid
    , getDomainsAtBlocks
    , hideBlocks
    , mergeDomainLists
    , oldestBlockState
    , removeHidden
    , replaceBlocks
    , setDomainState
    , updateDomain
    )

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)
import Util


type alias Domain =
    { name : String
    , reveal : Int
    , bids : Maybe Int
    , highestBid : Maybe Int
    , state : ElementState
    }


type alias Block =
    { height : Int
    , domains : List Domain
    , state : ElementState
    }


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
    Domain "" 0 Nothing Nothing Hidden


updateDomain : ElementState -> DomainUpdate
updateDomain state old new =
    let
        bids =
            Util.maybeUpdateField .bids old new

        highestBid =
            Util.maybeUpdateField .highestBid old new
    in
    Domain new.name new.reveal bids highestBid state


setDomainState : ElementState -> DomainUpdate
setDomainState state old _ =
    { old | state = state }


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


getDomainsAtBlocks : List Domain -> List Int -> List Block
getDomainsAtBlocks domains =
    let
        getBlock : List Domain -> Int -> Block
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


hideBlocks : Int -> List Block -> List Block
hideBlocks lastBlock =
    let
        hideEarlierThan : Int -> Block -> Block
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
