module Domains exposing
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

import Dict exposing (Dict)
import Set exposing (Set)
import Util


type alias Domain =
    { name : String
    , reveal : Int
    , bids : Maybe Int
    , highestBid : Maybe Int
    , state : ElementState
    }


type alias DomainsAtBlock =
    { block : Int
    , domains : List Domain
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
            case ( Dict.get name oldDomainsDict, Dict.get name newDomainsDict ) of
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


replaceDomain : DomainsAtBlock -> Domain -> DomainUpdate -> DomainsAtBlock
replaceDomain dab newDomain updateFun =
    case Util.splitOut (\d -> d.name == newDomain.name) dab.domains of
        Nothing ->
            dab

        Just ( before, oldDomain, after ) ->
            DomainsAtBlock dab.block (before ++ updateFun oldDomain newDomain :: after)


replaceDabs : List DomainsAtBlock -> Domain -> DomainUpdate -> List DomainsAtBlock
replaceDabs dabs domain updateFun =
    case Util.splitOut (\dab -> dab.block == domain.reveal) dabs of
        Nothing ->
            dabs

        Just ( before, found, after ) ->
            before ++ replaceDomain found domain updateFun :: after


domainsWithoutHighestBid : List DomainsAtBlock -> List Domain
domainsWithoutHighestBid dabs =
    List.concatMap .domains dabs
        |> List.filter (\d -> d.highestBid == Nothing)


getDomainsAtBlocks : List Domain -> List Int -> List DomainsAtBlock
getDomainsAtBlocks domains =
    let
        getBlock : Int -> List Domain -> DomainsAtBlock
        getBlock block =
            List.filter (\d -> d.reveal == block) >> DomainsAtBlock block
    in
    List.map (\d -> getBlock d domains)
