module Domains exposing
    ( Block
    , Domain
    , DomainUpdate
    , ElementState(..)
    , classBids
    , classG
    , classHighest
    , className
    , domainsWithoutHighestBid
    , getDomainsAtBlocks
    , hideBlocks
    , mergeDomainLists
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
import List.Extra
import Set exposing (Set)
import Util



-- MODEL


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



-- VIEW


classG =
    "pure-u-2-24 g"


className =
    "pure-u-14-24 name"


classBids =
    "pure-u-3-24 bids"


classHighest =
    "pure-u-5-24 highest"


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


viewDomain : Domain -> Html msg
viewDomain d =
    div [ class ("pure-g domain " ++ domainState d) ]
        [ div [ class classG ]
            [ a [ href <| "https://www.google.com/search?q=" ++ d.name ] [ text "g" ] ]
        , div [ class className ]
            [ a [ href <| "https://www.namebase.io/domains/" ++ d.name ] [ text d.name ] ]
        , div [ class classBids ] [ text <| displayMaybeNumber d.bids ]
        , div [ class classHighest ] [ text <| displayBid d.highestBid ]
        ]


blockState : Block -> String
blockState block =
    case block.state of
        Hidden ->
            "hide"

        _ ->
            ""


currentBlock : Int -> Int
currentBlock lastBlock =
    lastBlock + 1


timeLeft : Int -> Int -> Int
timeLeft chainHeight blockEndsAt =
    (blockEndsAt - currentBlock chainHeight) * 10


viewBlock : Int -> Block -> Html msg
viewBlock chainHeight block =
    div [ class ("pure-g section " ++ blockState block) ]
        [ div [ class "pure-u-5-24 block it gray" ]
            [ Util.divWrap <|
                text <|
                    "<"
                        ++ (String.fromInt <| timeLeft chainHeight block.height)
                        ++ " min left"
            ]
        , div [ class "pure-u-3-24 block it gray" ]
            [ Util.divWrap <| text <| String.fromInt <| block.height ]
        , div [ class "pure-u-16-24 domains" ] (List.map viewDomain block.domains)
        ]
