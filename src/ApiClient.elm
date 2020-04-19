module ApiClient exposing
    ( DomainDetails
    , DomainDetailsResult
    , EndingSoon
    , EndingSoonDomain
    , EndingSoonResult
    , getDomainDetails
    , getEndingSoon
    )

import Http
import Json.Decode as D



-- api global


baseUrl =
    -- "https://www.namebase.io/api" sends bad cors headers
    "http://nb-proxy.tasuki.org/api"


endingSoonPerPage =
    12



-- get "ending soon"


type alias EndingSoonDomain =
    { name : String
    , revealAt : Int
    , bids : Int
    }


type alias EndingSoon =
    { lastBlock : Int
    , domains : List EndingSoonDomain
    }


type alias EndingSoonResult =
    Result Http.Error EndingSoon


getEndingSoon : Int -> (EndingSoonResult -> msg) -> Cmd msg
getEndingSoon count ctor =
    Http.get
        { url =
            baseUrl
                ++ "/domains/ending-soon/"
                ++ (count * endingSoonPerPage |> String.fromInt)
        , expect = Http.expectJson ctor endingSoonDecoder
        }


endingSoonDecoder : D.Decoder EndingSoon
endingSoonDecoder =
    D.map2 EndingSoon
        (D.field "height" D.int)
        (D.field "domains" domainsDecoder)


domainsDecoder : D.Decoder (List EndingSoonDomain)
domainsDecoder =
    D.list <|
        D.map3 EndingSoonDomain
            (D.field "name" D.string)
            (D.field "reveal_block" D.int)
            (D.field "total_number_bids" D.int)



-- get "domain details"


type alias DomainDetails =
    { name : String
    , revealAt : Int
    , highestBid : Int
    }


type alias DomainDetailsResult =
    Result Http.Error DomainDetails


getDomainDetails : String -> (DomainDetailsResult -> msg) -> Cmd msg
getDomainDetails domain ctor =
    Http.get
        { url = baseUrl ++ "/domains/get/" ++ domain
        , expect = Http.expectJson ctor domainDetailsDecoder
        }


domainDetailsDecoder : D.Decoder DomainDetails
domainDetailsDecoder =
    D.map3 DomainDetails
        (D.field "name" D.string)
        (D.field "revealAt" D.int)
        (D.field "highestStakeAmount" D.int)
