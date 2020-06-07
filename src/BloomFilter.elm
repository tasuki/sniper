module BloomFilter exposing (create, empty, read)

import Array
import Bloom



-- settings


falsePositives =
    1 / 1000


hashFunctions =
    4



-- implementation


ln : Float -> Float
ln x =
    logBase e x


exp : Float -> Float
exp x =
    e ^ x


r : Float
r =
    -- as per https://hur.st/bloomfilter/
    -hashFunctions / ln (1 - exp (ln falsePositives / hashFunctions))


empty : Bloom.Filter
empty =
    Bloom.empty 1 hashFunctions


create : List String -> Bloom.Filter
create items =
    let
        itemCount =
            List.length items |> toFloat |> max 1

        bits =
            -- as per https://hur.st/bloomfilter/
            ceiling (itemCount * r)

        bitsRoundedToSix =
            6 * ceiling (toFloat bits / 6)

        emptyBloomFilter =
            Bloom.empty bitsRoundedToSix hashFunctions
    in
    List.foldr Bloom.add emptyBloomFilter items


read : Array.Array Int -> Bloom.Filter
read bits =
    let
        emptyFilter =
            Bloom.empty (Array.length bits) hashFunctions
    in
    { emptyFilter | set = bits }
