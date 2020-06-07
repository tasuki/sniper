module BloomTests exposing (..)

import Bloom
import Expect
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Random.Char
import Random.String exposing (string)
import Shrink
import Test exposing (describe, fuzz)


word : Generator String
word =
    string 10 Random.Char.english


words : Generator (List String)
words =
    Random.list 100000 word


wordsFuzzer : Fuzzer (List String)
wordsFuzzer =
    Fuzz.custom words Shrink.noShrink


filter : Int -> Bloom.Filter
filter hashes =
    Bloom.add "sample" (Bloom.empty 10 hashes)


bloomTest =
    describe "Bloom"
        [ fuzz wordsFuzzer "does not find too many uncontained strings" <|
            \wordList ->
                let
                    testOne : Int -> List Bool
                    testOne hashes =
                        let
                            x : List Bool
                            x =
                                List.map (\w -> Bloom.test w (filter hashes)) wordList

                            _ =
                                Debug.log "hashes" hashes

                            _ =
                                Debug.log "count" (List.length x)

                            _ =
                                Debug.log "found" (List.length <| List.filter (\xx -> xx == True) x)
                        in
                        x

                    _ =
                        List.map testOne <| List.reverse <| List.range 1 10
                in
                Expect.pass
        ]
