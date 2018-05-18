module Example exposing (..)
import ElmHub exposing (responseDecoder)
import Json.Decode exposing (Value, decodeString)
import String
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "GitHub Response Decoder"
        [ test "it results in an Err for invalid JSON" <|
            \() ->
                let
                    json =
                        """{ "pizza": [] }"""

                    isErrorResult result =
                        case result of
                            Ok _ -> False

                            Err _ -> True
                in
                json
                    |> decodeString responseDecoder
                    |> isErrorResult
                    |> Expect.true "Expected decoding an invalid response to return an Err."
        , test "it successfully decodes a valid response" <|
            \() ->
                """{ "items": [
                                { "id": 5, "full_name": "foo", "stargazers_count": 42 }
                              , { "id": 3, "full_name": "bar", "stargazers_count": 77 }
                            
                 ] }"""
                    |> decodeString responseDecoder
                    |> Expect.equal
                        (Ok
                            [ { id = 5, name = "foo", stars = 42 }
                            , { id = 3, name = "bar", stars = 77 }
                            ]
                        )
        , fuzz (list int) "it decodes one SearchResult for each 'item' in the JSON" <|
            \randomList ->
                let
                    ids = randomList
                        
                    jsonFromId id =
                        """{"id": """ ++ toString id ++ """, "full_name": "foo", "stargazers_count": 42}"""

                    jsonItems =
                        String.join ", " (List.map jsonFromId ids)

                    json =
                        """{ "items": [""" ++ jsonItems ++ """] }"""
                in
                case decodeString responseDecoder json of
                    Ok results ->
                        List.length results
                            |> Expect.equal (List.length ids)

                    Err err ->
                        Expect.fail ("JSON decoding failed unexpectedly: " ++ err)
        ]