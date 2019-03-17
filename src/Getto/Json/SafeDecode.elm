module Getto.Json.SafeDecode exposing
  ( Decoder
  , string
  , int
  , bool
  , list
  , at
  , valueAt
  )

{-| json utilities

    value |> SafeDecode.at ["name"]    (SafeDecode.string "")
    value |> SafeDecode.at ["age"]     (SafeDecode.string 0)
    value |> SafeDecode.at ["isValid"] (SafeDecode.bool False)

    value |> SafeDecode.at ["roles"] (SafeDecode.list (SafeDecode.string ""))

    obj |> SafeDecode.valueAt ["object"]

# Definition
@docs Decoder

# Decoders
@docs string, int, bool, list

# Decode
@docs at, valueAt
 -}


import Json.Encode as Encode
import Json.Decode as Decode


{-| decoder and default value
 -}
type Decoder a
  = Decoder (Decode.Decoder a) a


{-| string safe decoder
 -}
string : String -> Decoder String
string default = Decoder (Decode.string |> withDefault default) default


{-| int safe decoder
 -}
int : Int -> Decoder Int
int default = Decoder (Decode.int |> withDefault default) default


{-| bool safe decoder
 -}
bool : Bool -> Decoder Bool
bool default = Decoder (Decode.bool |> withDefault default) default


{-| list safe decoder
 -}
list : Decoder a -> Decoder (List a)
list decoder =
  case decoder of
    Decoder decode default ->
      Decoder (Decode.list decode |> withDefault []) []

withDefault : a -> Decode.Decoder a -> Decode.Decoder a
withDefault default decoder =
  Decode.oneOf
    [ decoder
    , Decode.succeed default
    ]


{-| decode from json value with safe decoder
 -}
at : List String -> Decoder a -> Decode.Value -> a
at names decoder =
  case decoder of
    Decoder decode default ->
      Decode.decodeValue (Decode.at names decode)
      >> Result.withDefault default


{-| decode to json value
 -}
valueAt : List String -> Decode.Value -> Decode.Value
valueAt names =
  Decode.decodeValue (Decode.at names Decode.value)
  >> Result.withDefault Encode.null
