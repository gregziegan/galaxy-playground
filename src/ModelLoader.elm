module ModelLoader exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


plymodel : String -> Attribute msg
plymodel value =
    attribute "ply-model" value
