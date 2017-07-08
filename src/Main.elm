module Main exposing (main)

import AFrame exposing (..)
import AFrame.Primitives exposing (box, sky, sphere)
import AFrame.Primitives.Attributes as A
import AFrame.Primitives.Light as Light
import Color exposing (Color)
import Galaxy exposing (Galaxy)
import Html exposing (..)
import Random exposing (Generator, float, int, list, map4)
import Random.Extra exposing (frequency)


type alias Model =
    { galaxies : List Galaxy
    , angle : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { galaxies = []
      , angle = 20
      }
    , Random.generate NewGalaxies generateGalaxies
    )


type Msg
    = NewGalaxies (List Galaxy)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGalaxies newGalaxies ->
            ( { model | galaxies = newGalaxies }, Cmd.none )


view : Model -> Html msg
view model =
    scene
        [ A.vrModeUi True ]
        (viewSky
            -- :: viewBlackHole ( 12, -4, -150 )
            :: List.map Galaxy.view model.galaxies
        )


generateGalaxies : Generator (List Galaxy)
generateGalaxies =
    Random.Extra.combine
        [ Galaxy.generate ( 20, -10, -150 )

        -- , generateGalaxy ( 100, 50, 150 )
        -- , generateGalaxy ( -300, 140, -400 )
        -- , generateGalaxy ( 200, 200, 0 )
        -- , generateGalaxy ( 300, 300, 0 )
        ]


viewBlackHole ( x, y, z ) =
    sphere
        [ Light.type_ Light.Point
        , A.intensity 2
        , A.distance 200
        , A.color Color.white
        , A.position x y z
        , A.radius 4
        ]
        []


subscriptions model =
    Sub.none


viewSky =
    sky [ A.color Color.black ] []


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
