module Main exposing (main)

import AFrame exposing (..)
import AFrame.Primitives exposing (box, sky, sphere)
import AFrame.Primitives.Attributes as A
import AFrame.Primitives.Light as Light
import Array
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes as Attr
import Random exposing (float, int, list, map3)
import Time


type alias Model =
    { randPairs : List (List ( Color, Float, Float ))
    , angle : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { randPairs = []
      , angle = 20
      }
    , Random.generate NewRandomPairs generateRandomPairs
    )


type Msg
    = NewRandomPairs (List (List ( Color, Float, Float )))
    | NewAngle


starColors =
    Array.fromList [ Color.red, Color.lightYellow, Color.lightBlue ]


colorGen =
    int 0 2
        |> Random.map (\index -> Array.get index starColors)
        |> Random.map (Maybe.withDefault Color.lightYellow)


randomPoint : Random.Generator ( Color, Float, Float )
randomPoint =
    map3 (,,) colorGen (float -5 5) (float -5 5)


generateRandomArm =
    list starsPerArm randomPoint


generateRandomPairs =
    list numArms generateRandomArm


update msg model =
    case msg of
        NewRandomPairs randomPairs ->
            ( { model | randPairs = randomPairs }, Cmd.none )

        NewAngle ->
            ( { model | angle = model.angle + 0.005 }, Cmd.none )


view : Model -> Html msg
view model =
    scene [ A.vrModeUi True ]
        (viewSky
            :: viewGalaxy model.angle model.randPairs
        )


subscriptions model =
    Time.every (100 * Time.millisecond) (\_ -> NewAngle)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


numArms =
    20


starsPerArm =
    75


armAngle =
    270 / numArms


radiusTuningParam =
    0.4


angleTuningParam =
    1


getStarHelper ( color, randX, randY ) radius angle =
    let
        x =
            (radius * cos angle) + randX

        y =
            (radius * sin angle) + randY
    in
    ( color, x, y )


getStar curArm index randPair =
    let
        radius =
            toFloat index / radiusTuningParam

        angle =
            toFloat index / (angleTuningParam + (armAngle * (toFloat curArm + 1)))

        newAngle =
            angle + (5 * toFloat (curArm + 1))
    in
    getStarHelper randPair radius newAngle


getArm : Int -> List ( Color, Float, Float ) -> List ( Color, Float, Float )
getArm curArm randPairs =
    List.indexedMap (getStar curArm) randPairs


viewGalaxy angle randPairs =
    List.indexedMap getArm randPairs
        |> List.concat
        |> List.map viewStar


viewSky =
    sky [ A.color Color.black ] []


viewStar ( color, dx, dy ) =
    sphere
        [ --Light.type_ Light.Point
          --, A.intensity 2
          --, A.distance 120
          -- , A.color Color.white
          A.radius 0.5
        , A.position dx dy -200
        , Attr.attribute "material" "color: #FFF; shader: flat"
        , A.color color
        ]
        []
