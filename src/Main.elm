module Main exposing (main)

import AFrame exposing (..)
import AFrame.Primitives exposing (box, sky, sphere)
import AFrame.Primitives.Attributes as A
import AFrame.Primitives.Light as Light
import Array
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes as Attr
import Random exposing (Generator, float, int, list, map4)
import Random.Extra exposing (frequency)
import Time


type alias Model =
    { galaxy : Galaxy
    , angle : Float
    }


type alias Galaxy =
    { arms : List GalaxyArm
    }


type alias GalaxyArm =
    List Star


type alias Position =
    ( Float, Float, Float )


type alias Star =
    { color : Color
    , position : Position
    }


init : ( Model, Cmd Msg )
init =
    ( { galaxy = { arms = [] }
      , angle = 20
      }
    , Random.generate NewGalaxies generateGalaxy
    )


type Msg
    = NewGalaxies Galaxy
    | NewAngle


starColors distance =
    Random.map (Maybe.withDefault Color.lightYellow) <|
        Random.Extra.sample <|
            if distance == 1 then
                [ Color.lightYellow, Color.lightBlue ]
            else if distance == 2 then
                [ Color.red, Color.lightYellow, Color.lightBlue ]
            else
                [ Color.red, Color.lightYellow ]


randomPoint : Random.Generator Position
randomPoint =
    Random.map3 (,,) (float -5 5) (float -5 5) (float -50 50)


randomStar : Int -> Random.Generator Star
randomStar distance =
    Random.map2 Star (starColors distance) randomPoint


generateRandomArm distance =
    list (starsPerArm // 3) (randomStar distance)


generateRandomPairs distance =
    list numArms (generateRandomArm distance)


(+++) a b c =
    a ++ b ++ c


generateGalaxy =
    Random.map Galaxy generateGalaxyArms


generateGalaxyArms : Generator (List GalaxyArm)
generateGalaxyArms =
    Random.map3 (+++) (generateRandomPairs 1) (generateRandomPairs 2) (generateRandomPairs 3)


update msg model =
    case msg of
        NewGalaxies newGalaxy ->
            ( { model | galaxy = newGalaxy }, Cmd.none )

        NewAngle ->
            ( { model | angle = model.angle + 0.005 }, Cmd.none )


view : Model -> Html msg
view model =
    scene [ A.vrModeUi True ]
        (viewSky
            :: viewGalaxy model.angle model.galaxy
        )


subscriptions model =
    Sub.none


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


offsetStarByArmAngle star radius angle =
    let
        ( randX, randY, randZ ) =
            star.position

        x =
            (radius * cos angle) + randX

        y =
            (radius * sin angle) + randY
    in
    { star | position = ( x, y, randZ ) }


getStar curArm index randPair =
    let
        radius =
            toFloat index / radiusTuningParam

        angle =
            toFloat index / (angleTuningParam + (armAngle * (toFloat curArm + 1)))

        newAngle =
            angle + (5 * toFloat (curArm + 1))
    in
    offsetStarByArmAngle randPair radius newAngle


getArm : Int -> List Star -> List Star
getArm curArm stars =
    List.indexedMap (getStar curArm) stars


viewGalaxy angle galaxy =
    List.indexedMap getArm galaxy.arms
        |> List.concat
        |> List.map viewStar


viewSky =
    sky [ A.color Color.black ] []


viewStar { color, position } =
    let
        ( x, y, z ) =
            position
    in
    sphere
        [ --Light.type_ Light.Point
          --, A.intensity 2
          --, A.distance 120
          -- , A.color Color.white
          A.radius 0.5
        , A.position x y (y - 150)
        , Attr.attribute "material" "color: #FFF; shader: flat"
        , A.color color
        ]
        []



--Time.every (100 * Time.millisecond) (\_ -> NewAngle)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
