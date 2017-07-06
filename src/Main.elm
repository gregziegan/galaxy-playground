module Main exposing (main)

import AFrame exposing (..)
import AFrame.Primitives exposing (box, sky, sphere)
import AFrame.Primitives.Attributes as A
import AFrame.Primitives.Light as Light
import Array
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes as Attr
import Random exposing (float, int, list, map4)
import Random.Extra exposing (frequency)
import Time


type alias Model =
    { randPairs : List (List ( Color, Float, Float, Float ))
    , angle : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { randPairs = []
      , angle = 20
      }
    , Random.generate NewRandomPairs generateGalaxy
    )


type Msg
    = NewRandomPairs (List (List ( Color, Float, Float, Float )))
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


randomPoint : Int -> Random.Generator ( Color, Float, Float, Float )
randomPoint distance =
    map4 (,,,) (starColors distance) (float -5 5) (float -5 5) (float -50 50)


generateRandomArm distance =
    list (starsPerArm // 3) (randomPoint distance)


generateRandomPairs distance =
    list numArms (generateRandomArm distance)


(+++) a b c =
    a ++ b ++ c


generateGalaxy =
    Random.map3 (+++) (generateRandomPairs 1) (generateRandomPairs 2) (generateRandomPairs 3)


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
    Sub.none



--Time.every (100 * Time.millisecond) (\_ -> NewAngle)


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


getStarHelper ( color, randX, randY, z ) radius angle =
    let
        x =
            (radius * cos angle) + randX

        y =
            (radius * sin angle) + randY
    in
    ( color, x, y, z )


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


getArm : Int -> List ( Color, Float, Float, Float ) -> List ( Color, Float, Float, Float )
getArm curArm randPairs =
    List.indexedMap (getStar curArm) randPairs


viewGalaxy angle randPairs =
    List.indexedMap getArm randPairs
        |> List.concat
        |> List.map viewStar


viewSky =
    sky [ A.color Color.black ] []


viewStar ( color, dx, dy, dz ) =
    sphere
        [ --Light.type_ Light.Point
          --, A.intensity 2
          --, A.distance 120
          -- , A.color Color.white
          A.radius 0.5
        , A.position dx dy (dz - 150)
        , Attr.attribute "material" "color: #FFF; shader: flat"
        , A.color color
        ]
        []
