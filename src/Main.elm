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
    { galaxies : List Galaxy
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


type Region
    = Center
    | Middle
    | Outskirts


type alias StarInfo =
    { index : Int
    , curArm : Int
    , armAngle : Float
    , position : Position
    , region : Region
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
    | NewAngle


starColors region =
    Random.map (Maybe.withDefault Color.lightYellow) <|
        Random.Extra.sample <|
            case region of
                Center ->
                    [ Color.lightYellow, Color.lightBlue ]

                Middle ->
                    [ Color.red, Color.lightYellow, Color.lightBlue ]

                Outskirts ->
                    [ Color.red, Color.lightYellow ]


randomPos : StarInfo -> Random.Generator Position
randomPos { index, curArm, armAngle, position } =
    let
        ( x, y, z ) =
            position

        radius =
            toFloat index / radiusTuningParam

        angle =
            toFloat index / (angleTuningParam + (armAngle * (toFloat curArm + 1)))

        newAngle =
            angle + (5 * toFloat (curArm + 1))

        xPos =
            (radius * cos newAngle) + x

        yPos =
            (radius * sin newAngle) + y

        zPos =
            z
    in
    Random.map3 (,,) (float (0 + xPos) (7 + xPos)) (float (0 + yPos) (3 + yPos)) (float (-2 + zPos) (2 + zPos))


randomStar : StarInfo -> Random.Generator Star
randomStar starInfo =
    Random.map2 Star (starColors starInfo.region) (randomPos starInfo)


generateGalaxyArm : Position -> Int -> Generator GalaxyArm
generateGalaxyArm center curArm =
    let
        starInfo index region =
            StarInfo index curArm armAngle center region

        genArm region =
            List.range 1 (starsPerArm // 3)
                |> List.map (\index -> randomStar (starInfo (index * 3) region))
                |> Random.Extra.combine
    in
    Random.map3 (+++) (genArm Center) (genArm Middle) (genArm Outskirts)


(+++) a b c =
    a ++ b ++ c


generateGalaxy center =
    List.range 0 numArms
        |> List.map (generateGalaxyArm center)
        |> Random.Extra.combine
        |> Random.map Galaxy


generateGalaxies =
    Random.Extra.combine
        [ generateGalaxy ( 20, -10, -150 )

        -- , generateGalaxy ( -100, -200, 0 )
        -- , generateGalaxy ( -200, -200, 0 )
        -- , generateGalaxy ( 200, 200, 0 )
        -- , generateGalaxy ( 300, 300, 0 )
        ]


update msg model =
    case msg of
        NewGalaxies newGalaxies ->
            ( { model | galaxies = newGalaxies }, Cmd.none )

        NewAngle ->
            ( { model | angle = model.angle + 0.005 }, Cmd.none )


view : Model -> Html msg
view model =
    scene [ A.vrModeUi True ]
        (viewSky
            :: viewBlackHole ( 12, -4, -150 )
            :: List.map (viewGalaxy model.angle) model.galaxies
        )


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


viewArm stars =
    List.map viewStar stars


viewGalaxy angle galaxy =
    galaxy.arms
        |> List.concat
        |> List.map viewStar
        |> entity []


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
        , A.position x y (z - 150)
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
