module Galaxy exposing (Galaxy, generate, view)

{-| Module for generating randomize galaxies:

Plans to support:

  - [ ] Spiral
  - [ ] Elliptical
  - [ ] Lenticular
  - [ ] Irregular

-}

import AFrame exposing (entity)
import AFrame.Animations as Anim
import AFrame.Primitives exposing (box, sky, sphere)
import AFrame.Primitives.Attributes as A
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attr
import Random exposing (Generator, float, int, list, map4)
import Random.Extra exposing (frequency)


type Galaxy
    = Spiral SpiralGalaxy


type alias SpiralGalaxy =
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
    | Arms


type alias StarInfo =
    { index : Int
    , curArm : Int
    , armAngle : Float
    , position : Position
    , region : Region
    }


numArms =
    20


starsPerArm =
    50


armAngle =
    270 / numArms


radiusTuningParam =
    0.4


angleTuningParam =
    1


generate : Position -> Generator Galaxy
generate center =
    List.range 0 numArms
        |> List.map (generateGalaxyArm center)
        |> Random.Extra.combine
        |> Random.map (Spiral << SpiralGalaxy)


starColors region =
    Random.map (Maybe.withDefault Color.lightYellow) <|
        Random.Extra.sample <|
            case region of
                Center ->
                    [ Color.red, Color.darkYellow ]

                Arms ->
                    [ Color.lightYellow, Color.lightBlue ]


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
    Random.map2 (++) (genArm Center) (genArm Arms)


viewArm stars =
    List.map viewStar stars


view : Galaxy -> Html msg
view galaxy =
    case galaxy of
        Spiral spiralGalaxy ->
            spiralGalaxy.arms
                |> List.concat
                |> List.map viewStar
                |> (::) animateGalaxy
                |> entity []


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
        , A.position x y z
        , Attr.attribute "material" "color: #FFF; shader: flat"
        , A.color color
        ]
        []


animateGalaxy =
    Anim.animation
        [ Anim.attribute_ "rotation"
        , Anim.dur 100000
        , Anim.fill "forwards"
        , Anim.to "0 0 360"
        , Anim.repeat "indefinite"
        , Anim.easing "linear"
        ]
        []
