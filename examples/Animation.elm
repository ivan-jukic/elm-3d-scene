module Animation exposing (main)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Element
import Element.Font
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { angle : Angle -- Rotation angle of the cube
    }


type Msg
    = Tick Duration -- Elapsed time since last animation frame


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { angle = Quantity.zero }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick duration) model =
    let
        -- Speed at which the cube rotates
        rotationRate =
            Angle.degrees 90 |> Quantity.per Duration.second

        -- Update the current angle by adding a delta equal to the elapsed time
        -- since the last animation frame multiplied by the rotation rate
        updatedAngle =
            model.angle |> Quantity.plus (duration |> Quantity.at rotationRate)
    in
    ( { model | angle = updatedAngle }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Subscribe to animation frames and wrap each time step (a number of
    -- milliseconds) into a typed Duration value
    Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


{-| Create a cube entity with different-colored faces by constructing six
separate quads
-}
cube : Scene3d.Entity WorldCoordinates
cube =
    let
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.cssPixels -8

        positive =
            Length.cssPixels 8

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces
        bottom =
            Scene3d.quad (Material.color Color.blue) p1 p2 p3 p4

        top =
            Scene3d.quad (Material.color Color.blue) p5 p6 p7 p8

        front =
            Scene3d.quad (Material.color Color.orange) p2 p3 p7 p6

        back =
            Scene3d.quad (Material.color Color.orange) p1 p4 p8 p5

        left =
            Scene3d.quad (Material.color Color.green) p1 p2 p6 p5

        right =
            Scene3d.quad (Material.color Color.green) p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]


view : Model -> Html Msg
view model =
    let
        -- Define the axis that the cube rotates around
        rotationAxis =
            Axis3d.through Point3d.origin (Direction3d.xz (Angle.degrees 45))

        -- Rotate the initial cube around the rotation axis by the current angle
        rotatedCube =
            cube |> Scene3d.rotateAround rotationAxis model.angle

        -- Create an isometric camera
        camera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.isometric
                        { focalPoint = Point3d.origin
                        , distance = Length.cssPixels 100
                        }
                , viewportHeight = Length.cssPixels 32
                }
    in
    -- Create a little loading spinner using elm-ui
    Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.row [ Element.centerX, Element.centerY ]
            [ Element.html <|
                Scene3d.unlit
                    { camera = camera
                    , dimensions = ( Pixels.pixels 32, Pixels.pixels 32 )
                    , entities = [ rotatedCube ]
                    , clipDepth = Length.cssPixels 10
                    , background = Scene3d.transparentBackground
                    }
            , Element.text "Loading..."
            ]
