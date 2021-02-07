module Skybox exposing (main)

{-|
-}

import Angle exposing (Angle)
import Browser
import Browser.Dom
import Browser.Events
import Block3d
import Camera3d
import Color
import Direction3d
import Frame3d
import Json.Decode as Decode exposing (Decoder)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Task
import Viewpoint3d
import Length
import WebGL.Texture as Texture
import Scene3d.Skybox as Skybox exposing
    ( SkyboxTexture
    , SkyboxTextureResult
    , loadEquirectTextureCmd
    )


type alias Model =
    { width : Quantity Int Pixels -- Width of the browser window
    , height : Quantity Int Pixels -- Height of the browser window
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , skyboxTexture : Maybe SkyboxTexture
    }


type Msg
    = MouseUp
    | MouseDown
    | Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | SkyboxLoaded SkyboxTextureResult


type WorldCoordinates
    = WorldCoordinates


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


init : () -> (Model, Cmd Msg)
init _ =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , orbiting = False
      , azimuth = Angle.degrees 135
      , elevation = Angle.degrees 5
      , skyboxTexture = Nothing
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize
                    (Pixels.int (round viewport.width))
                    (Pixels.int (round viewport.height))
            )
            Browser.Dom.getViewport
        
        -- Load equirectangular texture
        , Skybox.loadEquirectTextureCmd
            SkyboxLoaded
            "../assets/equirect-skybox-aurora.jpg"
        ]
    )


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Browser.Events.onResize
            (\w h -> Resize (Pixels.int w) (Pixels.int h))

        --
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model | width = w, height = h }, Cmd.none )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -60) (Angle.degrees 60)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )
        

        SkyboxLoaded (Ok texture) ->
            ( { model | skyboxTexture = Just texture }, Cmd.none )
        
        SkyboxLoaded (Err err) ->
            let
                _ = Debug.log "skybox err" err
            in
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 20
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "Skybox"
    , body =
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60)
            , shadows = False
            , dimensions = ( model.width, model.height )
            , camera = camera
            , clipDepth = Length.centimeters 10
            , background = 
                model.skyboxTexture
                    |> Maybe.map Scene3d.backgroundSkybox
                    |> Maybe.withDefault (Scene3d.backgroundColor Color.lightBlue)
            , entities =
                [ Scene3d.block
                    ( Material.matte Color.lightBrown)
                    ( Block3d.centeredOn 
                        (Frame3d.atPoint
                            (Point3d.centimeters 0 0 20)
                        )
                        ( Length.centimeters 20
                        , Length.centimeters 20
                        , Length.centimeters 20
                        )
                    )
                ]
            }
        ]
    }
