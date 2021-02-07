module Scene3d.Skybox exposing
    ( SkyboxTexture
    , SkyboxTextureResult
    , loadEquirectTextureCmd
    , loadEquirectTextureTask
    , quad
    )

import Camera3d exposing (Camera3d)
import Direction3d exposing (Direction3d)
import Length exposing (Length, Meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Point3d exposing (Point3d)
import Scene3d.Entity as Entity exposing (Entity)
import Task exposing (Task, attempt)
import Viewpoint3d exposing (eyePoint)
import WebGL
import WebGL.Matrices as Matrices exposing (modelViewProjectionMatrix)
import WebGL.Texture as WebGL


type alias SkyboxVertex =
    { position : Vec2
    }


type alias SkyboxVarying =
    { vposition : Vec2
    }


type alias SkyboxUniforms =
    { skyboxTexture : WebGL.Texture
    , eyePoint : Vec3
    , inverseViewProjectionMatrix : Mat4
    }



-- LOADING EQUIRECTANGULAR


{-| Types of skybox textures we can use.

    At the moment we are only supporting equirectangular textures, but we have
    plans to support cube maps as well.

-}
type SkyboxTexture
    = EquirectTexture WebGL.Texture


type alias TextureURL =
    String


type alias SkyboxTextureTask =
    Task WebGL.Error SkyboxTexture


type alias SkyboxTextureResult =
    Result WebGL.Error SkyboxTexture


loadEquirectTextureTask : TextureURL -> SkyboxTextureTask
loadEquirectTextureTask =
    Task.map EquirectTexture
        << WebGL.loadWith
            { magnify = WebGL.linear
            , minify = WebGL.nearest
            , horizontalWrap = WebGL.clampToEdge
            , verticalWrap = WebGL.clampToEdge
            , flipY = True
            }


{-| Task for loading a skybox cubemap!
-}
loadEquirectTextureCmd : (SkyboxTextureResult -> msg) -> TextureURL -> Cmd msg
loadEquirectTextureCmd action =
    Task.attempt action << loadEquirectTextureTask



-- SKYBOX QUAD


type alias SkyboxQuad =
    WebGL.Entity


quad : Camera3d Meters coordinates -> Mat4 -> SkyboxTexture -> SkyboxQuad
quad camera modelViewProjectionMatrix (EquirectTexture texture) =
    let
        ( ex, ey, ez ) =
            camera
                |> Camera3d.viewpoint
                |> Viewpoint3d.eyePoint
                |> Point3d.toTuple Length.inMeters

        inverseViewProjectionMatrix =
            modelViewProjectionMatrix
                |> Math.Matrix4.inverse
                |> Maybe.withDefault Math.Matrix4.identity
    in
    WebGL.entity
        skyboxVertexShader
        skyboxFragmentShader
        mesh
        { skyboxTexture = texture
        , eyePoint = vec3 ex ey ez
        , inverseViewProjectionMatrix = inverseViewProjectionMatrix
        }



-- SKYBOX MESH


type alias SkyboxMesh =
    WebGL.Mesh SkyboxVertex


mesh : SkyboxMesh
mesh =
    let
        bottomLeft =
            SkyboxVertex (vec2 -1 -1)

        bottomRight =
            SkyboxVertex (vec2 1 -1)

        topLeft =
            SkyboxVertex (vec2 -1 1)

        topRight =
            SkyboxVertex (vec2 1 1)
    in
    WebGL.triangles
        [ ( bottomLeft, bottomRight, topLeft )
        , ( topLeft, bottomRight, topRight )
        ]


skyboxVertexShader : WebGL.Shader SkyboxVertex SkyboxUniforms SkyboxVarying
skyboxVertexShader =
    [glsl|
        attribute vec2 position;
        varying vec2 vposition;

        void main() {
            vposition = position;
            gl_Position = vec4(position.x, position.y, 0, 1.0);
        }
    |]


skyboxFragmentShader : WebGL.Shader {} SkyboxUniforms SkyboxVarying
skyboxFragmentShader =
    [glsl|
        precision mediump float;

        const float PI = 3.1415926535897932384626433832795;
        const float M_PI = 1.0 / PI;      
        const float M_2PI = 1.0 / (2.0 * PI);

        uniform vec3 eyePoint;
        uniform sampler2D skyboxTexture;
        uniform mat4 inverseViewProjectionMatrix;

        varying vec2 vposition;

        void main() {
            vec2 textureCoordinate;

            vec4 projPos = inverseViewProjectionMatrix * vec4(vposition.x, vposition.y, 0.0, 1.0);
            vec3 skyboxPoint = projPos.xyz/projPos.w;

            vec3 skyboxRay = normalize(skyboxPoint - eyePoint);

            textureCoordinate.x = 0.5 + atan(skyboxRay.x, skyboxRay.y) * M_2PI;
            textureCoordinate.y = 0.5 + asin(skyboxRay.z) * M_PI;

            gl_FragColor = texture2D(skyboxTexture, textureCoordinate);
        }
    |]
