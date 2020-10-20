module Scene3d.Skybox exposing (..)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import WebGL
import WebGL.Settings.DepthTest as DepthTest exposing (default)


type alias Vertex =
    { position : Vec2
    , color : Vec3
    }


quad : WebGL.Entity
quad =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        {}


mesh : WebGL.Mesh Vertex
mesh =
    let
        bottomLeft =
            Vertex (vec2 -1 -1) (vec3 1 0 0)

        bottomRight =
            Vertex (vec2 1 -1) (vec3 1 1 0)

        topLeft =
            Vertex (vec2 -1 1) (vec3 1 0 1)

        topRight =
            Vertex (vec2 1 1) (vec3 0 1 0)
    in
    WebGL.triangles
        [ ( bottomLeft, bottomRight, topLeft )
        , ( topLeft, bottomRight, topRight )
        ]


type alias Varying =
    { vposition : Vec2
    , vcolor : Vec3
    }


vertexShader : WebGL.Shader Vertex {} Varying
vertexShader =
    [glsl|
        attribute vec2 position;
        attribute vec3 color;

        varying vec2 vposition;
        varying vec3 vcolor;

        void main() {
            vposition = position;
            vcolor = color;
            gl_Position = vec4(position.x, position.y, 0, 1.0);
        }
    |]



-- fragmentShader : WebGL.Shader {} { uniforms | u_viewDirectionProjectionInverse : Mat4 } { v_position : Vec4 }
-- fragmentShader =
--     [glsl|
--         precision mediump float;
--         uniform samplerCube u_skybox;
--         uniform mat4 u_viewDirectionProjectionInverse;
--         varying vec4 v_position;
--         void main() {
--             vec4 t = u_viewDirectionProjectionInverse * v_position;
--             gl_FragColor = textureCube(u_skybox, normalize(t.xyz / t.w));
--         }
--     |]


fragmentShader : WebGL.Shader {} {} Varying
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec2 vposition;
        varying vec3 vcolor;

        void main() {
            gl_FragColor = vec4(vcolor, 1);
        }
    |]
