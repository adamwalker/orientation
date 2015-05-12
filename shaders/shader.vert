#version 110

attribute vec3 coord;
attribute vec3 normal;

varying vec3 normal_cameraspace;
varying vec3 light_cameraspace;
varying vec3 vertex_cameraspace;

uniform mat4 mvp;
uniform mat4 mv;
uniform mat4 v;

void main(){
    gl_Position        = mvp * vec4(coord, 1.0);

    normal_cameraspace = (mv * vec4(normal, 0)).xyz;
    light_cameraspace  = (v * vec4(1, 1, 1, 0)).xyz;
    vertex_cameraspace = (mv * vec4(coord, 1)).xyz;
}
