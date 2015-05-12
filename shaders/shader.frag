#version 110

varying vec3 normal_cameraspace;
varying vec3 light_cameraspace;
varying vec3 vertex_cameraspace;

void main() {
    vec4 ambient   = vec4(0.2, 0, 0, 0);

    vec3 n         = normalize(normal_cameraspace);
    vec3 l         = normalize(light_cameraspace);
    float cosTheta = clamp(dot(n, l), 0, 1);
    vec4 diffuse   = vec4(vec3(1, 0, 0) * cosTheta, 1);

    vec3 v         = normalize(vertex_cameraspace);
    vec3 r         = reflect(l, n);
    float cosAlpha = clamp(dot(r, v), 0, 1);
    vec4 specular  = 0.3 * vec4(vec3(1, 1, 1) * pow(cosAlpha, 5), 1);

    gl_FragColor   = ambient + diffuse + specular; 
}

