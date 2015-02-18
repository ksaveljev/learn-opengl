#version 330 core

in vec3 position;
in vec3 color;

out vec3 ourColor; // output a color to the fragment shader

void main()
{
  gl_Position = vec4(position, 1.0);
  ourColor = color;
}
