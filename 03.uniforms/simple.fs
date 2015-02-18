#version 330 core

out vec4 color;

uniform vec4 outColor; // We set this variable in the OpenGL code

void main()
{
  color = outColor;
}
