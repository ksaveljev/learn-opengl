#version 330 core

in vec3 position;
in vec3 color;
in vec2 texCoord;

out vec3 ourColor;
out vec2 TexCoord;

void main()
{
  gl_Position = vec4(position, 1.0);
  ourColor = color;
  TexCoord = texCoord;
}
