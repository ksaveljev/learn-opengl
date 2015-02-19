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
  // we swap the y-axis by subtracting our coordinates from 1
  // this is done because most images have the top y-axis inversed with OpenGL's top y-axis
  TexCoord = vec2(texCoord.x, 1.0 - texCoord.y);
}
