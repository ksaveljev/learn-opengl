#version 330 core

in vec3 position;
in vec3 color;
in vec2 texCoord;

out vec3 ourColor;
out vec2 TexCoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
  gl_Position = projection * view * model * vec4(position, 1.0);
  ourColor = color;
  // we swap the y-axis by subtracting our coordinates from 1
  // this is done because most images have the top y-axis inversed with OpenGL's top y-axis
  TexCoord = vec2(texCoord.x, 1.0 - texCoord.y);
}
