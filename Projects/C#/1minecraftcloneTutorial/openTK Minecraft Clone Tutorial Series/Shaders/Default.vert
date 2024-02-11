#version 330 core

// Vertex coordinates
layout (location = 0) in vec3 aPosition;

void main()
{
	// Coordinates
	gl_Position = vec4(aPosition, 1.0);
}