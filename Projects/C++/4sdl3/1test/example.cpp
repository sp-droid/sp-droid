
#include <iostream>
#include <SDL3/SDL.h>

#include <crtdbg.h>

// You shouldn't really use this statement, but it's fine for small programs
using namespace std;

// You must include the command line parameters for your main function to be recognized by SDL
int main(int argc, char** args) {
	// Memory leaks
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

	// Pointers to our window and surface
	SDL_Surface* winSurface = NULL;
	SDL_Window* window = NULL;

	// Create our window
	window = SDL_CreateWindow( "Example", 1280, 720, 0 );

	// Make sure creating the window succeeded
	if ( !window ) {
		cout << "Error creating window: " << SDL_GetError()  << endl;
		system("pause");
		// End the program
		return 1;
	}

	// Get the surface from the window
	winSurface = SDL_GetWindowSurface( window );

	// Make sure getting the surface succeeded
	if ( !winSurface ) {
		cout << "Error getting surface: " << SDL_GetError() << endl;
		system("pause");
		// End the program
		return 1;
	}

	// Fill the window with a white rectangle
	Uint8 r = 255, g = 255, b = 0, a = 255;
	SDL_PixelFormat format = SDL_GetWindowPixelFormat(window);
	const SDL_PixelFormatDetails * formatDetails = SDL_GetPixelFormatDetails(format);
	Uint32 color = SDL_MapRGBA(formatDetails, NULL, r, g, b, a);
	SDL_FillSurfaceRect( winSurface, NULL, color);

	// Update the window display
	SDL_UpdateWindowSurface( window );

	// Wait
	//system("pause");
	SDL_Delay(2000);

	// Destroy the window. This will also destroy the surface
	SDL_DestroyWindow( window );

	// Quit SDL
	SDL_Quit();

	// End the program
	return 0;
}