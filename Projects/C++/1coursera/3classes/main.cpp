#include <iostream>
#include "cube.h"

using std::cout;
using std::endl;

int main() {
    uiuc::cube c;

    double length = 3.48;
    c.setLength(length);
    double volume = c.getVolume();

    // std (C standard library). Console output or cout is provided by iostream
    std::cout << "Volume: " << volume << std::endl;

    // Having declared "using std::cout", cout alone is linked to that function
    cout << "Hello World" << endl;

    // Edit length and calculate again
    cout << "\n";
    length = 2.4;
    c.setLength(length);
    cout << "Length: " << length << endl;
    cout << "Volume: " << c.getVolume() << endl;
    cout << "Surface area: " << c.getSurfaceArea() << endl;

    return 0;
}