#include <iostream>
#include "cube.h"

using std::cout;
using std::endl;

int main() {
    uiuc::cube c(2);
    cout << "Volume: " << c.getVolume() << endl;

    uiuc::cube c2;
    cout << "Volume (default should be 1): " << c2.getVolume() << endl;
    return 0;
}