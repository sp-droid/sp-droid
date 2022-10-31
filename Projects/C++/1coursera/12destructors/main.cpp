#include <iostream>
#include "cube.h"

using std::cout;
using std::endl;

double cubeOnStack() {
    uiuc::cube c(3);
    return c.getVolume();
}

void cubeOnHeap() {
    uiuc::cube * c1 = new uiuc::cube(10);
    uiuc::cube * c2 = new uiuc::cube;
    delete c1;
}

int main() {
    cubeOnStack();
    cubeOnHeap();
    cubeOnStack();

    return 0;
}