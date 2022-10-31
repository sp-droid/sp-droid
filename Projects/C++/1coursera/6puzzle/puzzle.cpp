double someOtherFunction();

#include <iostream>
#include "Cube.h"
using uiuc::cube;

cube *CreateUnitCube() {
    cube x;
    x.setLength(15);
    return &x;
}

int main() {
    cube *c = CreateUnitCube();
    someOtherFunction();
    double a = c->getSurfaceArea();
    std::cout << "Surface Area: " << a << std::endl;
    double v = c->getVolume();
    std::cout << "Volume: " << v << std::endl;
    return 0;
}

// Some other function that does something that uses some stack memory.
// In this code, we calculate the total volume of cubes of length from 0..99.
double someOtherFunction() {
  cube cubes[100];
  double totalVolume = 0;

  for (int i = 0; i < 100; i++) {
    cubes[i].setLength(i);
    totalVolume += cubes[i].getVolume();
  }

  return totalVolume;
}