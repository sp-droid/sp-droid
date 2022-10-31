// Reference to header file
#include "cube.h"

namespace uiuc {
    cube::cube() {
        length_ = 1;
    }

    cube::cube(double length) {
        length_ = length;
    }

    double cube::getVolume() {
        return length_*length_*length_;
    }

    double cube::getSurfaceArea() {
        return 6*length_*length_;
    }

    void cube::setLength(double length) {
        length_ = length;
    }
}