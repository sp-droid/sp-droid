// Reference to header file
#include "cube.h"

namespace uiuc {
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