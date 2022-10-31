// Reference to header file
#include "cube.h"
#include <iostream>

namespace uiuc {
    cube::cube() {
        length_ = 1;
        std::cout << "Created $1 (default)" << std::endl;
    }

    cube::cube(double length) {
        length_ = length;
        std::cout << "Created $" << getVolume() << std::endl;
    }

    cube::cube(const cube & obj) {
        length_ = obj.length_;
        std::cout << "Created $" << getVolume() << " via copy" << std::endl;
    }

    cube::~cube() {
        std::cout << "Destroyed $" << getVolume() << std::endl;
    }

    cube & cube::operator=(const cube & obj) {
        length_ = obj.length_;
        std::cout << "Transformed $" << getVolume() << "-> $" << obj.getVolume() << std::endl;
        return *this;
    }

    double cube::getVolume() const {
        return length_*length_*length_;
    }

    double cube::getSurfaceArea() const {
        return 6*length_*length_;
    }

    void cube::setLength(double length) {
        length_ = length;
    }
}