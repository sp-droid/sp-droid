// Always present in header files, "compile only once"
#pragma once

namespace uiuc {
    class cube {
        public:
            // Defining a constructor disables the automatic constructor. If we ONLY cube(double length), initializing it as cube c will output an error
            cube(); // Custom default constructor
            cube(double length); // Custom one argument constructor

            double getVolume ();
            double getSurfaceArea ();
            void setLength(double length);
        
        private:
        double length_;
    };
};