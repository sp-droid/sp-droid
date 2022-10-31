// Always present in header files, "compile only once"
#pragma once

namespace uiuc {
    class cube {
        public:
            // Defining a constructor disables the automatic constructor. If we ONLY cube(double length), initializing it as cube c will output an error
            cube(double length); // Custom one argument constructor
            cube(const cube & obj); // Custom copy constructor
            ~cube();  // Destructor

            cube & operator=(const cube & obj); // Custom assignment operator

            double getVolume () const;
            double getSurfaceArea ()  const;
            void setLength(double length);
        
        private:
        double length_;
    };
};