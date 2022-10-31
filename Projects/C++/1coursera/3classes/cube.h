// Always present in header files, "compile only once"
#pragma once

namespace uiuc {
    class cube {
        public:
            double getVolume ();
            double getSurfaceArea ();
            void setLength(double length);
        
        private:
        double length_;
    };
};