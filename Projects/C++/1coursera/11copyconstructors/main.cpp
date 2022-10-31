#include <iostream>
#include "cube.h"

using std::cout;
using std::endl;

bool byValue(uiuc::cube c) {
    return true;
}

bool byRef(uiuc::cube & c) {
    return true;
}

bool byPoint(uiuc::cube * c) {
    return true;
}


int main() {
    uiuc::cube c(10);

    cout << "By value, c2 is a copy of c" << endl;
    uiuc::cube c2 = c;

    cout << "By reference, both c3 and c are the same" << endl;
    uiuc::cube & c3 = c;

    cout << "By pointer, c4 points to c" << endl;
    uiuc::cube * c4 = &c;

    cout << "RETURN By value, c2 is a copy of c" << endl;
    byValue(c);
    cout << "RETURN By reference, both c3 and c are the same" << endl;
    byRef(c);
    cout << "RETURN By pointer, c4 points to c" << endl;
    byPoint(&c);

    return 0;
}