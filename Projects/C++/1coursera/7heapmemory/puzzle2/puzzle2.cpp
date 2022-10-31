#include <iostream>

using std::cout;
using std::endl;

int main() {
    int *x = new int;
    int &y = *x;
    y = 4;

    cout << &x << endl;
    cout << x << endl;
    cout << *x << endl;

    cout << &y << endl;
    cout << y << endl;
    // This can't be done cout << *y << endl;

    return 0;
}