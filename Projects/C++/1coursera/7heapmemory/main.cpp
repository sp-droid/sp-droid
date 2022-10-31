/* The "new" operator does 3 things: allocate memory for the data,
initialize the data structure and return a pointer to it. It
does NOT return just a copy of the data
The memory is only ever reclaimed when the pointer is passed
to the "delete" operator
*/

#include <iostream>

using std::cout;
using std::endl;

int main(){
    // Points to our heap memory with enough size to store an int
    int *numPointer = new int;

    cout << "*numPointer: " << *numPointer << endl;
    cout << " numPointer: " << numPointer << endl;
    cout << "&numPointer: " << &numPointer << endl;

    *numPointer = 42;
    cout << "*numPointer assigned 42" << endl;

    cout << "*numPointer: " << *numPointer << endl;
    cout << " numPointer: " << numPointer << endl;
    cout << "&numPointer: " << &numPointer << endl;

    return 0;
}