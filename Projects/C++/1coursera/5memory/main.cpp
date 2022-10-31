#include <iostream>

int main() {
    int num = 7;
    int * p = &num;
    int value_in_num = *p;

    std::cout << "Value of num: " << num << std::endl;
    std::cout << "Mem.Adress pointer: " << p << std::endl;
    std::cout << "Value inside pointed memory: " << value_in_num << std::endl;
    
    *p = 42;
    std::cout << "Changing *p changes the value inside pointed mem., num: " << num << std::endl;

    return 0;
}