#include <iostream>
#include <vector>

int main() {
    std::vector<int> v;
    for (int i=0; i<100; i++) {
        v.push_back(i*i);
    }

    std::cout << v[0] << std::endl;
    std::cout << v[1] << std::endl;
    std::cout << v[12] << std::endl;

    return 0;
}