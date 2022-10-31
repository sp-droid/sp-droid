// You should define Pair here:
// (Use as many lines as you need!)
// ...
// ...

// This main() function will help you test your work.
// Click Run to see what happens.
// When you're sure you're finished, click Submit for grading
// with our additional hidden tests.
#include <iostream>

class Pair {
  public:
    int a;
    int b;
    int sum() {
      return a+b;
    };
};

int main() {
  Pair p;
  p.a = 100;
  p.b = 200;
  if (p.a + p.b == p.sum()) {
    std::cout << "Success!" << std::endl;
  } else {
    std::cout << "p.sum() returns " << p.sum() << " instead of " << (p.a + p.b) << std::endl;
  }
  return 0;
}