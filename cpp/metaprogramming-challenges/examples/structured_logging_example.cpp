#include "structured_logging.hpp"
#include <string>

int main() {
    using namespace metaprogramming;

    std::cout << "=== Example 1: Basic usage ===" << std::endl;
    std::string user = "John";
    int balance = 42;
    LOG("Hello {user}, your balance is {balance}", user, balance);

    std::cout << "\n=== Example 2: Multiple variables ===" << std::endl;
    std::string name = "Alice";
    int age = 30;
    double score = 95.5;
    bool active = true;
    LOG("User {name} is {age} years old with score {score} (active: {active})",
        name, age, score, active);

    std::cout << "\n=== Example 3: Computed values ===" << std::endl;
    int x = 10;
    int y = 5;
    int sum = x + y;
    LOG("The sum of {x} and {y} is {sum}", x, y, sum);

    std::cout << "\n=== Example 4: String literals ===" << std::endl;
    std::string item = "widget";
    double price = 19.99;
    LOG("The {item} costs ${price}", item, price);

    return 0;
}
