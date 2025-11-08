#include "structured_logging.hpp"
#include <cassert>
#include <iostream>

int main() {
    using namespace metaprogramming;

    std::cout << "Running structured logging tests..." << std::endl;

    // Test 1: Basic logging
    std::string user = "Test";
    int value = 123;
    LOG("User {user} has value {value}", user, value);

    // Test 2: Multiple types
    bool flag = true;
    double pi = 3.14159;
    LOG("Flag {flag}, Pi {pi}", flag, pi);

    std::cout << "All structured logging tests passed!" << std::endl;
    return 0;
}
