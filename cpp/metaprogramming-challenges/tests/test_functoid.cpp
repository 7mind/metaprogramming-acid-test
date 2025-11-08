#include "functoid.hpp"
#include <cassert>
#include <iostream>
#include <string>

using namespace metaprogramming;

int add(int a, int b) {
    return a + b;
}

std::string concat(const std::string& a, const std::string& b) {
    return a + b;
}

int main() {
    std::cout << "Running functoid tests..." << std::endl;

    // Test 1: Simple function
    auto add_f = make_simple_functoid(add);
    assert(add_f.arity() == 2);
    assert(add_f.invoke(5, 3) == 8);

    // Test 2: Parameter info
    auto params = add_f.param_info();
    assert(params.size() == 2);
    assert(!params[0].type_name.empty());

    // Test 3: String function
    auto concat_f = make_simple_functoid(concat);
    assert(concat_f.arity() == 2);
    assert(concat_f.invoke(std::string("Hello"), std::string(" World")) == "Hello World");

    // Test 4: Lambda
    auto mult = [](int x, int y) { return x * y; };
    auto mult_f = make_simple_functoid(mult);
    assert(mult_f.invoke(4, 5) == 20);

    // Test 5: Return type check
    auto greet_f = make_simple_functoid(concat);
    assert(!greet_f.return_type_name().empty());

    std::cout << "All functoid tests passed!" << std::endl;
    return 0;
}
