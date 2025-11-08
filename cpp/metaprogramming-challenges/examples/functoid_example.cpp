#include "functoid.hpp"
#include <iostream>
#include <string>

using namespace metaprogramming;

// Simple function
int add(int x, int y) {
    return x + y;
}

// Function with multiple types
std::string greet(const std::string& name, int age) {
    return "Hello, " + name + "! You are " + std::to_string(age) + " years old.";
}

// Function for demonstration
std::string create_greeting(const std::string& msg, const std::string& name) {
    return msg + ", " + name;
}

int main() {
    std::cout << "=== Challenge 3: Functoid ===" << std::endl;

    // Example 1: Simple functoid without IDs
    std::cout << "\n--- Example 1: Simple Function ---" << std::endl;
    auto add_functoid = make_simple_functoid(add);

    std::cout << "Arity: " << add_functoid.arity() << std::endl;
    std::cout << "Return type: " << add_functoid.return_type_name() << std::endl;

    auto params = add_functoid.param_info();
    for (size_t i = 0; i < params.size(); ++i) {
        std::cout << "Param " << i << ": " << params[i].type_name;
        if (params[i].id) {
            std::cout << " @id(\"" << *params[i].id << "\")";
        }
        std::cout << std::endl;
    }

    std::cout << "Result: " << add_functoid.invoke(10, 20) << std::endl;

    // Example 2: String function
    std::cout << "\n--- Example 2: String Function ---" << std::endl;
    auto greet_functoid = make_simple_functoid(create_greeting);

    std::cout << "Arity: " << greet_functoid.arity() << std::endl;
    std::cout << "Return type: " << greet_functoid.return_type_name() << std::endl;

    auto greet_params = greet_functoid.param_info();
    for (size_t i = 0; i < greet_params.size(); ++i) {
        std::cout << "Param " << i << ": " << greet_params[i].type_name << std::endl;
    }

    std::cout << "Result: "
              << greet_functoid.invoke(std::string("Hello"), std::string("Alice"))
              << std::endl;

    // Example 3: Lambda functoid
    std::cout << "\n--- Example 3: Lambda Function ---" << std::endl;
    auto multiply = [](int a, int b) { return a * b; };
    auto mult_functoid = make_simple_functoid(multiply);

    std::cout << "Arity: " << mult_functoid.arity() << std::endl;
    std::cout << "Return type: " << mult_functoid.return_type_name() << std::endl;
    std::cout << "Result: " << mult_functoid.invoke(5, 7) << std::endl;

    // Example 4: Complex types
    std::cout << "\n--- Example 4: Complex Parameter Types ---" << std::endl;
    auto complex_func = [](const std::string& s, double d, bool b) {
        return s + " " + std::to_string(d) + " " + (b ? "true" : "false");
    };

    auto complex_functoid = make_simple_functoid(complex_func);
    auto complex_params = complex_functoid.param_info();

    for (size_t i = 0; i < complex_params.size(); ++i) {
        std::cout << "Param " << i << ": " << complex_params[i].type_name << std::endl;
    }

    std::cout << "Result: "
              << complex_functoid.invoke(std::string("test"), 3.14, true)
              << std::endl;

    return 0;
}
