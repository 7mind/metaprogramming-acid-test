#include "structured_logging.hpp"
#include "type_id.hpp"
#include "subtyping.hpp"
#include "functoid.hpp"
#include <iostream>
#include <string>

using namespace metaprogramming;

int main() {
    std::cout << "==========================================================\n";
    std::cout << "       C++ Metaprogramming Challenges - All Examples      \n";
    std::cout << "==========================================================\n";

    // Challenge 1: Structured Logging
    std::cout << "\n### CHALLENGE 1: STRUCTURED LOGGING ###\n" << std::endl;

    std::string user = "John";
    int balance = 42;
    LOG("Hello {user}, your balance is {balance}", user, balance);

    // Challenge 2: Type Reflection
    std::cout << "\n### CHALLENGE 2: TYPE REFLECTION ###\n" << std::endl;

    std::cout << "Type names:" << std::endl;
    std::cout << "  int: " << TypeId<int>::name() << std::endl;
    std::cout << "  std::string: " << TypeId<std::string>::name() << std::endl;

    std::cout << "\nType identity:" << std::endl;
    auto int_id = type_id<int>();
    auto string_id = type_id<std::string>();
    std::cout << "  is_same(int, int): " << is_same(int_id, int_id) << std::endl;
    std::cout << "  is_same(int, string): " << is_same(int_id, string_id) << std::endl;

    // Challenge 3: Functoid
    std::cout << "\n### CHALLENGE 3: FUNCTOID ###\n" << std::endl;

    auto add = [](int x, int y) { return x + y; };
    auto add_functoid = make_simple_functoid(add);

    std::cout << "Function introspection:" << std::endl;
    std::cout << "  Arity: " << add_functoid.arity() << std::endl;
    std::cout << "  Return type: " << add_functoid.return_type_name() << std::endl;

    auto params = add_functoid.param_info();
    for (size_t i = 0; i < params.size(); ++i) {
        std::cout << "  Param " << i << ": " << params[i].type_name << std::endl;
    }

    std::cout << "  Invocation: add(10, 20) = " << add_functoid.invoke(10, 20) << std::endl;

    std::cout << "\n==========================================================\n";
    std::cout << "                    All tests completed!                   \n";
    std::cout << "==========================================================\n";

    return 0;
}
