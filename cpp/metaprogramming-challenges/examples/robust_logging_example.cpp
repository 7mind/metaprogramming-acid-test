#include "structured_logging.hpp"
#include <vector>
#include <cmath>

int compute_value() {
    return 42;
}

int main() {
    using namespace metaprogramming;

    std::cout << "=== Robust Logging Examples ===" << std::endl;

    std::cout << "\n=== Example 1: Function calls ===" << std::endl;
    LOG_ROBUST("Computed: %result%", "result", compute_value());

    std::cout << "\n=== Example 2: Complex expressions ===" << std::endl;
    int x = 10;
    int y = 20;
    LOG_ROBUST("Sum: %sum%, Product: %product%",
               "sum", x + y,
               "product", x * y);

    std::cout << "\n=== Example 3: Method calls ===" << std::endl;
    std::string text = "Hello";
    LOG_ROBUST("Length: %len%, substr: %sub%",
               "len", text.length(),
               "sub", text.substr(0, 3));

    std::cout << "\n=== Example 4: Math functions ===" << std::endl;
    double pi = 3.14159;
    LOG_ROBUST("Pi: %pi%, sqrt: %sqrt%",
               "pi", pi,
               "sqrt", std::sqrt(16.0));

    std::cout << "\n=== Example 5: Collection operations ===" << std::endl;
    std::vector<int> numbers = {1, 2, 3, 4, 5};
    LOG_ROBUST("Size: %size%, first: %first%",
               "size", numbers.size(),
               "first", numbers[0]);

    std::cout << "\n=== Example 6: Nested expressions ===" << std::endl;
    LOG_ROBUST("Result: %result%",
               "result", compute_value() * 2);

    std::cout << "\n=== Example 7: Conditional expressions ===" << std::endl;
    int score = 85;
    LOG_ROBUST("Grade: %grade%, Score: %score%",
               "grade", (score >= 60 ? "Pass" : "Fail"),
               "score", score);

    std::cout << "\n=== Example 8: Type conversions ===" << std::endl;
    LOG_ROBUST("Int to string: %str%",
               "str", std::to_string(123));

    std::cout << "\n=== All robust examples completed! ===" << std::endl;

    return 0;
}
