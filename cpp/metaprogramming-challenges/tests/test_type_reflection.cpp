#include "type_id.hpp"
#include "subtyping.hpp"
#include <cassert>
#include <iostream>

class Base {
public:
    virtual ~Base() = default;
};

class Derived : public Base {};

int main() {
    using namespace metaprogramming;

    std::cout << "Running type reflection tests..." << std::endl;

    // Test TypeId
    auto int_id1 = type_id<int>();
    auto int_id2 = type_id<int>();
    auto double_id = type_id<double>();

    assert(is_same(int_id1, int_id2));
    assert(!is_same(int_id1, double_id));

    // Test type names are non-empty
    assert(!TypeId<int>::name().empty());
    assert(!TypeId<std::string>::name().empty());

    // Test subtyping
    assert(is_subtype_of(type_id<Derived>(), type_id<Base>()));
    assert(is_subtype_of(type_id<int>(), type_id<int>()));
    assert(!is_subtype_of(type_id<Base>(), type_id<Derived>()));

    std::cout << "All type reflection tests passed!" << std::endl;
    return 0;
}
