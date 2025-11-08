#include "type_id.hpp"
#include "subtyping.hpp"
#include <iostream>
#include <vector>

// Example class hierarchy for subtyping tests
class Animal {
public:
    virtual ~Animal() = default;
    virtual void speak() const = 0;
};

class Dog : public Animal {
public:
    void speak() const override {
        std::cout << "Woof!" << std::endl;
    }
};

class Cat : public Animal {
public:
    void speak() const override {
        std::cout << "Meow!" << std::endl;
    }
};

int main() {
    using namespace metaprogramming;

    std::cout << "=== Challenge 2: Type Reflection ===" << std::endl;

    // TypeId demonstration
    std::cout << "\n--- Type Names ---" << std::endl;
    std::cout << "int: " << TypeId<int>::name() << std::endl;
    std::cout << "double: " << TypeId<double>::name() << std::endl;
    std::cout << "std::string: " << TypeId<std::string>::name() << std::endl;
    std::cout << "std::vector<int>: " << TypeId<std::vector<int>>::name() << std::endl;

    // Type hashes
    std::cout << "\n--- Type Hashes ---" << std::endl;
    std::cout << "int hash: " << TypeId<int>::hash() << std::endl;
    std::cout << "double hash: " << TypeId<double>::hash() << std::endl;

    // Type identity comparison
    std::cout << "\n--- Type Identity Comparison ---" << std::endl;
    auto int_id = type_id<int>();
    auto int_id2 = type_id<int>();
    auto double_id = type_id<double>();

    std::cout << "is_same(int, int): " << is_same(int_id, int_id2) << std::endl;
    std::cout << "is_same(int, double): " << is_same(int_id, double_id) << std::endl;

    // Subtyping checks
    std::cout << "\n--- Subtype Checking ---" << std::endl;
    std::cout << "Dog is subtype of Animal: "
              << is_subtype_of(type_id<Dog>(), type_id<Animal>()) << std::endl;
    std::cout << "Cat is subtype of Animal: "
              << is_subtype_of(type_id<Cat>(), type_id<Animal>()) << std::endl;
    std::cout << "Dog is subtype of Cat: "
              << is_subtype_of(type_id<Dog>(), type_id<Cat>()) << std::endl;
    std::cout << "Animal is subtype of Animal: "
              << is_subtype_of(type_id<Animal>(), type_id<Animal>()) << std::endl;

    // Runtime polymorphic type checking
    std::cout << "\n--- Dynamic Type Checking ---" << std::endl;
    Dog dog;
    Animal* animal_ptr = &dog;

    std::cout << "Dynamic type of animal_ptr is Dog: "
              << is_dynamic_subtype<Animal, Dog>(animal_ptr) << std::endl;
    std::cout << "Dynamic type of animal_ptr is Cat: "
              << is_dynamic_subtype<Animal, Cat>(animal_ptr) << std::endl;

    // RTTI information
    std::cout << "\n--- RTTI Information ---" << std::endl;
    std::cout << "Static type: " << get_type_info<Animal>().name() << std::endl;
    std::cout << "Dynamic type: " << get_dynamic_type_info(*animal_ptr).name() << std::endl;

    return 0;
}
