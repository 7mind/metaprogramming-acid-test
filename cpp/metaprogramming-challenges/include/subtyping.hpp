#pragma once

#include "type_id.hpp"
#include <typeinfo>

namespace metaprogramming {

/**
 * Subtype checking - Challenge 2
 *
 * Uses both compile-time (std::is_base_of) and runtime (RTTI) approaches
 * to check subtype relationships.
 */

// Compile-time subtype check
template<typename Child, typename Parent>
constexpr bool is_subtype_of_compiletime() {
    return std::is_base_of_v<Parent, Child> || std::is_same_v<Parent, Child>;
}

// Runtime subtype check using TypeId
template<typename A, typename B>
bool is_subtype_of(TypeId<A>, TypeId<B>) {
    // At runtime, we can use compile-time information
    return is_subtype_of_compiletime<A, B>();
}

/**
 * Runtime polymorphic subtype check
 *
 * Uses RTTI to check if an object's dynamic type is a subtype
 * of a given static type.
 */
template<typename Base, typename Derived>
bool is_dynamic_subtype(const Base* obj) {
    static_assert(std::is_polymorphic_v<Base>,
                  "Base type must be polymorphic (have virtual functions)");
    return dynamic_cast<const Derived*>(obj) != nullptr;
}

/**
 * Get runtime type information
 */
template<typename T>
const std::type_info& get_type_info() {
    return typeid(T);
}

template<typename T>
const std::type_info& get_dynamic_type_info(const T& obj) {
    static_assert(std::is_polymorphic_v<T>,
                  "Type must be polymorphic to get dynamic type info");
    return typeid(obj);
}

/**
 * IsSubtype concept (C++20)
 */
template<typename Derived, typename Base>
concept IsSubtype = std::is_base_of_v<Base, Derived> || std::is_same_v<Base, Derived>;

} // namespace metaprogramming
