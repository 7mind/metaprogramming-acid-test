#pragma once

#include <string_view>
#include <cstddef>

namespace metaprogramming {

/**
 * TypeId - Compile-time type identification
 *
 * Challenge 2: Library-Based Reflection
 *
 * This implementation uses compiler-specific macros to extract type names
 * at compile time, similar to Boost.TypeIndex but simplified.
 *
 * Supported compilers:
 * - GCC/Clang: __PRETTY_FUNCTION__
 * - MSVC: __FUNCSIG__
 */

namespace detail {
    // Extract type name from function signature at compile time
    template<typename T>
    constexpr std::string_view type_name_impl() {
#if defined(__clang__) || defined(__GNUC__)
        // __PRETTY_FUNCTION__ format:
        // "constexpr std::string_view metaprogramming::detail::type_name_impl() [T = TYPE_NAME]"
        constexpr std::string_view full = __PRETTY_FUNCTION__;
        constexpr std::string_view prefix = "[T = ";
        constexpr std::string_view suffix = "]";

        constexpr auto start = full.find(prefix) + prefix.size();
        constexpr auto end = full.rfind(suffix);

        return full.substr(start, end - start);
#elif defined(_MSC_VER)
        // __FUNCSIG__ format:
        // "class std::basic_string_view<char,struct std::char_traits<char> > __cdecl metaprogramming::detail::type_name_impl<TYPE_NAME>(void)"
        constexpr std::string_view full = __FUNCSIG__;
        constexpr std::string_view prefix = "type_name_impl<";
        constexpr std::string_view suffix = ">(void)";

        constexpr auto start = full.find(prefix) + prefix.size();
        constexpr auto end = full.rfind(suffix);

        return full.substr(start, end - start);
#else
        return "unknown";
#endif
    }

    // Compile-time hash function (FNV-1a)
    constexpr std::size_t hash_string(std::string_view str) {
        std::size_t hash = 14695981039346656037ULL;
        for (char c : str) {
            hash ^= static_cast<std::size_t>(c);
            hash *= 1099511628211ULL;
        }
        return hash;
    }
}

/**
 * TypeId<T> - Compile-time type identifier
 *
 * Provides:
 * - name(): Get human-readable type name
 * - hash(): Get compile-time hash of type
 * - operator==: Compare type identities
 */
template<typename T>
class TypeId {
public:
    using type = T;

    constexpr TypeId() = default;

    // Get type name
    static constexpr std::string_view name() {
        return detail::type_name_impl<T>();
    }

    // Get type hash
    static constexpr std::size_t hash() {
        return detail::hash_string(name());
    }

    // Compare two type IDs
    template<typename U>
    constexpr bool operator==(const TypeId<U>&) const {
        return std::is_same_v<T, U>;
    }

    template<typename U>
    constexpr bool operator!=(const TypeId<U>& other) const {
        return !(*this == other);
    }
};

// Helper function to create TypeId
template<typename T>
constexpr TypeId<T> type_id() {
    return TypeId<T>{};
}

// Runtime type identity comparison
template<typename A, typename B>
constexpr bool is_same(TypeId<A>, TypeId<B>) {
    return std::is_same_v<A, B>;
}

} // namespace metaprogramming
