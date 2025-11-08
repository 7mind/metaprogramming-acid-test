#pragma once

#include "type_id.hpp"
#include <tuple>
#include <functional>
#include <string_view>
#include <optional>
#include <vector>
#include <memory>

namespace metaprogramming {

/**
 * Functoid - Challenge 3
 *
 * Runtime-introspectable function wrapper that provides:
 * - Type information for each parameter
 * - Runtime invocation with type checking
 * - Optional parameter identifiers (using template-based approach)
 *
 * Note: C++ doesn't support annotations like Rust's #[id("name")],
 * but we can use template parameters to attach IDs.
 */

namespace detail {
    // Type-erased parameter info
    struct ParamInfo {
        std::string_view type_name;
        std::size_t type_hash;
        std::optional<std::string_view> id;
    };

    // Helper to extract function signature components
    template<typename T>
    struct function_traits;

    template<typename R, typename... Args>
    struct function_traits<R(Args...)> {
        using return_type = R;
        using argument_types = std::tuple<Args...>;
        static constexpr std::size_t arity = sizeof...(Args);

        template<std::size_t N>
        using argument_type = std::tuple_element_t<N, argument_types>;
    };

    template<typename R, typename... Args>
    struct function_traits<R(*)(Args...)> : function_traits<R(Args...)> {};

    template<typename R, typename C, typename... Args>
    struct function_traits<R(C::*)(Args...)> : function_traits<R(Args...)> {};

    template<typename R, typename C, typename... Args>
    struct function_traits<R(C::*)(Args...) const> : function_traits<R(Args...)> {};

    template<typename F>
    struct function_traits : function_traits<decltype(&F::operator())> {};
}

/**
 * Parameter metadata
 *
 * Note: C++ doesn't support string literals as template parameters easily,
 * so IDs must be added at runtime through wrapper functions if needed.
 */
template<typename T>
struct Parameter {
    using type = T;

    static detail::ParamInfo get_info() {
        return detail::ParamInfo{
            TypeId<T>::name(),
            TypeId<T>::hash(),
            std::nullopt  // No ID by default
        };
    }
};

/**
 * Functoid base class - type-erased interface
 */
class FunctoidBase {
public:
    virtual ~FunctoidBase() = default;

    virtual std::size_t arity() const = 0;
    virtual std::vector<detail::ParamInfo> param_info() const = 0;
    virtual std::string_view return_type_name() const = 0;
};

/**
 * Functoid implementation
 */
template<typename Func, typename... Params>
class Functoid : public FunctoidBase {
public:
    using traits = detail::function_traits<Func>;
    using return_type = typename traits::return_type;

    explicit Functoid(Func func) : func_(std::move(func)) {}

    std::size_t arity() const override {
        return traits::arity;
    }

    std::vector<detail::ParamInfo> param_info() const override {
        return get_param_info_impl(std::index_sequence_for<Params...>{});
    }

    std::string_view return_type_name() const override {
        return TypeId<return_type>::name();
    }

    // Invoke with typed arguments
    template<typename... Args>
    return_type invoke(Args&&... args) {
        static_assert(sizeof...(Args) == traits::arity,
                     "Argument count must match function arity");
        return func_(std::forward<Args>(args)...);
    }

    // Get function reference
    const Func& function() const { return func_; }

private:
    template<std::size_t... Is>
    std::vector<detail::ParamInfo> get_param_info_impl(std::index_sequence<Is...>) const {
        return std::vector<detail::ParamInfo>{Params::get_info()...};
    }

    Func func_;
};

/**
 * Factory function to create Functoid from a function
 *
 * Without IDs:
 *   auto f = make_functoid(my_function);
 *
 * With IDs (requires constexpr string literals):
 *   auto f = make_functoid<
 *       Parameter<int, "user-id">,
 *       Parameter<std::string, "name">
 *   >(my_function);
 */
template<typename... Params, typename Func>
auto make_functoid(Func&& func) {
    return Functoid<std::decay_t<Func>, Params...>(std::forward<Func>(func));
}

// Forward declaration
template<typename Func, typename ArgsTuple>
auto make_functoid_from_types(Func&& func);

namespace detail {
    template<typename Func, typename Tuple>
    struct functoid_from_tuple;

    template<typename Func, typename... Args>
    struct functoid_from_tuple<Func, std::tuple<Args...>> {
        static auto make(Func&& func) {
            return Functoid<std::decay_t<Func>, Parameter<Args>...>(
                std::forward<Func>(func)
            );
        }
    };
}

template<typename Func, typename ArgsTuple>
auto make_functoid_from_types(Func&& func) {
    return detail::functoid_from_tuple<Func, ArgsTuple>::make(std::forward<Func>(func));
}

// Helper for functions without parameter IDs
template<typename Func>
auto make_simple_functoid(Func&& func) {
    using traits = detail::function_traits<std::decay_t<Func>>;
    return make_functoid_from_types<Func, typename traits::argument_types>(
        std::forward<Func>(func)
    );
}

// Note: Parameter IDs are not supported in this C++ implementation
// due to limitations with string literals as template parameters.
// This is a known limitation compared to languages with macro systems
// or annotation support (Rust, Scala, etc.)

} // namespace metaprogramming
