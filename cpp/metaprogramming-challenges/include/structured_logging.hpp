#pragma once

#include <string>
#include <string_view>
#include <sstream>
#include <iostream>
#include <utility>

namespace metaprogramming {

/**
 * Structured Logging - Challenge 1
 *
 * This implementation uses C++ macros and variadic templates to extract
 * variable names and values from logging statements.
 *
 * The LOG macro captures variable names at compile time and their values
 * at runtime, producing structured JSON output.
 */

namespace detail {
    // Helper to convert value to string representation
    template<typename T>
    std::string to_string_repr(const T& value) {
        std::ostringstream oss;
        oss << value;
        return oss.str();
    }

    // Specialization for strings to add quotes
    inline std::string to_string_repr(const std::string& value) {
        return "\"" + value + "\"";
    }

    inline std::string to_string_repr(const char* value) {
        return std::string("\"") + value + "\"";
    }

    inline std::string to_string_repr(std::string_view value) {
        return std::string("\"") + std::string(value) + "\"";
    }

    // Specialization for booleans
    inline std::string to_string_repr(bool value) {
        return value ? "true" : "false";
    }

    // Build JSON args object
    inline std::string build_args_json() {
        return "";
    }

    template<typename T, typename... Rest>
    std::string build_args_json(std::string_view name, const T& value, Rest&&... rest) {
        std::string result = "    \"" + std::string(name) + "\": " + to_string_repr(value);

        if constexpr (sizeof...(rest) > 0) {
            result += ",\n";
            result += build_args_json(std::forward<Rest>(rest)...);
        }

        return result;
    }

    // Replace placeholders in template
    inline std::string replace_placeholders(std::string_view template_str) {
        return std::string(template_str);
    }

    template<typename... Args>
    std::string replace_placeholders(std::string_view template_str,
                                    std::string_view name,
                                    Args&&... rest) {
        std::string result(template_str);

        // Replace {name} with %name%
        std::string placeholder = "{" + std::string(name) + "}";
        std::string replacement = "%" + std::string(name) + "%";

        size_t pos = result.find(placeholder);
        if (pos != std::string::npos) {
            result.replace(pos, placeholder.length(), replacement);
        }

        if constexpr (sizeof...(rest) > 0) {
            return replace_placeholders(result, std::forward<Args>(rest)...);
        }

        return result;
    }
}

/**
 * StructuredLogger - Outputs JSON formatted log entries
 */
class StructuredLogger {
public:
    template<typename... Args>
    void log_impl(std::string_view template_str, Args&&... args) {
        std::string json = "{\n";
        json += "  \"template\": \"" + std::string(template_str) + "\",\n";
        json += "  \"args\": {\n";

        if constexpr (sizeof...(args) > 0) {
            json += detail::build_args_json(std::forward<Args>(args)...);
            json += "\n";
        }

        json += "  }\n";
        json += "}";

        std::cout << json << std::endl;
    }
};

// Global logger instance
inline StructuredLogger logger;

} // namespace metaprogramming

/**
 * LOG Macro - Structured logging with automatic variable capture
 *
 * Usage: LOG("Hello {user}, balance: {balance}", user, balance)
 *
 * The macro transforms the template by replacing {var} with %var%
 * and pairs each variable name with its value.
 */
#define LOG_PAIR(var) #var, var

#define LOG_1(tmpl, v1) \
    metaprogramming::logger.log_impl( \
        metaprogramming::detail::replace_placeholders(tmpl, #v1), \
        LOG_PAIR(v1))

#define LOG_2(tmpl, v1, v2) \
    metaprogramming::logger.log_impl( \
        metaprogramming::detail::replace_placeholders(tmpl, #v1, #v2), \
        LOG_PAIR(v1), LOG_PAIR(v2))

#define LOG_3(tmpl, v1, v2, v3) \
    metaprogramming::logger.log_impl( \
        metaprogramming::detail::replace_placeholders(tmpl, #v1, #v2, #v3), \
        LOG_PAIR(v1), LOG_PAIR(v2), LOG_PAIR(v3))

#define LOG_4(tmpl, v1, v2, v3, v4) \
    metaprogramming::logger.log_impl( \
        metaprogramming::detail::replace_placeholders(tmpl, #v1, #v2, #v3, #v4), \
        LOG_PAIR(v1), LOG_PAIR(v2), LOG_PAIR(v3), LOG_PAIR(v4))

#define LOG_5(tmpl, v1, v2, v3, v4, v5) \
    metaprogramming::logger.log_impl( \
        metaprogramming::detail::replace_placeholders(tmpl, #v1, #v2, #v3, #v4, #v5), \
        LOG_PAIR(v1), LOG_PAIR(v2), LOG_PAIR(v3), LOG_PAIR(v4), LOG_PAIR(v5))

// Macro selector based on argument count
// Note: We need to skip the template arg in counting
#define GET_LOG_MACRO(_1, _2, _3, _4, _5, NAME, ...) NAME

#define LOG(tmpl, ...) \
    GET_LOG_MACRO(__VA_ARGS__, LOG_5, LOG_4, LOG_3, LOG_2, LOG_1, _)(tmpl, __VA_ARGS__)
