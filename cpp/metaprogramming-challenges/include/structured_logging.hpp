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

    // Robust version: takes expression string for fallback
    inline std::string build_args_json_robust() {
        return "";
    }

    template<typename T, typename... Rest>
    std::string build_args_json_robust(std::string_view name,
                                       std::string_view expr_str,
                                       const T& value,
                                       Rest&&... rest) {
        std::string result;
        try {
            result = "    \"" + std::string(name) + "\": " + to_string_repr(value);
        } catch (...) {
            // Fallback: use the expression string as a quoted value
            result = "    \"" + std::string(name) + "\": \"<expr: " + std::string(expr_str) + ">\"";
        }

        if constexpr (sizeof...(rest) > 0) {
            result += ",\n";
            result += build_args_json_robust(std::forward<Rest>(rest)...);
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

/**
 * Robust logging macros - handle complex expressions safely
 *
 * Usage: LOG_ROBUST("Template %name%", "name", expression)
 *
 * The macro captures both the expression string (via stringify) and its value,
 * providing a fallback if serialization fails.
 */
#define LOG_ROBUST_PAIR(name, expr) name, #expr, expr

#define LOG_ROBUST_1(tmpl, name1, expr1) \
    do { \
        std::string template_str = tmpl; \
        std::string args_json = metaprogramming::detail::build_args_json_robust( \
            LOG_ROBUST_PAIR(name1, expr1) \
        ); \
        std::string json = "{\n  \"template\": \"" + template_str + "\",\n  \"args\": {\n" + \
                          args_json + "\n  }\n}"; \
        std::cout << json << std::endl; \
    } while(0)

#define LOG_ROBUST_2(tmpl, name1, expr1, name2, expr2) \
    do { \
        std::string template_str = tmpl; \
        std::string args_json = metaprogramming::detail::build_args_json_robust( \
            LOG_ROBUST_PAIR(name1, expr1), \
            LOG_ROBUST_PAIR(name2, expr2) \
        ); \
        std::string json = "{\n  \"template\": \"" + template_str + "\",\n  \"args\": {\n" + \
                          args_json + "\n  }\n}"; \
        std::cout << json << std::endl; \
    } while(0)

#define LOG_ROBUST_3(tmpl, name1, expr1, name2, expr2, name3, expr3) \
    do { \
        std::string template_str = tmpl; \
        std::string args_json = metaprogramming::detail::build_args_json_robust( \
            LOG_ROBUST_PAIR(name1, expr1), \
            LOG_ROBUST_PAIR(name2, expr2), \
            LOG_ROBUST_PAIR(name3, expr3) \
        ); \
        std::string json = "{\n  \"template\": \"" + template_str + "\",\n  \"args\": {\n" + \
                          args_json + "\n  }\n}"; \
        std::cout << json << std::endl; \
    } while(0)

#define GET_LOG_ROBUST_MACRO(_1, _2, _3, _4, _5, _6, _7, NAME, ...) NAME

#define LOG_ROBUST(tmpl, ...) \
    GET_LOG_ROBUST_MACRO(__VA_ARGS__, _, LOG_ROBUST_3, _, LOG_ROBUST_2, _, LOG_ROBUST_1, _)(tmpl, __VA_ARGS__)
