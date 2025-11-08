//! Structured Logging - Challenge 1: Effortless Structured Logging
//!
//! This library implements structured logging that automatically extracts
//! both the template and variable values without compiler plugins.
//!
//! ## Example
//!
//! ```rust
//! use structured_logging::log;
//!
//! let user = "John";
//! let balance = 42;
//! log!("Hello {user}, your balance is {balance}", user, balance);
//! ```
//!
//! This outputs:
//! ```json
//! {
//!   "template": "Hello %user%, your balance is %balance%",
//!   "args": {
//!     "user": "John",
//!     "balance": 42
//!   }
//! }
//! ```

use serde::{Serialize, Deserialize};
use serde_json::Value;
use std::collections::HashMap;

/// The structured log entry containing template and arguments
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct LogEntry {
    pub template: String,
    pub args: HashMap<String, Value>,
}

impl LogEntry {
    /// Create a new log entry
    pub fn new(template: String, args: HashMap<String, Value>) -> Self {
        Self { template, args }
    }

    /// Convert to JSON string
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap()
    }

    /// Print the log entry as JSON
    pub fn print(&self) {
        println!("{}", self.to_json());
    }
}

/// Helper macro to extract identifier from expression
/// This converts {user} style syntax into the identifier name
#[doc(hidden)]
#[macro_export]
macro_rules! __extract_ident {
    ($e:ident) => {
        stringify!($e)
    };
    ($e:expr) => {
        stringify!($e)
    };
}

/// Main structured logging macro
///
/// Takes a format string and captures variable names and values.
///
/// # Example
///
/// ```rust
/// use structured_logging::log;
///
/// let name = "Alice";
/// let age = 30;
/// log!("User {name} is {age} years old", name, age);
/// ```
#[macro_export]
macro_rules! log {
    // Pattern: log!("template {var1} text {var2}", var1, var2)
    ($template:expr, $($var:ident),* $(,)?) => {{
        use std::collections::HashMap;
        use serde_json::json;

        let mut template_str = $template.to_string();
        let mut args: HashMap<String, serde_json::Value> = HashMap::new();

        $(
            // Replace {var} with %var% in template
            let var_name = stringify!($var);
            let placeholder_old = format!("{{{}}}", var_name);
            let placeholder_new = format!("%{}%", var_name);
            template_str = template_str.replace(&placeholder_old, &placeholder_new);

            // Add variable to args map
            args.insert(var_name.to_string(), json!($var));
        )*

        let entry = $crate::LogEntry::new(template_str, args);
        entry.print();
        entry
    }};

    // Pattern: log!("template {var1} text {var2}") - auto-capture from template
    // This requires a different approach using format_args introspection
    // For now, we require explicit variable passing
}

/// Alternative macro that uses a different syntax similar to format!
///
/// # Example
///
/// ```rust
/// use structured_logging::slog;
///
/// let user = "John";
/// let balance = 42;
/// slog!("Hello {}, your balance is {}", user => "user", balance => "balance");
/// ```
#[macro_export]
macro_rules! slog {
    ($template:expr, $($var:expr => $name:expr),* $(,)?) => {{
        use std::collections::HashMap;
        use serde_json::json;

        let template_str = $template.to_string();
        let mut args: HashMap<String, serde_json::Value> = HashMap::new();

        $(
            args.insert($name.to_string(), json!($var));
        )*

        let entry = $crate::LogEntry::new(template_str, args);
        entry.print();
        entry
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_basic() {
        let user = "John";
        let balance = 42;
        let entry = log!("Hello {user}, your balance is {balance}", user, balance);

        assert_eq!(entry.template, "Hello %user%, your balance is %balance%");
        assert_eq!(entry.args.get("user").unwrap(), "John");
        assert_eq!(entry.args.get("balance").unwrap(), &42);
    }

    #[test]
    fn test_log_multiple_types() {
        let name = "Alice";
        let age = 30;
        let score = 95.5;
        let active = true;

        let entry = log!(
            "User {name} is {age} years old with score {score} (active: {active})",
            name,
            age,
            score,
            active
        );

        assert_eq!(
            entry.template,
            "User %name% is %age% years old with score %score% (active: %active%)"
        );
        assert_eq!(entry.args.get("name").unwrap(), "Alice");
        assert_eq!(entry.args.get("age").unwrap(), &30);
        assert_eq!(entry.args.get("score").unwrap(), &95.5);
        assert_eq!(entry.args.get("active").unwrap(), &true);
    }

    #[test]
    fn test_log_expressions() {
        let x = 10;
        let y = 5;
        let sum = x + y;

        let entry = log!("The sum of {x} and {y} is {sum}", x, y, sum);

        assert_eq!(entry.template, "The sum of %x% and %y% is %sum%");
        assert_eq!(entry.args.get("x").unwrap(), &10);
        assert_eq!(entry.args.get("y").unwrap(), &5);
        assert_eq!(entry.args.get("sum").unwrap(), &15);
    }

    #[test]
    fn test_log_trailing_comma() {
        let user = "Bob";
        let entry = log!("User: {user}", user,);

        assert_eq!(entry.template, "User: %user%");
        assert_eq!(entry.args.get("user").unwrap(), "Bob");
    }

    #[test]
    fn test_to_json() {
        let user = "Test";
        let entry = log!("User: {user}", user);

        let json = entry.to_json();
        assert!(json.contains("\"template\""));
        assert!(json.contains("\"args\""));
        assert!(json.contains("User: %user%"));
    }
}
