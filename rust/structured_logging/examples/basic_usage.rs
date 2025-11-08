//! Basic usage examples of structured logging
//!
//! Run with: cargo run --example basic_usage

use structured_logging::log;

fn main() {
    println!("=== Example 1: Basic usage ===");
    let user = "John";
    let balance = 42;
    log!("Hello {user}, your balance is {balance}", user, balance);

    println!("\n=== Example 2: Multiple variables of different types ===");
    let name = "Alice";
    let age = 30;
    let score = 95.5;
    let active = true;
    log!(
        "User {name} is {age} years old with score {score} (active: {active})",
        name,
        age,
        score,
        active
    );

    println!("\n=== Example 3: Expressions ===");
    let x = 10;
    let y = 5;
    let sum = x + y;
    log!("The sum of {x} and {y} is {sum}", x, y, sum);

    println!("\n=== Example 4: String formatting ===");
    let price = 19.99;
    let item = "widget";
    log!("The {item} costs ${price}", item, price);

    println!("\n=== Example 5: Single variable ===");
    let message = "Hello, World!";
    log!("Message: {message}", message);
}
