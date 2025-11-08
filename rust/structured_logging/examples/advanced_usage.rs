//! Advanced usage examples of structured logging
//!
//! Run with: cargo run --example advanced_usage

use structured_logging::log;
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
struct User {
    name: String,
    age: u32,
}

fn main() {
    println!("=== Example 1: Complex data structures ===");
    let user = User {
        name: "Alice".to_string(),
        age: 30,
    };
    log!("User data: {user:?}", user);

    println!("\n=== Example 2: Vec and collections ===");
    let numbers = vec![1, 2, 3, 4, 5];
    log!("Numbers: {numbers:?}", numbers);

    println!("\n=== Example 3: Multiple messages in a function ===");
    process_order("Widget", 3, 19.99);

    println!("\n=== Example 4: Computed values ===");
    let base = 10;
    let multiplier = 5;
    let result = base * multiplier;
    log!(
        "Base {base} times {multiplier} equals {result}",
        base,
        multiplier,
        result
    );

    println!("\n=== Example 5: Boolean and Option types ===");
    let is_admin = true;
    let role = "administrator";
    log!("User is admin: {is_admin}, role: {role}", is_admin, role);
}

fn process_order(item: &str, quantity: u32, price: f64) {
    let total = quantity as f64 * price;
    log!(
        "Processing order: {quantity} x {item} @ ${price} = ${total}",
        quantity,
        item,
        price,
        total
    );
}
