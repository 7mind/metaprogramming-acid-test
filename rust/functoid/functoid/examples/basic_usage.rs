use functoid::{functoid, Functoid};

// Simple arithmetic function
#[functoid]
fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Function with different types
#[functoid]
fn format_price(price: f64, currency: String) -> String {
    format!("{} {:.2}", currency, price)
}

// Function with @Id annotations for dependency injection
#[functoid]
fn create_user_greeting(
    #[id("user-name")] name: String,
    #[id("greeting-template")] template: String,
) -> String {
    template.replace("{name}", &name)
}

// Function with complex return type
#[functoid]
fn create_range(start: i32, end: i32) -> Vec<i32> {
    (start..end).collect()
}

fn main() {
    println!("=== Functoid Demo ===\n");

    // Example 1: Simple function introspection
    println!("1. Simple Addition:");
    let add_fn = AddFunctoid::new();

    println!("  Parameters:");
    for param in add_fn.param_info() {
        println!("    - {}: {}", param.name, param.type_info);
    }

    println!("  Return type: {}", add_fn.return_type());

    let result = add_fn.invoke(vec![Box::new(5i32), Box::new(7i32)]);
    println!("  add(5, 7) = {}\n", result);

    // Example 2: Different types
    println!("2. Format Price:");
    let price_fn = FormatPriceFunctoid::new();

    println!("  Parameters:");
    for param in price_fn.param_info() {
        println!("    - {}: {}", param.name, param.type_info);
    }

    let result = price_fn.invoke(vec![
        Box::new(42.50f64),
        Box::new("USD".to_string()),
    ]);
    println!("  Result: {}\n", result);

    // Example 3: @Id annotations
    println!("3. User Greeting with @Id:");
    let greet_fn = CreateUserGreetingFunctoid::new();

    println!("  Parameters with @Id:");
    for param in greet_fn.param_info() {
        if let Some(id) = param.id {
            println!("    - {}: {} @Id(\"{}\")", param.name, param.type_info, id);
        } else {
            println!("    - {}: {}", param.name, param.type_info);
        }
    }

    let result = greet_fn.invoke(vec![
        Box::new("Alice".to_string()),
        Box::new("Hello, {name}!".to_string()),
    ]);
    println!("  Result: {}\n", result);

    // Example 4: Complex return type
    println!("4. Range Function:");
    let range_fn = CreateRangeFunctoid::new();

    println!("  Return type: {}", range_fn.return_type());

    let result = range_fn.invoke(vec![Box::new(1i32), Box::new(6i32)]);
    println!("  create_range(1, 6) = {:?}\n", result);

    // Example 5: Runtime type checking
    println!("5. Type Safety:");
    println!("  Attempting to call add with wrong types...");
    let result = std::panic::catch_unwind(|| {
        add_fn.invoke(vec![Box::new("not a number"), Box::new(5i32)]);
    });

    match result {
        Ok(_) => println!("  Unexpectedly succeeded!"),
        Err(_) => println!("  Correctly panicked due to type mismatch ✓"),
    }

    println!("\n=== Demo Complete ===");
}
