use functoid::{functoid, Functoid, TypeInfo};
use std::collections::HashMap;

#[functoid]
fn example_function(
    #[id("config")] config: String,
    count: i32,
    rate: f64,
) -> Vec<String> {
    vec![config; count as usize]
}

fn main() {
    println!("=== Type Tag Demonstration ===\n");

    // Get type tags directly
    let string_tag = TypeInfo::of::<String>();
    let i32_tag = TypeInfo::of::<i32>();
    let f64_tag = TypeInfo::of::<f64>();

    println!("1. Direct TypeInfo creation:");
    println!("   String: {:?}", string_tag);
    println!("   i32: {:?}", i32_tag);
    println!("   f64: {:?}", f64_tag);

    // Type tags are comparable
    println!("\n2. Type tag equality:");
    let another_string_tag = TypeInfo::of::<String>();
    println!("   String == String: {}", string_tag == another_string_tag);
    println!("   String == i32: {}", string_tag == i32_tag);

    // Extract type tags from functoid
    println!("\n3. Type tags from Functoid:");
    let func = ExampleFunctionFunctoid::new();

    println!("   Parameters:");
    for (i, param) in func.param_info().iter().enumerate() {
        println!("     [{}] {} -> TypeId: {:?}, Name: {}",
            i,
            param.name,
            param.type_info.type_id,
            param.type_info.type_name
        );
        if let Some(id) = param.id {
            println!("         @Id: {}", id);
        }
    }

    println!("\n   Return type:");
    let ret = func.return_type();
    println!("     TypeId: {:?}", ret.type_id);
    println!("     Name: {}", ret.type_name);

    // Use TypeInfo as HashMap keys (for DI containers)
    println!("\n4. Using TypeInfo in a dependency registry:");
    let mut registry: HashMap<TypeInfo, Box<dyn std::any::Any>> = HashMap::new();

    registry.insert(TypeInfo::of::<String>(), Box::new("Hello".to_string()));
    registry.insert(TypeInfo::of::<i32>(), Box::new(42i32));
    registry.insert(TypeInfo::of::<f64>(), Box::new(3.14f64));

    // Lookup by type tag
    if let Some(value) = registry.get(&TypeInfo::of::<i32>()) {
        if let Some(num) = value.downcast_ref::<i32>() {
            println!("   Found i32 in registry: {}", num);
        }
    }

    // This is how a DI container would resolve dependencies
    println!("\n5. Simulated dependency resolution:");
    println!("   Function needs:");
    for param in func.param_info() {
        let found = registry.contains_key(&param.type_info);
        println!("     - {} ({}): {}",
            param.name,
            param.type_info,
            if found { "✓ Available" } else { "✗ Missing" }
        );
    }

    // TypeInfo works across different invocations
    println!("\n6. Type tag stability:");
    for i in 0..3 {
        let tag = TypeInfo::of::<String>();
        println!("   Iteration {}: TypeId = {:?}, Equal to first? {}",
            i, tag.type_id, tag == string_tag);
    }

    println!("\n=== Demo Complete ===");
}
