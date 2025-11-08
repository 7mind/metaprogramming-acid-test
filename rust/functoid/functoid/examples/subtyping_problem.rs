use std::any::TypeId;

trait Animal {
    fn speak(&self) -> &str;
}

struct Dog {
    name: String,
}

impl Animal for Dog {
    fn speak(&self) -> &str {
        "Woof!"
    }
}

fn main() {
    println!("=== TypeId Subtyping Problem ===\n");

    let dog_type = TypeId::of::<Dog>();
    let animal_trait_type = TypeId::of::<dyn Animal>();

    println!("Dog TypeId: {:?}", dog_type);
    println!("dyn Animal TypeId: {:?}", animal_trait_type);
    println!("Are they equal? {}", dog_type == animal_trait_type);

    println!("\n❌ Problem: TypeId cannot tell us that Dog implements Animal");
    println!("   We cannot check: Dog <:< Animal at runtime");
    println!("   This is fundamentally different from Scala's izumi-reflect\n");

    // What we want (pseudocode):
    // if type_implements::<Dog, Animal>() {
    //     println!("✓ Dog implements Animal");
    // }

    println!("We need a different approach...");
}
