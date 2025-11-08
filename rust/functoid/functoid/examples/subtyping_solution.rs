use functoid::subtyping::{global_trait_registry, implements, evidence, TraitId};
use functoid::{register_impl, functoid, Functoid};
use std::any::TypeId;

// Define trait hierarchy
trait Animal {
    fn speak(&self) -> &str;
}

trait Mammal: Animal {
    fn has_fur(&self) -> bool;
}

trait Pet {
    fn name(&self) -> &str;
}

// Concrete types
#[derive(Clone)]
struct Dog {
    name: String,
}

impl Animal for Dog {
    fn speak(&self) -> &str {
        "Woof!"
    }
}

impl Mammal for Dog {
    fn has_fur(&self) -> bool {
        true
    }
}

impl Pet for Dog {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone)]
struct Cat {
    name: String,
}

impl Animal for Cat {
    fn speak(&self) -> &str {
        "Meow!"
    }
}

impl Mammal for Cat {
    fn has_fur(&self) -> bool {
        true
    }
}

impl Pet for Cat {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone)]
struct Fish {
    name: String,
}

impl Animal for Fish {
    fn speak(&self) -> &str {
        "Blub!"
    }
}

impl Pet for Fish {
    fn name(&self) -> &str {
        &self.name
    }
}

// Function using subtyping constraints
// Note: Using concrete types here because our Functoid requires Clone
#[functoid]
fn feed_dog(#[id("pet")] dog: Dog) -> String {
    format!("Feeding a dog named {} that says: {}", dog.name, dog.speak())
}

#[functoid]
fn count_animals(#[id("count")] count: i32, #[id("species")] species: String) -> String {
    format!("Counted {} animals of species {}", count, species)
}

fn main() {
    println!("=== Runtime Subtyping Evidence (like izumi-reflect <:<) ===\n");

    // Register implementations (similar to Scala's implicit TagK)
    println!("1. Registering trait implementations:");
    register_impl!(Dog: dyn Animal);
    register_impl!(Dog: dyn Mammal);
    register_impl!(Dog: dyn Pet);

    register_impl!(Cat: dyn Animal);
    register_impl!(Cat: dyn Mammal);
    register_impl!(Cat: dyn Pet);

    register_impl!(Fish: dyn Animal);
    register_impl!(Fish: dyn Pet);
    println!("   ✓ Registered Dog, Cat, Fish implementations\n");

    // Check subtyping relationships (like <:<)
    println!("2. Checking subtyping evidence (T <: Trait):");

    println!("   Dog <: Animal? {}", implements::<Dog, dyn Animal>());
    println!("   Dog <: Mammal? {}", implements::<Dog, dyn Mammal>());
    println!("   Dog <: Pet? {}", implements::<Dog, dyn Pet>());
    println!();
    println!("   Fish <: Animal? {}", implements::<Fish, dyn Animal>());
    println!("   Fish <: Mammal? {}", implements::<Fish, dyn Mammal>()); // false!
    println!("   Fish <: Pet? {}", implements::<Fish, dyn Pet>());
    println!();

    // Get explicit evidence
    println!("3. Getting explicit evidence:");
    if let Some(ev) = evidence::<Dog, dyn Mammal>() {
        println!("   ✓ Evidence: {}", ev);
    }

    match evidence::<Fish, dyn Mammal>() {
        Some(ev) => println!("   ✓ Evidence: {}", ev),
        None => println!("   ✗ No evidence that Fish implements Mammal"),
    }
    println!();

    // Query all traits for a type
    println!("4. Querying all traits implemented by Dog:");
    let dog_traits = global_trait_registry().traits_of(TypeId::of::<Dog>());
    for trait_id in dog_traits {
        println!("   - {}", trait_id);
    }
    println!();

    // Query all implementors of a trait
    println!("5. Querying all types implementing Mammal:");
    let mammal_types = global_trait_registry().implementors_of(TraitId::of::<dyn Mammal>());
    println!("   Found {} implementors", mammal_types.len());
    println!();

    // Use in dependency injection context
    println!("6. Dependency Injection use case:");
    let feed_dog_fn = FeedDogFunctoid::new();

    println!("   Function 'feed_dog' expects:");
    for param in feed_dog_fn.param_info() {
        println!("     - {}: {}", param.name, param.type_info);
        println!("       Checking which traits Dog implements:");
        if implements::<Dog, dyn Animal>() {
            println!("         ✓ Dog implements Animal");
        }
        if implements::<Dog, dyn Mammal>() {
            println!("         ✓ Dog implements Mammal");
        }
        if implements::<Dog, dyn Pet>() {
            println!("         ✓ Dog implements Pet");
        }
    }
    println!();

    // Comparison with Scala
    println!("7. Comparison with Scala/izumi-reflect:");
    println!("   Scala:  implicitly[Dog <:< Animal]");
    println!("   Rust:   evidence::<Dog, dyn Animal>()");
    println!();
    println!("   Scala:  implicitly[Fish <:!< Mammal] // compile error");
    println!("   Rust:   evidence::<Fish, dyn Mammal>() // returns None");
    println!();

    // Runtime polymorphism decision
    println!("8. Runtime polymorphism decision:");
    let types_to_check = vec![
        ("Dog", TypeId::of::<Dog>()),
        ("Cat", TypeId::of::<Cat>()),
        ("Fish", TypeId::of::<Fish>()),
    ];

    let mammal_trait = TraitId::of::<dyn Mammal>();

    for (name, type_id) in types_to_check {
        let is_mammal = global_trait_registry().type_implements(type_id, mammal_trait);
        println!("   {} can be used where Mammal is required: {}", name, is_mammal);
    }

    println!("\n=== Key Insights ===");
    println!("• Rust TypeId alone CANNOT check subtyping (unlike Scala's TypeTag)");
    println!("• We must manually register trait implementations");
    println!("• This provides runtime evidence similar to izumi-reflect's <:<");
    println!("• Useful for dependency injection and runtime type resolution");
    println!("• Trade-off: Manual registration vs JVM's automatic reflection");
}
