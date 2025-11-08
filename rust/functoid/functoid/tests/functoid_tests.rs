use functoid::{functoid, Functoid};

// Test functions with #[functoid] macro

#[functoid]
fn get_constant() -> i32 {
    42
}

#[functoid]
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[functoid]
fn multiply_and_format(x: i32, multiplier: f64) -> String {
    format!("Result: {}", x as f64 * multiplier)
}

#[functoid]
fn greet(#[id("user-name")] name: String, #[id("greeting-msg")] msg: String) -> String {
    format!("{}, {}", msg, name)
}

#[functoid]
fn process_user(
    #[id("user-id")] id: u64,
    name: String,
    #[id("user-age")] age: u32
) -> String {
    format!("User {} (ID: {}) is {} years old", name, id, age)
}

#[functoid]
fn create_vec(size: usize, value: i32) -> Vec<i32> {
    vec![value; size]
}

#[test]
fn test_no_params_functoid() {
    let functoid = GetConstantFunctoid::new();

    let params = functoid.param_info();
    assert_eq!(params.len(), 0);

    let ret_type = functoid.return_type();
    assert_eq!(ret_type.type_name, "i32");

    let result = functoid.invoke(vec![]);
    assert_eq!(result, 42);

    assert_eq!(get_constant(), 42);
}

#[test]
fn test_simple_functoid() {
    let functoid = AddFunctoid::new();

    let params = functoid.param_info();
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].name, "x");
    assert_eq!(params[0].type_info.type_name, "i32");
    assert_eq!(params[0].id, None);
    assert_eq!(params[1].name, "y");
    assert_eq!(params[1].type_info.type_name, "i32");
    assert_eq!(params[1].id, None);

    let ret_type = functoid.return_type();
    assert_eq!(ret_type.type_name, "i32");

    let result = functoid.invoke(vec![
        Box::new(10i32),
        Box::new(20i32),
    ]);
    assert_eq!(result, 30);

    assert_eq!(add(10, 20), 30);
}

#[test]
fn test_different_types_functoid() {
    let functoid = MultiplyAndFormatFunctoid::new();

    let params = functoid.param_info();
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].name, "x");
    assert_eq!(params[0].type_info.type_name, "i32");
    assert_eq!(params[1].name, "multiplier");
    assert_eq!(params[1].type_info.type_name, "f64");

    let ret_type = functoid.return_type();
    assert_eq!(ret_type.type_name, "alloc::string::String");

    let result = functoid.invoke(vec![
        Box::new(10i32),
        Box::new(2.5f64),
    ]);
    assert_eq!(result, "Result: 25");
}

#[test]
fn test_id_annotation_functoid() {
    let functoid = GreetFunctoid::new();

    let params = functoid.param_info();
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].name, "name");
    assert_eq!(params[0].id, Some("user-name"));
    assert_eq!(params[1].name, "msg");
    assert_eq!(params[1].id, Some("greeting-msg"));

    let result = functoid.invoke(vec![
        Box::new("Alice".to_string()),
        Box::new("Hello".to_string()),
    ]);
    assert_eq!(result, "Hello, Alice");
}

#[test]
fn test_mixed_id_functoid() {
    let functoid = ProcessUserFunctoid::new();

    let params = functoid.param_info();
    assert_eq!(params.len(), 3);

    assert_eq!(params[0].name, "id");
    assert_eq!(params[0].id, Some("user-id"));
    assert_eq!(params[0].type_info.type_name, "u64");

    assert_eq!(params[1].name, "name");
    assert_eq!(params[1].id, None);
    assert_eq!(params[1].type_info.type_name, "alloc::string::String");

    assert_eq!(params[2].name, "age");
    assert_eq!(params[2].id, Some("user-age"));
    assert_eq!(params[2].type_info.type_name, "u32");

    let result = functoid.invoke(vec![
        Box::new(12345u64),
        Box::new("Bob".to_string()),
        Box::new(30u32),
    ]);
    assert_eq!(result, "User Bob (ID: 12345) is 30 years old");
}

#[test]
fn test_complex_return_type() {
    let functoid = CreateVecFunctoid::new();

    let ret_type = functoid.return_type();
    assert_eq!(ret_type.type_name, "alloc::vec::Vec<i32>");

    let result = functoid.invoke(vec![
        Box::new(5usize),
        Box::new(42i32),
    ]);
    assert_eq!(result, vec![42, 42, 42, 42, 42]);
}

#[test]
#[should_panic(expected = "Expected 2 arguments, got 1")]
fn test_wrong_number_of_arguments() {
    let functoid = AddFunctoid::new();
    functoid.invoke(vec![Box::new(10i32)]);
}

#[test]
#[should_panic(expected = "Argument 0 has wrong type")]
fn test_wrong_argument_type() {
    let functoid = AddFunctoid::new();
    functoid.invoke(vec![
        Box::new("wrong type"),
        Box::new(20i32),
    ]);
}

#[test]
fn test_type_info_equality() {
    let functoid1 = AddFunctoid::new();
    let functoid2 = MultiplyAndFormatFunctoid::new();

    let params1 = functoid1.param_info();
    let params2 = functoid2.param_info();

    assert_eq!(params1[0].type_info, params2[0].type_info);
}

#[test]
fn test_param_info_display() {
    let functoid = ProcessUserFunctoid::new();
    let params = functoid.param_info();

    let param_str = format!("{}", params[0]);
    assert!(param_str.contains("user-id"));
    assert!(param_str.contains("id"));

    let param_debug = format!("{:?}", params[1]);
    assert!(param_debug.contains("name"));
    assert!(!param_debug.contains("@Id"));
}

#[test]
fn test_introspection() {
    let functoid = GreetFunctoid::new();

    println!("Function parameters:");
    for (i, param) in functoid.param_info().iter().enumerate() {
        println!("  {}: {:?}", i, param);
    }

    println!("Return type: {}", functoid.return_type());

    assert_eq!(functoid.param_info().len(), 2);
}
