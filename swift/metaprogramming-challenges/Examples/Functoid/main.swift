import MetaprogrammingChallenges
import Foundation

print("=== Challenge 3: Functoid Concept ===\n")

// Example 1: Basic functoid introspection
print("Example 1: Basic functoid introspection")
let add = Functoid2(
    id: "add",
    paramNames: ["a", "b"],
    function: { (a: Int, b: Int) -> Int in a + b }
)

print("Function: \(add.id)")
print("Arity: \(add.arity)")
print("Parameters:")
for (i, param) in add.parameterTypes.enumerated() {
    print("  [\(i)] name: \(param.name), type: \(param.typeName)")
}
print("Return type: \(add.returnTypeName)")
print("Invocation: add(10, 32) = \(add.invoke(10, 32))")
print()

// Example 2: Functoid with mixed parameter types
print("Example 2: Functoid with mixed parameter types")
let greet = Functoid2(
    id: "greet",
    paramNames: ["name", "age"],
    function: { (name: String, age: Int) -> Bool in age >= 18 }
)

print("Function: \(greet.id)")
print("Arity: \(greet.arity)")
print("Parameters:")
for (i, param) in greet.parameterTypes.enumerated() {
    print("  [\(i)] name: \(param.name), type: \(param.typeName)")
}
print("Return type: \(greet.returnTypeName)")
print("Invocation: greet(\"Alice\", 25) = \(greet.invoke("Alice", 25))")
print()

// Example 3: Zero-parameter functoid
print("Example 3: Zero-parameter functoid")
let getAnswer = Functoid0(
    id: "getAnswer",
    function: { () -> Int in 42 }
)

print("Function: \(getAnswer.id)")
print("Arity: \(getAnswer.arity)")
print("Return type: \(getAnswer.returnTypeName)")
print("Invocation: getAnswer() = \(getAnswer.invoke())")
print()

// Example 4: Three-parameter functoid
print("Example 4: Three-parameter functoid")
let combine = Functoid3(
    id: "combine",
    paramNames: ["a", "b", "c"],
    function: { (a: Int, b: Int, c: Int) -> String in "\(a)-\(b)-\(c)" }
)

print("Function: \(combine.id)")
print("Arity: \(combine.arity)")
print("Parameters:")
for (i, param) in combine.parameterTypes.enumerated() {
    print("  [\(i)] name: \(param.name), type: \(param.typeName)")
}
print("Return type: \(combine.returnTypeName)")
print("Invocation: combine(1, 2, 3) = \(combine.invoke(1, 2, 3))")
print()

// Example 5: Heterogeneous collection using type erasure
print("Example 5: Heterogeneous collection of functoids")
let functoids: [AnyFunctoid] = [
    add.eraseToAny(),
    greet.eraseToAny(),
    getAnswer.eraseToAny()
]

print("Collection of \(functoids.count) functoids:")
for f in functoids {
    print("  - \(f.id): arity=\(f.arity), returns=\(f.returnTypeName)")
}
print()

// Example 6: Single-parameter functoid
print("Example 6: Single-parameter functoid")
let square = Functoid1(
    id: "square",
    paramNames: ["x"],
    function: { (x: Int) -> Int in x * x }
)

print("Function: \(square.id)")
print("Arity: \(square.arity)")
print("Parameters:")
for (i, param) in square.parameterTypes.enumerated() {
    print("  [\(i)] name: \(param.name), type: \(param.typeName)")
}
print("Return type: \(square.returnTypeName)")
print("Invocation: square(7) = \(square.invoke(7))")
