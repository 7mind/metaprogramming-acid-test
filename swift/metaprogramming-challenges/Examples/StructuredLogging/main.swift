import MetaprogrammingChallenges
import Foundation

print("=== Challenge 1: Effortless Structured Logging ===\n")

// Example 1: Basic logging with KeyValuePairs
print("Example 1: Basic logging")
let entry1 = log("Hello {user}, your balance is {balance}", [
    "user": "John",
    "balance": 42
])
print(entry1.toJSON())
print()

// Example 2: Multiple types
print("Example 2: Multiple types")
let entry2 = log("User {name} is {age} years old with score {score} (active: {active})", [
    "name": "Alice",
    "age": 30,
    "score": 95.5,
    "active": true
])
print(entry2.toJSON())
print()

// Example 3: Using struct-based logging
print("Example 3: With computed values")
struct LogData {
    let x: Int
    let y: Int
    let sum: Int
    let product: Int
}

let data = LogData(x: 10, y: 5, sum: 15, product: 50)
let entry3 = logWithStruct("x={x}, y={y}, sum={sum}, product={product}", data)
print(entry3.toJSON())
print()

// Example 4: Different value types
print("Example 4: Different value types")
let entry4 = log("Status: {status}, Code: {code}, Success: {success}", [
    "status": "OK",
    "code": 200,
    "success": true
])
print(entry4.toJSON())
