import MetaprogrammingChallenges
import Foundation

print("=== Challenge 2: Library-Based Reflection ===\n")

// Example 1: Type identity comparison
print("Example 1: Type identity comparison")
let intId1 = AnyTypeId(Int.self)
let intId2 = AnyTypeId(Int.self)
let stringId = AnyTypeId(String.self)

print("TypeId(Int) name: \(intId1.name)")
print("TypeId(Int) hash: \(intId1.hash)")
print("TypeId(String) name: \(stringId.name)")
print("TypeId(String) hash: \(stringId.hash)")
print("Int == Int: \(intId1.isSame(as: intId2))")
print("Int == String: \(intId1.isSame(as: stringId))")
print()

// Example 2: Different types
print("Example 2: Different types")
let boolId = AnyTypeId(Bool.self)
let doubleId = AnyTypeId(Double.self)

print("TypeId(Bool) name: \(boolId.name)")
print("TypeId(Double) name: \(doubleId.name)")
print("Int == Bool: \(intId1.isSame(as: boolId))")
print("Int == Double: \(intId1.isSame(as: doubleId))")
print()

// Example 3: Class hierarchy
print("Example 3: Class hierarchy and subtype checking")
class Animal {}
class Dog: Animal {}

let animalId = TypeId(Animal.self)
let dogId = TypeId(Dog.self)

print("Animal type: \(animalId.name)")
print("Dog type: \(dogId.name)")
print("Dog <: Animal: \(isSubtype(dogId, animalId))")
print("Animal <: Dog: \(isSubtype(animalId, dogId))")
print()

// Example 4: Type metadata
print("Example 4: Type metadata")
let intMeta = TypeMetadata(Int.self)
print("Int metadata:")
print("  name: \(intMeta.name)")
print("  size: \(intMeta.size)")
print("  alignment: \(intMeta.alignment)")
print("  is_class: \(intMeta.isClass)")
print("  is_struct: \(intMeta.isStruct)")
print()

struct Point {
    let x: Int
    let y: Int
}

let pointMeta = TypeMetadata(Point.self)
print("Point metadata:")
print("  name: \(pointMeta.name)")
print("  size: \(pointMeta.size)")
print("  alignment: \(pointMeta.alignment)")
print("  is_class: \(pointMeta.isClass)")
print("  is_struct: \(pointMeta.isStruct)")
print()

// Example 5: Runtime type info
print("Example 5: Runtime type information")
let point = Point(x: 10, y: 20)
let info = typeInfo(of: point)
print("Type: \(info.name), Kind: \(info.kind), Children: \(info.children)")
