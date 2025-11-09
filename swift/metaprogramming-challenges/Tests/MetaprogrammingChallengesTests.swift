import XCTest
@testable import MetaprogrammingChallenges

final class StructuredLoggingTests: XCTestCase {
    func testBasicLogging() {
        let entry = log("Hello {user}, your balance is {balance}", [
            "user": "John",
            "balance": 42
        ])

        XCTAssertEqual(entry.template, "Hello {user}, your balance is {balance}")
        XCTAssertEqual(entry.args.count, 2)
        XCTAssertNotNil(entry.args["user"])
        XCTAssertNotNil(entry.args["balance"])

        let json = entry.toJSON()
        XCTAssertTrue(json.contains("\"template\""))
        XCTAssertTrue(json.contains("\"args\""))
        XCTAssertTrue(json.contains("\"user\""))
        XCTAssertTrue(json.contains("\"balance\""))
    }

    func testMultipleTypes() {
        let entry = log("User {name} age {age} score {score} active {active}", [
            "name": "Alice",
            "age": 30,
            "score": 95.5,
            "active": true
        ])

        XCTAssertEqual(entry.args.count, 4)
        XCTAssertNotNil(entry.args["name"])
        XCTAssertNotNil(entry.args["age"])
        XCTAssertNotNil(entry.args["score"])
        XCTAssertNotNil(entry.args["active"])

        let json = entry.toJSON()
        XCTAssertTrue(json.contains("\"Alice\""))
        XCTAssertTrue(json.contains("30"))
        XCTAssertTrue(json.contains("95.5"))
        XCTAssertTrue(json.contains("true"))
    }

    func testStructBasedLogging() {
        struct TestData {
            let x: Int
            let y: Int
        }

        let data = TestData(x: 10, y: 20)
        let entry = logWithStruct("x={x}, y={y}", data)

        XCTAssertEqual(entry.args.count, 2)
        XCTAssertNotNil(entry.args["x"])
        XCTAssertNotNil(entry.args["y"])
    }
}

final class TypeReflectionTests: XCTestCase {
    func testTypeIdentity() {
        let id1 = AnyTypeId(Int.self)
        let id2 = AnyTypeId(Int.self)
        let id3 = AnyTypeId(String.self)

        XCTAssertTrue(id1.isSame(as: id2))
        XCTAssertFalse(id1.isSame(as: id3))
    }

    func testTypeName() {
        let intId = AnyTypeId(Int.self)
        let stringId = AnyTypeId(String.self)

        XCTAssertEqual(intId.name, "Int")
        XCTAssertEqual(stringId.name, "String")
    }

    func testTypeHash() {
        let id1 = AnyTypeId(Int.self)
        let id2 = AnyTypeId(Int.self)

        XCTAssertEqual(id1.hash, id2.hash)
    }

    func testSubtypeRelation() {
        class Animal {}
        class Dog: Animal {}

        let animalId = TypeId(Animal.self)
        let dogId = TypeId(Dog.self)

        // Dog is a subtype of Animal
        XCTAssertTrue(isSubtype(dogId, animalId))
        // Animal is not a subtype of Dog
        XCTAssertFalse(isSubtype(animalId, dogId))
        // Same type is subtype of itself
        XCTAssertTrue(isSubtype(dogId, dogId))
    }

    func testTypeMetadata() {
        let meta = TypeMetadata(Int.self)

        XCTAssertEqual(meta.name, "Int")
        XCTAssertGreaterThan(meta.size, 0)
        XCTAssertGreaterThan(meta.alignment, 0)
        XCTAssertFalse(meta.isClass)
    }

    func testStructMetadata() {
        struct Point {
            let x: Int
            let y: Int
        }

        let meta = TypeMetadata(Point.self)

        XCTAssertEqual(meta.name, "Point")
        XCTAssertGreaterThan(meta.size, 0)
        XCTAssertTrue(meta.isStruct)
        XCTAssertFalse(meta.isClass)
    }

    func testRuntimeTypeInfo() {
        struct Point {
            let x: Int
            let y: Int
        }

        let point = Point(x: 10, y: 20)
        let info = typeInfo(of: point)

        XCTAssertEqual(info.name, "Point")
        XCTAssertEqual(info.kind, "struct")
        XCTAssertEqual(info.children, 2)
    }
}

final class FunctoidTests: XCTestCase {
    func testFunctoid0() {
        let f = Functoid0(id: "getAnswer") { () -> Int in 42 }

        XCTAssertEqual(f.id, "getAnswer")
        XCTAssertEqual(f.arity, 0)
        XCTAssertEqual(f.returnTypeName, "Int")
        XCTAssertEqual(f.invoke(), 42)
    }

    func testFunctoid1() {
        let f = Functoid1(
            id: "square",
            paramNames: ["x"]
        ) { (x: Int) -> Int in x * x }

        XCTAssertEqual(f.id, "square")
        XCTAssertEqual(f.arity, 1)
        XCTAssertEqual(f.returnTypeName, "Int")
        XCTAssertEqual(f.parameterTypes.count, 1)
        XCTAssertEqual(f.parameterTypes[0].name, "x")
        XCTAssertEqual(f.parameterTypes[0].typeName, "Int")
        XCTAssertEqual(f.invoke(7), 49)
    }

    func testFunctoid2() {
        let f = Functoid2(
            id: "add",
            paramNames: ["a", "b"]
        ) { (a: Int, b: Int) -> Int in a + b }

        XCTAssertEqual(f.id, "add")
        XCTAssertEqual(f.arity, 2)
        XCTAssertEqual(f.returnTypeName, "Int")
        XCTAssertEqual(f.parameterTypes.count, 2)
        XCTAssertEqual(f.parameterTypes[0].name, "a")
        XCTAssertEqual(f.parameterTypes[1].name, "b")
        XCTAssertEqual(f.invoke(10, 32), 42)
    }

    func testFunctoid2MixedTypes() {
        let f = Functoid2(
            id: "greet",
            paramNames: ["name", "age"]
        ) { (name: String, age: Int) -> Bool in age >= 18 }

        XCTAssertEqual(f.arity, 2)
        XCTAssertEqual(f.parameterTypes[0].typeName, "String")
        XCTAssertEqual(f.parameterTypes[1].typeName, "Int")
        XCTAssertEqual(f.returnTypeName, "Bool")
        XCTAssertTrue(f.invoke("Alice", 25))
        XCTAssertFalse(f.invoke("Bob", 15))
    }

    func testFunctoid3() {
        let f = Functoid3(
            id: "combine",
            paramNames: ["a", "b", "c"]
        ) { (a: Int, b: Int, c: Int) -> String in "\(a)-\(b)-\(c)" }

        XCTAssertEqual(f.arity, 3)
        XCTAssertEqual(f.returnTypeName, "String")
        XCTAssertEqual(f.parameterTypes.count, 3)
        XCTAssertEqual(f.invoke(1, 2, 3), "1-2-3")
    }

    func testTypeErasedFunctoid() {
        let f1 = Functoid0(id: "f1") { 42 }
        let f2 = Functoid1(id: "f2", paramNames: ["x"]) { (x: Int) -> Int in x * 2 }
        let f3 = Functoid2(id: "f3", paramNames: ["a", "b"]) { (a: Int, b: Int) -> Int in a + b }

        let functoids: [AnyFunctoid] = [
            f1.eraseToAny(),
            f2.eraseToAny(),
            f3.eraseToAny()
        ]

        XCTAssertEqual(functoids.count, 3)
        XCTAssertEqual(functoids[0].arity, 0)
        XCTAssertEqual(functoids[1].arity, 1)
        XCTAssertEqual(functoids[2].arity, 2)

        // Test invocation through type erasure
        let result1 = functoids[0].invoke([]) as! Int
        XCTAssertEqual(result1, 42)

        let result2 = functoids[1].invoke([5]) as! Int
        XCTAssertEqual(result2, 10)

        let result3 = functoids[2].invoke([3, 4]) as! Int
        XCTAssertEqual(result3, 7)
    }
}
