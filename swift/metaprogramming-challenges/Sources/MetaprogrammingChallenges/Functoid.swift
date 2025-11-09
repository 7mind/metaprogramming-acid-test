import Foundation

/// Challenge 3: Functoid Concept
///
/// This implementation uses Swift's type system and closures to create
/// runtime-introspectable function wrappers.
///
/// A Functoid wraps an arbitrary function and provides:
/// - Runtime access to parameter type information
/// - Runtime access to return type information
/// - Ability to invoke the function
/// - Optional parameter identifiers
///
/// Example:
/// ```swift
/// let functoid = Functoid2(
///     id: "greet",
///     paramNames: ["name", "age"],
///     function: { (name: String, age: Int) -> Bool in age >= 18 }
/// )
/// let result = functoid.invoke("Alice", 25)
/// ```

/// Parameter information
public struct ParamInfo {
    public let name: String
    public let typeName: String
    public let typeHash: Int

    public init(name: String, typeName: String, typeHash: Int) {
        self.name = name
        self.typeName = typeName
        self.typeHash = typeHash
    }
}

/// Base protocol for all Functoids
public protocol FunctoidProtocol {
    var id: String { get }
    var arity: Int { get }
    var parameterTypes: [ParamInfo] { get }
    var returnTypeName: String { get }
}

/// Functoid for 0-parameter functions
public struct Functoid0<R>: FunctoidProtocol {
    public let id: String
    private let function: () -> R

    public init(
        id: String = "",
        function: @escaping () -> R
    ) {
        self.id = id
        self.function = function
    }

    public var arity: Int { 0 }

    public var parameterTypes: [ParamInfo] {
        return []
    }

    public var returnTypeName: String {
        return String(describing: R.self)
    }

    public func invoke() -> R {
        return function()
    }
}

/// Functoid for 1-parameter functions
public struct Functoid1<P1, R>: FunctoidProtocol {
    public let id: String
    private let function: (P1) -> R
    private let paramNames: [String]

    public init(
        id: String = "",
        paramNames: [String] = ["p1"],
        function: @escaping (P1) -> R
    ) {
        self.id = id
        self.function = function
        self.paramNames = paramNames
    }

    public var arity: Int { 1 }

    public var parameterTypes: [ParamInfo] {
        return [
            ParamInfo(
                name: paramNames[0],
                typeName: String(describing: P1.self),
                typeHash: String(describing: P1.self).hashValue
            )
        ]
    }

    public var returnTypeName: String {
        return String(describing: R.self)
    }

    public func invoke(_ p1: P1) -> R {
        return function(p1)
    }
}

/// Functoid for 2-parameter functions
public struct Functoid2<P1, P2, R>: FunctoidProtocol {
    public let id: String
    private let function: (P1, P2) -> R
    private let paramNames: [String]

    public init(
        id: String = "",
        paramNames: [String] = ["p1", "p2"],
        function: @escaping (P1, P2) -> R
    ) {
        self.id = id
        self.function = function
        self.paramNames = paramNames
    }

    public var arity: Int { 2 }

    public var parameterTypes: [ParamInfo] {
        return [
            ParamInfo(
                name: paramNames[0],
                typeName: String(describing: P1.self),
                typeHash: String(describing: P1.self).hashValue
            ),
            ParamInfo(
                name: paramNames[1],
                typeName: String(describing: P2.self),
                typeHash: String(describing: P2.self).hashValue
            )
        ]
    }

    public var returnTypeName: String {
        return String(describing: R.self)
    }

    public func invoke(_ p1: P1, _ p2: P2) -> R {
        return function(p1, p2)
    }
}

/// Functoid for 3-parameter functions
public struct Functoid3<P1, P2, P3, R>: FunctoidProtocol {
    public let id: String
    private let function: (P1, P2, P3) -> R
    private let paramNames: [String]

    public init(
        id: String = "",
        paramNames: [String] = ["p1", "p2", "p3"],
        function: @escaping (P1, P2, P3) -> R
    ) {
        self.id = id
        self.function = function
        self.paramNames = paramNames
    }

    public var arity: Int { 3 }

    public var parameterTypes: [ParamInfo] {
        return [
            ParamInfo(
                name: paramNames[0],
                typeName: String(describing: P1.self),
                typeHash: String(describing: P1.self).hashValue
            ),
            ParamInfo(
                name: paramNames[1],
                typeName: String(describing: P2.self),
                typeHash: String(describing: P2.self).hashValue
            ),
            ParamInfo(
                name: paramNames[2],
                typeName: String(describing: P3.self),
                typeHash: String(describing: P3.self).hashValue
            )
        ]
    }

    public var returnTypeName: String {
        return String(describing: R.self)
    }

    public func invoke(_ p1: P1, _ p2: P2, _ p3: P3) -> R {
        return function(p1, p2, p3)
    }
}

/// Type-erased Functoid for heterogeneous collections
public struct AnyFunctoid: FunctoidProtocol {
    public let id: String
    public let arity: Int
    public let parameterTypes: [ParamInfo]
    public let returnTypeName: String

    private let invokeImpl: ([Any]) -> Any

    public init<F: FunctoidProtocol>(_ functoid: F, invoke: @escaping ([Any]) -> Any) {
        self.id = functoid.id
        self.arity = functoid.arity
        self.parameterTypes = functoid.parameterTypes
        self.returnTypeName = functoid.returnTypeName
        self.invokeImpl = invoke
    }

    public func invoke(_ args: [Any]) -> Any {
        return invokeImpl(args)
    }
}

// Extensions to convert concrete Functoids to AnyFunctoid
extension Functoid0 {
    public func eraseToAny() -> AnyFunctoid {
        return AnyFunctoid(self) { _ in
            self.invoke()
        }
    }
}

extension Functoid1 {
    public func eraseToAny() -> AnyFunctoid {
        return AnyFunctoid(self) { args in
            guard args.count == 1,
                  let p1 = args[0] as? P1 else {
                fatalError("Invalid arguments")
            }
            return self.invoke(p1)
        }
    }
}

extension Functoid2 {
    public func eraseToAny() -> AnyFunctoid {
        return AnyFunctoid(self) { args in
            guard args.count == 2,
                  let p1 = args[0] as? P1,
                  let p2 = args[1] as? P2 else {
                fatalError("Invalid arguments")
            }
            return self.invoke(p1, p2)
        }
    }
}

extension Functoid3 {
    public func eraseToAny() -> AnyFunctoid {
        return AnyFunctoid(self) { args in
            guard args.count == 3,
                  let p1 = args[0] as? P1,
                  let p2 = args[1] as? P2,
                  let p3 = args[2] as? P3 else {
                fatalError("Invalid arguments")
            }
            return self.invoke(p1, p2, p3)
        }
    }
}
