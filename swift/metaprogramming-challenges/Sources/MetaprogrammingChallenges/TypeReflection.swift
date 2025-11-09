import Foundation

#if canImport(ObjectiveC)
import ObjectiveC
#endif

/// Challenge 2: Library-Based Reflection
///
/// This implementation uses Swift's type system and Mirror API to provide
/// runtime type identity comparison and type introspection.
///
/// Example:
/// ```swift
/// let id1 = TypeId(Int.self)
/// let id2 = TypeId(Int.self)
/// let id3 = TypeId(String.self)
///
/// assert(id1.isSame(as: id2))
/// assert(!id1.isSame(as: id3))
/// ```

/// TypeId - Runtime type identifier
public struct TypeId<T> {
    public let type: T.Type

    public init(_ type: T.Type) {
        self.type = type
    }

    /// Get the type name
    public var name: String {
        return String(describing: T.self)
    }

    /// Get a hash of the type
    public var hash: Int {
        return name.hashValue
    }

    /// Compare two type IDs for equality
    public func isSame<U>(as other: TypeId<U>) -> Bool {
        return T.self == U.self
    }
}

/// Alternative TypeId implementation using type erasure
public struct AnyTypeId {
    private let type: Any.Type
    private let typeName: String

    public init<T>(_ type: T.Type) {
        self.type = type
        self.typeName = String(describing: type)
    }

    public var name: String {
        return typeName
    }

    public var hash: Int {
        return typeName.hashValue
    }

    public func isSame(as other: AnyTypeId) -> Bool {
        return typeName == other.typeName
    }
}

/// Runtime type identity comparison
public func isSame<A, B>(_ a: TypeId<A>, _ b: TypeId<B>) -> Bool {
    return a.isSame(as: b)
}

/// Check if type A is a subtype of type B
/// In Swift, subtype relationships are primarily protocol conformance and class inheritance
public func isSubtype<Child, Parent>(
    _ child: TypeId<Child>,
    _ parent: TypeId<Parent>
) -> Bool {
    // Exact match
    if Child.self == Parent.self {
        return true
    }

    // Check if Child is a subclass of Parent (for classes)
    #if canImport(ObjectiveC)
    if let childClass = Child.self as? AnyClass,
       let parentClass = Parent.self as? AnyClass {
        var current: AnyClass? = childClass
        while let c = current {
            if c == parentClass {
                return true
            }
            current = class_getSuperclass(c)
        }
    }
    #endif

    // For protocols, we can't easily check at runtime without knowing the protocol
    // Swift's type system handles this at compile time

    return false
}

/// Type metadata - provides additional type information
public struct TypeMetadata<T> {
    public let type: T.Type

    public init(_ type: T.Type) {
        self.type = type
    }

    public var name: String {
        return String(describing: T.self)
    }

    public var size: Int {
        return MemoryLayout<T>.size
    }

    public var stride: Int {
        return MemoryLayout<T>.stride
    }

    public var alignment: Int {
        return MemoryLayout<T>.alignment
    }

    public var isClass: Bool {
        return T.self is AnyClass
    }

    public var isStruct: Bool {
        // In Swift, we can check if it's not a class and not a primitive
        return !isClass && !isEnum && !isFunction
    }

    public var isEnum: Bool {
        let mirror = Mirror(reflecting: type as Any)
        return mirror.displayStyle == .enum
    }

    public var isFunction: Bool {
        return String(describing: T.self).contains("->")
    }

    public var isOptional: Bool {
        return String(describing: T.self).contains("Optional")
    }

    /// Get field information using Mirror
    public func fields() -> [(label: String?, typeName: String)] {
        // Create a dummy instance if possible (this is a limitation)
        // For actual use, we'd need an instance
        let mirror = Mirror(reflecting: self.type as Any)
        var result: [(label: String?, typeName: String)] = []

        for child in mirror.children {
            let valueType = Swift.type(of: child.value)
            result.append((label: child.label, typeName: String(describing: valueType)))
        }

        return result
    }
}

/// Enhanced type information using Mirror
public func typeInfo<T>(of value: T) -> (name: String, kind: String, children: Int) {
    let mirror = Mirror(reflecting: value)
    let kind = mirror.displayStyle.map { String(describing: $0) } ?? "unknown"
    return (name: String(describing: T.self), kind: kind, children: mirror.children.count)
}
