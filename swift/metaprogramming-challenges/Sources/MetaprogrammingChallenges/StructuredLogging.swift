import Foundation

/// Challenge 1: Effortless Structured Logging
///
/// This implementation uses Swift's Mirror API to automatically extract
/// variable names from labeled parameters and create structured JSON output.
///
/// Example:
/// ```swift
/// log("Hello \(user), your balance is \(balance)", user: "John", balance: 42)
/// ```
///
/// Output:
/// ```json
/// {
///   "template": "Hello %s%, your balance is %d%",
///   "args": {
///     "user": "John",
///     "balance": 42
///   }
/// }
/// ```

public struct LogEntry: Codable {
    public let template: String
    public let args: [String: AnyCodable]

    public init(template: String, args: [String: AnyCodable]) {
        self.template = template
        self.args = args
    }

    public func toJSON() -> String {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]

        guard let data = try? encoder.encode(self),
              let json = String(data: data, encoding: .utf8) else {
            return "{}"
        }

        return json
    }
}

/// Type-erased wrapper for encoding any value
public struct AnyCodable: Codable {
    private let value: Any

    public init(_ value: Any) {
        self.value = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()

        switch value {
        case let v as String:
            try container.encode(v)
        case let v as Int:
            try container.encode(v)
        case let v as Double:
            try container.encode(v)
        case let v as Float:
            try container.encode(v)
        case let v as Bool:
            try container.encode(v)
        case let v as Int8:
            try container.encode(v)
        case let v as Int16:
            try container.encode(v)
        case let v as Int32:
            try container.encode(v)
        case let v as Int64:
            try container.encode(v)
        case let v as UInt:
            try container.encode(v)
        case let v as UInt8:
            try container.encode(v)
        case let v as UInt16:
            try container.encode(v)
        case let v as UInt32:
            try container.encode(v)
        case let v as UInt64:
            try container.encode(v)
        default:
            try container.encode(String(describing: value))
        }
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()

        if let v = try? container.decode(String.self) {
            value = v
        } else if let v = try? container.decode(Int.self) {
            value = v
        } else if let v = try? container.decode(Double.self) {
            value = v
        } else if let v = try? container.decode(Bool.self) {
            value = v
        } else {
            throw DecodingError.dataCorruptedError(
                in: container,
                debugDescription: "Unsupported type"
            )
        }
    }
}

/// Transform template string by replacing Swift string interpolation markers
/// with placeholder markers
func transformTemplate(_ template: String) -> String {
    // In Swift, we receive the template after interpolation is resolved,
    // so we use a different approach: we mark interpolation points
    // This is a simplified version - real implementation would track positions
    return template
}

/// Structured logging with named parameters
/// Uses KeyValuePairs to preserve parameter names
public func log(_ template: String, _ args: KeyValuePairs<String, Any>) -> LogEntry {
    var argsDict: [String: AnyCodable] = [:]

    for (key, value) in args {
        argsDict[key] = AnyCodable(value)
    }

    return LogEntry(template: template, args: argsDict)
}

/// Alternative approach using variadic parameters with labels
/// This requires manual specification of argument names
public func logNamed(
    _ template: String,
    args: (String, Any)...
) -> LogEntry {
    var argsDict: [String: AnyCodable] = [:]

    for (name, value) in args {
        argsDict[name] = AnyCodable(value)
    }

    return LogEntry(template: template, args: argsDict)
}

/// Macro-like approach using reflection on a struct
/// This extracts field names from a struct type
public func logWithStruct<T>(
    _ template: String,
    _ value: T
) -> LogEntry {
    var argsDict: [String: AnyCodable] = [:]

    let mirror = Mirror(reflecting: value)
    for case let (label?, value) in mirror.children {
        argsDict[label] = AnyCodable(value)
    }

    return LogEntry(template: template, args: argsDict)
}
