// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "MetaprogrammingChallenges",
    platforms: [
        .macOS(.v13),
        .iOS(.v16),
        .tvOS(.v16),
        .watchOS(.v9)
    ],
    products: [
        .library(
            name: "MetaprogrammingChallenges",
            targets: ["MetaprogrammingChallenges"]
        ),
        .executable(
            name: "StructuredLoggingExample",
            targets: ["StructuredLoggingExample"]
        ),
        .executable(
            name: "TypeReflectionExample",
            targets: ["TypeReflectionExample"]
        ),
        .executable(
            name: "FunctoidExample",
            targets: ["FunctoidExample"]
        ),
    ],
    targets: [
        .target(
            name: "MetaprogrammingChallenges",
            path: "Sources/MetaprogrammingChallenges"
        ),
        .executableTarget(
            name: "StructuredLoggingExample",
            dependencies: ["MetaprogrammingChallenges"],
            path: "Examples/StructuredLogging"
        ),
        .executableTarget(
            name: "TypeReflectionExample",
            dependencies: ["MetaprogrammingChallenges"],
            path: "Examples/TypeReflection"
        ),
        .executableTarget(
            name: "FunctoidExample",
            dependencies: ["MetaprogrammingChallenges"],
            path: "Examples/Functoid"
        ),
        .testTarget(
            name: "MetaprogrammingChallengesTests",
            dependencies: ["MetaprogrammingChallenges"],
            path: "Tests"
        ),
    ]
)
