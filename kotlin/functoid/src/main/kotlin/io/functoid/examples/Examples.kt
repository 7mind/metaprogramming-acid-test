package io.functoid.examples

import io.functoid.*
import kotlin.reflect.typeOf

/**
 * Examples demonstrating Functoid usage patterns.
 */
object Examples {

    // Example domain models
    data class Database(val name: String)
    data class Cache(val name: String)
    data class Logger(val name: String)
    data class Config(val settings: Map<String, String>)
    data class Service(val db: Database, val cache: Cache, val logger: Logger)

    /**
     * Example 1: Basic function wrapping
     */
    fun example1_basicWrapping() {
        println("=== Example 1: Basic Function Wrapping ===")

        val addOne = FunctoidFactory.from<Int, Int>({ x -> x + 1 })

        println("Arity: ${addOne.getArity()}")
        println("Parameter types: ${addOne.getParameterTypes()}")
        println("Return type: ${addOne.getReturnType()}")
        println("Result: ${addOne.invoke(listOf(41))}")
        println()
    }

    /**
     * Example 2: Multiple parameters with type introspection
     */
    fun example2_multipleParameters() {
        println("=== Example 2: Multiple Parameters ===")

        val concat = FunctoidFactory.from<String, String, Int, String>(
            { a, b, c -> "$a-$b-$c" }
        )

        println("Arity: ${concat.getArity()}")
        concat.getParameterTypes().forEachIndexed { index, typeTag ->
            println("Parameter $index: ${typeTag.type}")
        }
        println("Result: ${concat.invoke(listOf("hello", "world", 42))}")
        println()
    }

    /**
     * Example 3: Using @Id annotations for named dependencies
     */
    fun serviceFactory(
        @Id("primary") db: Database,
        @Id("distributed") cache: Cache,
        @Id("app") logger: Logger
    ): Service {
        return Service(db, cache, logger)
    }

    fun example3_idAnnotations() {
        println("=== Example 3: @Id Annotations ===")

        val functoid = FunctoidFactory.fromFunction(::serviceFactory)

        println("Arity: ${functoid.getArity()}")
        functoid.getParameterTypes().forEachIndexed { index, typeTag ->
            println("Parameter $index: type=${typeTag.type}, id=${typeTag.id}")
        }

        val service = functoid.invoke(
            listOf(
                Database("postgres"),
                Cache("redis"),
                Logger("log4j")
            )
        ) as Service

        println("Created service: $service")
        println()
    }

    /**
     * Example 4: Composing functoids with map
     */
    fun example4_mapping() {
        println("=== Example 4: Mapping Over Functoids ===")

        val double = FunctoidFactory.from<Int, Int>({ x -> x * 2 })
        val addTen = double.map({ it + 10 }, typeOf<Int>())

        println("Original arity: ${double.getArity()}")
        println("Mapped arity: ${addTen.getArity()}")
        println("double(5) = ${double.invoke(listOf(5))}")
        println("addTen(5) = ${addTen.invoke(listOf(5))}")
        println()
    }

    /**
     * Example 5: Combining functoids with map2
     */
    fun example5_map2() {
        println("=== Example 5: Combining with map2 ===")

        val getAge = FunctoidFactory.from<String, Int>({ name ->
            // Simulate database lookup
            mapOf("Alice" to 30, "Bob" to 25)[name] ?: 0
        })

        val getScore = FunctoidFactory.from<String, Int>({ name ->
            // Simulate score lookup
            mapOf("Alice" to 100, "Bob" to 95)[name] ?: 0
        })

        val combined = getAge.map2(getScore, { age, score ->
            "Age: $age, Score: $score"
        }, typeOf<String>())

        println("Combined arity: ${combined.getArity()}")
        println("Result: ${combined.invoke(listOf("Alice", "Alice"))}")
        println()
    }

    /**
     * Example 6: Zipping functoids
     */
    fun example6_zip() {
        println("=== Example 6: Zipping Functoids ===")

        val uppercase = FunctoidFactory.from<String, String>({ s -> s.uppercase() })
        val length = FunctoidFactory.from<String, Int>({ s -> s.length })

        val zipped = uppercase.zip(length)

        println("Zipped arity: ${zipped.getArity()}")
        val result = zipped.invoke(listOf("hello", "world")) as Pair<String, Int>
        println("Result: $result")
        println()
    }

    /**
     * Example 7: Pure values
     */
    fun example7_pure() {
        println("=== Example 7: Pure Values ===")

        val constant = Functoid.pure(42, typeOf<Int>())

        println("Pure arity: ${constant.getArity()}")
        println("Pure result: ${constant.invoke(emptyList())}")
        println()
    }

    /**
     * Example 8: Complex composition pipeline
     */
    fun example8_pipeline() {
        println("=== Example 8: Complex Pipeline ===")

        // Stage 1: Parse input
        val parse = FunctoidFactory.from<String, Int>({ s -> s.toIntOrNull() ?: 0 })

        // Stage 2: Validate
        val validate = parse.map({ value ->
            if (value > 0) value else throw IllegalArgumentException("Must be positive")
        }, typeOf<Int>())

        // Stage 3: Transform
        val transform = validate.map({ value -> value * 100 }, typeOf<Int>())

        // Stage 4: Format
        val format = transform.map({ value -> "Result: $value" }, typeOf<String>())

        println("Pipeline arity: ${format.getArity()}")
        println("Pipeline result: ${format.invoke(listOf("42"))}")
        println()
    }

    /**
     * Example 9: Dependency injection simulation
     */
    fun createDatabase(@Id("connectionString") connStr: String): Database {
        return Database(connStr)
    }

    fun createCache(@Id("host") host: String, @Id("port") port: Int): Cache {
        return Cache("$host:$port")
    }

    fun createLogger(@Id("level") level: String): Logger {
        return Logger(level)
    }

    fun example9_dependencyInjection() {
        println("=== Example 9: Dependency Injection Simulation ===")

        // Create functoids for each dependency
        val dbFunctoid = FunctoidFactory.fromFunction(::createDatabase)
        val cacheFunctoid = FunctoidFactory.fromFunction(::createCache)
        val loggerFunctoid = FunctoidFactory.fromFunction(::createLogger)

        // Inspect dependencies
        println("Database dependencies:")
        dbFunctoid.getParameterTypes().forEach { println("  - ${it.type} @Id(${it.id})") }

        println("Cache dependencies:")
        cacheFunctoid.getParameterTypes().forEach { println("  - ${it.type} @Id(${it.id})") }

        println("Logger dependencies:")
        loggerFunctoid.getParameterTypes().forEach { println("  - ${it.type} @Id(${it.id})") }

        // "Inject" dependencies by invoking with actual values
        val db = dbFunctoid.invoke(listOf("jdbc:postgresql://localhost/mydb")) as Database
        val cache = cacheFunctoid.invoke(listOf("localhost", 6379)) as Cache
        val logger = loggerFunctoid.invoke(listOf("INFO")) as Logger

        println("\nCreated components:")
        println("  DB: $db")
        println("  Cache: $cache")
        println("  Logger: $logger")
        println()
    }

    /**
     * Example 10: Type-safe factory pattern
     */
    interface Component {
        val name: String
    }

    data class ComponentA(override val name: String) : Component
    data class ComponentB(override val name: String) : Component

    fun example10_factoryPattern() {
        println("=== Example 10: Type-Safe Factory Pattern ===")

        // Registry of component factories
        val factories = mapOf<String, Functoid<Component>>(
            "A" to FunctoidFactory.from<String, ComponentA>({ name -> ComponentA(name) })
                .map({ it as Component }, typeOf<Component>()),
            "B" to FunctoidFactory.from<String, ComponentB>({ name -> ComponentB(name) })
                .map({ it as Component }, typeOf<Component>())
        )

        // Create components dynamically
        val componentA = factories["A"]?.invoke(listOf("Component A Instance"))
        val componentB = factories["B"]?.invoke(listOf("Component B Instance"))

        println("Created A: $componentA")
        println("Created B: $componentB")
        println()
    }

    @JvmStatic
    fun main(args: Array<String>) {
        example1_basicWrapping()
        example2_multipleParameters()
        example3_idAnnotations()
        example4_mapping()
        example5_map2()
        example6_zip()
        example7_pure()
        example8_pipeline()
        example9_dependencyInjection()
        example10_factoryPattern()

        println("All examples completed successfully!")
    }
}
