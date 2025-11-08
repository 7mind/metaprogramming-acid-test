package io.functoid.examples

import io.functoid.*
import kotlin.reflect.typeOf

/**
 * Demonstration of Functoid working with generic type parameters.
 */
object GenericTypesDemo {

    // Helper function for demonstration - must be at object level for reflection
    fun dataProcessor(
        @Id("source") input: List<String>,
        @Id("config") settings: Map<String, Any>
    ): String {
        val separator = settings["separator"] as? String ?: ","
        return input.joinToString(separator)
    }

    @JvmStatic
    fun main(args: Array<String>) {
        println("=== Functoid Generic Types Demonstration ===\n")

        // Example 1: Simple generic collections
        println("1. List<String> parameter:")
        val listFunc = FunctoidFactory.from<List<String>, String>({ list ->
            list.joinToString(", ")
        })
        println("   Type: ${listFunc.getParameterTypes()[0].type}")
        println("   Result: ${listFunc.invoke(listOf(listOf("Alice", "Bob", "Charlie")))}")
        println()

        // Example 2: Map with generic key-value types
        println("2. Map<String, Int> parameter:")
        val mapFunc = FunctoidFactory.from<Map<String, Int>, Int>({ map ->
            map.values.sum()
        })
        println("   Type: ${mapFunc.getParameterTypes()[0].type}")
        println("   Result: ${mapFunc.invoke(listOf(mapOf("a" to 10, "b" to 20, "c" to 30)))}")
        println()

        // Example 3: Multiple generic parameters
        println("3. Multiple generic types:")
        val multiFunc = FunctoidFactory.from<List<Int>, Set<String>, Map<String, Double>, String>(
            { list, set, map ->
                "List has ${list.size} items, Set has ${set.size} items, Map has ${map.size} entries"
            }
        )
        multiFunc.getParameterTypes().forEachIndexed { i, type ->
            println("   Param $i: ${type.type}")
        }
        val result = multiFunc.invoke(
            listOf(
                listOf(1, 2, 3),
                setOf("x", "y"),
                mapOf("pi" to 3.14, "e" to 2.71)
            )
        )
        println("   Result: $result")
        println()

        // Example 4: Nested generic types
        println("4. Nested generics List<List<Int>>:")
        val nestedFunc = FunctoidFactory.from<List<List<Int>>, List<Int>>({ nested ->
            nested.flatten()
        })
        println("   Input type: ${nestedFunc.getParameterTypes()[0].type}")
        println("   Return type: ${nestedFunc.getReturnType()}")
        val nested = nestedFunc.invoke(
            listOf(listOf(listOf(1, 2), listOf(3, 4), listOf(5, 6)))
        ) as List<Int>
        println("   Result: $nested")
        println()

        // Example 5: Complex nested structure
        println("5. Map<String, List<Int>> - complex nested type:")
        val complexFunc = FunctoidFactory.from<Map<String, List<Int>>, Map<String, Int>>({ map ->
            map.mapValues { it.value.sum() }
        })
        println("   Input type: ${complexFunc.getParameterTypes()[0].type}")
        val complexResult = complexFunc.invoke(
            listOf(
                mapOf(
                    "group1" to listOf(1, 2, 3),
                    "group2" to listOf(10, 20, 30)
                )
            )
        ) as Map<String, Int>
        println("   Result: $complexResult")
        println()

        // Example 6: Using @Id with generic types
        println("6. @Id annotation with generic types:")
        val functoid = FunctoidFactory.fromFunction(::dataProcessor)
        functoid.getParameterTypes().forEach { tag ->
            println("   ${tag.type} @Id(\"${tag.id}\")")
        }
        val idResult = functoid.invoke(
            listOf(
                listOf("one", "two", "three"),
                mapOf("separator" to " | ")
            )
        )
        println("   Result: $idResult")
        println()

        // Example 7: Generic return types
        println("7. Generic return type List<Pair<String, Int>>:")
        val returnFunc = FunctoidFactory.from<List<String>, List<Pair<String, Int>>>({ strings ->
            strings.mapIndexed { index, s -> Pair(s, index) }
        })
        println("   Return type: ${returnFunc.getReturnType()}")
        val pairs = returnFunc.invoke(listOf(listOf("a", "b", "c"))) as List<Pair<String, Int>>
        println("   Result: $pairs")
        println()

        // Example 8: Nullable generic types
        println("8. Nullable generic List<String?>:")
        val nullableFunc = FunctoidFactory.from<List<String?>, List<String>>({ list ->
            list.filterNotNull()
        })
        println("   Input type: ${nullableFunc.getParameterTypes()[0].type}")
        println("   Return type: ${nullableFunc.getReturnType()}")
        val filtered = nullableFunc.invoke(
            listOf(listOf("a", null, "b", null, "c"))
        ) as List<String>
        println("   Result: $filtered")
        println()

        // Example 9: Composing generic functoids
        println("9. Composing generic functoids:")
        val parseFunc = FunctoidFactory.from<String, List<Int>>({ s ->
            s.split(",").mapNotNull { it.trim().toIntOrNull() }
        })
        val sumFunc = FunctoidFactory.from<List<Int>, Int>({ list ->
            list.sum()
        })

        // Compose: String -> List<Int> -> Int
        val composed = parseFunc.map({ list ->
            sumFunc.invoke(listOf(list))
        }, typeOf<Int>())

        println("   Input type: ${composed.getParameterTypes()[0].type}")
        println("   Return type: ${composed.getReturnType()}")
        println("   Result: ${composed.invoke(listOf("1,2,3,4,5"))}")
        println()

        // Example 10: Triple and Pair generic types
        println("10. Working with Pair and Triple:")
        val pairFunc = FunctoidFactory.from<Pair<List<Int>, Map<String, Int>>, Int>(
            { pair ->
                val (list, map) = pair
                list.sum() + map.values.sum()
            }
        )
        println("   Input type: ${pairFunc.getParameterTypes()[0].type}")
        val pairResult = pairFunc.invoke(
            listOf(
                Pair(
                    listOf(1, 2, 3),
                    mapOf("x" to 10, "y" to 20)
                )
            )
        )
        println("   Result: $pairResult")
        println()

        println("=== All generic type demonstrations completed successfully! ===")
        println()
        println("Summary:")
        println("✅ Simple generic collections (List, Set, Map)")
        println("✅ Multiple generic parameters")
        println("✅ Nested generic types (List<List<T>>)")
        println("✅ Complex nested types (Map<K, List<V>>)")
        println("✅ @Id annotations with generic types")
        println("✅ Generic return types")
        println("✅ Nullable generic types")
        println("✅ Composing generic functoids")
        println("✅ Pair and Triple generic types")
        println()
        println("All generic type information is preserved at runtime via Kotlin's reified types!")
    }
}
