package io.functoid

import kotlin.reflect.typeOf
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GenericTypesTest {

    companion object {
        // Helper function for testing fromFunction with generics
        fun processData(
            @Id("input") data: List<String>,
            @Id("mapping") transform: Map<String, Int>
        ): List<Int> {
            return data.mapNotNull { transform[it] }
        }
    }

    @Test
    fun `test functoid with List parameter`() {
        val functoid = FunctoidFactory.from<List<String>, Int>({ list -> list.size })

        assertEquals(1, functoid.getArity())
        assertEquals(typeOf<List<String>>(), functoid.getParameterTypes()[0].type)
        assertEquals(typeOf<Int>(), functoid.getReturnType())

        val result = functoid.invoke(listOf(listOf("a", "b", "c")))
        assertEquals(3, result)
    }

    @Test
    fun `test functoid with Map parameter`() {
        val functoid = FunctoidFactory.from<Map<String, Int>, Int>({ map -> map.size })

        assertEquals(1, functoid.getArity())
        assertEquals(typeOf<Map<String, Int>>(), functoid.getParameterTypes()[0].type)

        val result = functoid.invoke(listOf(mapOf("a" to 1, "b" to 2)))
        assertEquals(2, result)
    }

    @Test
    fun `test functoid with multiple generic parameters`() {
        val functoid = FunctoidFactory.from<List<String>, Map<String, Int>, Set<Double>, String>(
            { list, map, set -> "List:${list.size}, Map:${map.size}, Set:${set.size}" }
        )

        assertEquals(3, functoid.getArity())
        assertEquals(typeOf<List<String>>(), functoid.getParameterTypes()[0].type)
        assertEquals(typeOf<Map<String, Int>>(), functoid.getParameterTypes()[1].type)
        assertEquals(typeOf<Set<Double>>(), functoid.getParameterTypes()[2].type)

        val result = functoid.invoke(
            listOf(
                listOf("a", "b"),
                mapOf("x" to 1),
                setOf(1.0, 2.0, 3.0)
            )
        )
        assertEquals("List:2, Map:1, Set:3", result)
    }

    @Test
    fun `test functoid with nested generics`() {
        val functoid = FunctoidFactory.from<List<List<Int>>, Int>({ nestedList ->
            nestedList.flatten().sum()
        })

        assertEquals(1, functoid.getArity())
        assertEquals(typeOf<List<List<Int>>>(), functoid.getParameterTypes()[0].type)

        val result = functoid.invoke(
            listOf(
                listOf(
                    listOf(1, 2),
                    listOf(3, 4),
                    listOf(5)
                )
            )
        )
        assertEquals(15, result)
    }

    @Test
    fun `test functoid with generic return type`() {
        val functoid = FunctoidFactory.from<Int, List<String>>({ n ->
            List(n) { "item$it" }
        })

        assertEquals(1, functoid.getArity())
        assertEquals(typeOf<Int>(), functoid.getParameterTypes()[0].type)
        assertEquals(typeOf<List<String>>(), functoid.getReturnType())

        val result = functoid.invoke(listOf(3)) as List<String>
        assertEquals(listOf("item0", "item1", "item2"), result)
    }

    @Test
    fun `test fromFunction with generic parameters`() {
        val functoid = FunctoidFactory.fromFunction(::processData)

        assertEquals(2, functoid.getArity())

        val param1 = functoid.getParameterTypes()[0]
        assertEquals(typeOf<List<String>>(), param1.type)
        assertEquals("input", param1.id)

        val param2 = functoid.getParameterTypes()[1]
        assertEquals(typeOf<Map<String, Int>>(), param2.type)
        assertEquals("mapping", param2.id)

        assertEquals(typeOf<List<Int>>(), functoid.getReturnType())

        val result = functoid.invoke(
            listOf(
                listOf("a", "b", "c"),
                mapOf("a" to 1, "c" to 3)
            )
        ) as List<Int>

        assertEquals(listOf(1, 3), result)
    }

    @Test
    fun `test type introspection preserves generic information`() {
        val functoid = FunctoidFactory.from<Map<String, List<Int>>, String>({ map ->
            map.entries.joinToString { "${it.key}:${it.value}" }
        })

        val paramType = functoid.getParameterTypes()[0].type

        // Verify the type string contains generic information
        val typeString = paramType.toString()
        assertTrue(typeString.contains("Map"), "Type should contain 'Map': $typeString")
        assertTrue(typeString.contains("String"), "Type should contain 'String': $typeString")
        assertTrue(typeString.contains("List"), "Type should contain 'List': $typeString")
        assertTrue(typeString.contains("Int"), "Type should contain 'Int': $typeString")

        println("Full generic type: $typeString")
    }

    @Test
    fun `test generic type composition with map`() {
        val functoid = FunctoidFactory.from<List<Int>, Int>({ list -> list.sum() })
        val mapped = functoid.map({ result -> List(result) { "item$it" } }, typeOf<List<String>>())

        assertEquals(1, mapped.getArity())
        assertEquals(typeOf<List<String>>(), mapped.getReturnType())

        val result = mapped.invoke(listOf(listOf(1, 2, 3))) as List<String>
        assertEquals(listOf("item0", "item1", "item2", "item3", "item4", "item5"), result)
    }

    @Test
    fun `test generic type with nullable types`() {
        val functoid = FunctoidFactory.from<List<String?>, List<String>>({ list ->
            list.filterNotNull()
        })

        assertEquals(1, functoid.getArity())
        assertEquals(typeOf<List<String?>>(), functoid.getParameterTypes()[0].type)
        assertEquals(typeOf<List<String>>(), functoid.getReturnType())

        val result = functoid.invoke(
            listOf(listOf("a", null, "b", null, "c"))
        ) as List<String>

        assertEquals(listOf("a", "b", "c"), result)
    }

    @Test
    fun `test generic pair and triple types`() {
        val functoid = FunctoidFactory.from<Pair<String, Int>, Triple<String, Int, Boolean>, String>(
            { pair, triple ->
                "${pair.first}:${pair.second}, ${triple.first}:${triple.second}:${triple.third}"
            }
        )

        assertEquals(2, functoid.getArity())
        assertEquals(typeOf<Pair<String, Int>>(), functoid.getParameterTypes()[0].type)
        assertEquals(typeOf<Triple<String, Int, Boolean>>(), functoid.getParameterTypes()[1].type)

        val result = functoid.invoke(
            listOf(
                Pair("hello", 42),
                Triple("world", 99, true)
            )
        )

        assertEquals("hello:42, world:99:true", result)
    }
}
