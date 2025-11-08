package io.functoid

import kotlin.reflect.typeOf
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class FunctoidTest {

    @Test
    fun `test functoid with zero parameters`() {
        val functoid = FunctoidFactory.from { 42 }

        assertEquals(0, functoid.getArity())
        assertEquals(42, functoid.invoke(emptyList()))
        assertEquals(emptyList(), functoid.getParameterTypes())
        assertEquals(typeOf<Int>(), functoid.getReturnType())
    }

    @Test
    fun `test functoid with one parameter`() {
        val functoid = FunctoidFactory.from<Int, Int>({ x: Int -> x * 2 })

        assertEquals(1, functoid.getArity())
        assertEquals(10, functoid.invoke(listOf(5)))
        assertEquals(1, functoid.getParameterTypes().size)
        assertEquals(typeOf<Int>(), functoid.getParameterTypes()[0].type)
        assertNull(functoid.getParameterTypes()[0].id)
        assertEquals(typeOf<Int>(), functoid.getReturnType())
    }

    @Test
    fun `test functoid with one parameter and Id annotation`() {
        val functoid = FunctoidFactory.from({ x: Int -> x * 2 }, id1 = "special")

        assertEquals(1, functoid.getArity())
        assertEquals(10, functoid.invoke(listOf(5)))
        assertEquals(1, functoid.getParameterTypes().size)
        assertEquals(typeOf<Int>(), functoid.getParameterTypes()[0].type)
        assertEquals("special", functoid.getParameterTypes()[0].id)
    }

    @Test
    fun `test functoid with two parameters`() {
        val functoid = FunctoidFactory.from<Int, String, String>({ x: Int, y: String -> "$y: $x" })

        assertEquals(2, functoid.getArity())
        assertEquals("result: 42", functoid.invoke(listOf(42, "result")))
        assertEquals(2, functoid.getParameterTypes().size)
        assertEquals(typeOf<Int>(), functoid.getParameterTypes()[0].type)
        assertEquals(typeOf<String>(), functoid.getParameterTypes()[1].type)
        assertEquals(typeOf<String>(), functoid.getReturnType())
    }

    @Test
    fun `test functoid with two parameters and Id annotations`() {
        val functoid = FunctoidFactory.from(
            { x: Int, y: String -> "$y: $x" },
            id1 = "number",
            id2 = "label"
        )

        assertEquals(2, functoid.getArity())
        assertEquals("result: 42", functoid.invoke(listOf(42, "result")))
        assertEquals("number", functoid.getParameterTypes()[0].id)
        assertEquals("label", functoid.getParameterTypes()[1].id)
    }

    @Test
    fun `test functoid with three parameters`() {
        val functoid = FunctoidFactory.from<Int, Int, Int, Int>({ x: Int, y: Int, z: Int -> x + y + z })

        assertEquals(3, functoid.getArity())
        assertEquals(6, functoid.invoke(listOf(1, 2, 3)))
    }

    @Test
    fun `test functoid with four parameters`() {
        val functoid = FunctoidFactory.from<Int, Int, Int, Int, Int>({ a: Int, b: Int, c: Int, d: Int -> a + b + c + d })

        assertEquals(4, functoid.getArity())
        assertEquals(10, functoid.invoke(listOf(1, 2, 3, 4)))
    }

    @Test
    fun `test functoid with five parameters`() {
        val functoid = FunctoidFactory.from<Int, Int, Int, Int, Int, Int>({ a: Int, b: Int, c: Int, d: Int, e: Int ->
            a + b + c + d + e
        })

        assertEquals(5, functoid.getArity())
        assertEquals(15, functoid.invoke(listOf(1, 2, 3, 4, 5)))
    }

    @Test
    fun `test functoid map operation`() {
        val functoid = FunctoidFactory.from<Int, Int>({ x: Int -> x * 2 })
        val mapped = functoid.map({ it + 1 }, typeOf<Int>())

        assertEquals(1, mapped.getArity())
        assertEquals(11, mapped.invoke(listOf(5)))
    }

    @Test
    fun `test functoid map2 operation`() {
        val functoid1 = FunctoidFactory.from<Int, Int>({ x: Int -> x * 2 })
        val functoid2 = FunctoidFactory.from<Int, Int>({ y: Int -> y + 1 })
        val combined = functoid1.map2(functoid2, { a, b -> a + b }, typeOf<Int>())

        assertEquals(2, combined.getArity())
        assertEquals(21, combined.invoke(listOf(5, 10)))  // (5*2) + (10+1) = 10 + 11 = 21
    }

    @Test
    fun `test functoid zip operation`() {
        val functoid1 = FunctoidFactory.from<Int, Int>({ x: Int -> x * 2 })
        val functoid2 = FunctoidFactory.from<String, String>({ y: String -> y.uppercase() })
        val zipped = functoid1.zip(functoid2)

        assertEquals(2, zipped.getArity())
        val result = zipped.invoke(listOf(5, "hello"))
        assertNotNull(result)
        assertEquals(Pair(10, "HELLO"), result)
    }

    @Test
    fun `test functoid pure operation`() {
        val functoid = Functoid.pure(42, typeOf<Int>())

        assertEquals(0, functoid.getArity())
        assertEquals(42, functoid.invoke(emptyList()))
    }

    @Test
    fun `test functoid invoke with wrong number of arguments`() {
        val functoid = FunctoidFactory.from<Int, Int>({ x: Int -> x * 2 })

        assertFailsWith<IllegalArgumentException> {
            functoid.invoke(emptyList())
        }

        assertFailsWith<IllegalArgumentException> {
            functoid.invoke(listOf(1, 2))
        }
    }

    @Test
    fun `test functoid with different types`() {
        data class Person(val name: String, val age: Int)

        val functoid = FunctoidFactory.from<String, Int, Person>({ name: String, age: Int ->
            Person(name, age)
        })

        assertEquals(2, functoid.getArity())
        val person = functoid.invoke(listOf("Alice", 30))
        assertEquals(Person("Alice", 30), person)
    }

    @Test
    fun `test complex functoid composition`() {
        // Create three functoids
        val f1 = FunctoidFactory.from<Int, Int>({ x: Int -> x + 1 })
        val f2 = FunctoidFactory.from<Int, Int>({ y: Int -> y * 2 })

        // Compose them using map2
        val composed = f1.map2(f2, { a, b -> a + b }, typeOf<Int>())
            .map({ it * 10 }, typeOf<Int>())

        assertEquals(2, composed.getArity())
        // (5+1) + (10*2) = 6 + 20 = 26, then 26 * 10 = 260
        assertEquals(260, composed.invoke(listOf(5, 10)))
    }

    @Test
    fun `test TypeTag toString with Id`() {
        val tag1 = TypeTag.of<Int>()
        val tag2 = TypeTag.of<Int>("special")

        assertEquals("kotlin.Int", tag1.toString())
        assertEquals("kotlin.Int @Id(\"special\")", tag2.toString())
    }

    @Test
    fun `test TypeTag hasId`() {
        val tag1 = TypeTag.of<Int>()
        val tag2 = TypeTag.of<Int>("special")

        assertEquals(false, tag1.hasId())
        assertEquals(true, tag2.hasId())
    }
}
