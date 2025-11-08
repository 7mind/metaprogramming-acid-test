package io.functoid

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class FunctoidAnnotationTest {

    // Test function with @Id annotations
    fun serviceWithIds(
        @Id("primary") db: String,
        @Id("cache") cache: String
    ): String {
        return "$db-$cache"
    }

    // Test function without @Id annotations
    fun serviceWithoutIds(db: String, cache: String): String {
        return "$db-$cache"
    }

    // Test function with mixed annotations
    fun serviceWithMixedIds(
        @Id("primary") db: String,
        cache: String,
        @Id("logger") logger: String
    ): String {
        return "$db-$cache-$logger"
    }

    @Test
    fun `test fromFunction with Id annotations`() {
        val functoid = FunctoidFactory.fromFunction(::serviceWithIds)

        assertEquals(2, functoid.getArity())
        assertEquals(2, functoid.getParameterTypes().size)

        // Check first parameter
        val param1 = functoid.getParameterTypes()[0]
        assertEquals("primary", param1.id)

        // Check second parameter
        val param2 = functoid.getParameterTypes()[1]
        assertEquals("cache", param2.id)

        // Test invocation
        val result = functoid.invoke(listOf("postgres", "redis"))
        assertEquals("postgres-redis", result)
    }

    @Test
    fun `test fromFunction without Id annotations`() {
        val functoid = FunctoidFactory.fromFunction(::serviceWithoutIds)

        assertEquals(2, functoid.getArity())
        assertEquals(2, functoid.getParameterTypes().size)

        // Check that no @Id annotations are present
        val param1 = functoid.getParameterTypes()[0]
        assertNull(param1.id)

        val param2 = functoid.getParameterTypes()[1]
        assertNull(param2.id)

        // Test invocation
        val result = functoid.invoke(listOf("postgres", "redis"))
        assertEquals("postgres-redis", result)
    }

    @Test
    fun `test fromFunction with mixed Id annotations`() {
        val functoid = FunctoidFactory.fromFunction(::serviceWithMixedIds)

        assertEquals(3, functoid.getArity())
        assertEquals(3, functoid.getParameterTypes().size)

        // Check first parameter (has @Id)
        val param1 = functoid.getParameterTypes()[0]
        assertEquals("primary", param1.id)

        // Check second parameter (no @Id)
        val param2 = functoid.getParameterTypes()[1]
        assertNull(param2.id)

        // Check third parameter (has @Id)
        val param3 = functoid.getParameterTypes()[2]
        assertEquals("logger", param3.id)

        // Test invocation
        val result = functoid.invoke(listOf("postgres", "redis", "log4j"))
        assertEquals("postgres-redis-log4j", result)
    }

    // Note: Lambdas cannot be used with fromFunction as they don't have KFunction type
    // They should be used with the typed from() methods instead

    // Helper function for testing complex types
    fun complexService(
        @Id("config") config: Map<String, Int>,
        @Id("settings") settings: List<String>
    ): Int {
        return config.size + settings.size
    }

    @Test
    fun `test fromFunction preserves types correctly`() {
        val functoid = FunctoidFactory.fromFunction(::complexService)

        assertEquals(2, functoid.getArity())

        val param1 = functoid.getParameterTypes()[0]
        assertEquals("config", param1.id)

        val param2 = functoid.getParameterTypes()[1]
        assertEquals("settings", param2.id)

        val result = functoid.invoke(
            listOf(
                mapOf("a" to 1, "b" to 2),
                listOf("x", "y", "z")
            )
        )
        assertEquals(5, result)
    }
}
