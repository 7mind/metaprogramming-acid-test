package io.functoid

import kotlin.reflect.KFunction
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.jvm.javaType
import kotlin.reflect.typeOf

/**
 * Factory methods for creating Functoids from various function types.
 * Provides compile-time type safety while enabling runtime introspection.
 */
object FunctoidFactory {

    /**
     * Create a Functoid from a function with 0 parameters.
     */
    inline fun <reified R> from(noinline fn: () -> R): Functoid<R> {
        val provider = Provider(
            parameterTypes = emptyList(),
            returnType = typeOf<R>(),
            function = { _ -> fn() }
        )
        return Functoid(provider)
    }

    /**
     * Create a Functoid from a function with 1 parameter.
     */
    inline fun <reified T1, reified R> from(
        noinline fn: (T1) -> R,
        id1: String? = null
    ): Functoid<R> {
        val provider = Provider(
            parameterTypes = listOf(TypeTag.of<T1>(id1)),
            returnType = typeOf<R>(),
            function = { args ->
                @Suppress("UNCHECKED_CAST")
                fn(args[0] as T1)
            }
        )
        return Functoid(provider)
    }

    /**
     * Create a Functoid from a function with 2 parameters.
     */
    inline fun <reified T1, reified T2, reified R> from(
        noinline fn: (T1, T2) -> R,
        id1: String? = null,
        id2: String? = null
    ): Functoid<R> {
        val provider = Provider(
            parameterTypes = listOf(
                TypeTag.of<T1>(id1),
                TypeTag.of<T2>(id2)
            ),
            returnType = typeOf<R>(),
            function = { args ->
                @Suppress("UNCHECKED_CAST")
                fn(args[0] as T1, args[1] as T2)
            }
        )
        return Functoid(provider)
    }

    /**
     * Create a Functoid from a function with 3 parameters.
     */
    inline fun <reified T1, reified T2, reified T3, reified R> from(
        noinline fn: (T1, T2, T3) -> R,
        id1: String? = null,
        id2: String? = null,
        id3: String? = null
    ): Functoid<R> {
        val provider = Provider(
            parameterTypes = listOf(
                TypeTag.of<T1>(id1),
                TypeTag.of<T2>(id2),
                TypeTag.of<T3>(id3)
            ),
            returnType = typeOf<R>(),
            function = { args ->
                @Suppress("UNCHECKED_CAST")
                fn(args[0] as T1, args[1] as T2, args[2] as T3)
            }
        )
        return Functoid(provider)
    }

    /**
     * Create a Functoid from a function with 4 parameters.
     */
    inline fun <reified T1, reified T2, reified T3, reified T4, reified R> from(
        noinline fn: (T1, T2, T3, T4) -> R,
        id1: String? = null,
        id2: String? = null,
        id3: String? = null,
        id4: String? = null
    ): Functoid<R> {
        val provider = Provider(
            parameterTypes = listOf(
                TypeTag.of<T1>(id1),
                TypeTag.of<T2>(id2),
                TypeTag.of<T3>(id3),
                TypeTag.of<T4>(id4)
            ),
            returnType = typeOf<R>(),
            function = { args ->
                @Suppress("UNCHECKED_CAST")
                fn(args[0] as T1, args[1] as T2, args[2] as T3, args[3] as T4)
            }
        )
        return Functoid(provider)
    }

    /**
     * Create a Functoid from a function with 5 parameters.
     */
    inline fun <reified T1, reified T2, reified T3, reified T4, reified T5, reified R> from(
        noinline fn: (T1, T2, T3, T4, T5) -> R,
        id1: String? = null,
        id2: String? = null,
        id3: String? = null,
        id4: String? = null,
        id5: String? = null
    ): Functoid<R> {
        val provider = Provider(
            parameterTypes = listOf(
                TypeTag.of<T1>(id1),
                TypeTag.of<T2>(id2),
                TypeTag.of<T3>(id3),
                TypeTag.of<T4>(id4),
                TypeTag.of<T5>(id5)
            ),
            returnType = typeOf<R>(),
            function = { args ->
                @Suppress("UNCHECKED_CAST")
                fn(args[0] as T1, args[1] as T2, args[2] as T3, args[3] as T4, args[4] as T5)
            }
        )
        return Functoid(provider)
    }

    /**
     * Create a Functoid from a KFunction with automatic @Id annotation extraction.
     */
    fun <R> fromFunction(function: KFunction<R>): Functoid<R> {
        val parameterTypes = function.parameters.map { param ->
            val idAnnotation = param.findAnnotation<Id>()
            TypeTag.from(param.type, idAnnotation?.value)
        }

        val provider = Provider(
            parameterTypes = parameterTypes,
            returnType = function.returnType,
            function = { args ->
                function.call(*args.toTypedArray())
            }
        )
        return Functoid(provider)
    }
}
