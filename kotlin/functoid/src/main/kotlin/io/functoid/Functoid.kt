package io.functoid

import kotlin.reflect.KType

/**
 * A function that can be introspected at runtime.
 * Functoid wraps a Provider and provides applicative functor operations.
 *
 * Similar to Izumi Distage's Functoid, this allows:
 * - Runtime introspection of function signatures
 * - Type-safe dependency injection
 * - Composability through map/map2 operations
 */
data class Functoid<out A>(val provider: Provider) {

    /**
     * Get the list of parameter type tags.
     */
    fun getParameterTypes(): List<TypeTag<*>> = provider.parameterTypes

    /**
     * Get the return type.
     */
    fun getReturnType(): KType = provider.returnType

    /**
     * Get the arity (number of parameters).
     */
    fun getArity(): Int = provider.arity

    /**
     * Invoke the functoid with the given arguments.
     */
    fun invoke(args: List<Any?>): Any? = provider.invoke(args)

    /**
     * Map over the result of this functoid.
     * Applicative functor operation.
     */
    fun <B> map(f: (A) -> B, returnType: KType): Functoid<B> {
        val newProvider = Provider(
            parameterTypes = provider.parameterTypes,
            returnType = returnType,
            function = { args ->
                @Suppress("UNCHECKED_CAST")
                val result = provider.invoke(args) as A
                f(result)
            }
        )
        return Functoid(newProvider)
    }

    /**
     * Combine two functoids.
     * Applicative functor operation.
     */
    fun <B, C> map2(
        that: Functoid<B>,
        f: (A, B) -> C,
        returnType: KType
    ): Functoid<C> {
        val newProvider = Provider(
            parameterTypes = provider.parameterTypes + that.provider.parameterTypes,
            returnType = returnType,
            function = { args ->
                val argsA = args.take(provider.arity)
                val argsB = args.drop(provider.arity)
                @Suppress("UNCHECKED_CAST")
                val resultA = provider.invoke(argsA) as A
                @Suppress("UNCHECKED_CAST")
                val resultB = that.provider.invoke(argsB) as B
                f(resultA, resultB)
            }
        )
        return Functoid(newProvider)
    }

    /**
     * Zip two functoids into a pair.
     */
    fun <B> zip(that: Functoid<B>): Functoid<Pair<A, B>> {
        return map2(that, { a, b -> Pair(a, b) }, getReturnType())
    }

    companion object {
        /**
         * Lift a pure value into a Functoid.
         * Applicative pure operation.
         */
        fun <A> pure(value: A, returnType: KType): Functoid<A> {
            val provider = Provider(
                parameterTypes = emptyList(),
                returnType = returnType,
                function = { _ -> value }
            )
            return Functoid(provider)
        }
    }
}
