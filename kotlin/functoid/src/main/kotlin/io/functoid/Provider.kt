package io.functoid

import kotlin.reflect.KType

/**
 * Provider wraps a function along with its parameter type information.
 * This allows runtime introspection of function signatures.
 */
data class Provider(
    val parameterTypes: List<TypeTag<*>>,
    val returnType: KType,
    val function: (List<Any?>) -> Any?
) {
    /**
     * Get the number of parameters this provider accepts.
     */
    val arity: Int
        get() = parameterTypes.size

    /**
     * Invoke the provider with the given arguments.
     */
    fun invoke(args: List<Any?>): Any? {
        require(args.size == arity) {
            "Expected $arity arguments but got ${args.size}"
        }
        return function(args)
    }
}
