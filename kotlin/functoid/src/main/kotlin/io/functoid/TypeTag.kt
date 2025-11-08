package io.functoid

import kotlin.reflect.KType
import kotlin.reflect.typeOf

/**
 * Runtime type information holder.
 * Represents a type with optional @Id qualifier for dependency injection.
 *
 * Similar to Izumi Distage's type tags but adapted for Kotlin.
 */
data class TypeTag<T>(
    val type: KType,
    val id: String? = null
) {
    /**
     * Check if this type tag has an identifier.
     */
    fun hasId(): Boolean = id != null

    /**
     * Get a string representation of this type tag.
     */
    override fun toString(): String {
        return if (id != null) {
            "$type @Id(\"$id\")"
        } else {
            type.toString()
        }
    }

    companion object {
        /**
         * Create a TypeTag from a reified type parameter.
         */
        inline fun <reified T> of(id: String? = null): TypeTag<T> {
            return TypeTag(typeOf<T>(), id)
        }

        /**
         * Create a TypeTag from a KType.
         */
        fun from(type: KType, id: String? = null): TypeTag<*> {
            return TypeTag<Any>(type, id)
        }
    }
}
