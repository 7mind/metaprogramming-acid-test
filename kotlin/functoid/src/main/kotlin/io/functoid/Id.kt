package io.functoid

/**
 * Annotation for marking dependency identifiers in function parameters.
 * Similar to Izumi Distage's @Id annotation.
 *
 * Example:
 * ```
 * fun service(@Id("primary") db: Database, @Id("cache") db2: Database): Service
 * ```
 */
@Target(AnnotationTarget.VALUE_PARAMETER, AnnotationTarget.TYPE)
@Retention(AnnotationRetention.RUNTIME)
annotation class Id(val value: String)
