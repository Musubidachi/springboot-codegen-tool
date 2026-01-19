package com.mainframe.generator.codegen.model;

import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

/**
 * Describes a Java type used in generated models.
 *
 * Pure structure only – no resolution or mapping logic.
 */
@Value
@Builder(toBuilder = true)
public class JavaType {

    /**
     * Fully-qualified Java type name or simple name if imported.
     * Examples: "String", "BigDecimal", "List", "com.foo.Bar".
     */
    @NonNull
    String rawType;

    /**
     * Whether this type represents a collection (List, Set, etc.).
     */
    boolean collection;

    /**
     * Element type if this is a collection.
     */
    JavaType elementType;

    /**
     * Whether the type is a Java primitive.
     */
    boolean primitive;

    /**
     * Optional hint for custom serialization/deserialization.
     * Example: "LocalDateSerializer".
     */
    String serializerHint;
}
