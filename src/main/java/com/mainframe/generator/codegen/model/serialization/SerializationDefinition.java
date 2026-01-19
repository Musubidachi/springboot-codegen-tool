package com.mainframe.generator.codegen.model.serialization;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.util.List;

import com.mainframe.generator.codegen.model.ModelDefinition;

/**
 * Describes serialization/deserialization artifacts to generate for a given model.
 *
 * Pure structure only.
 */
@Value
@Builder(toBuilder = true)
public class SerializationDefinition {

    @NonNull
    ModelDefinition model;

    /**
     * Serializer class name (simple name).
     */
    @NonNull
    String serializerClassName;

    /**
     * Deserializer class name (simple name).
     */
    @NonNull
    String deserializerClassName;

    /**
     * Package name where serialization artifacts will live.
     */
    @NonNull
    String packageName;

    /**
     * Field-level hints for serialization (optional).
     */
    @NonNull
    @Singular("fieldHint")
    List<SerializationFieldHint> fieldHints;

    @Value
    @Builder(toBuilder = true)
    public static class SerializationFieldHint {

        @NonNull
        String fieldName;

        /**
         * Free-form hint string (e.g., "ODO_COUNT_FIELD", "PAD_LEFT", "CUSTOM_FORMAT:yyyyMMdd").
         */
        @NonNull
        String hint;
    }
}
