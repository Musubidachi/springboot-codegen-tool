package com.mainframe.generator.codegen.model.serialization;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.util.Map;

/**
 * Holds deserialization-time contextual values (e.g., ODO counts, runtime-derived lengths).
 *
 * Pure structure only.
 */
@Value
@Builder(toBuilder = true)
public class DeserializationContext {

    /**
     * Key/value bag for derived field values.
     * Example keys: copybook field name, path, or generated java field name.
     */
    @NonNull
    @Singular("value")
    Map<String, String> fieldValues;
}
