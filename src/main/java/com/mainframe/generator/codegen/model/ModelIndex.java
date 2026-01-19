package com.mainframe.generator.codegen.model;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.util.Map;
import java.util.Set;

/**
 * Index of all model definitions participating in generation.
 */
@Value
@Builder(toBuilder = true)
public class ModelIndex {

    /**
     * Lookup by fully-qualified model name or logical model name.
     */
    @NonNull
    @Singular("modelByName")
    Map<String, ModelDefinition> byName;

    /**
     * Models grouped by structural signature (used for deduplication).
     */
    @NonNull
    @Singular("modelsBySignature")
    Map<ModelSignature, Set<ModelDefinition>> bySignature;
}
