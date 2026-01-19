package com.mainframe.generator.codegen.model;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.mainframe.generator.codegen.model.input.CopybookModel;

/**
 * Canonical internal representation of one generated Java "model" class.
 *
 * Pure structure only (no validation, no generation logic).
 */
@Value
@Builder(toBuilder = true)
public class ModelDefinition {

    /**
     * Simple class name to generate (e.g., "Request", "Response", "Claimant").
     */
    @NonNull
    String modelName;

    /**
     * Java package name for the generated class (e.g., "com.acme.app.model.request").
     */
    @NonNull
    String packageName;

    /**
     * Classification (request/response/shared).
     */
    @NonNull
    ModelKind kind;

    /**
     * Originating copybook backing this model.
     */
    @NonNull
    CopybookModel source;

    /**
     * Structural signature used for deduplication/identity.
     */
    @NonNull
    ModelSignature signature;

    /**
     * Fields on this model.
     */
    @NonNull
    @Singular("field")
    List<ModelField> fields;

    /**
     * Nested model definitions (inner classes) if you choose to generate them that way.
     * Keep as structure; generation decides whether these become inner vs top-level classes.
     */
    @NonNull
    @Singular("nestedModel")
    List<ModelDefinition> nestedModels;

    /**
     * Optional generation-time hints/attributes (stringly-typed on purpose for now).
     * Examples: "implements" -> "Serializable", "annotate" -> "Jackson", etc.
     */
    @NonNull
    @Singular("attribute")
    Map<String, String> attributes;

    /**
     * Tags for categorization / downstream decisions (e.g., "HAS_ODO", "NEEDS_CUSTOM_SERIALIZER").
     */
    @NonNull
    @Singular("tag")
    Set<String> tags;

    /**
     * Fully qualified name convenience (structure-only).
     */
    public String getFqn() {
        return packageName + "." + modelName;
    }
}
