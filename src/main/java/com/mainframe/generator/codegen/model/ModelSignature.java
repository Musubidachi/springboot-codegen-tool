package com.mainframe.generator.codegen.model;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.util.List;

/**
 * Structural identity of a model definition, used primarily for deduplication.
 *
 * Pure structure only (no hashing logic here; that belongs in a calculator/service).
 */
@Value
@Builder(toBuilder = true)
public class ModelSignature {

    /**
     * A stable string fingerprint (often a hash) computed from the field structure.
     */
    @NonNull
    String fingerprint;

    /**
     * Optional: the field-level signature details that contributed to the fingerprint.
     * Useful for debugging signature collisions.
     */
    @NonNull
    @Singular("field")
    List<FieldSignature> fields;

    @Value
    @Builder(toBuilder = true)
    public static class FieldSignature {

        @NonNull
        String name;

        @NonNull
        String type;

        /**
         * Useful when distinguishing scalar vs list/array fields.
         */
        boolean repeated;

        /**
         * Optional: include size/precision/scale/etc if you care about those.
         */
        String modifiers;
    }
}
