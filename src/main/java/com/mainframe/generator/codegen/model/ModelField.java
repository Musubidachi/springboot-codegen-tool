package com.mainframe.generator.codegen.model;

import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

import java.util.Map;

/**
 * Describes a single field on a generated model.
 *
 * Pure structure only (no validation / mapping logic).
 */
@Value
@Builder(toBuilder = true)
public class ModelField {

    /**
     * Java field name (camelCase).
     */
    @NonNull
    String name;

    /**
     * Java type information (e.g., String, BigDecimal, List<String>, custom type).
     * Keep as a model so generators don't rely on stringly-typed logic.
     */
    @NonNull
    JavaType type;

    /**
     * Optional field-level attributes/hints.
     * Examples: "jsonName" -> "CLAIMANT-NAME", "odo" -> "COUNT-FIELD".
     */
    @NonNull
    @Builder.Default
    Map<String, String> attributes = Map.of();

    /**
     * Whether the field should be omitted from output (e.g., mapping ignore).
     */
    boolean ignored;
}
