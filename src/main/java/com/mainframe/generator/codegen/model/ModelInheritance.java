package com.mainframe.generator.codegen.model;

import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

/**
 * Represents an inheritance relationship between two model definitions.
 *
 * Pure structure only.
 */
@Value
@Builder(toBuilder = true)
public class ModelInheritance {

    @NonNull
    ModelDefinition child;

    @NonNull
    ModelDefinition parent;
}
