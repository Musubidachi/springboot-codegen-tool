package com.mainframe.generator.codegen.model.wrapper;

import com.mainframe.generator.codegen.model.ModelDefinition;

import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

/**
 * Represents a wrapper class to be generated around a model.
 *
 * Pure structure only.
 */
@Value
@Builder(toBuilder = true)
public class WrapperDefinition {

    @NonNull
    WrapperType type;

    /**
     * The model being wrapped.
     */
    @NonNull
    ModelDefinition model;

    /**
     * Wrapper class simple name.
     */
    @NonNull
    String className;

    /**
     * Wrapper package name.
     */
    @NonNull
    String packageName;
}
