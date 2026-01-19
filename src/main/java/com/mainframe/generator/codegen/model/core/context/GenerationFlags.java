package com.mainframe.generator.codegen.model.core.context;

import lombok.Builder;
import lombok.Value;

/**
 * Boolean switches that influence generation behavior.
 */
@Value
@Builder(toBuilder = true)
public class GenerationFlags {

    boolean testMode;
    boolean runMavenTests;
    boolean overwriteExisting;
    boolean heuristicMode;
    boolean generateSamples;
}
