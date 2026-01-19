package com.mainframe.generator.codegen.model.core.context;

import lombok.Builder;
import lombok.Value;

/**
 * Aggregated statistics for a generation run.
 */
@Value
@Builder(toBuilder = true)
public class GenerationStats {

    int modelCount;
    int enumCount;
    int wrapperCount;
    int fileCount;

    long generationTimeMillis;
}
