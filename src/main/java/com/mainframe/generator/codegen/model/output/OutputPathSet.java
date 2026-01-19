package com.mainframe.generator.codegen.model.output;

import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

import java.nio.file.Path;

/**
 * Well-known output paths for a generated project.
 */
@Value
@Builder(toBuilder = true)
public class OutputPathSet {

    @NonNull
    Path projectRoot;

    @NonNull
    Path srcMainJava;

    @NonNull
    Path srcTestJava;

    @NonNull
    Path srcMainResources;

    @NonNull
    Path srcTestResources;

    @NonNull
    Path modelPackagePath;

    @NonNull
    Path wrapperPackagePath;
}
