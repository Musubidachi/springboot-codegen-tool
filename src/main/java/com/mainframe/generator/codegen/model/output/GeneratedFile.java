package com.mainframe.generator.codegen.model.output;

import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

import java.nio.file.Path;

/**
 * Represents a generated file artifact (path + contents).
 *
 * Pure structure only.
 */
@Value
@Builder(toBuilder = true)
public class GeneratedFile {

    @NonNull
    Path path;

    @NonNull
    String contents;

    @NonNull
    GeneratedFileType type;
}
