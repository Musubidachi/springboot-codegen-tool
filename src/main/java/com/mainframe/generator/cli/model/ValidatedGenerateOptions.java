package com.mainframe.generator.cli.model;

import java.nio.file.Path;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Validated and derived values from GenerateOptions.
 * Keeps GenerateCommand thin and focused on orchestration.
 */
@Data
@AllArgsConstructor
public class ValidatedGenerateOptions {
    /**
     * Normalized output directory path.
     */
    private Path normalizedOutputDir;

    /**
     * Full path to the generated project.
     */
    private Path projectPath;
}
