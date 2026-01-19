package com.mainframe.generator.cli.model;

import java.nio.file.Path;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Derived values needed by the executor. Keeps GenerateCommand thin.
 */
@Data
@AllArgsConstructor
public class ValidatedGenerateOptions {
    boolean usingFolderBasedSelection;
    Path normalizedOutputDir;
    Path projectPath;
    List<Path> externalCopybookDirs;
}