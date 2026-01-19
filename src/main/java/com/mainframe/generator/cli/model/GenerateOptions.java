package com.mainframe.generator.cli.model;

import java.nio.file.Path;

import lombok.Getter;
import picocli.CommandLine.Option;

/**
 * Holds all CLI options for the "generate" command.
 *
 * Required options:
 * - --serviceName: Name of the mainframe service
 * - --outputDir: Output directory for generated project
 * - --requestCopybookDir: Directory containing request copybooks
 * - --responseCopybookDir: Directory containing response copybooks
 *
 * Optional flags:
 * - --dryRun: Perform full parsing and planning but write no files
 * - --verbose: Enable DEBUG-level logging
 *
 * Locked settings (NON-OVERRIDABLE):
 * - Encoding: IBM037 (EBCDIC)
 * - Endianness: Big Endian
 */
@Getter
public class GenerateOptions {

    @Option(names = { "--serviceName" }, required = true,
            description = "Name of the mainframe service (used for project name and endpoint)")
    private String serviceName;

    @Option(names = { "--outputDir" }, required = true,
            description = "Output directory for the generated project")
    private Path outputDir;

    @Option(names = { "--requestCopybookDir" }, required = true,
            description = "Directory containing request-related COBOL copybooks")
    private Path requestCopybookDir;

    @Option(names = { "--responseCopybookDir" }, required = true,
            description = "Directory containing response-related COBOL copybooks")
    private Path responseCopybookDir;

    @Option(names = { "--dryRun" },
            description = "Perform full parsing and planning but write no files")
    private boolean dryRun;

    @Option(names = { "--verbose" },
            description = "Enable DEBUG-level logging")
    private boolean verbose;

    @Option(names = { "--force", "-f" },
            description = "Overwrite existing output directory")
    private boolean force;

    // LOCKED SETTINGS - exposed as getters but not configurable via CLI

    /**
     * Returns the encoding (IBM037). This is locked and not configurable.
     * @return IBM037 encoding string
     */
    public String getEncoding() {
        return "IBM037";
    }
}
