package com.mainframe.generator.codegen.model.core.context;

import lombok.Builder;
import lombok.Data;

import java.nio.file.Path;

/**
 * Configuration for the project generator.
 *
 * This configuration contains all settings needed to generate a Spring Boot
 * mainframe integration project from COBOL copybooks.
 *
 * Locked settings (cannot be changed):
 * - Encoding: IBM037 (EBCDIC)
 * - Endianness: Big Endian
 */
@Data
@Builder
public class GeneratorConfig {

    /**
     * Name of the mainframe service.
     * Used for project name, package names, and API endpoint.
     */
    private String serviceName;

    /**
     * Directory containing request COBOL copybooks.
     */
    private Path requestCopybookDir;

    /**
     * Directory containing response COBOL copybooks.
     */
    private Path responseCopybookDir;

    /**
     * Output directory for the generated project.
     */
    private Path outputDir;

    /**
     * Character encoding (locked to IBM037).
     */
    private String encoding;

    /**
     * Whether to overwrite existing output directory.
     */
    private boolean force;

    /**
     * Whether this is a dry run (no files written).
     */
    private boolean dryRun;

    /**
     * Whether verbose logging is enabled.
     */
    private boolean verbose;

    /**
     * Returns the encoding, always IBM037.
     * This is a locked setting that cannot be overridden.
     *
     * @return IBM037 encoding
     */
    public String getEncoding() {
        return "IBM037";
    }

    /**
     * Returns the project name derived from service name.
     *
     * @return the project name
     */
    public String getProjectName() {
        return serviceName;
    }
}
