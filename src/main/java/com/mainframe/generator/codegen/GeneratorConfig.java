package com.mainframe.generator.codegen;

import com.mainframe.generator.model.FramingMode;
import lombok.Builder;
import lombok.Data;

import java.nio.file.Path;
import java.util.List;

/**
 * Configuration for the project generator.
 */
@Data
@Builder
public class GeneratorConfig {
    private String projectName;
    private Path copybookDir;
    private Path mappingDoc;
    private List<Path> externalCopybookDirs;
    private String programId;
    private String encoding;
    private String tcpHost;
    private int tcpPort;
    private int tcpConnectTimeout;
    private int tcpReadTimeout;
    private FramingMode framingMode;
    private Path outputDir;
    private boolean force;
    private boolean skipTests;
    
    /**
     * Get the base package name for generated code.
     */
    public String getBasePackage() {
        String name = projectName.toLowerCase()
                .replaceAll("[^a-z0-9]", "")
                .replaceAll("^[0-9]+", "");
        return "com." + name;
    }
    
    /**
     * Get the artifact ID for Maven.
     */
    public String getArtifactId() {
        return projectName.toLowerCase().replaceAll("[^a-z0-9-]", "-");
    }
    
    /**
     * Get the program ID in a URL-safe format.
     */
    public String getProgramIdPath() {
        return programId.toLowerCase().replace("-", "");
    }
}
