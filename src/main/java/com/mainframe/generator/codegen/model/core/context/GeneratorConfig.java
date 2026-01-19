package com.mainframe.generator.codegen.model.core.context;

import lombok.Builder;
import lombok.Data;

import java.nio.file.Path;
import java.util.List;

import com.mainframe.generator.codegen.model.input.FramingMode;

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

    // New folder-based copybook selection fields
    private Path requestCopybookDir;
    private Path responseCopybookDir;
    private Path sharedCopybookDir;
    private String requestRoot;
    private String responseRoot;
    private boolean inferInheritance;
    private boolean testMode;
    
}
