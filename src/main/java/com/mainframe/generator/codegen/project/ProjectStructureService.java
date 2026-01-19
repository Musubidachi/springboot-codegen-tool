package com.mainframe.generator.codegen.project;

import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Creates the output project directory and core source/resource roots.
 *
 * Does NOT assume Java package structure (no basePackage required).
 * Individual generators will create any needed package folders via FileWriteUtil.safeWriteString().
 */
public class ProjectStructureService {

    public Path createProjectStructure(GeneratorConfig config) throws IOException {
        Path projectDir = config.getOutputDir().resolve(config.getProjectName());

        if (Files.exists(projectDir)) {
            if (config.isForce()) {
                FileWriteUtil.deleteDirectory(projectDir);
            } else {
                throw new IOException("Project directory already exists: " + projectDir);
            }
        }

        // Minimal skeleton
        FileWriteUtil.createDirectories(projectDir.resolve("src/main/java"));
        FileWriteUtil.createDirectories(projectDir.resolve("src/main/resources"));
        FileWriteUtil.createDirectories(projectDir.resolve("src/test/java"));
        FileWriteUtil.createDirectories(projectDir.resolve("src/test/resources"));

        return projectDir;
    }
}
