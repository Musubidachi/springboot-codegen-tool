package com.mainframe.generator.codegen.config;

import com.mainframe.generator.codegen.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.model.FramingMode;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates Spring Boot application configuration files.
 */
public class ApplicationConfigGenerator {

    private final GeneratorConfig config;

    public ApplicationConfigGenerator(GeneratorConfig config) {
        this.config = config;
    }

    public void generate(Path projectDir) throws IOException {
        String yml = buildMainYamlContent();
        FileWriteUtil.safeWriteString(
                projectDir.resolve("src/main/resources/application.yml"), yml);

        String testYml = buildTestYamlContent();
        FileWriteUtil.safeWriteString(
                projectDir.resolve("src/test/resources/application.yml"), testYml);
    }

    private String buildMainYamlContent() {
        return String.format("""
                server:
                  port: 8080

                spring:
                  application:
                    name: %s

                mainframe:
                  program-id: %s
                  tcp:
                    host: %s
                    port: %d
                    connect-timeout-ms: %d
                    read-timeout-ms: %d
                    framing: %s
                    encoding: %s

                camel:
                  springboot:
                    name: %s

                logging:
                  level:
                    %s: DEBUG
                    org.apache.camel: INFO
                """,
                config.getProjectName(),
                config.getProgramId(),
                config.getTcpHost(),
                config.getTcpPort(),
                config.getTcpConnectTimeout(),
                config.getTcpReadTimeout(),
                config.getFramingMode(),
                config.getEncoding(),
                config.getProjectName(),
                config.getBasePackage()
        );
    }

    private String buildTestYamlContent() {
        return """
                spring:
                  profiles:
                    active: test

                mainframe:
                  tcp:
                    host: localhost
                    port: 0
                    framing: LENGTH_PREFIX_2
                """;
    }
}
