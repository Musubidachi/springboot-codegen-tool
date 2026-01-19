package com.mainframe.generator.codegen.project;

import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.model.input.FramingMode;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates application.yml files for main and test resources.
 */
public class ApplicationYamlGenerator {

    public void generate(Path projectDir, AppYamlInfo info) throws IOException {
        String yml = generateMainYaml(info);
        FileWriteUtil.safeWriteString(projectDir.resolve("src/main/resources/application.yml"), yml);

        String testYml = generateTestYaml();
        FileWriteUtil.safeWriteString(projectDir.resolve("src/test/resources/application.yml"), testYml);
    }

    public String generateMainYaml(AppYamlInfo info) {
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
                info.projectName(),
                info.programId(),
                info.tcpHost(),
                info.tcpPort(),
                info.tcpConnectTimeoutMs(),
                info.tcpReadTimeoutMs(),
                info.framingMode(),
                info.encoding(),
                info.projectName(),
                info.loggingPackage()
        );
    }

    public String generateTestYaml() {
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

    /**
     * Minimal configuration needed to generate application.yml without coupling to GeneratorConfig field names.
     */
    public record AppYamlInfo(
            String projectName,
            String programId,
            String tcpHost,
            int tcpPort,
            int tcpConnectTimeoutMs,
            int tcpReadTimeoutMs,
            FramingMode framingMode,
            String encoding,
            String loggingPackage
    ) {}
}
