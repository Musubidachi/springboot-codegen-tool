package com.mainframe.generator.codegen.project;

import com.mainframe.generator.codegen.util.FileWriteUtil;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates application.yml files for main and test resources.
 *
 * Note: Transport configuration is intentionally excluded.
 * Transport is handled by the implementing organization.
 */
public class ApplicationYamlGenerator {

    /**
     * Generates both main and test application.yml files.
     *
     * @param projectDir the project directory
     * @param info the configuration info
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir, AppYamlInfo info) throws IOException {
        String yml = generateMainYaml(info);
        FileWriteUtil.safeWriteString(projectDir.resolve("src/main/resources/application.yml"), yml);

        String testYml = generateTestYaml(info);
        FileWriteUtil.safeWriteString(projectDir.resolve("src/test/resources/application.yml"), testYml);
    }

    /**
     * Generates the main application.yml content.
     *
     * @param info the configuration info
     * @return the YAML content as a string
     */
    public String generateMainYaml(AppYamlInfo info) {
        return String.format("""
                server:
                  port: 8080

                spring:
                  application:
                    name: %s

                mainframe:
                  service-name: %s
                  encoding: IBM037

                camel:
                  springboot:
                    name: %s

                logging:
                  level:
                    %s: DEBUG
                    org.apache.camel: INFO
                """,
                info.projectName(),
                info.serviceName(),
                info.projectName(),
                info.loggingPackage()
        );
    }

    /**
     * Generates the test application.yml content.
     *
     * @param info the configuration info
     * @return the YAML content as a string
     */
    public String generateTestYaml(AppYamlInfo info) {
        return String.format("""
                spring:
                  profiles:
                    active: test

                mainframe:
                  service-name: %s
                  encoding: IBM037
                """,
                info.serviceName()
        );
    }

    /**
     * Configuration info needed to generate application.yml.
     *
     * @param projectName the project name
     * @param serviceName the mainframe service name
     * @param loggingPackage the package to enable DEBUG logging for
     */
    public record AppYamlInfo(
            String projectName,
            String serviceName,
            String loggingPackage
    ) {}
}
