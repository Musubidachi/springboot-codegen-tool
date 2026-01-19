package com.mainframe.generator.codegen.project;

import com.mainframe.generator.codegen.util.FileWriteUtil;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates the Maven pom.xml for the generated project.
 * Keeps ProjectGenerator orchestration-only.
 */
public class PomGenerator {

    // Keep versions centralized so later upgrades are easy.
    private static final String SPRING_BOOT_VERSION = "3.2.1";
    private static final String CAMEL_VERSION = "4.3.0";
    private static final String JAVA_VERSION = "17";

    public void generatePom(Path projectDir, PomInfo pomInfo) throws IOException {
        String pom = generatePomContent(pomInfo);
        FileWriteUtil.safeWriteString(projectDir.resolve("pom.xml"), pom);
    }

    public String generatePomContent(PomInfo pomInfo) {
        return String.format("""
                <?xml version="1.0" encoding="UTF-8"?>
                <project xmlns="http://maven.apache.org/POM/4.0.0"
                         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
                    <modelVersion>4.0.0</modelVersion>

                    <parent>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter-parent</artifactId>
                        <version>%s</version>
                        <relativePath/>
                    </parent>

                    <groupId>%s</groupId>
                    <artifactId>%s</artifactId>
                    <version>%s</version>
                    <packaging>jar</packaging>

                    <name>%s</name>
                    <description>%s</description>

                    <properties>
                        <java.version>%s</java.version>
                        <camel.version>%s</camel.version>
                    </properties>

                    <dependencyManagement>
                        <dependencies>
                            <dependency>
                                <groupId>org.apache.camel.springboot</groupId>
                                <artifactId>camel-spring-boot-bom</artifactId>
                                <version>${camel.version}</version>
                                <type>pom</type>
                                <scope>import</scope>
                            </dependency>
                        </dependencies>
                    </dependencyManagement>

                    <dependencies>
                        <!-- Spring Boot -->
                        <dependency>
                            <groupId>org.springframework.boot</groupId>
                            <artifactId>spring-boot-starter-web</artifactId>
                        </dependency>
                        <dependency>
                            <groupId>org.springframework.boot</groupId>
                            <artifactId>spring-boot-starter-validation</artifactId>
                        </dependency>

                        <!-- Apache Camel -->
                        <dependency>
                            <groupId>org.apache.camel.springboot</groupId>
                            <artifactId>camel-spring-boot-starter</artifactId>
                        </dependency>
                        <dependency>
                            <groupId>org.apache.camel.springboot</groupId>
                            <artifactId>camel-direct-starter</artifactId>
                        </dependency>

                        <!-- Lombok -->
                        <dependency>
                            <groupId>org.projectlombok</groupId>
                            <artifactId>lombok</artifactId>
                            <scope>provided</scope>
                        </dependency>

                        <!-- Jackson -->
                        <dependency>
                            <groupId>com.fasterxml.jackson.core</groupId>
                            <artifactId>jackson-databind</artifactId>
                        </dependency>
                        <dependency>
                            <groupId>com.fasterxml.jackson.datatype</groupId>
                            <artifactId>jackson-datatype-jsr310</artifactId>
                        </dependency>

                        <!-- Testing -->
                        <dependency>
                            <groupId>org.springframework.boot</groupId>
                            <artifactId>spring-boot-starter-test</artifactId>
                            <scope>test</scope>
                        </dependency>
                        <dependency>
                            <groupId>org.apache.camel</groupId>
                            <artifactId>camel-test-spring-junit5</artifactId>
                            <scope>test</scope>
                        </dependency>
                    </dependencies>

                    <build>
                        <plugins>
                            <plugin>
                                <groupId>org.springframework.boot</groupId>
                                <artifactId>spring-boot-maven-plugin</artifactId>
                                <configuration>
                                    <excludes>
                                        <exclude>
                                            <groupId>org.projectlombok</groupId>
                                            <artifactId>lombok</artifactId>
                                        </exclude>
                                    </excludes>
                                </configuration>
                            </plugin>
                        </plugins>
                    </build>
                </project>
                """,
                SPRING_BOOT_VERSION,
                pomInfo.groupId(),
                pomInfo.artifactId(),
                pomInfo.version(),
                pomInfo.name(),
                pomInfo.description(),
                JAVA_VERSION,
                CAMEL_VERSION
        );
    }

    /**
     * Minimal info needed to generate a pom.xml without binding to GeneratorConfig field names.
     */
    public record PomInfo(
            String groupId,
            String artifactId,
            String version,
            String name,
            String description
    ) {}
}
