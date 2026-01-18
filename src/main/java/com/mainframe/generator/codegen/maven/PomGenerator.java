package com.mainframe.generator.codegen.maven;

import com.mainframe.generator.codegen.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates Maven POM files for generated projects.
 */
public class PomGenerator {

    private final GeneratorConfig config;

    public PomGenerator(GeneratorConfig config) {
        this.config = config;
    }

    public void generate(Path projectDir) throws IOException {
        String pom = buildPomContent();
        FileWriteUtil.safeWriteString(projectDir.resolve("pom.xml"), pom);
    }

    private String buildPomContent() {
        return String.format("""
                <?xml version="1.0" encoding="UTF-8"?>
                <project xmlns="http://maven.apache.org/POM/4.0.0"
                         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
                    <modelVersion>4.0.0</modelVersion>

                    <parent>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-starter-parent</artifactId>
                        <version>3.2.1</version>
                        <relativePath/>
                    </parent>

                    <groupId>%s</groupId>
                    <artifactId>%s</artifactId>
                    <version>1.0.0-SNAPSHOT</version>
                    <packaging>jar</packaging>

                    <name>%s</name>
                    <description>Generated Spring Boot + Camel mainframe integration project</description>

                    <properties>
                        <java.version>17</java.version>
                        <camel.version>4.3.0</camel.version>
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
                """, config.getBasePackage(), config.getArtifactId(), config.getProjectName());
    }
}
