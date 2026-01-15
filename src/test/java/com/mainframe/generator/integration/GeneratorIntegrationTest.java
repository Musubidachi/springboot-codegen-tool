package com.mainframe.generator.integration;

import com.mainframe.generator.codegen.GeneratorConfig;
import com.mainframe.generator.codegen.GeneratorResult;
import com.mainframe.generator.codegen.ProjectGenerator;
import com.mainframe.generator.model.FramingMode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.*;

/**
 * Integration tests for the complete generation process.
 */
class GeneratorIntegrationTest {

    @TempDir
    Path tempDir;

    @Test
    void testGenerateProjectFromSampleCopybooks() throws IOException {
        // Create sample copybooks in temp directory
        Path copybookDir = tempDir.resolve("copybooks");
        Files.createDirectories(copybookDir);
        
        String requestCopybook = """
               01  TEST-REQUEST.
                   05  CUSTOMER-ID         PIC 9(10).
                   05  CUSTOMER-NAME       PIC X(30).
                   05  REQUEST-TYPE        PIC X(1).
                       88  TYPE-INQUIRY    VALUE 'I'.
                       88  TYPE-UPDATE     VALUE 'U'.
            """;
        
        Files.writeString(copybookDir.resolve("TEST-REQUEST.cpy"), requestCopybook);
        
        String responseCopybook = """
               01  TEST-RESPONSE.
                   05  RESPONSE-CODE       PIC X(2).
                       88  RESP-OK         VALUE '00'.
                       88  RESP-ERROR      VALUE '99'.
                   05  CUSTOMER-ID         PIC 9(10).
                   05  CUSTOMER-NAME       PIC X(30).
                   05  BALANCE             PIC S9(9)V99 COMP-3.
            """;
        
        Files.writeString(copybookDir.resolve("TEST-RESPONSE.cpy"), responseCopybook);

        // Configure generator
        GeneratorConfig config = GeneratorConfig.builder()
                .projectName("TestProject")
                .copybookDir(copybookDir)
                .programId("TEST-PROG")
                .encoding("cp037")
                .tcpHost("localhost")
                .tcpPort(5000)
                .tcpConnectTimeout(3000)
                .tcpReadTimeout(5000)
                .framingMode(FramingMode.LENGTH_PREFIX_2)
                .outputDir(tempDir)
                .force(true)
                .skipTests(true) // Skip maven tests in unit test
                .build();

        // Generate project
        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        // Verify result
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getCopybooksParsed()).isEqualTo(2);
        assertThat(result.getDtoClassesGenerated()).isGreaterThanOrEqualTo(2);
        assertThat(result.getRequestByteLength()).isGreaterThan(0);
        assertThat(result.getResponseByteLength()).isGreaterThan(0);

        // Verify generated files exist
        Path projectDir = tempDir.resolve("TestProject");
        assertThat(Files.exists(projectDir.resolve("pom.xml"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/resources/application.yml"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("sample-request.json"))).isTrue();
        
        // Verify main classes exist
        String basePackagePath = "com/testproject";
        assertThat(Files.exists(projectDir.resolve("src/main/java/" + basePackagePath + "/Application.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/" + basePackagePath + "/controller/MainframeController.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/" + basePackagePath + "/camel/MainframeRoute.java"))).isTrue();
    }

    @Test
    void testGenerateProjectWithMappingDocument() throws IOException {
        // Create copybook
        Path copybookDir = tempDir.resolve("copybooks");
        Files.createDirectories(copybookDir);
        
        String copybook = """
               01  DATE-RECORD.
                   05  DATE-YEAR           PIC 9(4).
                   05  DATE-MONTH          PIC 9(2).
                   05  DATE-DAY            PIC 9(2).
                   05  OLD-FIELD-NAME      PIC X(10).
            """;
        
        Files.writeString(copybookDir.resolve("DATE-REQUEST.cpy"), copybook);

        // Create mapping document
        Path mappingDoc = tempDir.resolve("mapping.txt");
        String mapping = """
            DATE-YEAR + DATE-MONTH + DATE-DAY = eventDate:LocalDate
            OLD-FIELD-NAME = newFieldName
            """;
        Files.writeString(mappingDoc, mapping);

        // Configure generator
        GeneratorConfig config = GeneratorConfig.builder()
                .projectName("MappingTest")
                .copybookDir(copybookDir)
                .mappingDoc(mappingDoc)
                .programId("DATE-PROG")
                .encoding("cp037")
                .tcpHost("localhost")
                .tcpPort(5000)
                .tcpConnectTimeout(3000)
                .tcpReadTimeout(5000)
                .framingMode(FramingMode.LENGTH_PREFIX_2)
                .outputDir(tempDir)
                .force(true)
                .skipTests(true)
                .build();

        // Generate project
        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        // Verify result
        assertThat(result.isSuccess()).isTrue();
    }

    @Test
    void testFailsOnMissingCopybookDirectory() {
        Path nonExistentDir = tempDir.resolve("nonexistent");

        GeneratorConfig config = GeneratorConfig.builder()
                .projectName("FailTest")
                .copybookDir(nonExistentDir)
                .programId("FAIL-PROG")
                .encoding("cp037")
                .tcpHost("localhost")
                .tcpPort(5000)
                .tcpConnectTimeout(3000)
                .tcpReadTimeout(5000)
                .framingMode(FramingMode.LENGTH_PREFIX_2)
                .outputDir(tempDir)
                .skipTests(true)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isFalse();
    }

    @Test
    void testFailsOnEmptyCopybookDirectory() throws IOException {
        Path emptyDir = tempDir.resolve("empty");
        Files.createDirectories(emptyDir);

        GeneratorConfig config = GeneratorConfig.builder()
                .projectName("EmptyTest")
                .copybookDir(emptyDir)
                .programId("EMPTY-PROG")
                .encoding("cp037")
                .tcpHost("localhost")
                .tcpPort(5000)
                .tcpConnectTimeout(3000)
                .tcpReadTimeout(5000)
                .framingMode(FramingMode.LENGTH_PREFIX_2)
                .outputDir(tempDir)
                .skipTests(true)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getErrorMessage()).contains("No copybooks found");
    }

    @Test
    void testValidationConstraintCounts() throws IOException {
        Path copybookDir = tempDir.resolve("copybooks");
        Files.createDirectories(copybookDir);
        
        String copybook = """
               01  VALIDATION-TEST.
                   05  REQUIRED-STRING     PIC X(20).
                   05  REQUIRED-NUMBER     PIC 9(5).
                   05  DECIMAL-FIELD       PIC S9(7)V99 COMP-3.
                   05  ARRAY-FIELD OCCURS 10 TIMES.
                       10  ARRAY-ITEM      PIC X(5).
            """;
        
        Files.writeString(copybookDir.resolve("VALIDATION-REQUEST.cpy"), copybook);

        GeneratorConfig config = GeneratorConfig.builder()
                .projectName("ValidationTest")
                .copybookDir(copybookDir)
                .programId("VAL-PROG")
                .encoding("cp037")
                .tcpHost("localhost")
                .tcpPort(5000)
                .tcpConnectTimeout(3000)
                .tcpReadTimeout(5000)
                .framingMode(FramingMode.LENGTH_PREFIX_2)
                .outputDir(tempDir)
                .force(true)
                .skipTests(true)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getNotNullCount()).isGreaterThan(0);
        assertThat(result.getSizeCount()).isGreaterThan(0);
        assertThat(result.getDigitsCount()).isGreaterThan(0);
    }
}
