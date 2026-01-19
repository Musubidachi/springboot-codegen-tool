package com.mainframe.generator.integration;

import com.mainframe.generator.codegen.GeneratorResult;
import com.mainframe.generator.codegen.ProjectGenerator;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;

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
        // Create separate request and response copybook directories
        Path requestDir = tempDir.resolve("request");
        Path responseDir = tempDir.resolve("response");
        Files.createDirectories(requestDir);
        Files.createDirectories(responseDir);

        String requestCopybook = """
               01  TEST-REQUEST-REC.
                   05  CUSTOMER-ID         PIC 9(10).
                   05  CUSTOMER-NAME       PIC X(30).
                   05  REQUEST-TYPE        PIC X(1).
                       88  TYPE-INQUIRY    VALUE 'I'.
                       88  TYPE-UPDATE     VALUE 'U'.
            """;
        Files.writeString(requestDir.resolve("TEST-REQUEST.cpy"), requestCopybook);

        String responseCopybook = """
               01  TEST-RESPONSE-REC.
                   05  RESPONSE-CODE       PIC X(2).
                       88  RESP-OK         VALUE '00'.
                       88  RESP-ERROR      VALUE '99'.
                   05  CUSTOMER-ID         PIC 9(10).
                   05  CUSTOMER-NAME       PIC X(30).
                   05  BALANCE             PIC S9(9)V99 COMP-3.
            """;
        Files.writeString(responseDir.resolve("TEST-RESPONSE.cpy"), responseCopybook);

        // Configure generator with new structure
        GeneratorConfig config = GeneratorConfig.builder()
                .serviceName("TestService")
                .requestCopybookDir(requestDir)
                .responseCopybookDir(responseDir)
                .outputDir(tempDir)
                .force(true)
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
        Path projectDir = tempDir.resolve("TestService");
        assertThat(Files.exists(projectDir.resolve("pom.xml"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/resources/application.yml"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("README.md"))).isTrue();

        // Verify main classes exist
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/TestServiceApplication.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/api/MainframeController.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/camel/MainframeRoute.java"))).isTrue();

        // Verify 5-step Camel pipeline
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/camel/RequestValidator.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/camel/ContainerAssembler.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/camel/TransportInvoker.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/camel/ResponseValidator.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/camel/ResponseDeserializer.java"))).isTrue();

        // Verify NoOp transport
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/mainframe/transport/MainframeTransport.java"))).isTrue();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/mainframe/transport/NoOpMainframeTransport.java"))).isTrue();
    }

    @Test
    void testFailsOnMissingRequestCopybookDirectory() {
        Path nonExistentDir = tempDir.resolve("nonexistent");
        Path responseDir = tempDir.resolve("response");

        GeneratorConfig config = GeneratorConfig.builder()
                .serviceName("FailTest")
                .requestCopybookDir(nonExistentDir)
                .responseCopybookDir(responseDir)
                .outputDir(tempDir)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isFalse();
    }

    @Test
    void testFailsOnEmptyCopybookDirectory() throws IOException {
        Path emptyRequestDir = tempDir.resolve("empty-request");
        Path emptyResponseDir = tempDir.resolve("empty-response");
        Files.createDirectories(emptyRequestDir);
        Files.createDirectories(emptyResponseDir);

        GeneratorConfig config = GeneratorConfig.builder()
                .serviceName("EmptyTest")
                .requestCopybookDir(emptyRequestDir)
                .responseCopybookDir(emptyResponseDir)
                .outputDir(tempDir)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isFalse();
        assertThat(result.getErrorMessage()).contains("No copybooks found");
    }

    @Test
    void testContainerKeysFromRecordNames() throws IOException {
        // Create copybooks with specific 01-level record names
        Path requestDir = tempDir.resolve("req");
        Path responseDir = tempDir.resolve("resp");
        Files.createDirectories(requestDir);
        Files.createDirectories(responseDir);

        String requestCopybook = """
               01  ABC-REQUEST-REC.
                   05  FIELD-A             PIC X(10).
            """;
        Files.writeString(requestDir.resolve("REQUEST.cpy"), requestCopybook);

        String responseCopybook = """
               01  ABC-RESPONSE-REC.
                   05  FIELD-B             PIC X(10).
            """;
        Files.writeString(responseDir.resolve("RESPONSE.cpy"), responseCopybook);

        GeneratorConfig config = GeneratorConfig.builder()
                .serviceName("ContainerKeyTest")
                .requestCopybookDir(requestDir)
                .responseCopybookDir(responseDir)
                .outputDir(tempDir)
                .force(true)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isTrue();

        // Verify the generated files use correct container key normalization
        // ABC-REQUEST-REC -> ABC_REQUEST_REC
        Path projectDir = tempDir.resolve("ContainerKeyTest");

        // Check that the generated serializer references the normalized container key
        Path requestSerializerPath = projectDir.resolve("src/main/java/com/mainframe/serde/request/AbcRequestRecSerializer.java");
        if (Files.exists(requestSerializerPath)) {
            String content = Files.readString(requestSerializerPath);
            assertThat(content).contains("ABC_REQUEST_REC");
        }
    }

    @Test
    void testNoTcpCodeGenerated() throws IOException {
        Path requestDir = tempDir.resolve("request");
        Path responseDir = tempDir.resolve("response");
        Files.createDirectories(requestDir);
        Files.createDirectories(responseDir);

        String requestCopybook = """
               01  NO-TCP-REQUEST.
                   05  DATA-FIELD          PIC X(20).
            """;
        Files.writeString(requestDir.resolve("REQUEST.cpy"), requestCopybook);

        String responseCopybook = """
               01  NO-TCP-RESPONSE.
                   05  RESULT-FIELD        PIC X(20).
            """;
        Files.writeString(responseDir.resolve("RESPONSE.cpy"), responseCopybook);

        GeneratorConfig config = GeneratorConfig.builder()
                .serviceName("NoTcpTest")
                .requestCopybookDir(requestDir)
                .responseCopybookDir(responseDir)
                .outputDir(tempDir)
                .force(true)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isTrue();

        Path projectDir = tempDir.resolve("NoTcpTest");

        // Verify NO TCP-related classes exist
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/mainframe/transport/TcpMainframeTransport.java"))).isFalse();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/mainframe/emulator/TcpEmulatorServer.java"))).isFalse();
        assertThat(Files.exists(projectDir.resolve("src/main/java/com/mainframe/mainframe/framing"))).isFalse();

        // Verify NoOpMainframeTransport throws UnsupportedOperationException
        Path noOpTransportPath = projectDir.resolve("src/main/java/com/mainframe/mainframe/transport/NoOpMainframeTransport.java");
        assertThat(Files.exists(noOpTransportPath)).isTrue();
        String noOpContent = Files.readString(noOpTransportPath);
        assertThat(noOpContent).contains("UnsupportedOperationException");
    }

    @Test
    void testTransportInterfaceUsesLinkedHashMap() throws IOException {
        Path requestDir = tempDir.resolve("request");
        Path responseDir = tempDir.resolve("response");
        Files.createDirectories(requestDir);
        Files.createDirectories(responseDir);

        Files.writeString(requestDir.resolve("REQ.cpy"), """
               01  REQ-REC.
                   05  FIELD-A             PIC X(10).
            """);
        Files.writeString(responseDir.resolve("RESP.cpy"), """
               01  RESP-REC.
                   05  FIELD-B             PIC X(10).
            """);

        GeneratorConfig config = GeneratorConfig.builder()
                .serviceName("LinkedHashMapTest")
                .requestCopybookDir(requestDir)
                .responseCopybookDir(responseDir)
                .outputDir(tempDir)
                .force(true)
                .build();

        ProjectGenerator generator = new ProjectGenerator(config);
        GeneratorResult result = generator.generate();

        assertThat(result.isSuccess()).isTrue();

        Path projectDir = tempDir.resolve("LinkedHashMapTest");
        Path transportInterface = projectDir.resolve("src/main/java/com/mainframe/mainframe/transport/MainframeTransport.java");

        assertThat(Files.exists(transportInterface)).isTrue();
        String content = Files.readString(transportInterface);

        // Verify the interface uses LinkedHashMap for request containers
        assertThat(content).contains("LinkedHashMap<String, byte[]> requestContainers");
        // Verify the return type is Map<String, byte[]>
        assertThat(content).contains("Map<String, byte[]> send");
    }
}
