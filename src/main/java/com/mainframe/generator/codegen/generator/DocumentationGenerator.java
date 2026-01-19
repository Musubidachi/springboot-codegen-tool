package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import com.mainframe.generator.codegen.model.ContainerDefinition;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;

/**
 * Generates documentation for the generated project.
 *
 * Generated documentation includes:
 * - README.md
 * - ARCHITECTURE.md
 * - COBOL-SUPPORT.md
 */
public class DocumentationGenerator {

    private final GeneratorConfig config;

    /**
     * Creates a new DocumentationGenerator.
     *
     * @param config the generator configuration
     */
    public DocumentationGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates all documentation files.
     *
     * @param projectDir the project directory
     * @param requestContainers request container definitions
     * @param responseContainers response container definitions
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir, List<ContainerDefinition> requestContainers,
                        List<ContainerDefinition> responseContainers) throws IOException {
        generateReadme(projectDir, requestContainers, responseContainers);
        generateArchitectureDoc(projectDir);
        generateCobolSupportDoc(projectDir);
    }

    private void generateReadme(Path projectDir, List<ContainerDefinition> requestContainers,
                               List<ContainerDefinition> responseContainers) throws IOException {
        String serviceName = config.getServiceName();
        String endpoint = "/mainframe/" + serviceName.toLowerCase().replace("-", "");

        String requestContainerTable = requestContainers.stream()
                .map(c -> "| %s | %s | %d |".formatted(c.getContainerKey(), c.getClassName(), c.getByteLength()))
                .collect(Collectors.joining("\n"));

        String responseContainerTable = responseContainers.stream()
                .map(c -> "| %s | %s | %d |".formatted(c.getContainerKey(), c.getClassName(), c.getByteLength()))
                .collect(Collectors.joining("\n"));

        String content = """
                # %s

                Generated Spring Boot service for mainframe integration.

                ## Quick Start

                ```bash
                # Build and test
                mvn clean test

                # Run the application
                mvn spring-boot:run

                # Test the endpoint
                curl -X POST http://localhost:8080%s \\
                    -H "Content-Type: application/json" \\
                    -d '{"..."}'
                ```

                ## API Endpoint

                **POST %s**

                Invokes the mainframe service with the provided request containers.

                ## Request Containers

                | Container Key | DTO Class | Byte Length |
                |--------------|-----------|-------------|
                %s

                ## Response Containers

                | Container Key | DTO Class | Byte Length |
                |--------------|-----------|-------------|
                %s

                ## Transport Layer

                The transport layer is **not implemented** by default. The generated
                `NoOpMainframeTransport` throws `UnsupportedOperationException`.

                Replace it with your organization's actual transport implementation:

                ```java
                @Component
                @Primary
                public class MyMainframeTransport implements MainframeTransport {
                    @Override
                    public Map<String, byte[]> send(LinkedHashMap<String, byte[]> requestContainers) {
                        // Your implementation here
                    }
                }
                ```

                ## Encoding

                - Character encoding: **IBM037 (EBCDIC)**
                - Numeric endianness: **Big Endian**
                - All serializers handle explicit padding/truncation

                ## Package Structure

                ```
                com.mainframe/
                ├── api/                    # REST controller
                ├── model/
                │   ├── request/            # Request DTOs
                │   └── response/           # Response DTOs
                ├── serde/
                │   ├── request/            # Request serializers
                │   └── response/           # Response deserializers
                ├── camel/                  # Camel pipeline processors
                ├── mainframe/transport/    # Transport interface and NoOp impl
                ├── runtime/                # Utility classes (EbcdicUtils)
                └── config/                 # Spring configuration
                ```

                ## Testing

                ```bash
                # Run all tests
                mvn test

                # Run with verbose output
                mvn test -Dtest=*SerializerTest
                ```
                """.formatted(
                serviceName,
                endpoint, endpoint,
                requestContainerTable,
                responseContainerTable
        );

        Path file = projectDir.resolve("README.md");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateArchitectureDoc(Path projectDir) throws IOException {
        String content = """
                # Architecture

                ## Overview

                This service follows a clean pipeline architecture for mainframe communication.

                ## Request Flow

                ```
                HTTP Request → Controller → Camel Route → Transport → Mainframe
                                                ↓
                                        5-Step Pipeline:
                                        1. RequestValidator
                                        2. ContainerAssembler
                                        3. TransportInvoker
                                        4. ResponseValidator
                                        5. ResponseDeserializer
                ```

                ## Component Responsibilities

                ### API Layer
                - `MainframeController`: REST endpoint, request validation
                - `GlobalExceptionHandler`: Error response formatting

                ### Camel Pipeline
                - `RequestValidator`: Jakarta Bean Validation on input DTOs
                - `ContainerAssembler`: DTO → byte[] serialization
                - `TransportInvoker`: Sends to mainframe, receives response
                - `ResponseValidator`: Validates response containers present
                - `ResponseDeserializer`: byte[] → DTO deserialization

                ### Transport Layer
                - `MainframeTransport`: Interface with `send(LinkedHashMap)` method
                - `NoOpMainframeTransport`: Default stub that throws exception

                ### Serialization
                - Each container has a dedicated `*Serializer` class
                - Handles IBM037 encoding, COMP-3, binary, zoned decimal
                - Deterministic byte layout

                ## Container Key Strategy

                Container keys are derived from 01-level COBOL record names:
                - `ABC-REQUEST-REC` → `ABC_REQUEST_REC`
                - Used consistently across serialization, transport, and deserialization

                ## Error Handling

                | HTTP Status | Cause |
                |-------------|-------|
                | 400 | Validation failure |
                | 501 | NoOp transport (not implemented) |
                | 500 | Internal error |
                """;

        Path file = projectDir.resolve("ARCHITECTURE.md");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateCobolSupportDoc(Path projectDir) throws IOException {
        String content = """
                # COBOL Support

                ## Supported Features

                ### Data Types

                | COBOL | Java | Notes |
                |-------|------|-------|
                | PIC X(n) | String | EBCDIC encoded |
                | PIC 9(n) | Integer/Long | Zoned decimal |
                | PIC S9(n) | Integer/Long | Signed zoned decimal |
                | PIC 9(n)V9(m) | BigDecimal | Implicit decimal |
                | COMP / COMP-4 | Integer | Big Endian binary |
                | COMP-3 | BigDecimal | Packed decimal |

                ### Clauses

                | Clause | Support |
                |--------|---------|
                | PIC | Full |
                | USAGE | Full |
                | OCCURS (fixed) | Full |
                | VALUE | Parsed, not used |
                | 88-level | Enums generated |
                | REDEFINES | Warning only |
                | DEPENDING ON | Warning, treated as max |

                ### Structure

                | Feature | Support |
                |---------|---------|
                | Nested groups | Full |
                | COPY directives | Full |
                | Multiple 01-levels | Full |

                ## Unsupported Features

                The generator will **fail fast** and report clear errors for:

                - OCCURS DEPENDING ON (variable length)
                - REDEFINES with different layouts
                - COMP-1 / COMP-2 (floating point)
                - COPY REPLACING
                - INDEXED BY

                ## Encoding Details

                - Charset: IBM037 (EBCDIC)
                - Numeric endianness: Big Endian
                - Space padding: EBCDIC 0x40
                - Zero padding: EBCDIC 0xF0
                """;

        Path file = projectDir.resolve("COBOL-SUPPORT.md");
        FileWriteUtil.safeWriteString(file, content);
    }
}
