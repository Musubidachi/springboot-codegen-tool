# Spring Boot Mainframe Code Generator

A code generator that creates complete, production-ready Spring Boot + Apache Camel projects for mainframe integration based on COBOL copybooks.

## Core Vision

- One generated project = **one mainframe service**
- Input: COBOL copybooks (separate request and response directories)
- Output: A Spring Boot service that:
  - Accepts Java DTOs
  - Serializes them into byte-accurate mainframe containers
  - Sends them in a deterministic order
  - Receives a `Map<String, byte[]>`
  - Deserializes into Java DTOs
- Encoding: **IBM037 (EBCDIC)** - locked, non-configurable
- Endianness: **Big Endian** - locked, non-configurable
- **Transport configuration is explicitly out of scope**

## Features

- **Copybook Parsing**: Full support for COBOL copybook syntax:
  - Nested groups (01/05/10... levels)
  - PIC clauses (X, 9, S9, V99, etc.)
  - USAGE types (DISPLAY, COMP, COMP-3, BINARY)
  - OCCURS arrays (fixed)
  - 88-level condition names (generates enums)
  - COPY directive resolution
  - VALUE clauses

- **Container Key Strategy**: Container keys derived from 01-level record names:
  - Example: `ABC-REQUEST-REC` → `ABC_REQUEST_REC`
  - Used consistently across serialization, transport, and deserialization

- **Jakarta Validation**: Automatic constraint generation:
  - @Size for string lengths
  - @Digits for numeric precision
  - @Valid for nested objects

- **Serialization**: Byte-accurate COBOL-compatible serialization:
  - IBM037 (EBCDIC) encoding
  - Big-endian binary numerics
  - Packed decimal (COMP-3)
  - Zoned decimal
  - Explicit padding/truncation

- **5-Step Camel Pipeline**:
  1. Validate request
  2. Assemble containers
  3. Invoke transport
  4. Validate response
  5. Deserialize response

## Quick Start

### 1. Build the Generator

```bash
mvn clean package
```

### 2. Generate a Project

```bash
java -jar target/springboot-codegen-tool-1.0.0.jar \
  --serviceName CustomerInquiry \
  --requestCopybookDir ./copybooks/request \
  --responseCopybookDir ./copybooks/response \
  --outputDir ./generated
```

### 3. Build and Run the Generated Project

```bash
cd generated/CustomerInquiry
mvn clean test
mvn spring-boot:run
```

### 4. Test the API

```bash
curl -X POST http://localhost:8080/mainframe/customerinquiry \
  -H "Content-Type: application/json" \
  -d @sample-request.json
```

Note: The default NoOp transport will return 501 Not Implemented until you provide your own transport implementation.

## Command Line Options

### Required Options

| Option | Description |
|--------|-------------|
| `--serviceName` | Name of the mainframe service (used for project name and endpoint) |
| `--outputDir` | Output directory for the generated project |
| `--requestCopybookDir` | Directory containing request COBOL copybooks |
| `--responseCopybookDir` | Directory containing response COBOL copybooks |

### Optional Flags

| Flag | Description |
|------|-------------|
| `--dryRun` | Perform full parsing and planning but write no files |
| `--verbose` | Enable DEBUG-level logging |
| `--force`, `-f` | Overwrite existing output directory |

### Locked Settings (Non-overridable)

- **Encoding**: IBM037 (EBCDIC)
- **Endianness**: Big Endian

## Generated Project Structure

```
<service-name>/
├── pom.xml
├── README.md
├── ARCHITECTURE.md
├── COBOL-SUPPORT.md
├── src/main/java/com/mainframe/
│   ├── <ServiceName>Application.java
│   ├── api/
│   │   ├── MainframeController.java
│   │   └── GlobalExceptionHandler.java
│   ├── camel/
│   │   ├── MainframeRoute.java
│   │   ├── RequestValidator.java
│   │   ├── ContainerAssembler.java
│   │   ├── TransportInvoker.java
│   │   ├── ResponseValidator.java
│   │   └── ResponseDeserializer.java
│   ├── mainframe/transport/
│   │   ├── MainframeTransport.java
│   │   └── NoOpMainframeTransport.java
│   ├── model/
│   │   ├── request/
│   │   │   ├── MainframeRequest.java
│   │   │   └── <Container>*.java
│   │   └── response/
│   │       ├── MainframeResponse.java
│   │       └── <Container>*.java
│   ├── serde/
│   │   ├── request/
│   │   │   └── *Serializer.java
│   │   └── response/
│   │       └── *Serializer.java
│   ├── runtime/
│   │   └── EbcdicUtils.java
│   └── config/
│       ├── ValidationConfig.java
│       └── CamelConfig.java
├── src/main/resources/
│   └── application.yml
└── src/test/java/com/mainframe/
    ├── api/
    │   └── MainframeControllerTest.java
    ├── camel/
    │   └── MainframeRouteTest.java
    └── serde/
        └── *SerializerTest.java
```

## Transport Layer

The transport layer is **intentionally not implemented**. The generated `NoOpMainframeTransport` throws `UnsupportedOperationException`.

### Transport Interface

```java
public interface MainframeTransport {
    Map<String, byte[]> send(LinkedHashMap<String, byte[]> requestContainers);
}
```

### Implementing Your Own Transport

Replace `NoOpMainframeTransport` with your organization's implementation:

```java
@Component
@Primary
public class MyMainframeTransport implements MainframeTransport {
    @Override
    public Map<String, byte[]> send(LinkedHashMap<String, byte[]> requestContainers) {
        // Your mainframe communication logic here
        // Container keys are derived from 01-level record names
    }
}
```

## API Contract

- **Endpoint**: `POST /mainframe/{serviceName}`
- **Request**: `MainframeRequest` containing all request container DTOs
- **Response**: `MainframeResponse` containing all response container DTOs

## COBOL Support

### Supported

| Feature | Notes |
|---------|-------|
| PIC X / 9 / S9 | Full support |
| V (decimal) | Implicit decimal |
| COMP / COMP-3 | Binary and packed decimal |
| OCCURS (fixed) | Fixed-length arrays |
| COPY (nested) | With directory resolution |
| 88-level enums | Generated as Java enums |
| VALUE clauses | Parsed |

### Unsupported (Will Fail Fast)

- OCCURS DEPENDING ON (variable length)
- REDEFINES with different layouts
- COMP-1 / COMP-2 (floating point)
- COPY REPLACING

## Code Quality

Generated code follows these rules:
- ≤5 methods per class
- ≤20 lines of code per method
- Javadoc on every public method
- No god classes

## Testing

```bash
# Run all tests
mvn test

# Run serializer tests only
mvn test -Dtest=*SerializerTest
```

## Requirements

- **Java**: 17+
- **Maven**: 3.8+
- **Spring Boot**: 3.2.x (generated projects)
- **Apache Camel**: 4.3.x (generated projects)

## License

MIT License
