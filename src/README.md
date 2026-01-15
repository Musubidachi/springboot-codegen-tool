# Copybook Spring Camel Generator

A code generator that creates complete, production-ready Spring Boot + Apache Camel projects for mainframe integration via raw TCP sockets based on COBOL copybooks.

## Features

- **Copybook Parsing**: Full support for COBOL copybook syntax including:
  - Nested groups (01/05/10... levels)
  - PIC clauses (X, 9, S9, V99, etc.)
  - USAGE types (DISPLAY, COMP, COMP-3, BINARY)
  - OCCURS arrays
  - 88-level condition names (generates enums)
  - REDEFINES (detection with warnings)
  - COPY directive resolution

- **Field Mapping**: Optional mapping document for:
  - Field renaming
  - Combining fields into higher-level types (e.g., date components → LocalDate)
  - Enum generation
  - Field ignoring

- **Jakarta Validation**: Automatic constraint generation:
  - @NotNull for required fields
  - @Size for string lengths and array limits
  - @Digits for numeric precision
  - @PositiveOrZero for unsigned numerics
  - @Valid for nested object cascading

- **Serialization**: Byte-accurate COBOL-compatible serialization:
  - EBCDIC encoding (CP037 default)
  - Big-endian binary numerics
  - Packed decimal (COMP-3)
  - Zoned decimal

- **TCP Transport**: Configurable mainframe communication:
  - Length-prefix framing (2 or 4 bytes)
  - Fixed-length framing
  - Configurable timeouts
  - Built-in TCP emulator for testing

## Quick Start

### 1. Build the Generator

```bash
cd copybook-spring-camel-gen
mvn clean package
```

### 2. Generate a Project

```bash
java -jar target/copybook-spring-camel-gen-1.0.0.jar generate \
  --project-name CustomerInquiry \
  --copybook-dir ./sample-copybooks \
  --mapping-doc ./sample-mapping/mapping.txt \
  --program-id CUSTINQ \
  --tcp-host localhost \
  --tcp-port 5000
```

### 3. Build and Run the Generated Project

```bash
cd CustomerInquiry
mvn clean test
mvn spring-boot:run
```

### 4. Test the API

```bash
curl -X POST http://localhost:8080/api/custinq/execute \
  -H "Content-Type: application/json" \
  -d @sample-request.json
```

## Command Line Options

| Option | Required | Default | Description |
|--------|----------|---------|-------------|
| `--project-name`, `-n` | Yes* | - | Name of the generated project |
| `--copybook-dir`, `-c` | Yes | - | Directory containing COBOL copybook files |
| `--mapping-doc`, `-m` | No | - | Path to field mapping document |
| `--external-copybook-dirs`, `-e` | No | - | Additional directories for COPY resolution |
| `--program-id`, `-p` | No | MAINFRAME-PROG | Mainframe program identifier |
| `--encoding` | No | cp037 | EBCDIC character encoding |
| `--tcp-host` | No | localhost | TCP host for mainframe connection |
| `--tcp-port` | No | 5000 | TCP port for mainframe connection |
| `--tcp-connect-timeout-ms` | No | 3000 | Connection timeout in milliseconds |
| `--tcp-read-timeout-ms` | No | 5000 | Read timeout in milliseconds |
| `--framing` | No | LENGTH_PREFIX_2 | TCP framing mode |
| `--output-dir`, `-o` | No | . | Output directory for generated project |
| `--force`, `-f` | No | false | Overwrite existing output directory |
| `--skip-tests` | No | false | Skip running tests after generation |

*Interactive prompt if not provided

## Mapping Document Format

The mapping document is a plain text file with the following syntax:

```text
# Comments start with #

# Rename a field
OLD-COBOL-NAME = newJavaName

# Combine multiple fields into one with a specific type
YEAR-FIELD + MONTH-FIELD + DAY-FIELD = birthDate:LocalDate

# Force enum generation
STATUS-CODE = StatusCode:enum

# Ignore a field
FILLER = IGNORE
```

### Mapping Rules

1. **Left side**: COBOL field names exactly as in the copybook
2. **Right side**: Java field name, optionally with `:Type`
3. **Combination**: Use `+` to combine multiple source fields
4. **Types**: LocalDate, String, Integer, Long, BigDecimal, or custom

## TCP Framing Modes

| Mode | Description |
|------|-------------|
| `LENGTH_PREFIX_2` | 2-byte big-endian length prefix before payload |
| `LENGTH_PREFIX_4` | 4-byte big-endian length prefix before payload |
| `FIXED` | Fixed-length messages (length from copybook) |

## Generated Project Structure

```
<project-name>/
├── pom.xml
├── sample-request.json
├── src/main/java/.../
│   ├── Application.java
│   ├── config/
│   │   └── FramingConfig.java
│   ├── controller/
│   │   ├── MainframeController.java
│   │   └── GlobalExceptionHandler.java
│   ├── camel/
│   │   └── MainframeRoute.java
│   ├── mainframe/
│   │   ├── transport/
│   │   │   ├── MainframeTransport.java
│   │   │   └── TcpMainframeTransport.java
│   │   ├── framing/
│   │   │   ├── FramingStrategy.java
│   │   │   ├── LengthPrefixFraming.java
│   │   │   └── FixedLengthFraming.java
│   │   └── emulator/
│   │       └── TcpEmulatorServer.java
│   ├── model/
│   │   ├── request/
│   │   │   └── <ProgramId>Request.java
│   │   └── response/
│   │       └── <ProgramId>Response.java
│   └── util/
│       ├── CobolSerializer.java
│       ├── EbcdicUtils.java
│       ├── <ProgramId>RequestSerializer.java
│       └── <ProgramId>ResponseSerializer.java
├── src/main/resources/
│   └── application.yml
└── src/test/java/.../
    ├── model/request/
    │   └── <ProgramId>RequestTest.java
    ├── serializer/
    │   └── <ProgramId>SerializerTest.java
    ├── tcp/
    │   └── TcpTransportTest.java
    └── controller/
        └── MainframeControllerTest.java
```

## Sample Copybooks

The `sample-copybooks` directory contains example COBOL copybooks demonstrating various features:

- **CUSTINQ-REQUEST.cpy**: Customer inquiry request with groups, OCCURS, 88-levels
- **CUSTINQ-RESPONSE.cpy**: Customer inquiry response with nested structures
- **DATESTR.cpy**: Common date structure for COPY testing

## Validation Constraints

The generator automatically produces Jakarta Bean Validation constraints:

| Copybook Construct | Generated Constraint |
|-------------------|---------------------|
| `PIC X(n)` | `@Size(max = n)` |
| `PIC 9(n)` | `@Digits(integer = n, fraction = 0)` |
| `PIC 9(n)V9(m)` | `@Digits(integer = n, fraction = m)` |
| Unsigned numeric | `@PositiveOrZero` |
| `OCCURS n TIMES` | `@Size(max = n)` |
| Required field | `@NotNull` |
| Nested group/list | `@Valid` |

## Error Handling

Invalid requests return HTTP 400 with detailed error information:

```json
{
  "status": "error",
  "message": "Validation failed",
  "errors": {
    "customerId": "customerId is required",
    "accountTypes": "accountTypes must not have more than 5 elements"
  }
}
```

## Testing

The generated project includes comprehensive tests:

1. **DTO Tests**: Lombok behavior, validation constraints
2. **Serializer Tests**: Round-trip serialization, byte length verification
3. **TCP Tests**: Framing strategies, transport behavior
4. **Controller Tests**: Integration tests with TCP emulator

Run all tests:
```bash
mvn test
```

## Requirements

- **Java**: 17+
- **Maven**: 3.8+
- **Spring Boot**: 3.2.x (generated projects)
- **Apache Camel**: 4.3.x (generated projects)

## Limitations

- REDEFINES: Detected and warned, but not fully supported
- Variable-length OCCURS (DEPENDING ON): Detected but treated as fixed
- Complex COPY with REPLACING: Not supported

## License

MIT License

## Contributing

Contributions welcome! Please submit issues and pull requests on GitHub.
