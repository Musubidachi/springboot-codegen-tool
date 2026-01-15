# Spring Boot + Apache Camel + Raw TCP Mainframe Copybook Tool (Codegen) — README.md

This README is for Claude to generate a complete, working codebase.

## Goal

Generate a **Spring Boot REST** project that:

1. Exposes a REST controller.
2. Invokes an **Apache Camel route**.
3. Marshals a request into a **byte array** matching the **COBOL copybook layout** (ordering, alignment, numeric formats).
4. Sends bytes to a **mainframe program via a raw TCP socket** (configurable host/port, request/reply).
5. Receives bytes back, unmarshals into a response DTO, and returns JSON.

Additionally, the generator tool must:

- Accept a folder containing **all COBOL COPYBOOKs** required.
- Resolve nested copybook dependencies (`COPY <name>`) across the folder.
- Detect when dependencies are missing and report whether they must be provided by another “container” (external copybook library).
- Accept an optional **mapping document** to rename fields and combine fields into richer types (ex: `LocalDate`).
- Generate a domain model that **adheres to copybook constraints** and includes **Jakarta Bean Validation** annotations (lengths, ranges, occurs limits, required fields, enum membership).
- Handle:
  - nested groups (`01/05/10...`)
  - `REDEFINES` (at least detect + warn; optional support)
  - `OCCURS` arrays (support)
  - **88-level fields** (generate enums)
  - date fields, compression fields, grouped-field combinations where appropriate
- Generate Java classes with:
  - typed fields (including enums / nested objects / lists)
  - a `byte[]` payload representation used for the mainframe call
- Handle **big-endian** and **ASCII↔EBCDIC** encoding consistently.

The final generated project must run with **no additional user intervention** beyond providing:
- project name
- copybook folder path
- optional mapping document path
- TCP connection config (host/port, timeouts, program-id if applicable)

---

## Assumptions / Constraints

- Target Java: **Java 17**
- Spring Boot: current stable compatible with Camel (use a BOM).
- Apache Camel: Camel Spring Boot starter.
- Build tool: Maven.
- Copybooks are plain text, one or more per file.
- Encoding: default mainframe encoding is **EBCDIC CP037** unless overridden.
- Byte order for binary numerics: **big-endian**.
- Mainframe transport: **raw TCP socket request/reply**.
- Generated REST API validates inbound DTOs using **Jakarta Validation** (`jakarta.validation`) and rejects invalid payloads with clear error responses.

---

## Inputs

### Required
- `--project-name <name>`
- `--copybook-dir <path>`

### Optional
- `--mapping-doc <path>` (format below)
- `--external-copybook-dirs <path1,path2,...>` (optional fallback containers)
- `--program-id <id>` (or include in application.yml)
- `--encoding <cp037|...>` default `cp037`
- `--tcp-host <host>` (default `localhost`)
- `--tcp-port <port>` (default `5000`)
- `--tcp-connect-timeout-ms <n>` (default `3000`)
- `--tcp-read-timeout-ms <n>` (default `5000`)
- `--force` overwrite output dir

---

## Mapping Document Format

Plain text lines:

- Rename:
  - `TMP-BUS-DAY = TemporaryBusinessDay`
- Combine:
  - `TMP-BUS-DT + TMP-BUS-DAY = TemporaryBusinessDate:LocalDate`
- Enum override (optional):
  - `STATUS-CODE = StatusCode:enum`
- Ignore field (optional):
  - `FILLER = IGNORE`
- Comments:
  - `# comment`

Rules:
- Left side uses **copybook field names** exactly as in the parsed model.
- Combination uses `+` and must reference leaf fields (or groups if explicitly supported).
- Target Java type optional; if omitted infer type.

---

## What the Generator Must Produce

### 1) A code generator CLI tool
Project: `copybook-spring-camel-gen` (this is the generator itself)

It generates a second project (output) like:
`<project-name>/`

### 2) Generated Spring Boot REST Project
Must include:
- `Controller` (REST)
- Request/Response DTOs derived from copybooks + mapping
- Jakarta Validation annotations based on copybook constraints
- Camel routes
- TCP transport client implementation
- Serialization/deserialization pipeline
- Unit + integration tests
- `application.yml`
- One-command run: `mvn spring-boot:run`

---

# High-Level Architecture

## Generator (CLI)
1. Read args (project name, dirs, mapping doc, tcp config defaults).
2. Load copybook files.
3. Parse copybooks into an AST/model.
4. Resolve `COPY` dependencies.
5. Build a semantic model (fields, groups, occurs, 88s).
6. Compute validation constraints from copybook metadata.
7. Apply mapping transformations:
   - renames
   - combinations (ex: LocalDate)
   - enum generation / overrides
8. Generate code:
   - Spring Boot project scaffolding
   - DTOs + validators + serializers
   - Camel routes + controller
   - TCP transport + emulator
   - tests + sample payloads
9. Validate output (compile + run tests automatically).
10. Print a final “success” summary and next commands.

---

# Step-by-Step Tasks (with Validation Requirements)

## STEP A — CLI + Project Scaffolding (Generator)

### A1. Create generator CLI
- Use `picocli` (preferred).
- Command: `generate`
- Prompts user for project name if missing.

**Validation**
- `mvn -q test` passes.
- `java -jar target/...jar generate --help` prints usage.
- Missing `--project-name` triggers an interactive prompt and continues.

### A2. Create output project directory
- If `<project-name>` exists, fail unless `--force`.

**Validation**
- Generator fails fast with clear message when output dir exists.
- With `--force`, dir is overwritten or cleaned safely.

---

## STEP B — Copybook Parsing

### B1. Copybook tokenizer + parser
Parse:
- Levels: `01, 05, 10, ...`
- Field name
- `PIC` clauses: `X(n)`, `9(n)`, `S9(n)`, `9(n)V9(m)`, etc.
- USAGE: `COMP`, `COMP-3`, `DISPLAY` (default), `BINARY` synonyms
- `OCCURS n TIMES`
- `REDEFINES` (detect)
- `COPY <name>` statements
- 88-level condition names

Output:
- `CopybookModel` containing nodes:
  - `GroupNode`
  - `FieldNode`
  - `Enum88Node`
  - `CopyDirectiveNode`

**Validation**
- Unit tests parsing representative copybooks.
- Parser preserves:
  - original field order
  - nesting hierarchy
  - picture and usage metadata

### B2. Dependency resolution (COPY)
- For each `COPY X`, locate a file matching `X`, `X.cpy`, `X.CPY`, etc. in:
  1) `--copybook-dir`
  2) `--external-copybook-dirs` (if provided)
- Build a dependency graph.
- Detect cycles.

If missing:
- classify as “external container required” and fail with:
  - missing name
  - referencing file
  - suggested external dir list

**Validation**
- Unit tests for:
  - local resolution
  - external resolution
  - missing copybook produces actionable error
  - cycle detection triggers clear failure message

---

## STEP C — Semantic Model + Type Inference + Constraints

### C1. Field typing
Infer Java type from PIC + USAGE:

Minimum support:
- `PIC X(n)` → `String`
- `PIC 9(n)` DISPLAY → `String` or numeric type (prefer numeric unless leading zeros are semantically important; if unsure, default numeric)
- `COMP`/`BINARY` → integer types by size
- `COMP-3` → `BigDecimal` for decimals or `Long` for integers (prefer BigDecimal when V is present)

Also compute metadata for serialization:
- length in bytes
- encoding (EBCDIC for DISPLAY text)
- numeric format (packed decimal, binary, zoned)
- signedness

**Validation**
- Unit tests for a matrix of PIC/USAGE combos.
- For each, assert inferred type + computed byte length.

### C2. Generate Jakarta Validation constraints (MANDATORY)
The generated domain model must include validation annotations that reflect copybook rules.

**Constraints to generate**
1. **String length**
   - For `PIC X(n)` or alphanumeric DISPLAY of fixed length:
     - `@Size(max = n)` and optionally `@Size(min = n)` if the field must be fixed-length *as provided by the client*.
     - Prefer: `@Size(max = n)` plus serialization pads/truncates per policy; but validation must reject if input exceeds n.
   - Also add `@Pattern` only if mapping doc specifies a pattern.

2. **Numeric digits / ranges**
   - For numeric fields:
     - `@Digits(integer = n, fraction = m)` for `9(n)V9(m)`
     - For integers `9(n)`:
       - `@Digits(integer = n, fraction = 0)`
     - If signed: allow negative; if unsigned: `@PositiveOrZero`
   - If a safe range can be inferred (fits in int/long), optionally add `@Min/@Max`; otherwise rely on `@Digits`.

3. **Required / Not null**
   - Default rule: **all leaf fields are required** unless mapping doc marks optional or field is known filler.
   - Generate `@NotNull` on non-primitive fields (String, BigDecimal, LocalDate, nested objects, list).
   - For Strings that must not be blank: use `@NotBlank` only if mapping doc requests it; otherwise `@NotNull`.

4. **OCCURS limit**
   - For `OCCURS n TIMES` arrays/lists:
     - `@Size(max = n)` (and `min = n` only if truly required to always supply all entries; default: max only)
   - Example: “definition only allows 5 entries” => list gets `@Size(max=5)`.

5. **88-level enums**
   - Generate enum types; field becomes enum.
   - Validate membership automatically by typing; if raw values are also exposed, add `@Pattern` or custom validator.

6. **Group / nested objects**
   - Add `@Valid` on nested objects and lists of nested objects so validation cascades.

**Validation**
- Generated project contains tests that:
  - call controller with invalid payload (too-long string, too-many occurs entries, too many digits)
  - asserts HTTP 400 and error body contains the violating field names
- Unit tests for constraint generation from copybook nodes.

### C3. Group handling
- Groups become nested classes by default.
- Maintain exact ordering for serialization.

**Validation**
- Ensure group expansion ordering matches COBOL layout.
- Unit test: serialized byte array matches expected order.

### C4. OCCURS arrays
- Generate `List<T>` (preferred) or `T[]`.
- Serialization repeats element layout N times.

**Validation**
- Unit test: array with N items produces correct byte length and correct concatenation.

### C5. 88-level fields (Enums)
Example:
- `05 STATUS-CODE PIC X.`
- `88 STATUS-OK VALUE 'Y'.`
- `88 STATUS-NO VALUE 'N'.`

Generate:
- `enum StatusCode { STATUS_OK("Y"), STATUS_NO("N") }`
- parsing method from raw value.

**Validation**
- Unit test: raw bytes decode to enum and enum encodes back to correct value.

### C6. Compression fields detection
- If `USAGE COMP-3` (packed) → treat as compressed numeric.
- If mapping doc marks field as “compressed” → honor it.
- If field name matches common patterns: `-CMP`, `-PACK`, `-PCK` → warn + treat as candidate.

**Validation**
- Test that COMP-3 generates packed decimal serializer.
- Test that ambiguous patterns produce warnings but still compile.

---

## STEP D — Mapping Transformations

### D1. Rename fields
Apply mapping: `TMP-BUS-DAY = TemporaryBusinessDay`
- Rename in DTO field name, getters/setters, JSON property name.
- Keep original COBOL name in annotation: `@CobolName("TMP-BUS-DAY")`

**Validation**
- Unit test: mapping applied, JSON uses renamed field.
- Ensure serializer still uses original COBOL field order.

### D2. Combine grouped fields into higher-level types
Example mapping:
- `TMP-BUS-DT + TMP-BUS-DAY = TemporaryBusinessDate:LocalDate`

Behavior:
- Generate derived property `LocalDate temporaryBusinessDate`.
- Keep underlying raw fields internal (not part of public JSON) unless configured.
- Serialization must:
  - split `LocalDate` into required components in the correct fields (or compose if mainframe expects combined date string/number)
- Deserialization must:
  - read raw fields → build `LocalDate`

Also add validation:
- `@NotNull` on the derived LocalDate field if required.
- Ensure derived field validation replaces the removed raw fields.

**Validation**
- Unit test: given LocalDate, outgoing bytes reflect correct field values.
- Unit test: given bytes, DTO has correct LocalDate.
- Controller test: missing LocalDate triggers HTTP 400.

### D3. Date field inference (automatic)
If mapping doc does not specify, infer date candidates by:
- Name heuristics: `-DT`, `-DATE`, `-YYYYMMDD`, `-YYMMDD`
- PIC patterns: 8-digit numeric for YYYYMMDD, etc.
- Group with components (YYYY, MM, DD)

When inferred:
- Prefer generating `LocalDate` if unambiguous, otherwise keep as String/Integer with a warning.

**Validation**
- Tests for:
  - explicit mapping always wins
  - heuristic inference produces LocalDate only when unambiguous
  - ambiguous inference produces warning + leaves raw type

---

## STEP E — Serialization / Deserialization (Byte Array)

### E1. Byte layout engine
Generate (or implement reusable):
- `CobolSerializer<T>`
- `CobolDeserializer<T>`
- A `Layout` model containing ordered `FieldLayout`s.

Rules:
- DISPLAY text uses EBCDIC encoding (cp037 by default).
- Binary numerics are big-endian.
- Packed decimals (COMP-3) follow standard nibble packing.
- Sign handling (S9) must be correct.
- Ensure exact total length matches expected mainframe commarea length.
- FILLER contributes bytes but is not exposed as a DTO field.

**Validation**
- Golden tests:
  - For a known DTO object, expected byte array matches a fixture.
  - For known byte array fixture, DTO matches expected values.
- Verify computed length equals:
  - sum of leaf field lengths * occurs multiplicity

### E2. Generated DTOs include `byte[] toBytes()` and `static fromBytes(byte[])`
Each request/response class must:
- expose typed fields
- implement serialization using the layout engine

**Validation**
- Unit tests call `toBytes()` and `fromBytes()` for every generated DTO.

---

## STEP F — Spring Boot REST + Camel + Raw TCP Socket

### F1. REST Controller
Generated controller:
- `POST /api/<program-id>/execute`
- Accepts JSON request DTO
- Annotate request with `@Valid`
- Returns JSON response DTO

Error handling:
- Provide a `@ControllerAdvice` that converts validation errors into a structured JSON response.

**Validation**
- `@SpringBootTest` + `MockMvc` test:
  - POST sample JSON
  - expects 200
  - validates response structure
- Another test:
  - POST invalid JSON (too many occurs entries / string too long)
  - expects 400
  - error response includes field and message

### F2. Camel Route
Route flow:
1. REST controller calls a service (or uses `ProducerTemplate`) to send to `direct:mainframeCall`.
2. Route:
   - marshal request DTO → `byte[]`
   - send to TCP endpoint (configurable)
   - receive `byte[]`
   - unmarshal → response DTO
3. Return response DTO to controller.

**Validation**
- Camel route test using `AdviceWith` to intercept TCP call and return fixture response bytes.
- Ensure route completes and response DTO matches expected.

### F3. Raw TCP socket transport (MANDATORY)
Implement:
- `TcpMainframeTransport implements MainframeTransport`
- Behavior:
  - open socket to host/port
  - write request bytes
  - read response bytes (must define framing)
  - close socket (or optionally pool, but default simple)

**Framing requirement (must be configurable)**
Support at least:
1. **Length-prefixed** (common):
   - write 2-byte or 4-byte big-endian length prefix, followed by payload
   - read prefix, then read exact payload bytes
2. **Fixed-length**:
   - response length is known from copybook-derived layout length
   - read exactly N bytes

Expose in `application.yml`:
- `mainframe.tcp.framing: LENGTH_PREFIX_2 | LENGTH_PREFIX_4 | FIXED`
- `mainframe.tcp.requestLengthPrefixBytes: 2|4` (if relevant)
- `mainframe.tcp.responseLength: <N>` (if fixed)

**Validation**
- Unit test for framing:
  - LENGTH_PREFIX_2 writes correct prefix and reads correct payload
  - FIXED reads exact N bytes
- Integration test using an in-process emulator TCP server.

### F4. Emulator TCP server (for tests/local)
Generated project must include a tiny TCP server used for tests:
- listens on random port
- reads request using configured framing
- returns a deterministic response fixture (or echoes request with modifications)

**Validation**
- `mvn -q test` passes without any external dependencies.
- `@SpringBootTest` can run using emulator enabled by profile `local` or `test`.

---

## STEP G — End-to-End Validation with Zero Manual Fixes

### G1. Generator self-validates output
After generating:
- Run `mvn -q test` inside the generated project.
- If tests fail, generator reports:
  - failing test name
  - probable cause
  - exits non-zero

**Validation**
- Generator CLI integration test:
  - generate project in temp dir
  - assert `mvn test` succeeds

### G2. Smoke run
Provide docs:
- `mvn spring-boot:run`
- `curl` sample request to endpoint
- response should be non-empty and valid JSON

**Validation**
- Integration test uses `TestRestTemplate` against random port.
- Verifies endpoint responds.

---

## STEP H — Auto-Generated Domain Unit Tests (JUnit 5 + Lombok REQUIRED)

This step defines **mandatory JUnit tests** that must be **auto-generated for every domain class** derived from COBOL COPYBOOKs.

The objective is to guarantee:
- correctness of Lombok-generated behavior
- enforcement of Jakarta Validation constraints
- correctness of 88-level enum handling
- correctness of serialization and deserialization
- correctness of derived and combined fields (e.g., `LocalDate`)

All generated domain tests must run with **no manual modification**.

---

## STEP H1 — Lombok Requirements (NON-NEGOTIABLE)

### H1.1 Lombok usage rules

All generated domain classes **MUST** use Lombok and **MUST NOT** contain handwritten boilerplate.

Required Lombok annotations:
- `@Data` **OR** (`@Getter`, `@Setter`, `@EqualsAndHashCode`, `@ToString`)
- `@NoArgsConstructor`
- `@AllArgsConstructor` (where applicable)
- `@Builder` (**MANDATORY** for test readability)

Manual implementation of the following is **PROHIBITED**:
- getters / setters
- `equals()`
- `hashCode()`
- `toString()`

### H1.2 Lombok dependency

The generated project **MUST** include Lombok configured as follows:

```xml
<dependency>
  <groupId>org.projectlombok</groupId>
  <artifactId>lombok</artifactId>
  <scope>provided</scope>
</dependency>
```

## STEP H2 — Domain Test Class Generation

For **every auto-generated domain class** (request and response), the generator **MUST** generate a corresponding JUnit 5 test class.

### H2.1 Naming and location rules

- Test classes MUST:
  - Mirror the domain package structure
  - Use the naming convention: `<DomainClassName>Test`
- Test classes MUST be placed under:
  - `src/test/java`

Example:

src/main/java/com/example/model/request/CustomerRequest.java
src/test/java/com/example/model/request/CustomerRequestTest.java


### H2.2 Test class scope

Each generated test class MUST validate:
- Lombok-generated behavior
- Jakarta Validation constraints
- Serialization and deserialization correctness
- Enum (88-level) behavior
- Derived / combined field behavior (if applicable)

---

## STEP H3 — Lombok Behavior Verification

Each domain test class MUST include tests that verify Lombok-generated functionality.

### H3.1 Required Lombok tests

The following tests are REQUIRED for every domain:

1. Builder creates equivalent objects
2. `equals()` and `hashCode()` are consistent
3. `toString()` executes without throwing exceptions

### H3.2 Lombok verification criteria

- All Lombok-generated fields MUST be exercised
- Equality MUST be based on field values
- No Lombok-generated method may be overridden manually

### H3.3 Validation

- Tests MUST compile without warnings
- Lombok annotations MUST be present on all domain classes
- Test execution MUST confirm Lombok correctness

---

## STEP H4 — Jakarta Validation Constraint Tests

Each domain test class MUST include tests that validate Jakarta Bean Validation constraints derived from the COPYBOOK.

### H4.1 Validator setup requirements

- Tests MUST use:
  - `jakarta.validation.Validator`
  - `Validation.buildDefaultValidatorFactory()`
- Spring context MUST NOT be used

### H4.2 Required validation scenarios

For each constraint generated from the copybook, a corresponding failing test MUST exist.

| Copybook Constraint | Required Test Condition |
|--------------------|-------------------------|
| `PIC X(n)` | String length > n fails |
| `PIC 9(n)` | Excess digits fail |
| `PIC 9(n)V9(m)` | Excess fractional digits fail |
| `OCCURS n TIMES` | Collection size > n fails |
| Required field | `null` value fails |
| Nested group | Child violation propagates |
| 88-level enum | Invalid value fails |

### H4.3 Validation assertions

Tests MUST assert:
- Violation count
- Violated property path
- Constraint message (where deterministic)

---

## STEP H5 — Enum (88-Level) Domain Tests

For every enum generated from 88-level COPYBOOK fields, the following tests are REQUIRED.

### H5.1 Enum mapping tests

- COBOL raw value maps to enum
- Enum maps back to COBOL raw value

### H5.2 Invalid enum handling

- Invalid raw value MUST:
  - throw a deterministic exception, OR
  - be rejected during validation

The chosen strategy MUST be consistent across all enums.

### H5.3 Validation

- All enum constants MUST be exercised
- Mapping logic MUST be reversible

---

## STEP H6 — Serialization and Deserialization Tests

Each domain test class MUST include serialization tests.

### H6.1 Required serialization tests

1. Domain object serializes to `byte[]`
2. Serialized bytes deserialize back into domain object
3. Deserialized object equals the original

### H6.2 Byte-level assertions

Tests MUST assert:
- Byte array length equals copybook-derived layout length
- Field ordering matches COPYBOOK definition
- Serialization is deterministic

---

## STEP H7 — Derived and Combined Field Tests

For domains containing derived or combined fields (e.g., `LocalDate`), the following tests are REQUIRED.

### H7.1 Required derived-field tests

1. Derived field serializes into correct raw fields
2. Raw bytes deserialize into correct derived field
3. Missing derived field fails validation

### H7.2 Encapsulation rules

- Raw component fields MUST NOT be publicly exposed
- Derived field MUST be the single public representation

---

## STEP H8 — Domain Test Rules and Constraints

All generated domain tests MUST:

- Use JUnit 5
- Avoid Mockito and Spring context
- Be deterministic and repeatable
- Use behavior-based test naming
- Avoid hard-coded magic values unless derived from COPYBOOK metadata

---

## STEP H9 — Generator Enforcement Rules

The generator MUST fail if:

- Any domain class lacks a corresponding test class
- Any Lombok annotation is missing
- Any Jakarta Validation constraint lacks a test
- Any serialization test fails
- Any enum mapping test fails

### H9.1 Enforcement mechanism

The generator MUST:
1. Execute `mvn -q test` on the generated project
2. Surface missing test classes or failures explicitly
3. Exit with non-zero status on failure

---

## Acceptance Criteria — Domain Testing

The generated project is NOT complete unless:

- Lombok is used for all domain boilerplate
- Every domain has:
  - Lombok behavior tests
  - Validation tests
  - Serialization tests
- All 88-level enums are fully tested
- All derived fields are validated and tested
- All tests pass with no user intervention

---

# Generated Project Layout (Required)

<project-name>/
pom.xml
src/main/java/.../
config/
controller/
camel/
mainframe/
transport/
framing/
emulator/
model/
request/
response/
layout/
service/
util/
src/test/java/.../
parser/
serializer/
tcp/
camel/
controller/
src/main/resources/
application.yml


---

# Non-Negotiable Quality Requirements

1. **No manual edits required** after generation.
2. Every step has tests.
3. Copybook parsing and serialization must preserve **exact byte ordering**.
4. Mapping doc overrides inference.
5. Missing copybooks produce **actionable errors**.
6. Must support:
   - nesting
   - OCCURS + max validation
   - 88 enums
   - COMP-3 packed decimal
   - EBCDIC encoding and big-endian
   - TCP socket request/reply with configurable framing
7. Generated DTOs must include **Jakarta Validation** constraints reflecting copybook restrictions.
8. Clear logging at INFO for milestones and DEBUG for byte-level traces (guarded; do not log raw sensitive payloads by default).

---

# Acceptance Criteria (Definition of Done)

A single command run produces a working project:

1) Build generator:
- `mvn -q clean test`

2) Generate project:
- `java -jar target/copybook-spring-camel-gen.jar generate --project-name DemoMainframeApi --copybook-dir ./copybooks --mapping-doc ./mapping.txt --tcp-host localhost --tcp-port 5000`

3) Build generated project:
- `cd DemoMainframeApi`
- `mvn -q clean test`

4) Run generated project:
- `mvn spring-boot:run`

5) Call endpoint:
- `curl -X POST http://localhost:8080/api/<program-id>/execute -H "Content-Type: application/json" -d @sample-request.json`

Expected:
- HTTP 200 for valid request
- HTTP 400 for invalid request (violating copybook constraints), with field-level errors
- JSON response matches schema
- Logs show route executed and serialization length matches expected

---

# Questions the Generator Must Ask the User (Interactive if missing)

1. Project name
2. Copybook directory path
3. Program ID (or default `MAINFRAME-PROG`)
4. Mapping document path (optional)
5. Encoding (default cp037)
6. TCP host/port + framing mode (default LENGTH_PREFIX_2 unless overridden)

Validation:
- interactive prompts must be skippable via args.

---

# Example Mapping (included in generated sample)

TMP-BUS-DAY = TemporaryBusinessDay
TMP-BUS-DT + TMP-BUS-DAY = TemporaryBusinessDate:LocalDate
STATUS-CODE = StatusCode:enum



---

# Output Requirements (Generator)
The generator must print:
- Absolute path to generated project
- Total copybooks parsed
- Total DTO classes generated
- Computed request/response byte lengths
- Detected validation constraints summary (counts of @Size/@Digits/@NotNull etc.)
- TCP framing mode and required response length (if FIXED)
- Commands to run tests and start the app
- A sample curl command and sample JSON payload file path

