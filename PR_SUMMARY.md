# Multi-Copybook CLI Enhancements - PR Summary

## Overview

This PR implements comprehensive multi-copybook support for the COBOL copybook → Spring Boot code generator, enabling:
- **One model per copybook**: Each copybook generates its own unique DTO class
- **Multi-copybook wrappers**: `{ProgramId}ApiRequest` and `{ProgramId}ApiResponse` classes that aggregate multiple copybook DTOs
- **Concatenation/splitting serializers**: Wrapper-level serializers that delegate to per-copybook serializers
- **Structural deduplication**: Automatic detection and sharing of structurally identical DTOs
- **Inheritance factoring**: Optional `extends` relationships for prefix/subset DTOs (when `--infer-inheritance` is set)
- **Improved collision handling**: Deterministic disambiguation with comprehensive logging

## Key Features Implemented

### 1. Multi-Copybook Wrapper Classes

**Problem**: Users previously only saw `{ProgramId}Request` and `{ProgramId}Response`, even when multiple copybooks were specified.

**Solution**:
- In folder-based mode with multiple copybooks, the generator now creates:
  - `{ProgramId}ApiRequest`: Container with one field per request copybook DTO
  - `{ProgramId}ApiResponse`: Container with one field per response copybook DTO
- Each individual copybook still generates its own DTO class
- REST controller and Camel routes automatically use wrapper classes when appropriate

**Files modified**:
- `ProjectGenerator.java`: Added `generateWrapperClasses()`, `generateApiRequestWrapper()`, `generateApiResponseWrapper()`
- Generated output packages:
  - `model/request/{ProgramId}ApiRequest.java` (when multiple request copybooks)
  - `model/response/{ProgramId}ApiResponse.java` (when multiple response copybooks)

### 2. Concatenation/Splitting Serializer Logic

**Problem**: No mechanism to serialize/deserialize multiple copybook containers as a single byte stream.

**Solution**:
- **Request serialization**: Concatenates bytes from each request copybook serializer in deterministic order (sorted by class name, with requestRoot first)
- **Response deserialization**: Splits response bytes by known lengths and deserializes each container DTO sequentially
- **Error handling**:
  - Short payloads: Deserialize what's available, log warning, leave missing containers null
  - Long payloads: Ignore trailing bytes with warning
- **No framing protocol**: Simple concatenation approach (not length-prefix protocol)

**Files modified**:
- `ProjectGenerator.java`: Added `generateWrapperSerializers()`, `generateApiRequestSerializer()`, `generateApiResponseSerializer()`
- Generated output:
  - `util/request/{ProgramId}ApiRequestSerializer.java`
  - `util/response/{ProgramId}ApiResponseSerializer.java`

**Example generated serializer logic**:
```java
// Request: concatenate in order
byte[] result = new byte[BYTE_LENGTH];
int offset = 0;
byte[] container1Bytes = container1Serializer.serialize(request.getContainer1());
System.arraycopy(container1Bytes, 0, result, offset, container1Bytes.length);
offset += container1Bytes.length;
// ... repeat for each container

// Response: split by known lengths
int offset = 0;
byte[] container1Bytes = Arrays.copyOfRange(bytes, offset, offset + CONTAINER1_LENGTH);
builder.container1(container1Serializer.deserialize(container1Bytes));
offset += CONTAINER1_LENGTH;
// ... repeat for each container
```

### 3. Structural Deduplication

**Problem**: Duplicate DTOs generated when request and response copybooks have identical structure.

**Solution**:
- New `StructuralSignatureCalculator` computes SHA-256 hash of normalized copybook structure
- Signature includes: field names, PIC/usage, OCCURS counts, ODO dependencies, REDEFINES, byte lengths/offsets
- When multiple copybooks share a signature:
  - Pick canonical DTO (prefer `shared` package, then alphabetically first)
  - Move canonical to `model/shared` package
  - Mark duplicates as deduped (skip generation, reference canonical)
  - Log: `"DEDUPED DTO: X and Y share signature abc123 -> using shared type Z"`

**Files added**:
- `StructuralSignatureCalculator.java`: Calculates structural signatures and detects subset relationships

**Files modified**:
- `ProjectGenerator.java`: Added `buildDtoMetadata()`, `signatureToModels` map, deduplication logic

### 4. Inheritance Factoring (--infer-inheritance)

**Problem**: No support for generating `extends` relationships when DTOs have prefix/subset structure.

**Solution**:
- When `--infer-inheritance` is set, the generator detects structural subset relationships
- If DTO A is a safe structural prefix of DTO B:
  - Generate `class B extends A`
  - B only contains additional fields (not duplicating A's fields)
  - Lombok annotations: `@EqualsAndHashCode(callSuper = true)`, `@ToString(callSuper = true)`
- Safety constraints:
  - Offsets must be compatible
  - No REDEFINES that change semantics
  - OCCURS counts must match for prefix fields

**Files modified**:
- `StructuralSignatureCalculator.java`: Added `isStructuralSubset()` method
- `ProjectGenerator.java`: Added `detectInheritance()`, `inheritanceMap`, `generateDtoClassWithMetadata()` with extends support

### 5. Improved Collision Handling

**Problem**: Class name collisions led to overwrites or unclear disambiguation.

**Solution**:
- Centralized collision handling in `buildDtoMetadata()`
- Deterministic disambiguation using source path hash
- Enhanced logging:
  - `"COLLISION: Class name 'Foo' already used. Copybook 'bar.cpy' will use 'Foo_42' (hash from path: /path/to/bar.cpy)"`
  - `"FILE WRITTEN: /path/to/generated/Foo.java"` for every DTO file

**Files added**:
- `DtoMetadata.java`: Tracks metadata for each DTO (class name, package, signature, dedup status, inheritance)

**Files modified**:
- `ProjectGenerator.java`: Added `dtoMetadataMap`, `determineClassName()`, comprehensive logging

### 6. Model Generation Refactor

**Problem**: Complex, redundant logic in `generateModelClasses()`.

**Solution**:
- Refactored to use `dtoMetadataMap` as single source of truth
- Simpler iteration: loop through metadata, skip deduped DTOs, generate with inheritance support
- Removed duplicate disambiguation logic (now in `determineClassName()`)

**Files modified**:
- `ProjectGenerator.java`: Completely refactored `generateModelClasses()`, added `generateDtoClassWithMetadata()`

### 7. Controller and Route Updates

**Problem**: Controller and Camel routes hardcoded to use `{ProgramId}Request`/`{ProgramId}Response`.

**Solution**:
- Automatically detect multi-copybook mode
- Use `{ProgramId}ApiRequest`/`{ProgramId}ApiResponse` when multiple copybooks present
- Use wrapper serializers in Camel routes

**Files modified**:
- `ProjectGenerator.java`: Updated `generateController()` and `generateCamelRoutes()` with conditional logic

## Usage

### Folder-Based Mode with Multiple Copybooks

```bash
java -jar generator.jar generate \
  --copybook-dir /path/to/copybooks \
  --request-copybook-dir /path/to/copybooks/request \
  --response-copybook-dir /path/to/copybooks/response \
  --request-root REQUEST-MAIN.cpy \
  --response-root RESPONSE-MAIN.cpy \
  --program-id ACCTINQ \
  --output-dir ./output
```

**Expected output**:
- `model/request/AcctinqRequest.java` (from REQUEST-MAIN.cpy)
- `model/request/CustomerInfo.java` (additional request copybook)
- `model/request/AccountDetails.java` (additional request copybook)
- `model/request/AcctinqApiRequest.java` (wrapper with 3 fields)
- `model/response/AcctinqResponse.java` (from RESPONSE-MAIN.cpy)
- `model/response/ErrorInfo.java` (additional response copybook)
- `model/response/AcctinqApiResponse.java` (wrapper with 2 fields)
- `util/request/AcctinqApiRequestSerializer.java` (concatenation logic)
- `util/response/AcctinqApiResponseSerializer.java` (splitting logic)

### With Deduplication

If `CustomerInfo.java` (request) and `ErrorInfo.java` (response) have identical structure:
- Only `model/shared/CustomerInfo.java` is generated
- Log: `"DEDUPED DTO: CustomerInfo and ErrorInfo share signature abc123 -> using shared type CustomerInfo"`
- Wrapper classes reference the shared DTO

### With Inheritance Factoring

```bash
java -jar generator.jar generate \
  --request-copybook-dir /path/to/request \
  --infer-inheritance \
  ...
```

If `BasicAccount` is a structural prefix of `ExtendedAccount`:
- `BasicAccount.java` generated with all fields
- `ExtendedAccount.java` generated as `class ExtendedAccount extends BasicAccount { ... }`
- Log: `"INHERITANCE: ExtendedAccount extends BasicAccount (structural prefix detected)"`

### Test Mode

```bash
java -jar generator.jar generate \
  --test-mode \
  --copybook-dir /path/to/all/copybooks \
  ...
```

**Behavior**:
- Generates DTO + serializer for **every** parsed copybook in `model/layout` and `util/layout`
- Generates torture tests for each DTO/serializer pair
- Tests include:
  - File existence checks
  - Serializer round-trip stability
  - Byte length validation
  - Multi-container concatenation/splitting test (for wrappers)

## Technical Details

### Concatenation/Splitting Order

**Request order** (deterministic):
- Request root (from `--request-root`) first, OR
- Stable sort by copybook class name

**Response order** (deterministic):
- Response root (from `--response-root`) first
- Then remaining sorted by class name

### Structural Signature Algorithm

```
For each node in copybook AST:
  GROUP: name + occursCount + odoDepend + redefinesTarget + offset + length
  FIELD: name + pic + usage + occursCount + odoDepend + redefinesTarget + offset + length + digits + scale + signed
  ENUM88: name + value

SHA-256 hash of normalized representation → 16-char hex signature
```

### Inheritance Detection Algorithm

```
For each pair of DTOs (A, B):
  If isStructuralSubset(A, B):
    If inheritance is safe (offsets compatible, no problematic REDEFINES):
      Generate: class B extends A
      Store: inheritanceMap[B] = A
```

## Breaking Changes

**None**. This PR is backward compatible:
- Heuristic mode (no folder-based selection) unchanged
- Single-copybook folder-based mode unchanged
- Existing CLI options unchanged
- Existing serializer correctness preserved (COMP-1/2, ODO, REDEFINES, packed decimal)

## Testing

### Unit Tests
- All existing tests pass (serializer correctness, ODO handling, REDEFINES, etc.)

### Integration Tests (Manual)
Recommended test scenarios:
1. **Single request + single response copybook**: No wrappers, existing behavior
2. **Multiple request copybooks**: ApiRequest wrapper generated, concatenation serializer
3. **Identical request/response copybooks**: Deduplication to `model/shared`
4. **Prefix/subset copybooks with --infer-inheritance**: Extends relationships generated
5. **Collision scenarios**: Two copybooks with same name in different dirs → disambiguated
6. **Test mode**: All copybooks generate DTOs + serializers in `layout` package

### Known Limitations

1. **No true framing protocol**: Simple concatenation/splitting by known lengths
   - Mainframes using length-prefix or delimiter protocols will need custom framing
2. **Inheritance only for exact structural prefixes**: Does not handle field reordering or partial overlaps
3. **Deduplication requires identical structure**: Minor PIC differences prevent dedup

## Files Changed

### Added
- `src/main/java/com/mainframe/generator/codegen/DtoMetadata.java` (134 lines)
- `src/main/java/com/mainframe/generator/codegen/StructuralSignatureCalculator.java` (187 lines)

### Modified
- `src/main/java/com/mainframe/generator/codegen/ProjectGenerator.java` (+800 lines, -150 lines)
  - Added fields: `dtoMetadataMap`, `signatureToModels`, `inheritanceMap`
  - Added methods: `buildDtoMetadata()`, `determineClassName()`, `detectInheritance()`, `generateWrapperClasses()`, `generateApiRequestWrapper()`, `generateApiResponseWrapper()`, `generateWrapperSerializers()`, `generateApiRequestSerializer()`, `generateApiResponseSerializer()`, `generateDtoClassWithMetadata()`, `getRequestDtosForWrapper()`, `getResponseDtosForWrapper()`, `capitalize()`
  - Refactored: `generateModelClasses()`, `generateController()`, `generateCamelRoutes()`
  - Removed: Old `disambiguateClassName()` (replaced by `determineClassName()`)

## Migration Guide

No migration required. Existing projects continue to work as before.

To adopt multi-copybook features:
1. Organize copybooks into directories (request/response/shared)
2. Use `--request-copybook-dir` and `--response-copybook-dir`
3. Optionally use `--infer-inheritance` for automatic extends relationships

## Future Enhancements

1. **Framing protocol support**: Add `--framing=LENGTH_PREFIX_4` for true protocol handling
2. **Enhanced test mode**: Generate multi-container integration tests with mocked mainframe responses
3. **Collision test fixtures**: Auto-generate test cases for collision scenarios
4. **Inheritance constraints validation**: Stricter safety checks for extends relationships
5. **Deduplication by normalized PIC**: Allow minor PIC variations (e.g., `PIC 9(5)` vs `PIC 99999`)

## Acknowledgments

- Maintained compatibility with existing serializer hardening (COMP-1/2, ODO, REDEFINES)
- No modifications to `safeWriteString` (as requested)
- Preserved all existing validation constraint generation
- Lombok recursion prevention remains in place (`@EqualsAndHashCode.Exclude` on parent/children)
