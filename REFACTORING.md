# Code Refactoring Progress

## Goals
- Remove unused methods
- Maximum 5 methods per class
- Maximum 20 lines per method

## Current Status

### Completed

#### 1. Utility Enhancements
- ✅ Made `NamingUtil.capitalize()` public for reuse
- ✅ Already have `FileWriteUtil.safeWriteString()` and `deleteDirectory()`

#### 2. Extracted Classes

**PomGenerator** (`codegen/maven/PomGenerator.java`)
- Responsibility: Generate Maven POM files
- Methods: 2 (constructor, generate)
- Extracted from: ProjectGenerator.generatePom() and generatePomContent()
- Status: ✅ Complete

**ApplicationConfigGenerator** (`codegen/config/ApplicationConfigGenerator.java`)
- Responsibility: Generate Spring Boot configuration YAML files
- Methods: 4 (constructor, generate, 2 private helpers)
- Extracted from: ProjectGenerator.generateApplicationYml()
- Status: ✅ Complete

### In Progress

#### ProjectGenerator Refactoring
- Original: 84 methods, 3,664 lines
- Target: Extract into 15+ specialized generator classes
- Current: 2 generators extracted (~180 lines moved)

### Remaining Work

#### High Priority - Additional Generators to Extract from ProjectGenerator

1. **MainApplicationGenerator** (5-10 methods)
   - generateMainApplication()
   - Easy win, ~26 lines

2. **SampleDataGenerator** (3-5 methods)
   - generateSampleFiles()
   - generateSampleValue()
   - ~45 lines total

3. **TestGenerator** (5 methods max per class, split if needed)
   - generateTests()
   - generateTortureTests()
   - generateTortureTest()
   - determineSubPackageForModel()
   - ~330 lines (may need to split into TestGenerator + TortureTestGenerator)

4. **SerializationCodeGenerator** (needs sub-classes)
   - Generate serialization utility classes
   - ~850 lines across 14 methods
   - Needs: SerializerFieldWriter, EbcdicUtilityGenerator sub-classes

5. **CamelRouteGenerator** (3-5 methods)
   - generateCamelRoutes()
   - ~110 lines

6. **ControllerGenerator** (3-5 methods)
   - generateController()
   - ~125 lines

7. **TransportGenerator** (5 methods)
   - generateTcpTransport()
   - generateFramingClasses()
   - generateTcpEmulator()
   - ~280 lines

8. **CopybookClassifier** (5 methods)
   - identifyRequestResponse()
   - identifyRequestResponseFromFolders()
   - identifyRequestResponseHeuristic()
   - findCopybookByNameOrFile()
   - selectFirstDeterministic()
   - ~170 lines

9. **ModelClassOrchestrator** (5 methods)
   - Orchestrate DTO and enum generation
   - ~200 lines

10. **WrapperGenerator Enhancement**
    - Move wrapper generation methods from ProjectGenerator
    - generateWrapperClasses()
    - getRequestDtosForWrapper()
    - getResponseDtosForWrapper()
    - generateApiRequestWrapper()
    - generateApiResponseWrapper()

#### Medium Priority - Other Classes

**CopybookParser** (21 methods)
- Target: Max 5 methods
- Strategy: Extract DataItemParser and OffsetCalculator
- Problem methods:
  - parseDataItem() - 194 lines (needs breaking into 8+ methods)
  - calculateOffsets() - 93 lines (needs extraction)
  - parsePictureClause() - 39 lines (borderline)

**CopybookResolver** (12 methods)
- Assessment: Well-designed, methods appropriately sized
- Recommendation: Leave as-is or minimal changes

**CopybookTokenizer** (8 methods)
- Target: Max 5 methods
- Strategy: Extract preprocessing and token reading logic
- Problem methods:
  - preprocessSource() - 62 lines
  - nextToken() - 48 lines
  - readStringLiteral() - 30 lines
  - readIdentifierOrKeyword() - 30 lines

**ValidationConstraintGenerator** (8 methods)
- Target: Max 5 methods
- Strategy: Break down generateConstraints() into constraint-specific methods
- Problem method:
  - generateConstraints() - 97 lines (needs breaking into 5+ methods)

#### Low Priority

**Other Classes** (already within limits)
- GroupNode (6 methods) - OK
- FieldNode (8 methods) - OK
- PictureClause (6 methods) - OK
- DtoClassGenerator (6 methods) - OK
- DtoFieldGenerator (8 methods) - OK
- StructuralSignatureCalculator (7 methods) - OK

### Removed Duplicates

Need to remove these duplicate methods from ProjectGenerator and use utilities instead:
- ❌ `toPascalCase()` → Use `NamingUtil.toPascalCase()`
- ❌ `toCamelCase()` → Use `NamingUtil.toCamelCase()`
- ❌ `capitalize()` → Use `NamingUtil.capitalize()`
- ❌ `safeWriteString()` → Use `FileWriteUtil.safeWriteString()`
- ❌ `deleteDirectory()` → Use `FileWriteUtil.deleteDirectory()`
- ❌ `disambiguateClassName()` → Use `NamingUtil.disambiguateClassName()`

### Testing Strategy

After each extraction:
1. Update ProjectGenerator to use new class
2. Run integration tests
3. Verify generated output matches baseline
4. Fix any issues before proceeding

## Estimated Effort

- **Completed**: ~10% (2 of 15+ classes extracted)
- **Remaining**: 4-6 weeks for full refactoring
  - Week 1-2: Extract remaining generators
  - Week 3-4: Refactor parser classes
  - Week 5: Refactor other classes
  - Week 6: Testing and documentation

## Next Steps

1. Update ProjectGenerator to use PomGenerator and ApplicationConfigGenerator
2. Extract MainApplicationGenerator and SampleDataGenerator (easy wins)
3. Run tests to verify no regressions
4. Continue with remaining extractions

## Notes

- This refactoring maintains backward compatibility
- All existing tests should continue to pass
- Generated code output should remain identical
- Focus on single responsibility principle for each new class
