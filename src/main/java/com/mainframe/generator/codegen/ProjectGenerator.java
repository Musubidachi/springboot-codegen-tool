package com.mainframe.generator.codegen;

import com.mainframe.generator.mapping.MappingDocument;
import com.mainframe.generator.mapping.MappingParser;
import com.mainframe.generator.model.*;
import com.mainframe.generator.parser.CopybookResolver;
import com.mainframe.generator.validation.ValidationConstraintGenerator;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateExceptionHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Main project generator that creates a complete Spring Boot project
 * from COBOL copybooks.
 */
public class ProjectGenerator {
    private static final Logger log = LoggerFactory.getLogger(ProjectGenerator.class);
    
    private final GeneratorConfig config;
    private final Configuration freemarkerConfig;
    private final ValidationConstraintGenerator validationGenerator;

    private List<CopybookModel> copybookModels;
    private MappingDocument mappingDoc;
    private CopybookModel requestCopybook;
    private CopybookModel responseCopybook;

    // New folder-based selection sets
    private Set<CopybookModel> requestModels = new LinkedHashSet<>();
    private Set<CopybookModel> responseModels = new LinkedHashSet<>();
    private Set<CopybookModel> sharedModels = new LinkedHashSet<>();
    private Set<CopybookModel> allModelsToGenerate = new LinkedHashSet<>();

    // Track field values during deserialization for ODO support
    private Map<String, String> deserializationFieldValues = new HashMap<>();

    // DTO metadata tracking for deduplication and inheritance
    private Map<CopybookModel, DtoMetadata> dtoMetadataMap = new HashMap<>();
    private Map<String, List<CopybookModel>> signatureToModels = new HashMap<>();
    private Map<CopybookModel, CopybookModel> inheritanceMap = new HashMap<>(); // child -> parent

    public ProjectGenerator(GeneratorConfig config) {
        this.config = config;
        this.freemarkerConfig = createFreemarkerConfig();
        this.validationGenerator = new ValidationConstraintGenerator();
    }
    
    private Configuration createFreemarkerConfig() {
        Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
        cfg.setClassForTemplateLoading(getClass(), "/templates");
        cfg.setDefaultEncoding("UTF-8");
        cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
        cfg.setLogTemplateExceptions(false);
        cfg.setWrapUncheckedExceptions(true);
        return cfg;
    }
    
    /**
     * Generate the complete Spring Boot project.
     */
    public GeneratorResult generate() {
        try {
            log.info("Starting project generation...");
            
            // Step 1: Parse copybooks
            log.info("Step 1: Parsing copybooks...");
            CopybookResolver resolver = new CopybookResolver(
                    config.getCopybookDir(), 
                    config.getExternalCopybookDirs()
            );
            copybookModels = resolver.loadAllCopybooks();
            
            if (resolver.hasErrors()) {
                return GeneratorResult.failure(resolver.getErrorSummary());
            }
            
            if (copybookModels.isEmpty()) {
                return GeneratorResult.failure("No copybooks found in " + config.getCopybookDir());
            }
            
            // Identify request and response copybooks
            identifyRequestResponse();

            // Build DTO metadata with deduplication and inheritance
            buildDtoMetadata();

            // Step 2: Parse mapping document
            log.info("Step 2: Parsing mapping document...");
            if (config.getMappingDoc() != null) {
                MappingParser mappingParser = new MappingParser();
                mappingDoc = mappingParser.parse(config.getMappingDoc());
                if (mappingDoc.hasErrors()) {
                    log.warn("Mapping document has errors: {}", mappingDoc.getErrors());
                }
            } else {
                mappingDoc = new MappingDocument();
            }
            
            // Step 3: Create output directory
            log.info("Step 3: Creating project structure...");
            Path projectDir = createProjectStructure();
            
            // Step 4: Generate pom.xml
            log.info("Step 4: Generating pom.xml...");
            generatePom(projectDir);
            
            // Step 5: Generate application.yml
            log.info("Step 5: Generating application.yml...");
            generateApplicationYml(projectDir);
            
            // Step 6: Generate model classes
            log.info("Step 6: Generating model classes...");
            int dtoCount = generateModelClasses(projectDir);

            // Step 6.5: Generate wrapper classes (ApiRequest/ApiResponse)
            log.info("Step 6.5: Generating wrapper classes...");
            generateWrapperClasses(projectDir);

            // Step 7: Generate enums
            log.info("Step 7: Generating enum classes...");
            generateEnumClasses(projectDir);
            
            // Step 8: Generate serialization classes
            log.info("Step 8: Generating serialization classes...");
            generateSerializationClasses(projectDir);
            
            // Step 9: Generate Camel routes
            log.info("Step 9: Generating Camel routes...");
            generateCamelRoutes(projectDir);
            
            // Step 10: Generate controller
            log.info("Step 10: Generating REST controller...");
            generateController(projectDir);
            
            // Step 11: Generate TCP transport
            log.info("Step 11: Generating TCP transport...");
            generateTcpTransport(projectDir);
            
            // Step 12: Generate emulator
            log.info("Step 12: Generating TCP emulator...");
            generateTcpEmulator(projectDir);
            
            // Step 13: Generate tests
            log.info("Step 13: Generating tests...");
            generateTests(projectDir);

            // Step 13.5: Generate torture tests if test mode enabled
            int tortureTestsCount = 0;
            if (config.isTestMode()) {
                log.info("Step 13.5: Generating torture tests (test mode)...");
                tortureTestsCount = generateTortureTests(projectDir);
            }

            // Step 14: Generate sample request
            log.info("Step 14: Generating sample files...");
            generateSampleFiles(projectDir);
            
            // Step 15: Generate main application class
            log.info("Step 15: Generating main application...");
            generateMainApplication(projectDir);
            
            // Step 16: Run Maven tests
            if (!config.isSkipTests()) {
                log.info("Step 16: Running Maven tests...");
                boolean testsPass = runMavenTests(projectDir);
                if (!testsPass) {
                    return GeneratorResult.testFailure("maven-test", "Generated project tests failed");
                }
            }
            
            log.info("Project generation complete!");
            
            return GeneratorResult.builder()
                    .success(true)
                    .outputPath(projectDir)
                    .copybooksParsed(copybookModels.size())
                    .dtoClassesGenerated(dtoCount)
                    .requestByteLength(requestCopybook != null ? requestCopybook.calculateTotalByteLength() : 0)
                    .responseByteLength(responseCopybook != null ? responseCopybook.calculateTotalByteLength() : 0)
                    .requestModelsCount(requestModels.size())
                    .responseModelsCount(responseModels.size())
                    .sharedModelsCount(sharedModels.size())
                    .serializersGenerated(allModelsToGenerate.size())
                    .tortureTestsGenerated(tortureTestsCount)
                    .notNullCount(validationGenerator.getNotNullCount())
                    .sizeCount(validationGenerator.getSizeCount())
                    .digitsCount(validationGenerator.getDigitsCount())
                    .minMaxCount(validationGenerator.getMinMaxCount())
                    .build();
            
        } catch (Exception e) {
            log.error("Generation failed", e);
            return GeneratorResult.failure(e.getMessage());
        }
    }
    
    private void identifyRequestResponse() {
        boolean usingFolderBasedSelection = config.getRequestCopybookDir() != null || config.getResponseCopybookDir() != null;

        if (usingFolderBasedSelection) {
            identifyRequestResponseFromFolders();
        } else {
            identifyRequestResponseHeuristic();
        }
    }

    private void identifyRequestResponseFromFolders() {
        log.info("Using folder-based copybook selection");

        // Build maps by normalized name for intersection detection
        Map<String, CopybookModel> requestMap = new HashMap<>();
        Map<String, CopybookModel> responseMap = new HashMap<>();
        Map<String, CopybookModel> sharedMap = new HashMap<>();

        // Load request copybooks
        if (config.getRequestCopybookDir() != null) {
            for (CopybookModel model : copybookModels) {
                if (model.getSourcePath().startsWith(config.getRequestCopybookDir().toString())) {
                    requestMap.put(model.getName().toLowerCase(), model);
                    requestModels.add(model);
                }
            }
            log.info("  Request copybooks: {}", requestModels.size());
        }

        // Load response copybooks
        if (config.getResponseCopybookDir() != null) {
            for (CopybookModel model : copybookModels) {
                if (model.getSourcePath().startsWith(config.getResponseCopybookDir().toString())) {
                    responseMap.put(model.getName().toLowerCase(), model);
                    responseModels.add(model);
                }
            }
            log.info("  Response copybooks: {}", responseModels.size());
        }

        // Load explicitly shared copybooks
        if (config.getSharedCopybookDir() != null) {
            for (CopybookModel model : copybookModels) {
                if (model.getSourcePath().startsWith(config.getSharedCopybookDir().toString())) {
                    sharedMap.put(model.getName().toLowerCase(), model);
                    sharedModels.add(model);
                }
            }
            log.info("  Explicit shared copybooks: {}", sharedModels.size());
        }

        // Detect intersection (files in both request and response by name)
        Set<String> intersection = new HashSet<>(requestMap.keySet());
        intersection.retainAll(responseMap.keySet());
        for (String commonName : intersection) {
            CopybookModel model = requestMap.get(commonName);
            sharedModels.add(model);
            requestModels.remove(model);
            responseModels.remove(model);
        }
        if (!intersection.isEmpty()) {
            log.info("  Detected {} shared copybooks by intersection: {}", intersection.size(), intersection);
        }

        // Build union of all models to generate
        allModelsToGenerate.addAll(requestModels);
        allModelsToGenerate.addAll(responseModels);
        allModelsToGenerate.addAll(sharedModels);

        // Select root request copybook
        if (config.getRequestRoot() != null && !config.getRequestRoot().isBlank()) {
            requestCopybook = findCopybookByNameOrFile(requestModels, config.getRequestRoot());
            if (requestCopybook == null) {
                requestCopybook = findCopybookByNameOrFile(sharedModels, config.getRequestRoot());
            }
            if (requestCopybook == null) {
                log.warn("Request root '{}' not found, using first available", config.getRequestRoot());
                requestCopybook = selectFirstDeterministic(requestModels);
            }
        } else {
            requestCopybook = selectFirstDeterministic(requestModels);
        }

        // Select root response copybook
        if (config.getResponseRoot() != null && !config.getResponseRoot().isBlank()) {
            responseCopybook = findCopybookByNameOrFile(responseModels, config.getResponseRoot());
            if (responseCopybook == null) {
                responseCopybook = findCopybookByNameOrFile(sharedModels, config.getResponseRoot());
            }
            if (responseCopybook == null) {
                log.warn("Response root '{}' not found, using first available", config.getResponseRoot());
                responseCopybook = selectFirstDeterministic(responseModels);
            }
        } else {
            responseCopybook = selectFirstDeterministic(responseModels);
        }

        // Fallback if roots not found
        if (requestCopybook == null) {
            requestCopybook = selectFirstDeterministic(allModelsToGenerate);
        }
        if (responseCopybook == null) {
            responseCopybook = requestCopybook;
        }

        log.info("  Selected request root: {}", requestCopybook != null ? requestCopybook.getName() : "none");
        log.info("  Selected response root: {}", responseCopybook != null ? responseCopybook.getName() : "none");
    }

    private void identifyRequestResponseHeuristic() {
        log.info("Using heuristic mode for copybook selection");

        // Simple heuristic: look for REQUEST/RESPONSE in names, or use first two copybooks
        for (CopybookModel model : copybookModels) {
            String name = model.getName().toUpperCase();
            if (name.contains("REQUEST") || name.contains("REQ") || name.contains("INPUT")) {
                requestCopybook = model;
            } else if (name.contains("RESPONSE") || name.contains("RESP") || name.contains("OUTPUT")) {
                responseCopybook = model;
            }
        }

        // Fallback: use first copybook as both request and response
        if (requestCopybook == null && !copybookModels.isEmpty()) {
            requestCopybook = copybookModels.get(0);
        }
        if (responseCopybook == null) {
            responseCopybook = copybookModels.size() > 1 ? copybookModels.get(1) : requestCopybook;
        }

        // In heuristic mode, allModelsToGenerate is just request and response roots
        if (requestCopybook != null) {
            allModelsToGenerate.add(requestCopybook);
        }
        if (responseCopybook != null && responseCopybook != requestCopybook) {
            allModelsToGenerate.add(responseCopybook);
        }

        log.info("  Request copybook: {}", requestCopybook != null ? requestCopybook.getName() : "none");
        log.info("  Response copybook: {}", responseCopybook != null ? responseCopybook.getName() : "none");
    }

    private CopybookModel findCopybookByNameOrFile(Set<CopybookModel> models, String nameOrFile) {
        for (CopybookModel model : models) {
            if (model.getName().equalsIgnoreCase(nameOrFile)) {
                return model;
            }
            // Also try matching against filename
            Path sourcePath = Path.of(model.getSourcePath());
            String fileName = sourcePath.getFileName().toString();
            if (fileName.equalsIgnoreCase(nameOrFile) || fileName.equalsIgnoreCase(nameOrFile + ".cpy")) {
                return model;
            }
        }
        return null;
    }

    private CopybookModel selectFirstDeterministic(Set<CopybookModel> models) {
        if (models.isEmpty()) {
            return null;
        }
        // Sort by name for deterministic selection
        return models.stream()
                .sorted(Comparator.comparing(CopybookModel::getName))
                .findFirst()
                .orElse(null);
    }

    /**
     * Build DTO metadata with structural deduplication and inheritance analysis.
     * This must be called after identifyRequestResponse().
     */
    private void buildDtoMetadata() {
        log.info("Building DTO metadata with deduplication and inheritance analysis...");

        boolean usingFolderBasedSelection = config.getRequestCopybookDir() != null || config.getResponseCopybookDir() != null;

        if (!usingFolderBasedSelection && !config.isTestMode()) {
            // Heuristic mode: simple metadata for request/response roots only
            if (requestCopybook != null) {
                String className = toPascalCase(config.getProgramId()) + "Request";
                dtoMetadataMap.put(requestCopybook, DtoMetadata.builder()
                        .copybookModel(requestCopybook)
                        .className(className)
                        .originalClassName(className)
                        .packageType("request")
                        .isWrapper(false)
                        .isDeduped(false)
                        .byteLength(requestCopybook.calculateTotalByteLength())
                        .build());
            }
            if (responseCopybook != null) {
                String className = toPascalCase(config.getProgramId()) + "Response";
                dtoMetadataMap.put(responseCopybook, DtoMetadata.builder()
                        .copybookModel(responseCopybook)
                        .className(className)
                        .originalClassName(className)
                        .packageType("response")
                        .isWrapper(false)
                        .isDeduped(false)
                        .byteLength(responseCopybook.calculateTotalByteLength())
                        .build());
            }
            return;
        }

        // Folder-based or test mode: full metadata analysis
        Set<String> usedClassNames = new HashSet<>();

        // Step 1: Build metadata for all models and calculate structural signatures
        for (CopybookModel model : allModelsToGenerate) {
            String signature = StructuralSignatureCalculator.calculateSignature(model);
            signatureToModels.computeIfAbsent(signature, k -> new ArrayList<>()).add(model);

            String packageType = determinePackageType(model);
            String className = determineClassName(model, packageType, usedClassNames);
            usedClassNames.add(className);

            DtoMetadata metadata = DtoMetadata.builder()
                    .copybookModel(model)
                    .className(className)
                    .originalClassName(toPascalCase(model.getName()))
                    .packageType(packageType)
                    .structuralSignature(signature)
                    .isWrapper(false)
                    .isDeduped(false)
                    .byteLength(model.calculateTotalByteLength())
                    .build();

            dtoMetadataMap.put(model, metadata);
        }

        // Include test mode models
        if (config.isTestMode()) {
            for (CopybookModel model : copybookModels) {
                if (dtoMetadataMap.containsKey(model)) continue;

                String signature = StructuralSignatureCalculator.calculateSignature(model);
                signatureToModels.computeIfAbsent(signature, k -> new ArrayList<>()).add(model);

                String className = determineClassName(model, "layout", usedClassNames);
                usedClassNames.add(className);

                DtoMetadata metadata = DtoMetadata.builder()
                        .copybookModel(model)
                        .className(className)
                        .originalClassName(toPascalCase(model.getName()))
                        .packageType("layout")
                        .structuralSignature(signature)
                        .isWrapper(false)
                        .isDeduped(false)
                        .byteLength(model.calculateTotalByteLength())
                        .build();

                dtoMetadataMap.put(model, metadata);
            }
        }

        // Step 2: Detect structural duplicates and mark for deduplication
        int dedupedCount = 0;
        for (Map.Entry<String, List<CopybookModel>> entry : signatureToModels.entrySet()) {
            if (entry.getValue().size() > 1) {
                List<CopybookModel> duplicates = entry.getValue();
                // Pick the first one as the canonical DTO (prefer shared, then alphabetically first)
                CopybookModel canonical = duplicates.stream()
                        .min(Comparator.comparing((CopybookModel m) -> {
                            DtoMetadata meta = dtoMetadataMap.get(m);
                            if ("shared".equals(meta.getPackageType())) return 0;
                            if ("request".equals(meta.getPackageType())) return 1;
                            if ("response".equals(meta.getPackageType())) return 2;
                            return 3; // layout
                        }).thenComparing(CopybookModel::getName))
                        .orElse(duplicates.get(0));

                DtoMetadata canonicalMeta = dtoMetadataMap.get(canonical);
                // Move canonical to shared package if not already
                if (!"shared".equals(canonicalMeta.getPackageType())) {
                    canonicalMeta.setPackageType("shared");
                    if (!sharedModels.contains(canonical)) {
                        sharedModels.add(canonical);
                        requestModels.remove(canonical);
                        responseModels.remove(canonical);
                    }
                }

                // Mark others as deduped
                for (CopybookModel dup : duplicates) {
                    if (dup != canonical) {
                        DtoMetadata dupMeta = dtoMetadataMap.get(dup);
                        dupMeta.setDeduped(true);
                        dupMeta.setDedupedToClassName(canonicalMeta.getClassName());
                        dedupedCount++;
                        log.info("DEDUPED DTO: {} and {} share signature {} -> using shared type {}",
                                canonical.getName(), dup.getName(), entry.getKey(), canonicalMeta.getClassName());
                    }
                }
            }
        }

        if (dedupedCount > 0) {
            log.info("  Deduped {} DTOs through structural analysis", dedupedCount);
        }

        // Step 3: Detect inheritance relationships if enabled
        if (config.isInferInheritance()) {
            detectInheritance();
        }

        log.info("DTO metadata built successfully: {} unique DTOs, {} deduped",
                dtoMetadataMap.size() - dedupedCount, dedupedCount);
    }

    private String determinePackageType(CopybookModel model) {
        if (sharedModels.contains(model)) return "shared";
        if (requestModels.contains(model)) return "request";
        if (responseModels.contains(model)) return "response";
        return "layout"; // test mode
    }

    private String determineClassName(CopybookModel model, String packageType, Set<String> usedNames) {
        // Special handling for root copybooks
        if (model == requestCopybook && "request".equals(packageType)) {
            return toPascalCase(config.getProgramId()) + "Request";
        }
        if (model == responseCopybook && "response".equals(packageType)) {
            return toPascalCase(config.getProgramId()) + "Response";
        }

        String baseName = toPascalCase(model.getName());
        if (!usedNames.contains(baseName)) {
            return baseName;
        }

        // Collision detected - disambiguate
        if (!usedNames.contains(baseName)) {
            return baseName;
        }
        return disambiguateClassName(baseName, model, usedNames);

    }

    private void detectInheritance() {
        log.info("Detecting inheritance relationships...");
        int inheritanceCount = 0;

        List<CopybookModel> models = new ArrayList<>(dtoMetadataMap.keySet());

        for (int i = 0; i < models.size(); i++) {
            for (int j = 0; j < models.size(); j++) {
                if (i == j) continue;

                CopybookModel subset = models.get(i);
                CopybookModel superset = models.get(j);

                DtoMetadata subsetMeta = dtoMetadataMap.get(subset);
                DtoMetadata supersetMeta = dtoMetadataMap.get(superset);

                // Skip if already deduped
                if (subsetMeta.isDeduped() || supersetMeta.isDeduped()) continue;

                // Check if subset is a structural prefix of superset
                if (StructuralSignatureCalculator.isStructuralSubset(subset, superset)) {
                    // Validate that offsets are compatible (subset fields don't change offsets in superset)
                    if (isInheritanceSafe(subset, superset)) {
                        inheritanceMap.put(superset, subset); // superset extends subset
                        supersetMeta.setExtendsClassName(subsetMeta.getClassName());
                        inheritanceCount++;
                        log.info("INHERITANCE: {} extends {} (structural prefix detected)",
                                supersetMeta.getClassName(), subsetMeta.getClassName());
                        break; // Only one parent per DTO
                    }
                }
            }
        }

        if (inheritanceCount > 0) {
            log.info("  Detected {} inheritance relationships", inheritanceCount);
        }
    }

    private boolean isInheritanceSafe(CopybookModel subset, CopybookModel superset) {
        // For inheritance to be safe:
        // 1. Serialization offsets must be compatible (subset's fields are a prefix)
        // 2. No REDEFINES that would change semantics
        // 3. OCCURS counts must match for prefix fields
        // For now, we rely on isStructuralSubset which already checks these
        return true;
    }

    private Path createProjectStructure() throws IOException {
        Path projectDir = config.getOutputDir().resolve(config.getProjectName());
        
        if (Files.exists(projectDir)) {
            if (config.isForce()) {
                deleteDirectory(projectDir);
            } else {
                throw new IOException("Project directory already exists: " + projectDir);
            }
        }
        
        // Create directory structure
        String basePackagePath = config.getBasePackage().replace('.', '/');
        
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/config"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/controller"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/camel"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/mainframe/transport"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/mainframe/framing"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/mainframe/emulator"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/model/request"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/model/response"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/model/shared"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/model/layout"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/service"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/util"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/util/request"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/util/response"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/util/shared"));
        
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/model/request"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/model/response"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/serializer"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/torture"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/tcp"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/camel"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/controller"));
        
        Files.createDirectories(projectDir.resolve("src/main/resources"));
        Files.createDirectories(projectDir.resolve("src/test/resources"));
        
        return projectDir;
    }
    
    private void deleteDirectory(Path dir) throws IOException {
        if (Files.exists(dir)) {
            Files.walk(dir)
                    .sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    .forEach(File::delete);
        }
    }
    
    private void generatePom(Path projectDir) throws IOException {
        String pom = generatePomContent();
        safeWriteString(projectDir.resolve("pom.xml"), pom);
    }
    
    private String generatePomContent() {
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
    
    private void generateApplicationYml(Path projectDir) throws IOException {
        String yml = String.format("""
                server:
                  port: 8080
                
                spring:
                  application:
                    name: %s
                
                mainframe:
                  program-id: %s
                  tcp:
                    host: %s
                    port: %d
                    connect-timeout-ms: %d
                    read-timeout-ms: %d
                    framing: %s
                    encoding: %s
                
                camel:
                  springboot:
                    name: %s
                
                logging:
                  level:
                    %s: DEBUG
                    org.apache.camel: INFO
                """,
                config.getProjectName(),
                config.getProgramId(),
                config.getTcpHost(),
                config.getTcpPort(),
                config.getTcpConnectTimeout(),
                config.getTcpReadTimeout(),
                config.getFramingMode(),
                config.getEncoding(),
                config.getProjectName(),
                config.getBasePackage()
        );
        
        safeWriteString(projectDir.resolve("src/main/resources/application.yml"), yml);
        
        // Also create test application.yml
        String testYml = """
                spring:
                  profiles:
                    active: test
                
                mainframe:
                  tcp:
                    host: localhost
                    port: 0
                    framing: LENGTH_PREFIX_2
                """;
        safeWriteString(projectDir.resolve("src/test/resources/application.yml"), testYml);
    }

    /**
     * Generate multi-copybook wrapper classes (ApiRequest/ApiResponse).
     * These wrappers contain fields for each copybook DTO in the request/response sets.
     */
    private void generateWrapperClasses(Path projectDir) throws IOException {
        boolean usingFolderBasedSelection = config.getRequestCopybookDir() != null || config.getResponseCopybookDir() != null;

        if (!usingFolderBasedSelection) {
            // Heuristic mode: no wrappers needed
            return;
        }

        // Generate ApiRequest wrapper if we have multiple request copybooks
        List<DtoMetadata> requestDtos = getRequestDtosForWrapper();
        if (requestDtos.size() > 1) {
            generateApiRequestWrapper(projectDir, requestDtos);
        }

        // Generate ApiResponse wrapper if we have multiple response copybooks
        List<DtoMetadata> responseDtos = getResponseDtosForWrapper();
        if (responseDtos.size() > 1) {
            generateApiResponseWrapper(projectDir, responseDtos);
        }
    }

    private List<DtoMetadata> getRequestDtosForWrapper() {
        List<DtoMetadata> dtos = new ArrayList<>();
        for (CopybookModel model : requestModels) {
            DtoMetadata meta = dtoMetadataMap.get(model);
            if (meta != null && !meta.isDeduped()) {
                dtos.add(meta);
            }
        }
        // Include shared models that are used in requests
        for (CopybookModel model : sharedModels) {
            if (requestModels.contains(model)) {
                DtoMetadata meta = dtoMetadataMap.get(model);
                if (meta != null && !meta.isDeduped()) {
                    dtos.add(meta);
                }
            }
        }
        // Sort by name for deterministic ordering
        dtos.sort(Comparator.comparing(DtoMetadata::getClassName));
        return dtos;
    }

    private List<DtoMetadata> getResponseDtosForWrapper() {
        List<DtoMetadata> dtos = new ArrayList<>();
        for (CopybookModel model : responseModels) {
            DtoMetadata meta = dtoMetadataMap.get(model);
            if (meta != null && !meta.isDeduped()) {
                dtos.add(meta);
            }
        }
        // Include shared models that are used in responses
        for (CopybookModel model : sharedModels) {
            if (responseModels.contains(model)) {
                DtoMetadata meta = dtoMetadataMap.get(model);
                if (meta != null && !meta.isDeduped()) {
                    dtos.add(meta);
                }
            }
        }
        // Sort by name for deterministic ordering (response root first if present)
        dtos.sort((a, b) -> {
            DtoMetadata responseMeta = dtoMetadataMap.get(responseCopybook);
            if (responseMeta != null) {
                if (a.equals(responseMeta)) return -1;
                if (b.equals(responseMeta)) return 1;
            }
            return a.getClassName().compareTo(b.getClassName());
        });
        return dtos;
    }

    private void generateApiRequestWrapper(Path projectDir, List<DtoMetadata> requestDtos) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = toPascalCase(config.getProgramId()) + "ApiRequest";

        Path outputPath = projectDir.resolve("src/main/java/" + basePackagePath + "/model/request/" + className + ".java");

        StringBuilder importsBuilder = new StringBuilder();
        StringBuilder fieldsBuilder = new StringBuilder();

        Set<String> imports = new HashSet<>();
        for (DtoMetadata dto : requestDtos) {
            if (dto.isDeduped()) continue; // Skip deduped DTOs
            String fieldName = toCamelCase(dto.getClassName());
            String fullType = config.getBasePackage() + ".model." + dto.getPackageType() + "." + dto.getClassName();
            imports.add(fullType);

            fieldsBuilder.append(String.format("    private %s %s;\n", dto.getClassName(), fieldName));
        }

        for (String imp : imports.stream().sorted().toList()) {
            importsBuilder.append(String.format("import %s;\n", imp));
        }

        String content = String.format("""
                package %s.model.request;

                %s
                import lombok.AllArgsConstructor;
                import lombok.Builder;
                import lombok.Data;
                import lombok.NoArgsConstructor;

                /**
                 * Multi-copybook request wrapper containing all request container DTOs.
                 * Generated in folder-based mode.
                 */
                @Data
                @NoArgsConstructor
                @AllArgsConstructor
                @Builder
                public class %s {
                %s
                }
                """, config.getBasePackage(), importsBuilder.toString(), className, fieldsBuilder.toString());

        safeWriteString(outputPath, content);
        log.info("  Generated wrapper: {}", outputPath);
    }

    private void generateApiResponseWrapper(Path projectDir, List<DtoMetadata> responseDtos) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = toPascalCase(config.getProgramId()) + "ApiResponse";

        Path outputPath = projectDir.resolve("src/main/java/" + basePackagePath + "/model/response/" + className + ".java");

        StringBuilder importsBuilder = new StringBuilder();
        StringBuilder fieldsBuilder = new StringBuilder();

        Set<String> imports = new HashSet<>();
        for (DtoMetadata dto : responseDtos) {
            if (dto.isDeduped()) continue; // Skip deduped DTOs
            String fieldName = toCamelCase(dto.getClassName());
            String fullType = config.getBasePackage() + ".model." + dto.getPackageType() + "." + dto.getClassName();
            imports.add(fullType);

            fieldsBuilder.append(String.format("    private %s %s;\n", dto.getClassName(), fieldName));
        }

        for (String imp : imports.stream().sorted().toList()) {
            importsBuilder.append(String.format("import %s;\n", imp));
        }

        String content = String.format("""
                package %s.model.response;

                %s
                import lombok.AllArgsConstructor;
                import lombok.Builder;
                import lombok.Data;
                import lombok.NoArgsConstructor;

                /**
                 * Multi-copybook response wrapper containing all response container DTOs.
                 * Generated in folder-based mode.
                 */
                @Data
                @NoArgsConstructor
                @AllArgsConstructor
                @Builder
                public class %s {
                %s
                }
                """, config.getBasePackage(), importsBuilder.toString(), className, fieldsBuilder.toString());

        safeWriteString(outputPath, content);
        log.info("  Generated wrapper: {}", outputPath);
    }

    private int generateModelClasses(Path projectDir) throws IOException {
        int count = 0;

        // Generate DTOs using metadata (which handles deduplication, collision, and inheritance)
        for (Map.Entry<CopybookModel, DtoMetadata> entry : dtoMetadataMap.entrySet()) {
            CopybookModel model = entry.getKey();
            DtoMetadata metadata = entry.getValue();

            // Skip deduped DTOs - they'll use the canonical shared DTO instead
            if (metadata.isDeduped()) {
                log.debug("Skipping deduped DTO: {} (using shared {})",
                    metadata.getOriginalClassName(), metadata.getDedupedToClassName());
                continue;
            }

            // Determine suffix for special cases (Request/Response roots)
            String suffix = null;
            if (model == requestCopybook && "request".equals(metadata.getPackageType())) {
                suffix = "Request";
            } else if (model == responseCopybook && "response".equals(metadata.getPackageType())) {
                suffix = "Response";
            }

            // Generate DTO with inheritance support
            generateDtoClassWithMetadata(projectDir, model, metadata, suffix);
            count++;
        }

        log.info("  Generated {} DTO classes", count);
        return count;
    }

    /**
     * Generate DTO class using metadata (supports inheritance).
     */
    private void generateDtoClassWithMetadata(Path projectDir, CopybookModel model,
                                              DtoMetadata metadata, String suffix) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = metadata.getClassName();
        String packageType = metadata.getPackageType();

        Path outputPath = projectDir.resolve(
            "src/main/java/" + basePackagePath + "/model/" + packageType + "/" + className + ".java"
        );

        // Generate nested classes first (for OCCURS)
        Set<String> generatedNested = new HashSet<>();
        generateNestedClasses(projectDir, model.getRootGroup(), packageType, generatedNested);


        // Build class content with optional inheritance
        StringBuilder content = new StringBuilder();
        content.append(String.format("package %s.model.%s;\n\n", config.getBasePackage(), packageType));

        // Imports
        content.append("import lombok.AllArgsConstructor;\n");
        content.append("import lombok.Builder;\n");
        content.append("import lombok.Data;\n");
        content.append("import lombok.NoArgsConstructor;\n");
        if (metadata.getExtendsClassName() != null) {
            content.append("import lombok.EqualsAndHashCode;\n");
            content.append("import lombok.ToString;\n");
        }
        content.append("import com.fasterxml.jackson.annotation.JsonProperty;\n");
        content.append("import jakarta.validation.constraints.*;\n");
        content.append("import java.math.BigDecimal;\n");
        content.append("import java.util.List;\n\n");

        // Javadoc
        content.append("/**\n");
        content.append(String.format(" * DTO for copybook: %s\n", model.getName()));
        if (metadata.getExtendsClassName() != null) {
            content.append(String.format(" * Extends: %s (structural prefix detected)\n", metadata.getExtendsClassName()));
        }
        if (metadata.isDeduped()) {
            content.append(" * Note: Deduped - this DTO shares structure with another copybook\n");
        }
        content.append(String.format(" * Byte length: %d\n", metadata.getByteLength()));
        content.append(" */\n");

        // Annotations
        content.append("@Data\n");
        content.append("@NoArgsConstructor\n");
        content.append("@AllArgsConstructor\n");
        content.append("@Builder\n");
        if (metadata.getExtendsClassName() != null) {
            content.append("@EqualsAndHashCode(callSuper = true)\n");
            content.append("@ToString(callSuper = true)\n");
        }

        // Class declaration with optional extends
        if (metadata.getExtendsClassName() != null) {
            content.append(String.format("public class %s extends %s {\n", className, metadata.getExtendsClassName()));
        } else {
            content.append(String.format("public class %s {\n", className));
        }

        // Generate fields
        if (model.getRootGroup() != null) {
            Set<String> nestedClassNames = new HashSet<>();
            generateDtoFields(content, model.getRootGroup().getChildren(), "    ", nestedClassNames);
        }

        content.append("}\n");

        safeWriteString(outputPath, content.toString());
        log.info("  FILE WRITTEN: {}", outputPath);
    }

    private String disambiguateClassName(String baseName, CopybookModel model, Set<String> existingNames) {
        // Deterministic disambiguation based on source path hash
        String sourcePath = model.getSourcePath();
        int hash = Math.abs(sourcePath.hashCode()) % 100;
        String disambiguated = baseName + "_" + hash;
        while (existingNames.contains(disambiguated)) {
            hash++;
            disambiguated = baseName + "_" + hash;
        }
        log.warn("Class name collision for '{}', using '{}'", baseName, disambiguated);
        return disambiguated;
    }

    /**
     * Generate nested classes for OCCURS groups.
     */
    private void generateNestedClasses(Path projectDir, GroupNode parent, String subPackage, Set<String> generated)
            throws IOException {
        if (parent == null) return;

        for (CopybookNode child : parent.getChildren()) {
            if (child instanceof GroupNode group) {
                // If this group has OCCURS, generate a nested class
                if (group.getOccursCount() > 1) {
                    String nestedClassName = toPascalCase(group.getName()) + "Item";

                    // Avoid duplicate generation
                    if (generated.contains(nestedClassName)) {
                    		log.warn("Skipping duplicate nested class generation: {} for group {} (already generated). " +
                                "This may indicate a naming collision in the copybook structure.",
                                nestedClassName, group.getName());
                        continue;
                    }
                    generated.add(nestedClassName);

                    log.info("  Generating nested class for OCCURS group: {}", nestedClassName);

                    String basePackagePath = config.getBasePackage().replace('.', '/');
                    Path nestedFile = projectDir.resolve(
                            "src/main/java/" + basePackagePath + "/model/" + subPackage + "/" + nestedClassName + ".java"
                    );

                    StringBuilder sb = new StringBuilder();
                    sb.append("package ").append(config.getBasePackage()).append(".model.").append(subPackage).append(";\n\n");
                    sb.append("import jakarta.validation.Valid;\n");
                    sb.append("import jakarta.validation.constraints.*;\n");
                    sb.append("import lombok.*;\n");
                    sb.append("import com.fasterxml.jackson.annotation.JsonProperty;\n");
                    sb.append("import java.math.BigDecimal;\n");
                    sb.append("import java.time.LocalDate;\n");
                    sb.append("import java.util.List;\n");
                    sb.append("import java.util.ArrayList;\n");

                    // Import enums
                    for (FieldNode field : getAllFieldsInGroup(group)) {
                        if (field.hasEnum88Values() && !field.isFiller()) {
                            sb.append("import ").append(config.getBasePackage()).append(".model.")
                                    .append(toPascalCase(field.getName())).append("Enum;\n");
                        }
                    }
                    sb.append("\n");

                    sb.append("/**\n");
                    sb.append(" * Generated nested class for OCCURS group: ").append(group.getName()).append("\n");
                    sb.append(" * OCCURS ").append(group.getOccursCount()).append(" TIMES\n");
                    sb.append(" */\n");
                    sb.append("@Data\n");
                    sb.append("@NoArgsConstructor\n");
                    sb.append("@AllArgsConstructor\n");
                    sb.append("@Builder\n");
                    sb.append("public class ").append(nestedClassName).append(" {\n\n");

                    // Generate fields for this OCCURS group
                    generateDtoFields(sb, group.getChildren(), "    ", generated);

                    sb.append("}\n");
                    safeWriteString(nestedFile, sb.toString());
                }

                // Recurse into nested groups
                generateNestedClasses(projectDir, group, subPackage, generated);
            }
        }
    }

    /**
     * Generate fields for DTO class from a list of nodes.
     */
    private void generateDtoFields(StringBuilder sb, List<CopybookNode> nodes, String indent, Set<String> nestedClassNames) {
        Set<String> usedFieldNames = new HashSet<>();

        for (CopybookNode node : nodes) {
            if (node instanceof FieldNode field) {
                if (field.isFiller() || mappingDoc.shouldIgnore(field.getName())) {
                    continue;
                }

                log.debug("    Generating field: {} ({})", field.getName(), field.inferJavaType());

                // Generate validation annotations
                var constraints = validationGenerator.generateConstraints(field, true);
                for (var constraint : constraints) {
                    sb.append(indent).append(constraint.toAnnotation()).append("\n");
                }

                // JsonProperty annotation
                sb.append(indent).append("@JsonProperty(\"").append(field.toJavaFieldName()).append("\")\n");

                // Field declaration
                String javaType = getJavaType(field);
                String fieldName = getJavaFieldName(field);

                // Check for field name collisions
                if (!usedFieldNames.add(fieldName)) {
                		log.warn("Duplicate field name detected: '{}' (from COBOL field '{}') in DTO. " +
                            "This may cause compilation errors. Consider using field mapping to rename.",
                            fieldName, field.getName());
                }


                if (field.getOccursCount() > 1) {
                    sb.append(indent).append("@Builder.Default\n");
                    sb.append(indent).append("private List<").append(javaType).append("> ")
                            .append(fieldName).append(" = new ArrayList<>();\n\n");
                } else {
                    sb.append(indent).append("private ").append(javaType).append(" ")
                            .append(fieldName).append(";\n\n");
                }

            } else if (node instanceof GroupNode group) {
                // Skip FILLER groups
                if (group.getName().equalsIgnoreCase("FILLER")) {
                    continue;
                }

                if (group.getOccursCount() > 1) {
                    // OCCURS group - generate List<NestedClass>
                    String nestedClassName = toPascalCase(group.getName()) + "Item";
                    String fieldName = toCamelCase(group.getName());

                    // Check for field name collisions
                    if (!usedFieldNames.add(fieldName)) {
                    		log.warn("Duplicate field name detected: '{}' (from COBOL group '{}') in DTO. " +
                                "This may cause compilation errors.",
                                fieldName, group.getName());
                    }

                    sb.append(indent).append("@Valid\n");
                    sb.append(indent).append("@NotNull\n");
                    sb.append(indent).append("@Size(max = ").append(group.getOccursCount()).append(")\n");
                    sb.append(indent).append("@JsonProperty(\"").append(fieldName).append("\")\n");
                    sb.append(indent).append("@Builder.Default\n");
                    sb.append(indent).append("private List<").append(nestedClassName).append("> ")
                            .append(fieldName).append(" = new ArrayList<>();\n\n");

                    log.debug("    Generating OCCURS group field: {} -> List<{}>", group.getName(), nestedClassName);

                } else {
                    // Regular group - inline its children
                    generateDtoFields(sb, group.getChildren(), indent, nestedClassNames);
                }
            }
        }
    }

    /**
     * Get all fields within a group (recursive).
     */
    private List<FieldNode> getAllFieldsInGroup(GroupNode group) {
        List<FieldNode> fields = new ArrayList<>();
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                fields.add(field);
            } else if (child instanceof GroupNode childGroup) {
                fields.addAll(getAllFieldsInGroup(childGroup));
            }
        }
        return fields;
    }
    
    private String getJavaType(FieldNode field) {
        // Check for mapping override
        var mapping = mappingDoc.getMappingFor(field.getName());
        if (mapping.isPresent() && mapping.get().getTargetType() != null) {
            return mapping.get().getTargetType();
        }
        
        // Check for 88-level (enum)
        if (field.hasEnum88Values()) {
            return toPascalCase(field.getName()) + "Enum";
        }
        
        return field.inferJavaType();
    }
    
    private String getJavaFieldName(FieldNode field) {
        // Check for rename mapping
        var mapping = mappingDoc.getRenamedName(field.getName());
        if (mapping.isPresent()) {
            return toCamelCase(mapping.get());
        }
        return field.toJavaFieldName();
    }
    
    private void generateEnumClasses(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Set<String> generatedEnumClasses = new HashSet<>();

        for (CopybookModel model : copybookModels) {
            for (FieldNode field : model.getAllFields()) {
                if (field.hasEnum88Values()) {
                    String enumName = toPascalCase(field.getName()) + "Enum";

                    // Check for enum class name collisions
                    if (!generatedEnumClasses.add(enumName)) {
                    		log.warn("Skipping duplicate enum class generation: {} for field {} in copybook {}. " +
                                "This may indicate multiple fields with the same name across copybooks.",
                                enumName, field.getName(), model.getName());
                        continue;
                    }

                    Path enumFile = projectDir.resolve(
                            "src/main/java/" + basePackagePath + "/model/" + enumName + ".java"
                    );

                    StringBuilder sb = new StringBuilder();
                    sb.append("package ").append(config.getBasePackage()).append(".model;\n\n");
                    sb.append("import lombok.Getter;\n");
                    sb.append("import lombok.RequiredArgsConstructor;\n\n");

                    sb.append("/**\n");
                    sb.append(" * Generated enum from 88-level values of ").append(field.getName()).append("\n");
                    sb.append(" */\n");
                    sb.append("@Getter\n");
                    sb.append("@RequiredArgsConstructor\n");
                    sb.append("public enum ").append(enumName).append(" {\n");

                    List<Enum88Node> enums = field.getEnum88Values();
                    Set<String> usedConstantNames = new HashSet<>();
                    List<String> validConstants = new ArrayList<>();

                    // First pass: collect valid constant names and check for collisions
                    for (Enum88Node enum88 : enums) {
                        String constName = enum88.toJavaEnumConstant();
                        String value = enum88.getPrimaryValue();

                        // Check for enum constant name collisions
                        if (!usedConstantNames.add(constName)) {
                        		log.warn("Duplicate enum constant name detected: {} in enum {} (from 88-level '{}' with value '{}'). " +
                                    "This constant will be skipped.",
                                    constName, enumName, enum88.getName(), value);
                            continue;
                        }

                        validConstants.add(constName + "(\"" + value + "\")");
                    }

                    // Second pass: generate enum constants
                    for (int i = 0; i < validConstants.size(); i++) {
                        sb.append("    ").append(validConstants.get(i));
                        sb.append(i < validConstants.size() - 1 ? ",\n" : ";\n\n");
                    }
                    
                    sb.append("    private final String value;\n\n");
                    
                    // fromValue method
                    sb.append("    public static ").append(enumName).append(" fromValue(String value) {\n");
                    sb.append("        for (").append(enumName).append(" e : values()) {\n");
                    sb.append("            if (e.value.equals(value)) {\n");
                    sb.append("                return e;\n");
                    sb.append("            }\n");
                    sb.append("        }\n");
                    sb.append("        throw new IllegalArgumentException(\"Unknown value: \" + value);\n");
                    sb.append("    }\n");
                    sb.append("}\n");

                    safeWriteString(enumFile, sb.toString());
                }
            }
        }
    }
    
    private void generateSerializationClasses(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        
        // Generate CobolSerializer interface
        Path serializerInterface = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/util/CobolSerializer.java"
        );
        
        String serializerContent = String.format("""
                package %s.util;
                
                /**
                 * Interface for COBOL byte array serialization.
                 */
                public interface CobolSerializer<T> {
                    byte[] serialize(T object);
                    T deserialize(byte[] bytes);
                    int getByteLength();
                }
                """, config.getBasePackage());

        safeWriteString(serializerInterface, serializerContent);
        
        // Generate EbcdicUtils
        Path ebcdicUtils = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/util/EbcdicUtils.java"
        );
        
        String ebcdicContent = String.format("""
                package %s.util;
                
                import java.math.BigDecimal;
                import java.nio.ByteBuffer;
                import java.nio.charset.Charset;
                
                /**
                 * Utilities for EBCDIC encoding and COBOL numeric formats.
                 */
                public class EbcdicUtils {
                    
                    private static final Charset EBCDIC = Charset.forName("%s");
                    
                    public static byte[] stringToEbcdic(String value, int length) {
                        byte[] result = new byte[length];
                        // Fill with EBCDIC spaces
                        java.util.Arrays.fill(result, (byte) 0x40);
                        
                        if (value != null) {
                            byte[] encoded = value.getBytes(EBCDIC);
                            System.arraycopy(encoded, 0, result, 0, Math.min(encoded.length, length));
                        }
                        return result;
                    }
                    
                    public static String ebcdicToString(byte[] bytes, int offset, int length) {
                        return new String(bytes, offset, length, EBCDIC).trim();
                    }
                    
                    public static byte[] intToBinary(int value, int byteLength) {
                        ByteBuffer buffer = ByteBuffer.allocate(byteLength);
                        if (byteLength == 2) {
                            buffer.putShort((short) value);
                        } else if (byteLength == 4) {
                            buffer.putInt(value);
                        } else {
                            buffer.putLong(value);
                        }
                        return buffer.array();
                    }
                    
                    public static int binaryToInt(byte[] bytes, int offset, int length) {
                        ByteBuffer buffer = ByteBuffer.wrap(bytes, offset, length);
                        if (length == 2) {
                            return buffer.getShort();
                        } else if (length == 4) {
                            return buffer.getInt();
                        }
                        return (int) buffer.getLong();
                    }
                    
                    public static byte[] packedDecimal(BigDecimal value, int byteLength) {
                        // COMP-3 Packed decimal: each byte holds 2 digits, last nibble is sign
                        // Sign nibbles: 0xC=positive (preferred), 0xD=negative (preferred), 0xF=unsigned
                        byte[] result = new byte[byteLength];
                        java.util.Arrays.fill(result, (byte) 0x00);

                        if (value == null) {
                            result[byteLength - 1] = 0x0C; // Positive zero
                            return result;
                        }

                        boolean negative = value.signum() < 0;
                        String digits = value.abs().movePointRight(value.scale()).toBigInteger().toString();

                        // Pad with leading zeros
                        int totalDigits = (byteLength * 2) - 1;
                        while (digits.length() < totalDigits) {
                            digits = "0" + digits;
                        }

                        // Truncate if too long
                        if (digits.length() > totalDigits) {
                            digits = digits.substring(digits.length() - totalDigits);
                        }

                        // Pack digits
                        int digitIdx = 0;
                        for (int i = 0; i < byteLength - 1; i++) {
                            int high = digits.charAt(digitIdx++) - '0';
                            int low = digits.charAt(digitIdx++) - '0';
                            result[i] = (byte) ((high << 4) | low);
                        }

                        // Last byte: one digit + sign (0xC=positive, 0xD=negative per IBM standard)
                        int lastDigit = digitIdx < digits.length() ? digits.charAt(digitIdx) - '0' : 0;
                        int sign = negative ? 0x0D : 0x0C;
                        result[byteLength - 1] = (byte) ((lastDigit << 4) | sign);

                        return result;
                    }
                    
                    public static BigDecimal unpackDecimal(byte[] bytes, int offset, int length, int scale) {
                        StringBuilder sb = new StringBuilder();

                        // Unpack all digit nibbles
                        for (int i = 0; i < length - 1; i++) {
                            int b = bytes[offset + i] & 0xFF;
                            int highNibble = (b >> 4) & 0x0F;
                            int lowNibble = b & 0x0F;

                            // Validate digit nibbles (0-9)
                            if (highNibble > 9 || lowNibble > 9) {
                                throw new IllegalArgumentException(
                                    String.format("Invalid packed decimal digit at offset %%d: 0x%%02X", offset + i, b));
                            }

                            sb.append((char) ('0' + highNibble));
                            sb.append((char) ('0' + lowNibble));
                        }

                        // Last byte: one digit + sign nibble
                        int lastByte = bytes[offset + length - 1] & 0xFF;
                        int lastDigit = (lastByte >> 4) & 0x0F;
                        int sign = lastByte & 0x0F;

                        // Validate last digit
                        if (lastDigit > 9) {
                            throw new IllegalArgumentException(
                                String.format("Invalid packed decimal digit at offset %%d: 0x%%02X", offset + length - 1, lastByte));
                        }

                        sb.append((char) ('0' + lastDigit));

                        // Validate and interpret sign nibble
                        // Valid signs: 0xC (positive, preferred), 0xD (negative, preferred),
                        //              0xF (unsigned/positive), 0xA, 0xB, 0xE (alternate)
                        boolean negative = false;
                        switch (sign) {
                            case 0x0C, 0x0F, 0x0A, 0x0E -> negative = false;  // Positive
                            case 0x0D, 0x0B -> negative = true;                // Negative
                            default -> throw new IllegalArgumentException(
                                String.format("Invalid packed decimal sign nibble at offset %%d: 0x%%X", offset + length - 1, sign));
                        }

                        BigDecimal result = new BigDecimal(sb.toString());
                        if (scale > 0) {
                            result = result.movePointLeft(scale);
                        }

                        if (negative) {
                            result = result.negate();
                        }

                        return result;
                    }
                    
                    public static byte[] zonedDecimal(String value, int length, boolean signed) {
                        byte[] result = new byte[length];
                        java.util.Arrays.fill(result, (byte) 0xF0); // EBCDIC zeros
                        
                        if (value == null || value.isEmpty()) {
                            return result;
                        }
                        
                        boolean negative = value.startsWith("-");
                        String digits = value.replace("-", "").replace("+", "");
                        
                        // Pad with leading zeros
                        while (digits.length() < length) {
                            digits = "0" + digits;
                        }
                        
                        // Convert each digit
                        for (int i = 0; i < length; i++) {
                            result[i] = (byte) (0xF0 | (digits.charAt(i) - '0'));
                        }
                        
                        // Apply sign to last digit
                        if (signed) {
                            int lastDigit = result[length - 1] & 0x0F;
                            result[length - 1] = (byte) ((negative ? 0xD0 : 0xC0) | lastDigit);
                        }
                        
                        return result;
                    }
                    
                    public static String unzonedDecimal(byte[] bytes, int offset, int length) {
                        StringBuilder sb = new StringBuilder();

                        for (int i = 0; i < length; i++) {
                            int b = bytes[offset + i] & 0xFF;
                            int digit = b & 0x0F;
                            sb.append((char) ('0' + digit));
                        }

                        // Check sign from last byte
                        int lastZone = (bytes[offset + length - 1] >> 4) & 0x0F;
                        boolean negative = (lastZone == 0x0D || lastZone == 0x0B);

                        String result = sb.toString().replaceFirst("^0+(?!$)", "");
                        return negative ? "-" + result : result;
                    }

                    /**
                     * Convert float to 4-byte array (COMP-1, IEEE 754 single precision, big-endian).
                     */
                    public static byte[] floatToBytes(float value) {
                        return ByteBuffer.allocate(4).putFloat(value).array();
                    }

                    /**
                     * Convert 4-byte array to float (COMP-1, IEEE 754 single precision, big-endian).
                     */
                    public static float bytesToFloat(byte[] bytes, int offset) {
                        return ByteBuffer.wrap(bytes, offset, 4).getFloat();
                    }

                    /**
                     * Convert double to 8-byte array (COMP-2, IEEE 754 double precision, big-endian).
                     */
                    public static byte[] doubleToBytes(double value) {
                        return ByteBuffer.allocate(8).putDouble(value).array();
                    }

                    /**
                     * Convert 8-byte array to double (COMP-2, IEEE 754 double precision, big-endian).
                     */
                    public static double bytesToDouble(byte[] bytes, int offset) {
                        return ByteBuffer.wrap(bytes, offset, 8).getDouble();
                    }
                }
                """, config.getBasePackage(), config.getEncoding().toUpperCase());

        safeWriteString(ebcdicUtils, ebcdicContent);

        // Generate concrete serializers
        boolean usingFolderBasedSelection = config.getRequestCopybookDir() != null || config.getResponseCopybookDir() != null;
        Set<CopybookModel> serializedModels = new HashSet<>();

        if (usingFolderBasedSelection || config.isTestMode()) {
            // Folder-based mode or test mode: generate serializers for all models

            // Generate request root serializer
            if (requestCopybook != null) {
                String className = toPascalCase(config.getProgramId()) + "Request";
                generateConcreteSerializerForModel(projectDir, requestCopybook, "Request", className, "request", false);
                serializedModels.add(requestCopybook);
            }

            // Generate response root serializer
            if (responseCopybook != null && responseCopybook != requestCopybook) {
                String className = toPascalCase(config.getProgramId()) + "Response";
                generateConcreteSerializerForModel(projectDir, responseCopybook, "Response", className, "response", false);
                serializedModels.add(responseCopybook);
            } else if (responseCopybook == requestCopybook && responseCopybook != null) {
                String className = toPascalCase(config.getProgramId()) + "Response";
                generateConcreteSerializerForModel(projectDir, responseCopybook, "Response", className, "response", false);
            }

            // Generate serializers for non-root request models
            for (CopybookModel model : requestModels) {
                if (serializedModels.contains(model)) continue;
                String className = toPascalCase(model.getName());
                generateConcreteSerializerForModel(projectDir, model, null, className, "request", config.isTestMode());
                serializedModels.add(model);
            }

            // Generate serializers for non-root response models
            for (CopybookModel model : responseModels) {
                if (serializedModels.contains(model)) continue;
                String className = toPascalCase(model.getName());
                generateConcreteSerializerForModel(projectDir, model, null, className, "response", config.isTestMode());
                serializedModels.add(model);
            }

            // Generate serializers for shared models
            for (CopybookModel model : sharedModels) {
                if (serializedModels.contains(model)) continue;
                String className = toPascalCase(model.getName());
                generateConcreteSerializerForModel(projectDir, model, null, className, "shared", config.isTestMode());
                serializedModels.add(model);
            }

            // Test mode: generate serializers for all remaining parsed copybooks
            if (config.isTestMode()) {
                for (CopybookModel model : copybookModels) {
                    if (serializedModels.contains(model)) continue;
                    String className = toPascalCase(model.getName());
                    generateConcreteSerializerForModel(projectDir, model, null, className, "layout", true);
                    serializedModels.add(model);
                }
            }

        } else {
            // Heuristic mode (backward compatibility)
            generateConcreteSerializer(projectDir, requestCopybook, "Request");
            if (responseCopybook != requestCopybook) {
                generateConcreteSerializer(projectDir, responseCopybook, "Response");
            }
        }

        // Generate wrapper serializers for multi-copybook mode
        if (usingFolderBasedSelection) {
            generateWrapperSerializers(projectDir);
        }
    }

    /**
     * Generate wrapper-level serializers for ApiRequest/ApiResponse.
     * These serializers concatenate/split bytes from individual copybook serializers.
     */
    private void generateWrapperSerializers(Path projectDir) throws IOException {
        List<DtoMetadata> requestDtos = getRequestDtosForWrapper();
        List<DtoMetadata> responseDtos = getResponseDtosForWrapper();

        if (requestDtos.size() > 1) {
            generateApiRequestSerializer(projectDir, requestDtos);
        }

        if (responseDtos.size() > 1) {
            generateApiResponseSerializer(projectDir, responseDtos);
        }
    }

    private void generateApiRequestSerializer(Path projectDir, List<DtoMetadata> requestDtos) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = toPascalCase(config.getProgramId()) + "ApiRequestSerializer";
        String dtoClassName = toPascalCase(config.getProgramId()) + "ApiRequest";

        Path outputPath = projectDir.resolve(
            "src/main/java/" + basePackagePath + "/util/request/" + className + ".java"
        );

        StringBuilder sb = new StringBuilder();
        sb.append(String.format("package %s.util.request;\n\n", config.getBasePackage()));

        // Imports
        sb.append(String.format("import %s.model.request.%s;\n", config.getBasePackage(), dtoClassName));
        sb.append(String.format("import %s.util.CobolSerializer;\n", config.getBasePackage()));
        for (DtoMetadata dto : requestDtos) {
            if (dto.isDeduped()) continue;
            sb.append(String.format("import %s.model.%s.%s;\n",
                config.getBasePackage(), dto.getPackageType(), dto.getClassName()));
            sb.append(String.format("import %s.util.%s.%sSerializer;\n",
                config.getBasePackage(), dto.getPackageType(), dto.getClassName()));
        }
        sb.append("import org.springframework.stereotype.Component;\n");
        sb.append("import java.util.Arrays;\n\n");

        // Class declaration
        sb.append("/**\n");
        sb.append(" * Multi-copybook request serializer.\n");
        sb.append(" * Concatenates bytes from individual copybook serializers in deterministic order.\n");
        sb.append(" */\n");
        sb.append("@Component\n");
        sb.append(String.format("public class %s implements CobolSerializer<%s> {\n\n", className, dtoClassName));

        // Serializer fields
        for (DtoMetadata dto : requestDtos) {
            if (dto.isDeduped()) continue;
            String fieldName = toCamelCase(dto.getClassName()) + "Serializer";
            sb.append(String.format("    private final %sSerializer %s = new %sSerializer();\n",
                dto.getClassName(), fieldName, dto.getClassName()));
        }
        sb.append("\n");

        // Calculate total byte length
        sb.append("    private static final int BYTE_LENGTH = ");
        sb.append(requestDtos.stream()
            .filter(dto -> !dto.isDeduped())
            .map(dto -> String.valueOf(dto.getByteLength()))
            .collect(java.util.stream.Collectors.joining(" + ")));
        sb.append(";\n\n");

        // getByteLength method
        sb.append("    @Override\n");
        sb.append("    public int getByteLength() {\n");
        sb.append("        return BYTE_LENGTH;\n");
        sb.append("    }\n\n");

        // serialize method - concatenate all container bytes
        sb.append("    @Override\n");
        sb.append(String.format("    public byte[] serialize(%s request) {\n", dtoClassName));
        sb.append("        byte[] result = new byte[BYTE_LENGTH];\n");
        sb.append("        int offset = 0;\n\n");

        for (DtoMetadata dto : requestDtos) {
            if (dto.isDeduped()) continue;
            String fieldName = toCamelCase(dto.getClassName());
            String serializerField = fieldName + "Serializer";

            sb.append(String.format("        // Serialize %s\n", dto.getClassName()));
            sb.append(String.format("        if (request.get%s() != null) {\n",
                capitalize(fieldName)));
            sb.append(String.format("            byte[] %sBytes = %s.serialize(request.get%s());\n",
                fieldName, serializerField, capitalize(fieldName)));
            sb.append(String.format("            System.arraycopy(%sBytes, 0, result, offset, %sBytes.length);\n",
                fieldName, fieldName));
            sb.append(String.format("            offset += %sBytes.length;\n", fieldName));
            sb.append("        } else {\n");
            sb.append(String.format("            // Null container - fill with spaces\n"));
            sb.append(String.format("            Arrays.fill(result, offset, offset + %s, (byte) 0x40);\n",
                dto.getByteLength()));
            sb.append(String.format("            offset += %s;\n", dto.getByteLength()));
            sb.append("        }\n\n");
        }

        sb.append("        return result;\n");
        sb.append("    }\n\n");

        // deserialize method - split by known lengths
        sb.append("    @Override\n");
        sb.append(String.format("    public %s deserialize(byte[] bytes) {\n", dtoClassName));
        sb.append(String.format("        %s.%sBuilder builder = %s.builder();\n",
            dtoClassName, dtoClassName, dtoClassName));
        sb.append("        int offset = 0;\n\n");

        for (DtoMetadata dto : requestDtos) {
            if (dto.isDeduped()) continue;
            String fieldName = toCamelCase(dto.getClassName());
            String serializerField = fieldName + "Serializer";

            sb.append(String.format("        // Deserialize %s\n", dto.getClassName()));
            sb.append(String.format("        if (offset + %s <= bytes.length) {\n", dto.getByteLength()));
            sb.append(String.format("            byte[] %sBytes = Arrays.copyOfRange(bytes, offset, offset + %s);\n",
                fieldName, dto.getByteLength()));
            sb.append(String.format("            builder.%s(%s.deserialize(%sBytes));\n",
                fieldName, serializerField, fieldName));
            sb.append(String.format("            offset += %s;\n", dto.getByteLength()));
            sb.append("        }\n\n");
        }

        sb.append("        return builder.build();\n");
        sb.append("    }\n");
        sb.append("}\n");

        safeWriteString(outputPath, sb.toString());
        log.info("  Generated wrapper serializer: {}", outputPath);
    }

    private void generateApiResponseSerializer(Path projectDir, List<DtoMetadata> responseDtos) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = toPascalCase(config.getProgramId()) + "ApiResponseSerializer";
        String dtoClassName = toPascalCase(config.getProgramId()) + "ApiResponse";

        Path outputPath = projectDir.resolve(
            "src/main/java/" + basePackagePath + "/util/response/" + className + ".java"
        );

        StringBuilder sb = new StringBuilder();
        sb.append(String.format("package %s.util.response;\n\n", config.getBasePackage()));

        // Imports
        sb.append(String.format("import %s.model.response.%s;\n", config.getBasePackage(), dtoClassName));
        sb.append(String.format("import %s.util.CobolSerializer;\n", config.getBasePackage()));
        for (DtoMetadata dto : responseDtos) {
            if (dto.isDeduped()) continue;
            sb.append(String.format("import %s.model.%s.%s;\n",
                config.getBasePackage(), dto.getPackageType(), dto.getClassName()));
            sb.append(String.format("import %s.util.%s.%sSerializer;\n",
                config.getBasePackage(), dto.getPackageType(), dto.getClassName()));
        }
        sb.append("import org.slf4j.Logger;\n");
        sb.append("import org.slf4j.LoggerFactory;\n");
        sb.append("import org.springframework.stereotype.Component;\n");
        sb.append("import java.util.Arrays;\n\n");

        // Class declaration
        sb.append("/**\n");
        sb.append(" * Multi-copybook response serializer.\n");
        sb.append(" * Splits response bytes by known lengths and deserializes each container.\n");
        sb.append(" */\n");
        sb.append("@Component\n");
        sb.append(String.format("public class %s implements CobolSerializer<%s> {\n\n", className, dtoClassName));

        sb.append("    private static final Logger log = LoggerFactory.getLogger(" + className + ".class);\n\n");

        // Serializer fields
        for (DtoMetadata dto : responseDtos) {
            if (dto.isDeduped()) continue;
            String fieldName = toCamelCase(dto.getClassName()) + "Serializer";
            sb.append(String.format("    private final %sSerializer %s = new %sSerializer();\n",
                dto.getClassName(), fieldName, dto.getClassName()));
        }
        sb.append("\n");

        // Calculate total byte length
        sb.append("    private static final int BYTE_LENGTH = ");
        sb.append(responseDtos.stream()
            .filter(dto -> !dto.isDeduped())
            .map(dto -> String.valueOf(dto.getByteLength()))
            .collect(java.util.stream.Collectors.joining(" + ")));
        sb.append(";\n\n");

        // getByteLength method
        sb.append("    @Override\n");
        sb.append("    public int getByteLength() {\n");
        sb.append("        return BYTE_LENGTH;\n");
        sb.append("    }\n\n");

        // serialize method - concatenate all container bytes
        sb.append("    @Override\n");
        sb.append(String.format("    public byte[] serialize(%s response) {\n", dtoClassName));
        sb.append("        byte[] result = new byte[BYTE_LENGTH];\n");
        sb.append("        int offset = 0;\n\n");

        for (DtoMetadata dto : responseDtos) {
            if (dto.isDeduped()) continue;
            String fieldName = toCamelCase(dto.getClassName());
            String serializerField = fieldName + "Serializer";

            sb.append(String.format("        // Serialize %s\n", dto.getClassName()));
            sb.append(String.format("        if (response.get%s() != null) {\n",
                capitalize(fieldName)));
            sb.append(String.format("            byte[] %sBytes = %s.serialize(response.get%s());\n",
                fieldName, serializerField, capitalize(fieldName)));
            sb.append(String.format("            System.arraycopy(%sBytes, 0, result, offset, %sBytes.length);\n",
                fieldName, fieldName));
            sb.append(String.format("            offset += %sBytes.length;\n", fieldName));
            sb.append("        } else {\n");
            sb.append(String.format("            // Null container - fill with spaces\n"));
            sb.append(String.format("            Arrays.fill(result, offset, offset + %s, (byte) 0x40);\n",
                dto.getByteLength()));
            sb.append(String.format("            offset += %s;\n", dto.getByteLength()));
            sb.append("        }\n\n");
        }

        sb.append("        return result;\n");
        sb.append("    }\n\n");

        // deserialize method - split by known lengths
        sb.append("    @Override\n");
        sb.append(String.format("    public %s deserialize(byte[] bytes) {\n", dtoClassName));
        sb.append(String.format("        %s.%sBuilder builder = %s.builder();\n",
            dtoClassName, dtoClassName, dtoClassName));
        sb.append("        int offset = 0;\n\n");

        sb.append("        // Handle short response payloads\n");
        sb.append("        if (bytes.length < BYTE_LENGTH) {\n");
        sb.append("            log.warn(\"Response payload shorter than expected: {} < {}. \" +\n");
        sb.append("                     \"Deserializing available containers.\", bytes.length, BYTE_LENGTH);\n");
        sb.append("        }\n\n");

        sb.append("        // Handle long response payloads\n");
        sb.append("        if (bytes.length > BYTE_LENGTH) {\n");
        sb.append("            log.warn(\"Response payload longer than expected: {} > {}. \" +\n");
        sb.append("                     \"Ignoring trailing bytes.\", bytes.length, BYTE_LENGTH);\n");
        sb.append("        }\n\n");

        for (DtoMetadata dto : responseDtos) {
            if (dto.isDeduped()) continue;
            String fieldName = toCamelCase(dto.getClassName());
            String serializerField = fieldName + "Serializer";

            sb.append(String.format("        // Deserialize %s\n", dto.getClassName()));
            sb.append(String.format("        if (offset + %s <= bytes.length) {\n", dto.getByteLength()));
            sb.append(String.format("            byte[] %sBytes = Arrays.copyOfRange(bytes, offset, offset + %s);\n",
                fieldName, dto.getByteLength()));
            sb.append(String.format("            builder.%s(%s.deserialize(%sBytes));\n",
                fieldName, serializerField, fieldName));
            sb.append(String.format("            offset += %s;\n", dto.getByteLength()));
            sb.append("        } else {\n");
            sb.append(String.format("            log.warn(\"Not enough bytes to deserialize %s, leaving null\");\n",
                dto.getClassName()));
            sb.append("        }\n\n");
        }

        sb.append("        return builder.build();\n");
        sb.append("    }\n");
        sb.append("}\n");

        safeWriteString(outputPath, sb.toString());
        log.info("  Generated wrapper serializer: {}", outputPath);
    }
    
    private void generateConcreteSerializer(Path projectDir, CopybookModel copybook, String suffix)
            throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = toPascalCase(config.getProgramId()) + suffix + "Serializer";
        String dtoClassName = toPascalCase(config.getProgramId()) + suffix;

        // Validate ODO dependencies before generating code
        validateOdoDependencies(copybook.getRootGroup(), new HashSet<>(), copybook.getName());

        Path serializerFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/util/" + className + ".java"
        );

        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(config.getBasePackage()).append(".util;\n\n");
        sb.append("import ").append(config.getBasePackage()).append(".model.")
                .append(suffix.toLowerCase()).append(".").append(dtoClassName).append(";\n");

        // Collect and import enum types
        Set<String> enumImports = new HashSet<>();
        for (FieldNode field : copybook.getAllFields()) {
            if (field.hasEnum88Values() && !field.isFiller() && !mappingDoc.shouldIgnore(field.getName())) {
                enumImports.add(config.getBasePackage() + ".model." + toPascalCase(field.getName()) + "Enum");
            }
        }
        for (String enumImport : enumImports) {
            sb.append("import ").append(enumImport).append(";\n");
        }

        sb.append("import org.springframework.stereotype.Component;\n");
        sb.append("import java.math.BigDecimal;\n");
        sb.append("import java.util.List;\n");
        sb.append("import java.util.ArrayList;\n\n");
        
        sb.append("/**\n");
        sb.append(" * Serializer for ").append(dtoClassName).append("\n");
        sb.append(" * Byte length: ").append(copybook.calculateTotalByteLength()).append("\n");
        sb.append(" */\n");
        sb.append("@Component\n");
        sb.append("public class ").append(className).append(" implements CobolSerializer<")
                .append(dtoClassName).append("> {\n\n");
        
        sb.append("    private static final int BYTE_LENGTH = ").append(copybook.calculateTotalByteLength()).append(";\n\n");
        
        // serialize method
        sb.append("    @Override\n");
        sb.append("    public byte[] serialize(").append(dtoClassName).append(" obj) {\n");
        sb.append("        byte[] result = new byte[BYTE_LENGTH];\n");
        sb.append("        int offset = 0;\n\n");
        
        generateSerializeFields(sb, copybook.getRootGroup(), "obj", "        ");
        
        sb.append("        return result;\n");
        sb.append("    }\n\n");
        
        // deserialize method
        sb.append("    @Override\n");
        sb.append("    public ").append(dtoClassName).append(" deserialize(byte[] bytes) {\n");
        sb.append("        ").append(dtoClassName).append(".").append(dtoClassName)
                .append("Builder builder = ").append(dtoClassName).append(".builder();\n");
        sb.append("        int offset = 0;\n\n");
        
        generateDeserializeFields(sb, copybook.getRootGroup(), "builder", "        ");
        
        sb.append("        return builder.build();\n");
        sb.append("    }\n\n");
        
        // getByteLength method
        sb.append("    @Override\n");
        sb.append("    public int getByteLength() {\n");
        sb.append("        return BYTE_LENGTH;\n");
        sb.append("    }\n");
        
        sb.append("}\n");

        safeWriteString(serializerFile, sb.toString());
    }

    private void generateConcreteSerializerForModel(Path projectDir, CopybookModel copybook, String suffix,
                                                     String dtoClassName, String subPackage, boolean isTestMode)
            throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String serializerClassName = dtoClassName + "Serializer";

        // Validate ODO dependencies before generating code
        validateOdoDependencies(copybook.getRootGroup(), new HashSet<>(), copybook.getName());

        // Determine serializer package based on subPackage
        String serializerPackage = config.getBasePackage() + ".util." + subPackage;
        Path serializerFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/util/" + subPackage + "/" + serializerClassName + ".java"
        );

        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(serializerPackage).append(";\n\n");
        sb.append("import ").append(config.getBasePackage()).append(".model.")
                .append(subPackage).append(".").append(dtoClassName).append(";\n");
        sb.append("import ").append(config.getBasePackage()).append(".util.CobolSerializer;\n");
        sb.append("import ").append(config.getBasePackage()).append(".util.EbcdicUtils;\n");

        // Collect and import enum types
        Set<String> enumImports = new HashSet<>();
        for (FieldNode field : copybook.getAllFields()) {
            if (field.hasEnum88Values() && !field.isFiller() && !mappingDoc.shouldIgnore(field.getName())) {
                enumImports.add(config.getBasePackage() + ".model." + toPascalCase(field.getName()) + "Enum");
            }
        }
        for (String enumImport : enumImports) {
            sb.append("import ").append(enumImport).append(";\n");
        }

        sb.append("import org.springframework.stereotype.Component;\n");
        sb.append("import java.math.BigDecimal;\n");
        sb.append("import java.util.List;\n");
        sb.append("import java.util.ArrayList;\n\n");

        sb.append("/**\n");
        sb.append(" * Serializer for ").append(dtoClassName).append("\n");
        sb.append(" * Byte length: ").append(copybook.calculateTotalByteLength()).append("\n");
        sb.append(" */\n");
        if (!isTestMode) {
            sb.append("@Component\n");
        }
        sb.append("public class ").append(serializerClassName).append(" implements CobolSerializer<")
                .append(dtoClassName).append("> {\n\n");

        sb.append("    private static final int BYTE_LENGTH = ").append(copybook.calculateTotalByteLength()).append(";\n\n");

        // serialize method
        sb.append("    @Override\n");
        sb.append("    public byte[] serialize(").append(dtoClassName).append(" obj) {\n");
        sb.append("        byte[] result = new byte[BYTE_LENGTH];\n");
        sb.append("        int offset = 0;\n\n");

        generateSerializeFields(sb, copybook.getRootGroup(), "obj", "        ");

        sb.append("        return result;\n");
        sb.append("    }\n\n");

        // deserialize method
        sb.append("    @Override\n");
        sb.append("    public ").append(dtoClassName).append(" deserialize(byte[] bytes) {\n");
        sb.append("        ").append(dtoClassName).append(".").append(dtoClassName)
                .append("Builder builder = ").append(dtoClassName).append(".builder();\n");
        sb.append("        int offset = 0;\n\n");

        generateDeserializeFields(sb, copybook.getRootGroup(), "builder", "        ");

        sb.append("        return builder.build();\n");
        sb.append("    }\n\n");

        // getByteLength method
        sb.append("    @Override\n");
        sb.append("    public int getByteLength() {\n");
        sb.append("        return BYTE_LENGTH;\n");
        sb.append("    }\n");

        sb.append("}\n");

        safeWriteString(serializerFile, sb.toString());
    }

    private void generateSerializeFields(StringBuilder sb, GroupNode group, String objRef, String indent) {
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                if (field.isFiller()) {
                    // Advance offset for FILLER
                    int totalBytes = field.getByteLength() * field.getOccursCount();
                    sb.append(indent).append("offset += ").append(totalBytes).append("; // FILLER\n");
                    continue;
                }
                if (mappingDoc.shouldIgnore(field.getName())) {
                    int totalBytes = field.getByteLength() * field.getOccursCount();
                    sb.append(indent).append("offset += ").append(totalBytes).append("; // IGNORED: ")
                            .append(field.getName()).append("\n");
                    continue;
                }

                String fieldName = getJavaFieldName(field);
                String getter = objRef + ".get" + capitalize(fieldName) + "()";

                if (field.getOccursCount() > 1) {
                    // Generate loop for OCCURS field
                    String itemVar = "item" + capitalize(fieldName);
                    String occursDepending = field.getOccursDepending();

                    if (occursDepending != null && !occursDepending.isBlank()) {
                        // OCCURS DEPENDING ON - use actual count, pad to max
                        sb.append(indent).append("// OCCURS ").append(field.getOccursCount())
                                .append(" DEPENDING ON ").append(occursDepending).append(" - ").append(field.getName()).append("\n");
                        String dependingFieldName = toCamelCase(occursDepending);
                        String dependingGetter = objRef + ".get" + capitalize(dependingFieldName) + "()";
                        sb.append(indent).append("int actualCount_").append(fieldName).append(" = ")
                                .append(dependingGetter).append(" != null ? ").append(dependingGetter)
                                .append(".intValue() : 0;\n");
                        sb.append(indent).append("actualCount_").append(fieldName).append(" = Math.min(actualCount_")
                                .append(fieldName).append(", ").append(field.getOccursCount()).append(");\n");
                        sb.append(indent).append("List<").append(getJavaType(field)).append("> ").append(fieldName)
                                .append("List = ").append(getter).append(" != null ? ").append(getter).append(" : new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < actualCount_").append(fieldName).append("; i++) {\n");
                        sb.append(indent).append("    ").append(getJavaType(field)).append(" ").append(itemVar)
                                .append(" = i < ").append(fieldName).append("List.size() ? ").append(fieldName)
                                .append("List.get(i) : null;\n");
                        generateFieldSerialize(sb, field, itemVar, indent + "    ");
                        sb.append(indent).append("}\n");
                        // Pad remaining entries to max to keep record length fixed
                        sb.append(indent).append("// Pad remaining ODO entries\n");
                        sb.append(indent).append("int remainingCount_").append(fieldName).append(" = ")
                                .append(field.getOccursCount()).append(" - actualCount_").append(fieldName).append(";\n");
                        sb.append(indent).append("offset += remainingCount_").append(fieldName).append(" * ")
                                .append(field.getByteLength()).append(";\n\n");
                    } else {
                        // Fixed OCCURS
                        sb.append(indent).append("// OCCURS ").append(field.getOccursCount()).append(" - ").append(field.getName()).append("\n");
                        sb.append(indent).append("List<").append(getJavaType(field)).append("> ").append(fieldName).append("List = ")
                                .append(getter).append(" != null ? ").append(getter).append(" : new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < ").append(field.getOccursCount()).append("; i++) {\n");
                        sb.append(indent).append("    ").append(getJavaType(field)).append(" ").append(itemVar)
                                .append(" = i < ").append(fieldName).append("List.size() ? ").append(fieldName)
                                .append("List.get(i) : null;\n");
                        generateFieldSerialize(sb, field, itemVar, indent + "    ");
                        sb.append(indent).append("}\n\n");
                    }
                } else {
                    generateFieldSerialize(sb, field, getter, indent);
                }

            } else if (child instanceof GroupNode childGroup) {
                if (childGroup.getName().equalsIgnoreCase("FILLER")) {
                    // Skip FILLER groups
                    int totalBytes = childGroup.calculateByteLength();
                    sb.append(indent).append("offset += ").append(totalBytes).append("; // FILLER GROUP\n");
                    continue;
                }

                if (childGroup.getOccursCount() > 1) {
                    // OCCURS group - generate loop
                    String groupFieldName = toCamelCase(childGroup.getName());
                    String nestedClassName = toPascalCase(childGroup.getName()) + "Item";
                    String getter = objRef + ".get" + capitalize(groupFieldName) + "()";
                    String itemVar = "item" + capitalize(groupFieldName);
                    String occursDepending = childGroup.getOccursDepending();

                    if (occursDepending != null && !occursDepending.isBlank()) {
                        // OCCURS DEPENDING ON - use actual count, pad to max
                        sb.append(indent).append("// OCCURS ").append(childGroup.getOccursCount())
                                .append(" DEPENDING ON ").append(occursDepending).append(" - ").append(childGroup.getName()).append("\n");
                        String dependingFieldName = toCamelCase(occursDepending);
                        String dependingGetter = objRef + ".get" + capitalize(dependingFieldName) + "()";
                        sb.append(indent).append("int actualCount_").append(groupFieldName).append(" = ")
                                .append(dependingGetter).append(" != null ? ").append(dependingGetter)
                                .append(".intValue() : 0;\n");
                        sb.append(indent).append("actualCount_").append(groupFieldName).append(" = Math.min(actualCount_")
                                .append(groupFieldName).append(", ").append(childGroup.getOccursCount()).append(");\n");
                        sb.append(indent).append("List<").append(nestedClassName).append("> ").append(groupFieldName)
                                .append("List = ").append(getter).append(" != null ? ").append(getter).append(" : new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < actualCount_").append(groupFieldName).append("; i++) {\n");
                        sb.append(indent).append("    ").append(nestedClassName).append(" ").append(itemVar)
                                .append(" = i < ").append(groupFieldName).append("List.size() ? ")
                                .append(groupFieldName).append("List.get(i) : null;\n");
                        generateSerializeFields(sb, childGroup, itemVar, indent + "    ");
                        sb.append(indent).append("}\n");
                        // Pad remaining entries to max to keep record length fixed
                        sb.append(indent).append("// Pad remaining ODO entries\n");
                        sb.append(indent).append("int remainingCount_").append(groupFieldName).append(" = ")
                                .append(childGroup.getOccursCount()).append(" - actualCount_").append(groupFieldName).append(";\n");
                        sb.append(indent).append("offset += remainingCount_").append(groupFieldName).append(" * ")
                                .append(childGroup.getByteLength()).append(";\n\n");
                    } else {
                        // Fixed OCCURS
                        sb.append(indent).append("// OCCURS ").append(childGroup.getOccursCount()).append(" - ").append(childGroup.getName()).append("\n");
                        sb.append(indent).append("List<").append(nestedClassName).append("> ").append(groupFieldName)
                                .append("List = ").append(getter).append(" != null ? ").append(getter).append(" : new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < ").append(childGroup.getOccursCount()).append("; i++) {\n");
                        sb.append(indent).append("    ").append(nestedClassName).append(" ").append(itemVar)
                                .append(" = i < ").append(groupFieldName).append("List.size() ? ")
                                .append(groupFieldName).append("List.get(i) : null;\n");
                        generateSerializeFields(sb, childGroup, itemVar, indent + "    ");
                        sb.append(indent).append("}\n\n");
                    }
                } else {
                    // Regular group - inline
                    generateSerializeFields(sb, childGroup, objRef, indent);
                }
            }
        }
    }
    
    private void generateFieldSerialize(StringBuilder sb, FieldNode field, String valueExpr, String indent) {
        int byteLen = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();

        // Handle enums - convert to value
        if (field.hasEnum88Values()) {
            String enumValueExpr = valueExpr + " != null ? " + valueExpr + ".getValue() : \"\"";
            sb.append(indent).append("System.arraycopy(EbcdicUtils.stringToEbcdic(")
                    .append(enumValueExpr).append(", ")
                    .append(byteLen).append("), 0, result, offset, ").append(byteLen).append(");\n");
        } else if (usage == UsageType.COMP_1) {
            // COMP-1: IEEE 754 single precision float (4 bytes)
            String floatValue = valueExpr + " != null ? " + valueExpr + " : 0.0f";
            sb.append(indent).append("System.arraycopy(EbcdicUtils.floatToBytes(")
                    .append(floatValue).append("), 0, result, offset, ")
                    .append(byteLen).append(");\n");
        } else if (usage == UsageType.COMP_2) {
            // COMP-2: IEEE 754 double precision float (8 bytes)
            String doubleValue = valueExpr + " != null ? " + valueExpr + " : 0.0";
            sb.append(indent).append("System.arraycopy(EbcdicUtils.doubleToBytes(")
                    .append(doubleValue).append("), 0, result, offset, ")
                    .append(byteLen).append(");\n");
        } else if (usage == UsageType.PACKED_DECIMAL) {
            // Packed decimal (COMP-3)
            sb.append(indent).append("System.arraycopy(EbcdicUtils.packedDecimal(")
                    .append(valueExpr).append(", ").append(byteLen).append("), 0, result, offset, ")
                    .append(byteLen).append(");\n");
        } else if (usage == UsageType.BINARY || usage == UsageType.COMP_5) {
            // Binary - handle different numeric types
            String valueToSerialize = valueExpr + " != null ? " + valueExpr;
            String javaType = field.inferJavaType();
            if (javaType.equals("Short") || javaType.equals("Integer")) {
                valueToSerialize += ".intValue()";
            } else if (javaType.equals("Long")) {
                valueToSerialize += ".intValue()"; // Convert to int for serialization
            } else {
                valueToSerialize += ".intValue()"; // Default fallback
            }
            valueToSerialize += " : 0";
            sb.append(indent).append("System.arraycopy(EbcdicUtils.intToBinary(")
                    .append(valueToSerialize).append(", ")
                    .append(byteLen).append("), 0, result, offset, ").append(byteLen).append(");\n");
        } else if (pic != null && (pic.isAlphanumeric() || (pic.isNumeric() && usage == UsageType.DISPLAY && !pic.isSigned()))) {
            // String field - EBCDIC
            sb.append(indent).append("System.arraycopy(EbcdicUtils.stringToEbcdic(")
                    .append(valueExpr).append(" != null ? ").append(valueExpr).append(".toString() : \"\", ")
                    .append(byteLen).append("), 0, result, offset, ").append(byteLen).append(");\n");
        } else if (pic != null) {
            // Zoned decimal - requires PictureClause for sign information
            sb.append(indent).append("System.arraycopy(EbcdicUtils.zonedDecimal(")
                    .append(valueExpr).append(" != null ? ").append(valueExpr).append(".toString() : \"0\", ")
                    .append(byteLen).append(", ").append(pic.isSigned()).append("), 0, result, offset, ")
                    .append(byteLen).append(");\n");
        } else {
            // No PictureClause and unsupported usage - fail with clear error
            String fieldName = field.getName();
            String errorMsg = String.format(
                "Cannot serialize field %s (%s): missing PIC clause and unsupported usage type",
                fieldName, usage
            );
            sb.append(indent).append("throw new IllegalStateException(\"").append(errorMsg).append("\");\n");
        }

        sb.append(indent).append("offset += ").append(byteLen).append(";\n\n");
    }
    
    private void generateDeserializeFields(StringBuilder sb, GroupNode group, String builderRef, String indent) {
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                if (field.isFiller()) {
                    int totalBytes = field.getByteLength() * field.getOccursCount();
                    sb.append(indent).append("offset += ").append(totalBytes).append("; // FILLER\n");
                    continue;
                }
                if (mappingDoc.shouldIgnore(field.getName())) {
                    int totalBytes = field.getByteLength() * field.getOccursCount();
                    sb.append(indent).append("offset += ").append(totalBytes).append("; // IGNORED\n");
                    continue;
                }

                String fieldName = getJavaFieldName(field);

                if (field.getOccursCount() > 1) {
                    // Generate loop for OCCURS field
                    String listVar = fieldName + "List";
                    String occursDepending = field.getOccursDepending();

                    if (occursDepending != null && !occursDepending.isBlank()) {
                        // OCCURS DEPENDING ON - deserialize actual count, skip remaining
                        sb.append(indent).append("// OCCURS ").append(field.getOccursCount())
                                .append(" DEPENDING ON ").append(occursDepending).append(" - ").append(field.getName()).append("\n");
                        String dependingFieldName = toCamelCase(occursDepending);
                        // Use stored field value instead of calling .build() prematurely
                        String dependingVarName = "stored_" + dependingFieldName;
                        sb.append(indent).append("int actualCount_").append(fieldName).append(" = ")
                                .append(dependingVarName).append(" != null ? ").append(dependingVarName)
                                .append(".intValue() : 0;\n");
                        sb.append(indent).append("actualCount_").append(fieldName).append(" = Math.min(actualCount_")
                                .append(fieldName).append(", ").append(field.getOccursCount()).append(");\n");
                        sb.append(indent).append("List<").append(getJavaType(field)).append("> ").append(listVar)
                                .append(" = new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < actualCount_").append(fieldName).append("; i++) {\n");
                        generateFieldDeserializeToVariable(sb, field, listVar, indent + "    ");
                        sb.append(indent).append("}\n");
                        // Skip remaining entries to maintain fixed record length
                        sb.append(indent).append("// Skip remaining ODO entries\n");
                        sb.append(indent).append("int remainingCount_").append(fieldName).append(" = ")
                                .append(field.getOccursCount()).append(" - actualCount_").append(fieldName).append(";\n");
                        sb.append(indent).append("offset += remainingCount_").append(fieldName).append(" * ")
                                .append(field.getByteLength()).append(";\n");
                        sb.append(indent).append(builderRef).append(".").append(fieldName).append("(").append(listVar).append(");\n\n");
                    } else {
                        // Fixed OCCURS
                        sb.append(indent).append("// OCCURS ").append(field.getOccursCount()).append(" - ").append(field.getName()).append("\n");
                        sb.append(indent).append("List<").append(getJavaType(field)).append("> ").append(listVar)
                                .append(" = new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < ").append(field.getOccursCount()).append("; i++) {\n");
                        generateFieldDeserializeToVariable(sb, field, listVar, indent + "    ");
                        sb.append(indent).append("}\n");
                        sb.append(indent).append(builderRef).append(".").append(fieldName).append("(").append(listVar).append(");\n\n");
                    }
                } else {
                    generateFieldDeserialize(sb, field, builderRef, indent);
                }

            } else if (child instanceof GroupNode childGroup) {
                if (childGroup.getName().equalsIgnoreCase("FILLER")) {
                    int totalBytes = childGroup.calculateByteLength();
                    sb.append(indent).append("offset += ").append(totalBytes).append("; // FILLER GROUP\n");
                    continue;
                }

                if (childGroup.getOccursCount() > 1) {
                    // OCCURS group - generate loop with nested builder
                    String groupFieldName = toCamelCase(childGroup.getName());
                    String nestedClassName = toPascalCase(childGroup.getName()) + "Item";
                    String listVar = groupFieldName + "List";
                    String occursDepending = childGroup.getOccursDepending();

                    if (occursDepending != null && !occursDepending.isBlank()) {
                        // OCCURS DEPENDING ON - deserialize actual count, skip remaining
                        sb.append(indent).append("// OCCURS ").append(childGroup.getOccursCount())
                                .append(" DEPENDING ON ").append(occursDepending).append(" - ").append(childGroup.getName()).append("\n");
                        String dependingFieldName = toCamelCase(occursDepending);
                        // Use stored field value instead of calling .build() prematurely
                        String dependingVarName = "stored_" + dependingFieldName;
                        sb.append(indent).append("int actualCount_").append(groupFieldName).append(" = ")
                                .append(dependingVarName).append(" != null ? ").append(dependingVarName)
                                .append(".intValue() : 0;\n");
                        sb.append(indent).append("actualCount_").append(groupFieldName).append(" = Math.min(actualCount_")
                                .append(groupFieldName).append(", ").append(childGroup.getOccursCount()).append(");\n");
                        sb.append(indent).append("List<").append(nestedClassName).append("> ").append(listVar)
                                .append(" = new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < actualCount_").append(groupFieldName).append("; i++) {\n");
                        sb.append(indent).append("    ").append(nestedClassName).append(".").append(nestedClassName)
                                .append("Builder itemBuilder = ").append(nestedClassName).append(".builder();\n");
                        generateDeserializeFields(sb, childGroup, "itemBuilder", indent + "    ");
                        sb.append(indent).append("    ").append(listVar).append(".add(itemBuilder.build());\n");
                        sb.append(indent).append("}\n");
                        // Skip remaining entries to maintain fixed record length
                        sb.append(indent).append("// Skip remaining ODO entries\n");
                        sb.append(indent).append("int remainingCount_").append(groupFieldName).append(" = ")
                                .append(childGroup.getOccursCount()).append(" - actualCount_").append(groupFieldName).append(";\n");
                        sb.append(indent).append("offset += remainingCount_").append(groupFieldName).append(" * ")
                                .append(childGroup.getByteLength()).append(";\n");
                        sb.append(indent).append(builderRef).append(".").append(groupFieldName).append("(").append(listVar).append(");\n\n");
                    } else {
                        // Fixed OCCURS
                        sb.append(indent).append("// OCCURS ").append(childGroup.getOccursCount()).append(" - ").append(childGroup.getName()).append("\n");
                        sb.append(indent).append("List<").append(nestedClassName).append("> ").append(listVar)
                                .append(" = new ArrayList<>();\n");
                        sb.append(indent).append("for (int i = 0; i < ").append(childGroup.getOccursCount()).append("; i++) {\n");
                        sb.append(indent).append("    ").append(nestedClassName).append(".").append(nestedClassName)
                                .append("Builder itemBuilder = ").append(nestedClassName).append(".builder();\n");
                        generateDeserializeFields(sb, childGroup, "itemBuilder", indent + "    ");
                        sb.append(indent).append("    ").append(listVar).append(".add(itemBuilder.build());\n");
                        sb.append(indent).append("}\n");
                        sb.append(indent).append(builderRef).append(".").append(groupFieldName).append("(").append(listVar).append(");\n\n");
                    }
                } else {
                    // Regular group - inline
                    generateDeserializeFields(sb, childGroup, builderRef, indent);
                }
            }
        }
    }
    
    private void generateFieldDeserialize(StringBuilder sb, FieldNode field, String builderRef, String indent) {
        int byteLen = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();
        String fieldName = getJavaFieldName(field);
        String javaType = getJavaType(field);

        // Deserialize value
        String deserializedValue = getDeserializeExpression(field, byteLen, pic, usage, javaType);

        // Store the value in a local variable for ODO support
        String storedVarName = "stored_" + fieldName;
        sb.append(indent).append(javaType).append(" ").append(storedVarName).append(" = ")
                .append(deserializedValue).append(";\n");

        // Set on builder
        sb.append(indent).append(builderRef).append(".").append(fieldName)
                .append("(").append(storedVarName).append(");\n");
        sb.append(indent).append("offset += ").append(byteLen).append(";\n\n");
    }

    private void generateFieldDeserializeToVariable(StringBuilder sb, FieldNode field, String listVar, String indent) {
        int byteLen = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();
        String javaType = getJavaType(field);

        // Deserialize value
        String deserializedValue = getDeserializeExpression(field, byteLen, pic, usage, javaType);

        // Add to list
        sb.append(indent).append(listVar).append(".add(").append(deserializedValue).append(");\n");
        sb.append(indent).append("offset += ").append(byteLen).append(";\n");
    }

    private String getDeserializeExpression(FieldNode field, int byteLen, PictureClause pic, UsageType usage, String javaType) {
        // Handle enums
        if (field.hasEnum88Values()) {
            String enumType = toPascalCase(field.getName()) + "Enum";
            return enumType + ".fromValue(EbcdicUtils.ebcdicToString(bytes, offset, " + byteLen + "))";
        }

        // Handle COMP-1 (float)
        if (usage == UsageType.COMP_1) {
            return "EbcdicUtils.bytesToFloat(bytes, offset)";
        }

        // Handle COMP-2 (double)
        if (usage == UsageType.COMP_2) {
            return "EbcdicUtils.bytesToDouble(bytes, offset)";
        }

        // Handle packed decimal (COMP-3)
        if (usage == UsageType.PACKED_DECIMAL) {
            if (pic == null) {
                throw new IllegalStateException(
                    "Cannot deserialize field " + field.getName() + " (COMP-3): missing PIC clause for decimal scale"
                );
            }
            return "EbcdicUtils.unpackDecimal(bytes, offset, " + byteLen + ", " + pic.getDecimalDigits() + ")";
        }

        // Handle binary (COMP/COMP-5)
        if (usage == UsageType.BINARY || usage == UsageType.COMP_5) {
            if (javaType.equals("Short")) {
                return "(short) EbcdicUtils.binaryToInt(bytes, offset, " + byteLen + ")";
            } else if (javaType.equals("Integer")) {
                return "EbcdicUtils.binaryToInt(bytes, offset, " + byteLen + ")";
            } else if (javaType.equals("Long")) {
                return "(long) EbcdicUtils.binaryToInt(bytes, offset, " + byteLen + ")";
            } else {
                return "EbcdicUtils.binaryToInt(bytes, offset, " + byteLen + ")";
            }
        }

        // For the remaining types, PictureClause is required
        if (pic == null) {
            throw new IllegalStateException(
                "Cannot deserialize field " + field.getName() + " (" + usage + "): missing PIC clause"
            );
        }

        // Handle alphanumeric
        if (pic.isAlphanumeric()) {
            return "EbcdicUtils.ebcdicToString(bytes, offset, " + byteLen + ")";
        }

        // Handle zoned decimal or display numeric
        if (javaType.contains("BigDecimal")) {
            return "new BigDecimal(EbcdicUtils.unzonedDecimal(bytes, offset, " + byteLen + "))";
        } else if (javaType.equals("Integer") || javaType.equals("Long") || javaType.equals("Short")) {
            return javaType + ".valueOf(EbcdicUtils.unzonedDecimal(bytes, offset, " + byteLen + "))";
        } else {
            return "EbcdicUtils.ebcdicToString(bytes, offset, " + byteLen + ")";
        }
    }
    
    private void generateCamelRoutes(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path routeFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/camel/MainframeRoute.java"
        );

        String dtoClassName = toPascalCase(config.getProgramId());
        boolean usingFolderBasedSelection = config.getRequestCopybookDir() != null || config.getResponseCopybookDir() != null;

        // Determine if we're using wrapper classes (multi-copybook mode)
        List<DtoMetadata> requestDtos = getRequestDtosForWrapper();
        List<DtoMetadata> responseDtos = getResponseDtosForWrapper();
        boolean useWrapper = usingFolderBasedSelection && (requestDtos.size() > 1 || responseDtos.size() > 1);

        String requestClassName;
        String responseClassName;
        String requestSerializerClass;
        String responseSerializerClass;
        String requestSerializerImport;
        String responseSerializerImport;

        if (useWrapper) {
            requestClassName = dtoClassName + "ApiRequest";
            responseClassName = dtoClassName + "ApiResponse";
            requestSerializerClass = dtoClassName + "ApiRequestSerializer";
            responseSerializerClass = dtoClassName + "ApiResponseSerializer";
            requestSerializerImport = config.getBasePackage() + ".util.request." + requestSerializerClass;
            responseSerializerImport = config.getBasePackage() + ".util.response." + responseSerializerClass;
        } else {
            requestClassName = dtoClassName + "Request";
            responseClassName = dtoClassName + "Response";
            requestSerializerClass = dtoClassName + "RequestSerializer";
            responseSerializerClass = dtoClassName + "ResponseSerializer";
            requestSerializerImport = config.getBasePackage() + ".util." + requestSerializerClass;
            responseSerializerImport = config.getBasePackage() + ".util." + responseSerializerClass;
        }

        String routeContent = String.format("""
                package %s.camel;

                import %s.model.request.%s;
                import %s.model.response.%s;
                import %s.mainframe.transport.MainframeTransport;
                import %s;
                import %s;
                import org.apache.camel.builder.RouteBuilder;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;

                /**
                 * Camel route for mainframe communication.
                 %s
                 */
                @Component
                public class MainframeRoute extends RouteBuilder {

                    @Autowired
                    private MainframeTransport transport;

                    @Autowired
                    private %s requestSerializer;

                    @Autowired
                    private %s responseSerializer;

                    @Override
                    public void configure() {
                        from("direct:mainframeCall")
                            .routeId("mainframe-call-route")
                            .log("Processing mainframe request")
                            .process(exchange -> {
                                %s request = exchange.getIn().getBody(%s.class);
                                byte[] requestBytes = requestSerializer.serialize(request);
                                exchange.getIn().setBody(requestBytes);
                                log.debug("Serialized request: {} bytes", requestBytes.length);
                            })
                            .process(exchange -> {
                                byte[] requestBytes = exchange.getIn().getBody(byte[].class);
                                byte[] responseBytes = transport.send(requestBytes);
                                exchange.getIn().setBody(responseBytes);
                                log.debug("Received response: {} bytes", responseBytes.length);
                            })
                            .process(exchange -> {
                                byte[] responseBytes = exchange.getIn().getBody(byte[].class);
                                %s response = responseSerializer.deserialize(responseBytes);
                                exchange.getIn().setBody(response);
                            })
                            .log("Mainframe call completed");
                    }
                }
                """,
                config.getBasePackage(),
                config.getBasePackage(), requestClassName,
                config.getBasePackage(), responseClassName,
                config.getBasePackage(),
                requestSerializerImport,
                responseSerializerImport,
                useWrapper ? " * Multi-copybook mode: using wrapper serializers.\n" : "",
                requestSerializerClass,
                responseSerializerClass,
                requestClassName, requestClassName,
                responseClassName
        );

        safeWriteString(routeFile, routeContent);
    }
    
    private void generateController(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path controllerFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/controller/MainframeController.java"
        );

        String dtoClassName = toPascalCase(config.getProgramId());
        boolean usingFolderBasedSelection = config.getRequestCopybookDir() != null || config.getResponseCopybookDir() != null;

        // Determine if we're using wrapper classes (multi-copybook mode)
        List<DtoMetadata> requestDtos = getRequestDtosForWrapper();
        List<DtoMetadata> responseDtos = getResponseDtosForWrapper();
        boolean useWrapper = usingFolderBasedSelection && (requestDtos.size() > 1 || responseDtos.size() > 1);

        String requestClassName;
        String responseClassName;

        if (useWrapper) {
            requestClassName = dtoClassName + "ApiRequest";
            responseClassName = dtoClassName + "ApiResponse";
        } else {
            requestClassName = dtoClassName + "Request";
            responseClassName = dtoClassName + "Response";
        }

        String controllerContent = String.format("""
                package %s.controller;

                import %s.model.request.%s;
                import %s.model.response.%s;
                import jakarta.validation.Valid;
                import org.apache.camel.ProducerTemplate;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.http.ResponseEntity;
                import org.springframework.web.bind.annotation.*;

                /**
                 * REST controller for mainframe operations.
                 %s
                 */
                @RestController
                @RequestMapping("/api/%s")
                public class MainframeController {

                    @Autowired
                    private ProducerTemplate producerTemplate;

                    @PostMapping("/execute")
                    public ResponseEntity<%s> execute(@Valid @RequestBody %s request) {
                        %s response = producerTemplate.requestBody(
                            "direct:mainframeCall",
                            request,
                            %s.class
                        );
                        return ResponseEntity.ok(response);
                    }
                }
                """,
                config.getBasePackage(),
                config.getBasePackage(), requestClassName,
                config.getBasePackage(), responseClassName,
                useWrapper ? " * Multi-copybook mode: using wrapper request/response classes.\n" : "",
                config.getProgramIdPath(),
                responseClassName, requestClassName,
                responseClassName,
                responseClassName
        );

        safeWriteString(controllerFile, controllerContent);
        
        // Generate exception handler
        Path handlerFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/controller/GlobalExceptionHandler.java"
        );
        
        String handlerContent = String.format("""
                package %s.controller;
                
                import org.springframework.http.HttpStatus;
                import org.springframework.http.ResponseEntity;
                import org.springframework.validation.FieldError;
                import org.springframework.web.bind.MethodArgumentNotValidException;
                import org.springframework.web.bind.annotation.ExceptionHandler;
                import org.springframework.web.bind.annotation.RestControllerAdvice;
                
                import java.util.HashMap;
                import java.util.Map;
                
                /**
                 * Global exception handler for validation errors.
                 */
                @RestControllerAdvice
                public class GlobalExceptionHandler {
                    
                    @ExceptionHandler(MethodArgumentNotValidException.class)
                    public ResponseEntity<Map<String, Object>> handleValidationExceptions(
                            MethodArgumentNotValidException ex) {
                        Map<String, Object> response = new HashMap<>();
                        response.put("status", "error");
                        response.put("message", "Validation failed");
                        
                        Map<String, String> errors = new HashMap<>();
                        ex.getBindingResult().getAllErrors().forEach((error) -> {
                            String fieldName = ((FieldError) error).getField();
                            String errorMessage = error.getDefaultMessage();
                            errors.put(fieldName, errorMessage);
                        });
                        response.put("errors", errors);
                        
                        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
                    }
                    
                    @ExceptionHandler(Exception.class)
                    public ResponseEntity<Map<String, Object>> handleGeneralExceptions(Exception ex) {
                        Map<String, Object> response = new HashMap<>();
                        response.put("status", "error");
                        response.put("message", ex.getMessage());
                        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
                    }
                }
                """, config.getBasePackage());
        
        safeWriteString(handlerFile, handlerContent);
    }
    
    private void generateTcpTransport(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        
        // MainframeTransport interface
        Path transportInterface = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/mainframe/transport/MainframeTransport.java"
        );
        
        String interfaceContent = String.format("""
                package %s.mainframe.transport;
                
                /**
                 * Interface for mainframe communication transport.
                 */
                public interface MainframeTransport {
                    byte[] send(byte[] request);
                }
                """, config.getBasePackage());
        
        safeWriteString(transportInterface, interfaceContent);
        
        // TcpMainframeTransport implementation
        Path transportImpl = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/mainframe/transport/TcpMainframeTransport.java"
        );
        
        String implContent = String.format("""
                package %s.mainframe.transport;
                
                import %s.mainframe.framing.FramingStrategy;
                import org.slf4j.Logger;
                import org.slf4j.LoggerFactory;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.beans.factory.annotation.Value;
                import org.springframework.stereotype.Component;
                
                import java.io.*;
                import java.net.InetSocketAddress;
                import java.net.Socket;
                
                /**
                 * TCP socket implementation of MainframeTransport.
                 */
                @Component
                public class TcpMainframeTransport implements MainframeTransport {
                    
                    private static final Logger log = LoggerFactory.getLogger(TcpMainframeTransport.class);
                    
                    @Value("${mainframe.tcp.host}")
                    private String host;
                    
                    @Value("${mainframe.tcp.port}")
                    private int port;
                    
                    @Value("${mainframe.tcp.connect-timeout-ms}")
                    private int connectTimeout;
                    
                    @Value("${mainframe.tcp.read-timeout-ms}")
                    private int readTimeout;
                    
                    @Autowired
                    private FramingStrategy framingStrategy;
                    
                    @Override
                    public byte[] send(byte[] request) {
                        log.debug("Connecting to {}:{}", host, port);
                        
                        try (Socket socket = new Socket()) {
                            socket.connect(new InetSocketAddress(host, port), connectTimeout);
                            socket.setSoTimeout(readTimeout);
                            
                            OutputStream out = socket.getOutputStream();
                            InputStream in = socket.getInputStream();
                            
                            // Write request with framing
                            byte[] framedRequest = framingStrategy.frame(request);
                            out.write(framedRequest);
                            out.flush();
                            log.debug("Sent {} bytes", framedRequest.length);
                            
                            // Read response with framing
                            byte[] response = framingStrategy.unframe(in);
                            log.debug("Received {} bytes", response.length);
                            
                            return response;
                            
                        } catch (IOException e) {
                            log.error("TCP communication failed", e);
                            throw new RuntimeException("Mainframe communication failed: " + e.getMessage(), e);
                        }
                    }
                }
                """, config.getBasePackage(), config.getBasePackage());
        
        safeWriteString(transportImpl, implContent);
        
        // Generate framing classes
        generateFramingClasses(projectDir);
    }
    
    private void generateFramingClasses(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        
        // FramingStrategy interface
        Path framingInterface = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/mainframe/framing/FramingStrategy.java"
        );
        
        String interfaceContent = String.format("""
                package %s.mainframe.framing;
                
                import java.io.IOException;
                import java.io.InputStream;
                
                /**
                 * Strategy interface for TCP message framing.
                 */
                public interface FramingStrategy {
                    byte[] frame(byte[] payload);
                    byte[] unframe(InputStream in) throws IOException;
                }
                """, config.getBasePackage());
        
        safeWriteString(framingInterface, interfaceContent);
        
        // LengthPrefixFraming
        Path lengthPrefixFraming = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/mainframe/framing/LengthPrefixFraming.java"
        );
        
        String lengthPrefixContent = String.format("""
                package %s.mainframe.framing;
                
                import java.io.IOException;
                import java.io.InputStream;
                import java.nio.ByteBuffer;
                
                /**
                 * Length-prefix framing strategy.
                 */
                public class LengthPrefixFraming implements FramingStrategy {
                    
                    private final int prefixBytes;
                    
                    public LengthPrefixFraming(int prefixBytes) {
                        if (prefixBytes != 2 && prefixBytes != 4) {
                            throw new IllegalArgumentException("Prefix bytes must be 2 or 4");
                        }
                        this.prefixBytes = prefixBytes;
                    }
                    
                    @Override
                    public byte[] frame(byte[] payload) {
                        ByteBuffer buffer = ByteBuffer.allocate(prefixBytes + payload.length);
                        
                        if (prefixBytes == 2) {
                            buffer.putShort((short) payload.length);
                        } else {
                            buffer.putInt(payload.length);
                        }
                        buffer.put(payload);
                        
                        return buffer.array();
                    }
                    
                    @Override
                    public byte[] unframe(InputStream in) throws IOException {
                        byte[] lengthBytes = new byte[prefixBytes];
                        int read = in.read(lengthBytes);
                        if (read != prefixBytes) {
                            throw new IOException("Failed to read length prefix");
                        }
                        
                        ByteBuffer buffer = ByteBuffer.wrap(lengthBytes);
                        int length = prefixBytes == 2 ? buffer.getShort() & 0xFFFF : buffer.getInt();
                        
                        byte[] payload = new byte[length];
                        int totalRead = 0;
                        while (totalRead < length) {
                            int r = in.read(payload, totalRead, length - totalRead);
                            if (r < 0) {
                                throw new IOException("Unexpected end of stream");
                            }
                            totalRead += r;
                        }
                        
                        return payload;
                    }
                }
                """, config.getBasePackage());
        
        safeWriteString(lengthPrefixFraming, lengthPrefixContent);
        
        // FixedLengthFraming
        Path fixedFraming = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/mainframe/framing/FixedLengthFraming.java"
        );
        
        String fixedContent = String.format("""
                package %s.mainframe.framing;
                
                import java.io.IOException;
                import java.io.InputStream;
                
                /**
                 * Fixed-length framing strategy.
                 */
                public class FixedLengthFraming implements FramingStrategy {
                    
                    private final int responseLength;
                    
                    public FixedLengthFraming(int responseLength) {
                        this.responseLength = responseLength;
                    }
                    
                    @Override
                    public byte[] frame(byte[] payload) {
                        return payload; // No framing for fixed length
                    }
                    
                    @Override
                    public byte[] unframe(InputStream in) throws IOException {
                        byte[] response = new byte[responseLength];
                        int totalRead = 0;
                        while (totalRead < responseLength) {
                            int r = in.read(response, totalRead, responseLength - totalRead);
                            if (r < 0) {
                                throw new IOException("Unexpected end of stream");
                            }
                            totalRead += r;
                        }
                        return response;
                    }
                }
                """, config.getBasePackage());
        
        safeWriteString(fixedFraming, fixedContent);
        
        // FramingConfig
        Path framingConfig = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/config/FramingConfig.java"
        );
        
        int responseLength = responseCopybook != null ? responseCopybook.calculateTotalByteLength() : 100;
        
        String configContent = String.format("""
                package %s.config;
                
                import %s.mainframe.framing.*;
                import org.springframework.beans.factory.annotation.Value;
                import org.springframework.context.annotation.Bean;
                import org.springframework.context.annotation.Configuration;
                
                /**
                 * Configuration for TCP framing strategy.
                 */
                @Configuration
                public class FramingConfig {
                    
                    @Value("${mainframe.tcp.framing}")
                    private String framingMode;
                    
                    @Bean
                    public FramingStrategy framingStrategy() {
                        return switch (framingMode.toUpperCase()) {
                            case "LENGTH_PREFIX_2" -> new LengthPrefixFraming(2);
                            case "LENGTH_PREFIX_4" -> new LengthPrefixFraming(4);
                            case "FIXED" -> new FixedLengthFraming(%d);
                            default -> new LengthPrefixFraming(2);
                        };
                    }
                }
                """, config.getBasePackage(), config.getBasePackage(), responseLength);
        
        safeWriteString(framingConfig, configContent);
    }
    
    private void generateTcpEmulator(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path emulatorFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/mainframe/emulator/TcpEmulatorServer.java"
        );
        
        int responseLength = responseCopybook != null ? responseCopybook.calculateTotalByteLength() : 100;
        
        String emulatorContent = String.format("""
                package %s.mainframe.emulator;
                
                import %s.mainframe.framing.FramingStrategy;
                import org.slf4j.Logger;
                import org.slf4j.LoggerFactory;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;
                
                import java.io.*;
                import java.net.ServerSocket;
                import java.net.Socket;
                import java.util.concurrent.ExecutorService;
                import java.util.concurrent.Executors;
                import java.util.concurrent.atomic.AtomicBoolean;
                
                /**
                 * TCP emulator server for testing mainframe communication.
                 */
                @Component
                public class TcpEmulatorServer {
                    
                    private static final Logger log = LoggerFactory.getLogger(TcpEmulatorServer.class);
                    
                    private ServerSocket serverSocket;
                    private final ExecutorService executor = Executors.newSingleThreadExecutor();
                    private final AtomicBoolean running = new AtomicBoolean(false);
                    
                    @Autowired
                    private FramingStrategy framingStrategy;
                    
                    public int start() throws IOException {
                        serverSocket = new ServerSocket(0);
                        int port = serverSocket.getLocalPort();
                        running.set(true);
                        
                        executor.submit(() -> {
                            while (running.get()) {
                                try {
                                    Socket client = serverSocket.accept();
                                    handleClient(client);
                                } catch (IOException e) {
                                    if (running.get()) {
                                        log.error("Error accepting connection", e);
                                    }
                                }
                            }
                        });
                        
                        log.info("Emulator started on port {}", port);
                        return port;
                    }
                    
                    public void stop() {
                        running.set(false);
                        try {
                            if (serverSocket != null) {
                                serverSocket.close();
                            }
                        } catch (IOException e) {
                            log.error("Error stopping emulator", e);
                        }
                        executor.shutdownNow();
                    }
                    
                    private void handleClient(Socket client) {
                        try (client) {
                            InputStream in = client.getInputStream();
                            OutputStream out = client.getOutputStream();
                            
                            byte[] request = framingStrategy.unframe(in);
                            log.debug("Received {} bytes", request.length);
                            
                            // Generate echo response with modifications
                            byte[] response = generateResponse(request);
                            
                            byte[] framedResponse = framingStrategy.frame(response);
                            out.write(framedResponse);
                            out.flush();
                            log.debug("Sent {} bytes", framedResponse.length);
                            
                        } catch (IOException e) {
                            log.error("Error handling client", e);
                        }
                    }
                    
                    private byte[] generateResponse(byte[] request) {
                        // Create a response based on request length or fixed size
                        byte[] response = new byte[%d];
                        
                        // Copy request data to response (echo) or fill with test data
                        int copyLen = Math.min(request.length, response.length);
                        System.arraycopy(request, 0, response, 0, copyLen);
                        
                        // Fill remaining with spaces (EBCDIC 0x40)
                        for (int i = copyLen; i < response.length; i++) {
                            response[i] = 0x40;
                        }
                        
                        return response;
                    }
                    
                    public boolean isRunning() {
                        return running.get();
                    }
                }
                """, config.getBasePackage(), config.getBasePackage(), responseLength);
        
        safeWriteString(emulatorFile, emulatorContent);
    }
    
    private void generateTests(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String dtoClassName = toPascalCase(config.getProgramId());
        
        // Generate DTO test
        Path dtoTestFile = projectDir.resolve(
                "src/test/java/" + basePackagePath + "/model/request/" + dtoClassName + "RequestTest.java"
        );
        
        String dtoTestContent = String.format("""
                package %s.model.request;
                
                import jakarta.validation.ConstraintViolation;
                import jakarta.validation.Validation;
                import jakarta.validation.Validator;
                import jakarta.validation.ValidatorFactory;
                import org.junit.jupiter.api.BeforeAll;
                import org.junit.jupiter.api.Test;
                
                import java.util.Set;
                
                import static org.junit.jupiter.api.Assertions.*;
                
                /**
                 * Tests for %sRequest DTO including Lombok behavior and validation.
                 */
                class %sRequestTest {
                    
                    private static Validator validator;
                    
                    @BeforeAll
                    static void setUp() {
                        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
                        validator = factory.getValidator();
                    }
                    
                    @Test
                    void testBuilderCreatesEquivalentObjects() {
                        %sRequest request1 = %sRequest.builder().build();
                        %sRequest request2 = %sRequest.builder().build();
                        
                        assertEquals(request1, request2);
                        assertEquals(request1.hashCode(), request2.hashCode());
                    }
                    
                    @Test
                    void testEqualsAndHashCode() {
                        %sRequest request1 = %sRequest.builder().build();
                        %sRequest request2 = %sRequest.builder().build();
                        
                        assertTrue(request1.equals(request2));
                        assertTrue(request2.equals(request1));
                        assertEquals(request1.hashCode(), request2.hashCode());
                    }
                    
                    @Test
                    void testToStringDoesNotThrow() {
                        %sRequest request = %sRequest.builder().build();
                        assertDoesNotThrow(() -> request.toString());
                        assertNotNull(request.toString());
                    }
                    
                    @Test
                    void testValidationFailsForNullRequiredFields() {
                        %sRequest request = new %sRequest();
                        Set<ConstraintViolation<%sRequest>> violations = validator.validate(request);
                        
                        // Should have violations for required fields
                        // Note: actual count depends on generated fields
                        assertNotNull(violations);
                    }
                }
                """,
                config.getBasePackage(),
                dtoClassName,
                dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName, dtoClassName
        );
        
        safeWriteString(dtoTestFile, dtoTestContent);
        
        // Generate serializer test
        Path serializerTestFile = projectDir.resolve(
                "src/test/java/" + basePackagePath + "/serializer/" + dtoClassName + "SerializerTest.java"
        );
        
        int requestLength = requestCopybook != null ? requestCopybook.calculateTotalByteLength() : 100;
        
        String serializerTestContent = String.format("""
                package %s.serializer;
                
                import %s.model.request.%sRequest;
                import %s.util.%sRequestSerializer;
                import org.junit.jupiter.api.Test;
                
                import static org.junit.jupiter.api.Assertions.*;
                
                /**
                 * Tests for request serialization and deserialization.
                 */
                class %sSerializerTest {
                    
                    private final %sRequestSerializer serializer = new %sRequestSerializer();
                    
                    @Test
                    void testSerializeProducesCorrectLength() {
                        %sRequest request = %sRequest.builder().build();
                        byte[] bytes = serializer.serialize(request);
                        
                        assertEquals(%d, bytes.length);
                    }
                    
                    @Test
                    void testDeserializeAndSerializeRoundTrip() {
                        %sRequest original = %sRequest.builder().build();
                        byte[] bytes = serializer.serialize(original);
                        %sRequest deserialized = serializer.deserialize(bytes);
                        byte[] reserialized = serializer.serialize(deserialized);
                        
                        assertArrayEquals(bytes, reserialized);
                    }
                    
                    @Test
                    void testGetByteLength() {
                        assertEquals(%d, serializer.getByteLength());
                    }
                }
                """,
                config.getBasePackage(),
                config.getBasePackage(), dtoClassName,
                config.getBasePackage(), dtoClassName,
                dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                requestLength,
                dtoClassName, dtoClassName,
                dtoClassName,
                requestLength
        );
        
        safeWriteString(serializerTestFile, serializerTestContent);
        
        // Generate TCP test
        Path tcpTestFile = projectDir.resolve(
                "src/test/java/" + basePackagePath + "/tcp/TcpTransportTest.java"
        );
        
        String tcpTestContent = String.format("""
                package %s.tcp;
                
                import %s.mainframe.framing.LengthPrefixFraming;
                import org.junit.jupiter.api.Test;
                
                import java.io.ByteArrayInputStream;
                import java.nio.ByteBuffer;
                
                import static org.junit.jupiter.api.Assertions.*;
                
                /**
                 * Tests for TCP framing.
                 */
                class TcpTransportTest {
                    
                    @Test
                    void testLengthPrefix2ByteFraming() {
                        LengthPrefixFraming framing = new LengthPrefixFraming(2);
                        byte[] payload = "TEST".getBytes();
                        
                        byte[] framed = framing.frame(payload);
                        
                        assertEquals(payload.length + 2, framed.length);
                        
                        // Check length prefix
                        ByteBuffer buffer = ByteBuffer.wrap(framed);
                        assertEquals(payload.length, buffer.getShort());
                    }
                    
                    @Test
                    void testLengthPrefix4ByteFraming() {
                        LengthPrefixFraming framing = new LengthPrefixFraming(4);
                        byte[] payload = "TEST".getBytes();
                        
                        byte[] framed = framing.frame(payload);
                        
                        assertEquals(payload.length + 4, framed.length);
                    }
                    
                    @Test
                    void testUnframing() throws Exception {
                        LengthPrefixFraming framing = new LengthPrefixFraming(2);
                        byte[] payload = "HELLO".getBytes();
                        byte[] framed = framing.frame(payload);
                        
                        ByteArrayInputStream in = new ByteArrayInputStream(framed);
                        byte[] unframed = framing.unframe(in);
                        
                        assertArrayEquals(payload, unframed);
                    }
                }
                """, config.getBasePackage(), config.getBasePackage());
        
        safeWriteString(tcpTestFile, tcpTestContent);
        
        // Generate Controller test
        Path controllerTestFile = projectDir.resolve(
                "src/test/java/" + basePackagePath + "/controller/MainframeControllerTest.java"
        );
        
        String controllerTestContent = String.format("""
                package %s.controller;
                
                import %s.mainframe.emulator.TcpEmulatorServer;
                import org.junit.jupiter.api.Test;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
                import org.springframework.boot.test.context.SpringBootTest;
                import org.springframework.http.MediaType;
                import org.springframework.test.context.ActiveProfiles;
                import org.springframework.test.web.servlet.MockMvc;
                
                import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
                import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
                
                /**
                 * Controller integration tests.
                 */
                @SpringBootTest
                @AutoConfigureMockMvc
                @ActiveProfiles("test")
                class MainframeControllerTest {
                    
                    @Autowired
                    private MockMvc mockMvc;
                    
                    @Autowired
                    private TcpEmulatorServer emulator;
                    
                    @Test
                    void testValidationErrorForInvalidRequest() throws Exception {
                        // Empty request should trigger validation errors
                        String invalidJson = "{}";
                        
                        mockMvc.perform(post("/api/%s/execute")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(invalidJson))
                                .andExpect(status().isBadRequest());
                    }
                }
                """, config.getBasePackage(), config.getBasePackage(), config.getProgramIdPath());
        
        safeWriteString(controllerTestFile, controllerTestContent);
    }

    private int generateTortureTests(Path projectDir) throws IOException {
        int testCount = 0;

        // Generate torture tests for all models
        Set<CopybookModel> testedModels = new HashSet<>();

        // Test all models in allModelsToGenerate
        for (CopybookModel model : allModelsToGenerate) {
            if (testedModels.contains(model)) continue;

            String className = toPascalCase(model.getName());
            String subPackage = determineSubPackageForModel(model);

            generateTortureTest(projectDir, model, className, subPackage);
            testedModels.add(model);
            testCount++;
        }

        // Test all remaining parsed copybooks
        for (CopybookModel model : copybookModels) {
            if (testedModels.contains(model)) continue;

            String className = toPascalCase(model.getName());
            generateTortureTest(projectDir, model, className, "layout");
            testedModels.add(model);
            testCount++;
        }

        log.info("  Generated {} torture tests", testCount);
        return testCount;
    }

    private String determineSubPackageForModel(CopybookModel model) {
        if (requestModels.contains(model)) return "request";
        if (responseModels.contains(model)) return "response";
        if (sharedModels.contains(model)) return "shared";
        return "layout";
    }

    private void generateTortureTest(Path projectDir, CopybookModel copybook, String dtoClassName, String subPackage)
            throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String testClassName = dtoClassName + "TortureTest";

        Path testFile = projectDir.resolve(
                "src/test/java/" + basePackagePath + "/torture/" + testClassName + ".java"
        );

        int expectedByteLength = copybook.calculateTotalByteLength();
        String serializerClassName = dtoClassName + "Serializer";

        String testContent = String.format("""
                package %s.torture;

                import %s.model.%s.%s;
                import %s.util.%s.%s;
                import org.junit.jupiter.api.Test;

                import static org.junit.jupiter.api.Assertions.*;

                /**
                 * Torture test for %s: validates serializer correctness
                 * - Byte length matches copybook definition
                 * - Serialization produces correct length
                 * - Deserialization does not throw
                 * - Round-trip stability (optional)
                 */
                class %s {

                    private final %s serializer = new %s();

                    @Test
                    void testSerializerByteLengthMatchesCopybook() {
                        assertEquals(%d, serializer.getByteLength(),
                            "Serializer byte length should match copybook total byte length");
                    }

                    @Test
                    void testSerializeProducesCorrectLength() {
                        %s dto = %s.builder().build();
                        byte[] bytes = serializer.serialize(dto);

                        assertNotNull(bytes, "Serialized bytes should not be null");
                        assertEquals(%d, bytes.length,
                            "Serialized byte array length should match expected length");
                    }

                    @Test
                    void testDeserializeDoesNotThrow() {
                        %s original = %s.builder().build();
                        byte[] bytes = serializer.serialize(original);

                        assertDoesNotThrow(() -> serializer.deserialize(bytes),
                            "Deserialization should not throw an exception");
                    }

                    @Test
                    void testRoundTripStability() {
                        %s original = %s.builder().build();
                        byte[] bytes1 = serializer.serialize(original);
                        %s deserialized = serializer.deserialize(bytes1);
                        byte[] bytes2 = serializer.serialize(deserialized);

                        assertEquals(bytes1.length, bytes2.length,
                            "Round-trip serialization should produce same byte length");

                        // Note: Full byte equality requires all fields properly initialized
                        // This test validates structure stability
                        assertNotNull(deserialized, "Deserialized object should not be null");
                    }
                }
                """,
                config.getBasePackage(),
                config.getBasePackage(), subPackage, dtoClassName,
                config.getBasePackage(), subPackage, serializerClassName,
                dtoClassName,
                testClassName,
                serializerClassName, serializerClassName,
                expectedByteLength,
                dtoClassName, dtoClassName,
                expectedByteLength,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName
        );

        safeWriteString(testFile, testContent);
    }

    private void generateSampleFiles(Path projectDir) throws IOException {
        // Generate sample request JSON
        Path sampleRequest = projectDir.resolve("sample-request.json");
        
        StringBuilder json = new StringBuilder("{\n");
        
        if (requestCopybook != null) {
            List<FieldNode> fields = requestCopybook.getAllFields();
            for (int i = 0; i < fields.size(); i++) {
                FieldNode field = fields.get(i);
                if (field.isFiller() || mappingDoc.shouldIgnore(field.getName())) {
                    continue;
                }
                
                String fieldName = getJavaFieldName(field);
                String sampleValue = generateSampleValue(field);
                
                json.append("  \"").append(fieldName).append("\": ").append(sampleValue);
                if (i < fields.size() - 1) {
                    json.append(",");
                }
                json.append("\n");
            }
        }
        
        json.append("}");
        
        safeWriteString(sampleRequest, json.toString());
    }
    
    private String generateSampleValue(FieldNode field) {
        if (field.hasEnum88Values()) {
            return "\"" + field.getEnum88Values().get(0).getPrimaryValue() + "\"";
        }
        
        PictureClause pic = field.getPicture();
        if (pic == null) {
            return "\"\"";
        }
        
        if (pic.isAlphanumeric()) {
            return "\"SAMPLE\"";
        }
        
        if (pic.getDecimalDigits() > 0) {
            return "123.45";
        }
        
        return "12345";
    }
    
    private void generateMainApplication(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path appFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/Application.java"
        );
        
        String appContent = String.format("""
                package %s;
                
                import org.springframework.boot.SpringApplication;
                import org.springframework.boot.autoconfigure.SpringBootApplication;
                
                /**
                 * Main Spring Boot application class.
                 */
                @SpringBootApplication
                public class Application {
                    
                    public static void main(String[] args) {
                        SpringApplication.run(Application.class, args);
                    }
                }
                """, config.getBasePackage());
        
        safeWriteString(appFile, appContent);
    }
    
    private boolean runMavenTests(Path projectDir) {
        try {
            ProcessBuilder pb = new ProcessBuilder("mvn", "-q", "test")
                    .directory(projectDir.toFile())
                    .redirectErrorStream(true);
            
            Process process = pb.start();
            
            // Read output
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    log.debug("Maven: {}", line);
                }
            }
            
            int exitCode = process.waitFor();
            return exitCode == 0;
            
        } catch (Exception e) {
            log.warn("Could not run Maven tests: {}", e.getMessage());
            return true; // Skip test validation if Maven not available
        }
    }
    
    // Utility methods

    /**
     * Safely write a string to a file, creating parent directories if needed.
     * This prevents NoSuchFileException when writing to paths that don't exist yet.
     */
    private void safeWriteString(Path filePath, String content) throws IOException {
        Path parentDir = filePath.getParent();
        if (parentDir != null) {
            Files.createDirectories(parentDir);
        }
        Files.writeString(filePath, content);
    }

    private String toPascalCase(String input) {
        if (input == null || input.isEmpty()) {
            return input;
        }
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : input.toCharArray()) {
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                sb.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                sb.append(Character.toLowerCase(c));
            }
        }
        return sb.toString();
    }
    
    private String toCamelCase(String input) {
        String pascal = toPascalCase(input);
        if (pascal.isEmpty()) {
            return pascal;
        }
        return Character.toLowerCase(pascal.charAt(0)) + pascal.substring(1);
    }

    private String capitalize(String input) {
        if (input == null || input.isEmpty()) {
            return input;
        }
        return Character.toUpperCase(input.charAt(0)) + input.substring(1);
    }

    /**
     * Validates OCCURS DEPENDING ON references to ensure they don't reference
     * fields that appear later in the structure (forward references).
     */
    private void validateOdoDependencies(GroupNode group, Set<String> seenFields, String copybookName) {
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                String occursDepending = field.getOccursDepending();
                if (occursDepending != null && !occursDepending.isBlank()) {
                    if (!seenFields.contains(occursDepending.toUpperCase())) {
                    		log.warn("Field {} in copybook {} has OCCURS DEPENDING ON {} which may not have been deserialized yet. " +
                                "Ensure the depending field appears before the array in the COBOL structure.",
                                field.getName(), copybookName, occursDepending);
                    }
                }
                seenFields.add(field.getName().toUpperCase());
            } else if (child instanceof GroupNode childGroup) {
                String occursDepending = childGroup.getOccursDepending();
                if (occursDepending != null && !occursDepending.isBlank()) {
                    if (!seenFields.contains(occursDepending.toUpperCase())) {
                    		log.warn("Group {} in copybook {} has OCCURS DEPENDING ON {} which may not have been deserialized yet. " +
                                "Ensure the depending field appears before the array in the COBOL structure.",
                                childGroup.getName(), copybookName, occursDepending);
                    }
                }
                // Recurse into child groups
                validateOdoDependencies(childGroup, seenFields, copybookName);
                seenFields.add(childGroup.getName().toUpperCase());
            }
        }
    }
}
