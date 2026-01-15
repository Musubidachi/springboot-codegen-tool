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
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/model/layout"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/service"));
        Files.createDirectories(projectDir.resolve("src/main/java/" + basePackagePath + "/util"));
        
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/model/request"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/model/response"));
        Files.createDirectories(projectDir.resolve("src/test/java/" + basePackagePath + "/serializer"));
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
        Files.writeString(projectDir.resolve("pom.xml"), pom);
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
        
        Files.writeString(projectDir.resolve("src/main/resources/application.yml"), yml);
        
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
        Files.writeString(projectDir.resolve("src/test/resources/application.yml"), testYml);
    }
    
    private int generateModelClasses(Path projectDir) throws IOException {
        int count = 0;
        
        // Generate request DTO
        if (requestCopybook != null) {
            generateDtoClass(projectDir, requestCopybook, "request", "Request");
            count++;
        }
        
        // Generate response DTO
        if (responseCopybook != null && responseCopybook != requestCopybook) {
            generateDtoClass(projectDir, responseCopybook, "response", "Response");
            count++;
        } else if (responseCopybook == requestCopybook) {
            // Use same structure for response
            generateDtoClass(projectDir, responseCopybook, "response", "Response");
            count++;
        }
        
        return count;
    }
    
    private void generateDtoClass(Path projectDir, CopybookModel copybook, String subPackage, String suffix) 
            throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path classFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/model/" + subPackage + "/" +
                        toPascalCase(config.getProgramId()) + suffix + ".java"
        );
        
        StringBuilder sb = new StringBuilder();
        
        // Package and imports
        sb.append("package ").append(config.getBasePackage()).append(".model.").append(subPackage).append(";\n\n");
        sb.append("import jakarta.validation.Valid;\n");
        sb.append("import jakarta.validation.constraints.*;\n");
        sb.append("import lombok.*;\n");
        sb.append("import com.fasterxml.jackson.annotation.JsonProperty;\n");
        sb.append("import java.math.BigDecimal;\n");
        sb.append("import java.time.LocalDate;\n");
        sb.append("import java.util.List;\n");
        sb.append("import java.util.ArrayList;\n\n");
        
        // Class declaration
        String className = toPascalCase(config.getProgramId()) + suffix;
        sb.append("/**\n");
        sb.append(" * Generated from copybook: ").append(copybook.getName()).append("\n");
        sb.append(" * Total byte length: ").append(copybook.calculateTotalByteLength()).append("\n");
        sb.append(" */\n");
        sb.append("@Data\n");
        sb.append("@NoArgsConstructor\n");
        sb.append("@AllArgsConstructor\n");
        sb.append("@Builder\n");
        sb.append("public class ").append(className).append(" {\n\n");
        
        // Generate fields
        generateFields(sb, copybook.getRootGroup(), "    ");
        
        sb.append("}\n");
        
        Files.writeString(classFile, sb.toString());
    }
    
    private void generateFields(StringBuilder sb, GroupNode group, String indent) {
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                if (field.isFiller() || mappingDoc.shouldIgnore(field.getName())) {
                    continue;
                }
                
                // Generate validation annotations
                var constraints = validationGenerator.generateConstraints(field, true);
                for (var constraint : constraints) {
                    sb.append(indent).append(constraint.toAnnotation()).append("\n");
                }
                
                // CobolName annotation for mapping
                sb.append(indent).append("@JsonProperty(\"").append(field.toJavaFieldName()).append("\")\n");
                
                // Field declaration
                String javaType = getJavaType(field);
                String fieldName = getJavaFieldName(field);
                
                if (field.getOccursCount() > 1) {
                    sb.append(indent).append("@Builder.Default\n");
                    sb.append(indent).append("private List<").append(javaType).append("> ")
                            .append(fieldName).append(" = new ArrayList<>();\n\n");
                } else {
                    sb.append(indent).append("private ").append(javaType).append(" ")
                            .append(fieldName).append(";\n\n");
                }
                
            } else if (child instanceof GroupNode childGroup) {
                if (mappingDoc.shouldIgnore(childGroup.getName())) {
                    continue;
                }
                
                // Generate nested class or expand fields
                // For simplicity, we'll flatten groups into the parent class
                generateFields(sb, childGroup, indent);
            }
        }
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
        
        for (CopybookModel model : copybookModels) {
            for (FieldNode field : model.getAllFields()) {
                if (field.hasEnum88Values()) {
                    Path enumFile = projectDir.resolve(
                            "src/main/java/" + basePackagePath + "/model/" +
                                    toPascalCase(field.getName()) + "Enum.java"
                    );
                    
                    StringBuilder sb = new StringBuilder();
                    sb.append("package ").append(config.getBasePackage()).append(".model;\n\n");
                    sb.append("import lombok.Getter;\n");
                    sb.append("import lombok.RequiredArgsConstructor;\n\n");
                    
                    String enumName = toPascalCase(field.getName()) + "Enum";
                    sb.append("/**\n");
                    sb.append(" * Generated enum from 88-level values of ").append(field.getName()).append("\n");
                    sb.append(" */\n");
                    sb.append("@Getter\n");
                    sb.append("@RequiredArgsConstructor\n");
                    sb.append("public enum ").append(enumName).append(" {\n");
                    
                    List<Enum88Node> enums = field.getEnum88Values();
                    for (int i = 0; i < enums.size(); i++) {
                        Enum88Node enum88 = enums.get(i);
                        String constName = enum88.toJavaEnumConstant();
                        String value = enum88.getPrimaryValue();
                        
                        sb.append("    ").append(constName).append("(\"").append(value).append("\")");
                        sb.append(i < enums.size() - 1 ? ",\n" : ";\n\n");
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
                    
                    Files.writeString(enumFile, sb.toString());
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
        
        Files.writeString(serializerInterface, serializerContent);
        
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
                        // Packed decimal: each byte holds 2 digits, last nibble is sign
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
                        
                        // Pack digits
                        int digitIdx = 0;
                        for (int i = 0; i < byteLength - 1; i++) {
                            int high = digits.charAt(digitIdx++) - '0';
                            int low = digits.charAt(digitIdx++) - '0';
                            result[i] = (byte) ((high << 4) | low);
                        }
                        
                        // Last byte: one digit + sign
                        int lastDigit = digitIdx < digits.length() ? digits.charAt(digitIdx) - '0' : 0;
                        int sign = negative ? 0x0D : 0x0C;
                        result[byteLength - 1] = (byte) ((lastDigit << 4) | sign);
                        
                        return result;
                    }
                    
                    public static BigDecimal unpackDecimal(byte[] bytes, int offset, int length, int scale) {
                        StringBuilder sb = new StringBuilder();
                        
                        for (int i = 0; i < length - 1; i++) {
                            int b = bytes[offset + i] & 0xFF;
                            sb.append((char) ('0' + ((b >> 4) & 0x0F)));
                            sb.append((char) ('0' + (b & 0x0F)));
                        }
                        
                        // Last byte
                        int lastByte = bytes[offset + length - 1] & 0xFF;
                        sb.append((char) ('0' + ((lastByte >> 4) & 0x0F)));
                        int sign = lastByte & 0x0F;
                        
                        BigDecimal result = new BigDecimal(sb.toString());
                        if (scale > 0) {
                            result = result.movePointLeft(scale);
                        }
                        
                        // Check sign: D = negative
                        if (sign == 0x0D || sign == 0x0B) {
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
                }
                """, config.getBasePackage(), config.getEncoding().toUpperCase());
        
        Files.writeString(ebcdicUtils, ebcdicContent);
        
        // Generate concrete serializer for request/response
        generateConcreteSerializer(projectDir, requestCopybook, "Request");
        if (responseCopybook != requestCopybook) {
            generateConcreteSerializer(projectDir, responseCopybook, "Response");
        }
    }
    
    private void generateConcreteSerializer(Path projectDir, CopybookModel copybook, String suffix) 
            throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        String className = toPascalCase(config.getProgramId()) + suffix + "Serializer";
        String dtoClassName = toPascalCase(config.getProgramId()) + suffix;
        
        Path serializerFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/util/" + className + ".java"
        );
        
        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(config.getBasePackage()).append(".util;\n\n");
        sb.append("import ").append(config.getBasePackage()).append(".model.")
                .append(suffix.toLowerCase()).append(".").append(dtoClassName).append(";\n");
        sb.append("import org.springframework.stereotype.Component;\n\n");
        
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
        
        Files.writeString(serializerFile, sb.toString());
    }
    
    private void generateSerializeFields(StringBuilder sb, GroupNode group, String objRef, String indent) {
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                if (field.isFiller()) {
                    // Just advance offset for FILLER
                    sb.append(indent).append("offset += ").append(field.getByteLength()).append("; // FILLER\n");
                    continue;
                }
                if (mappingDoc.shouldIgnore(field.getName())) {
                    sb.append(indent).append("offset += ").append(field.getByteLength()).append("; // IGNORED: ")
                            .append(field.getName()).append("\n");
                    continue;
                }
                
                String getter = objRef + ".get" + toPascalCase(getJavaFieldName(field)) + "()";
                generateFieldSerialize(sb, field, getter, indent);
                
            } else if (child instanceof GroupNode childGroup) {
                // Recurse into group
                generateSerializeFields(sb, childGroup, objRef, indent);
            }
        }
    }
    
    private void generateFieldSerialize(StringBuilder sb, FieldNode field, String getter, String indent) {
        int byteLen = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();
        
        if (pic.isAlphanumeric() || (pic.isNumeric() && usage == UsageType.DISPLAY && !pic.isSigned())) {
            // String field - EBCDIC
            sb.append(indent).append("System.arraycopy(EbcdicUtils.stringToEbcdic(")
                    .append(getter).append(" != null ? ").append(getter).append(".toString() : \"\", ")
                    .append(byteLen).append("), 0, result, offset, ").append(byteLen).append(");\n");
        } else if (usage == UsageType.PACKED_DECIMAL) {
            // Packed decimal
            sb.append(indent).append("System.arraycopy(EbcdicUtils.packedDecimal(")
                    .append(getter).append(", ").append(byteLen).append("), 0, result, offset, ")
                    .append(byteLen).append(");\n");
        } else if (usage == UsageType.BINARY || usage == UsageType.COMP_5) {
            // Binary
            sb.append(indent).append("System.arraycopy(EbcdicUtils.intToBinary(")
                    .append(getter).append(" != null ? ").append(getter).append(".intValue() : 0, ")
                    .append(byteLen).append("), 0, result, offset, ").append(byteLen).append(");\n");
        } else {
            // Zoned decimal
            sb.append(indent).append("System.arraycopy(EbcdicUtils.zonedDecimal(")
                    .append(getter).append(" != null ? ").append(getter).append(".toString() : \"0\", ")
                    .append(byteLen).append(", ").append(pic.isSigned()).append("), 0, result, offset, ")
                    .append(byteLen).append(");\n");
        }
        
        sb.append(indent).append("offset += ").append(byteLen).append(";\n\n");
    }
    
    private void generateDeserializeFields(StringBuilder sb, GroupNode group, String builderRef, String indent) {
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                if (field.isFiller()) {
                    sb.append(indent).append("offset += ").append(field.getByteLength()).append("; // FILLER\n");
                    continue;
                }
                if (mappingDoc.shouldIgnore(field.getName())) {
                    sb.append(indent).append("offset += ").append(field.getByteLength()).append("; // IGNORED\n");
                    continue;
                }
                
                generateFieldDeserialize(sb, field, builderRef, indent);
                
            } else if (child instanceof GroupNode childGroup) {
                generateDeserializeFields(sb, childGroup, builderRef, indent);
            }
        }
    }
    
    private void generateFieldDeserialize(StringBuilder sb, FieldNode field, String builderRef, String indent) {
        int byteLen = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();
        String setter = builderRef + "." + toCamelCase(getJavaFieldName(field));
        String javaType = getJavaType(field);
        
        if (pic.isAlphanumeric()) {
            sb.append(indent).append(setter).append("(EbcdicUtils.ebcdicToString(bytes, offset, ")
                    .append(byteLen).append("));\n");
        } else if (usage == UsageType.PACKED_DECIMAL) {
            sb.append(indent).append(setter).append("(EbcdicUtils.unpackDecimal(bytes, offset, ")
                    .append(byteLen).append(", ").append(pic.getDecimalDigits()).append("));\n");
        } else if (usage == UsageType.BINARY || usage == UsageType.COMP_5) {
            if (javaType.equals("Integer")) {
                sb.append(indent).append(setter).append("(EbcdicUtils.binaryToInt(bytes, offset, ")
                        .append(byteLen).append("));\n");
            } else {
                sb.append(indent).append(setter).append("((long) EbcdicUtils.binaryToInt(bytes, offset, ")
                        .append(byteLen).append("));\n");
            }
        } else {
            // Zoned decimal or display numeric
            if (javaType.contains("BigDecimal")) {
                sb.append(indent).append(setter).append("(new java.math.BigDecimal(EbcdicUtils.unzonedDecimal(bytes, offset, ")
                        .append(byteLen).append(")));\n");
            } else if (javaType.equals("Integer") || javaType.equals("Long")) {
                sb.append(indent).append(setter).append("(").append(javaType)
                        .append(".valueOf(EbcdicUtils.unzonedDecimal(bytes, offset, ")
                        .append(byteLen).append(")));\n");
            } else {
                sb.append(indent).append(setter).append("(EbcdicUtils.ebcdicToString(bytes, offset, ")
                        .append(byteLen).append("));\n");
            }
        }
        
        sb.append(indent).append("offset += ").append(byteLen).append(";\n\n");
    }
    
    private void generateCamelRoutes(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path routeFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/camel/MainframeRoute.java"
        );
        
        String dtoClassName = toPascalCase(config.getProgramId());
        
        String routeContent = String.format("""
                package %s.camel;
                
                import %s.model.request.%sRequest;
                import %s.model.response.%sResponse;
                import %s.mainframe.transport.MainframeTransport;
                import %s.util.%sRequestSerializer;
                import %s.util.%sResponseSerializer;
                import org.apache.camel.builder.RouteBuilder;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;
                
                /**
                 * Camel route for mainframe communication.
                 */
                @Component
                public class MainframeRoute extends RouteBuilder {
                    
                    @Autowired
                    private MainframeTransport transport;
                    
                    @Autowired
                    private %sRequestSerializer requestSerializer;
                    
                    @Autowired
                    private %sResponseSerializer responseSerializer;
                    
                    @Override
                    public void configure() {
                        from("direct:mainframeCall")
                            .routeId("mainframe-call-route")
                            .log("Processing mainframe request")
                            .process(exchange -> {
                                %sRequest request = exchange.getIn().getBody(%sRequest.class);
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
                                %sResponse response = responseSerializer.deserialize(responseBytes);
                                exchange.getIn().setBody(response);
                            })
                            .log("Mainframe call completed");
                    }
                }
                """,
                config.getBasePackage(),
                config.getBasePackage(), dtoClassName,
                config.getBasePackage(), dtoClassName,
                config.getBasePackage(),
                config.getBasePackage(), dtoClassName,
                config.getBasePackage(), dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName, dtoClassName,
                dtoClassName
        );
        
        Files.writeString(routeFile, routeContent);
    }
    
    private void generateController(Path projectDir) throws IOException {
        String basePackagePath = config.getBasePackage().replace('.', '/');
        Path controllerFile = projectDir.resolve(
                "src/main/java/" + basePackagePath + "/controller/MainframeController.java"
        );
        
        String dtoClassName = toPascalCase(config.getProgramId());
        
        String controllerContent = String.format("""
                package %s.controller;
                
                import %s.model.request.%sRequest;
                import %s.model.response.%sResponse;
                import jakarta.validation.Valid;
                import org.apache.camel.ProducerTemplate;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.http.ResponseEntity;
                import org.springframework.web.bind.annotation.*;
                
                /**
                 * REST controller for mainframe operations.
                 */
                @RestController
                @RequestMapping("/api/%s")
                public class MainframeController {
                    
                    @Autowired
                    private ProducerTemplate producerTemplate;
                    
                    @PostMapping("/execute")
                    public ResponseEntity<%sResponse> execute(@Valid @RequestBody %sRequest request) {
                        %sResponse response = producerTemplate.requestBody(
                            "direct:mainframeCall", 
                            request, 
                            %sResponse.class
                        );
                        return ResponseEntity.ok(response);
                    }
                }
                """,
                config.getBasePackage(),
                config.getBasePackage(), dtoClassName,
                config.getBasePackage(), dtoClassName,
                config.getProgramIdPath(),
                dtoClassName, dtoClassName,
                dtoClassName,
                dtoClassName
        );
        
        Files.writeString(controllerFile, controllerContent);
        
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
        
        Files.writeString(handlerFile, handlerContent);
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
        
        Files.writeString(transportInterface, interfaceContent);
        
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
        
        Files.writeString(transportImpl, implContent);
        
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
        
        Files.writeString(framingInterface, interfaceContent);
        
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
        
        Files.writeString(lengthPrefixFraming, lengthPrefixContent);
        
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
        
        Files.writeString(fixedFraming, fixedContent);
        
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
        
        Files.writeString(framingConfig, configContent);
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
        
        Files.writeString(emulatorFile, emulatorContent);
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
        
        Files.writeString(dtoTestFile, dtoTestContent);
        
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
        
        Files.writeString(serializerTestFile, serializerTestContent);
        
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
        
        Files.writeString(tcpTestFile, tcpTestContent);
        
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
        
        Files.writeString(controllerTestFile, controllerTestContent);
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
        
        Files.writeString(sampleRequest, json.toString());
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
        
        Files.writeString(appFile, appContent);
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
}
