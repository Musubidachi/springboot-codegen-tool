package com.mainframe.generator.codegen.dto.wrapper;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.ImportManager;
import com.mainframe.generator.codegen.util.NamingUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;

/**
 * Generates ApiRequest and ApiResponse wrapper classes.
 */
public class WrapperGenerator {
    private static final Logger log = LoggerFactory.getLogger(WrapperGenerator.class);

    private final String basePackage;
    private final String programId;

    public WrapperGenerator(String basePackage, String programId) {
        this.basePackage = basePackage;
        this.programId = programId;
    }

    /**
     * Generate ApiRequest wrapper class.
     */
    public void generateApiRequestWrapper(Path projectDir, List<DtoMetadata> requestDtos) throws IOException {
        String className = NamingUtil.toPascalCase(programId) + "ApiRequest";
        String packageName = basePackage + ".model.request";
        String packagePath = packageName.replace('.', '/');

        Path outputPath = projectDir.resolve("src/main/java/" + packagePath + "/" + className + ".java");

        String content = buildWrapperContent(className, packageName, requestDtos);
        FileWriteUtil.safeWriteString(outputPath, content);

        log.info("Generated ApiRequest wrapper: {}", className);
    }

    /**
     * Generate ApiResponse wrapper class.
     */
    public void generateApiResponseWrapper(Path projectDir, List<DtoMetadata> responseDtos) throws IOException {
        String className = NamingUtil.toPascalCase(programId) + "ApiResponse";
        String packageName = basePackage + ".model.response";
        String packagePath = packageName.replace('.', '/');

        Path outputPath = projectDir.resolve("src/main/java/" + packagePath + "/" + className + ".java");

        String content = buildWrapperContent(className, packageName, responseDtos);
        FileWriteUtil.safeWriteString(outputPath, content);

        log.info("Generated ApiResponse wrapper: {}", className);
    }

    private String buildWrapperContent(String className, String packageName,
                                        List<DtoMetadata> dtos) {
        StringBuilder sb = new StringBuilder();

        sb.append("package ").append(packageName).append(";\n\n");

        // Add imports
        ImportManager importManager = new ImportManager(packageName);
        importManager.addImport("lombok.Data");
        importManager.addImport("lombok.NoArgsConstructor");
        importManager.addImport("lombok.AllArgsConstructor");
        importManager.addImport("lombok.Builder");

        for (DtoMetadata dto : dtos) {
            if (!dto.isDeduped()) {
                String fullType = basePackage + ".model." + dto.getPackageType() + "." + dto.getClassName();
                importManager.addImport(fullType);
            }
        }

        sb.append(importManager.generateImports());
        sb.append("\n");

        // Add class
        sb.append("/**\n");
        sb.append(" * Wrapper class containing all ").append(className.contains("Request") ? "request" : "response");
        sb.append(" DTOs\n */\n");
        sb.append("@Data\n");
        sb.append("@NoArgsConstructor\n");
        sb.append("@AllArgsConstructor\n");
        sb.append("@Builder\n");
        sb.append("public class ").append(className).append(" {\n");

        // Add fields (sorted for determinism)
        dtos.stream()
                .sorted(Comparator.comparing(DtoMetadata::getClassName))
                .forEach(dto -> appendField(sb, dto));

        sb.append("}\n");

        return sb.toString();
    }

    private void appendField(StringBuilder sb, DtoMetadata dto) {
        if (!dto.isDeduped()) {
            String fieldName = NamingUtil.toCamelCase(dto.getClassName());
            sb.append("    private ").append(dto.getClassName()).append(" ").append(fieldName).append(";\n");
        }
    }
}
