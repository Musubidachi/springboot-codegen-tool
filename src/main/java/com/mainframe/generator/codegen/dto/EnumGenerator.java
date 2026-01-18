package com.mainframe.generator.codegen.dto;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.NamingUtil;
import com.mainframe.generator.mapping.MappingDocument;
import com.mainframe.generator.model.CopybookModel;
import com.mainframe.generator.model.Enum88Node;
import com.mainframe.generator.model.FieldNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;

/**
 * Generates enum classes from COBOL 88-level values.
 *
 * FIX: Enums are now generated into the SAME package as the DTO that references them.
 */
public class EnumGenerator {
    private static final Logger log = LoggerFactory.getLogger(EnumGenerator.class);

    private final String basePackage;
    private final MappingDocument mappingDoc;
    private final Map<CopybookModel, DtoMetadata> dtoMetadataMap;
    private final Map<String, String> generatedEnums = new HashMap<>(); // enumName -> packageType

    public EnumGenerator(String basePackage, MappingDocument mappingDoc,
                         Map<CopybookModel, DtoMetadata> dtoMetadataMap) {
        this.basePackage = basePackage;
        this.mappingDoc = mappingDoc;
        this.dtoMetadataMap = dtoMetadataMap;
    }

    /**
     * Generate all enum classes for the given models.
     */
    public void generateEnums(Path projectDir) throws IOException {
        log.info("Generating enum classes...");

        for (Map.Entry<CopybookModel, DtoMetadata> entry : dtoMetadataMap.entrySet()) {
            CopybookModel model = entry.getKey();
            DtoMetadata metadata = entry.getValue();

            // Skip deduped DTOs
            if (metadata.isDeduped()) {
                continue;
            }

            generateEnumsForModel(projectDir, model, metadata);
        }

        log.info("Generated {} enum classes", generatedEnums.size());
    }

    private void generateEnumsForModel(Path projectDir, CopybookModel model,
                                        DtoMetadata metadata) throws IOException {
        String packageType = metadata.getPackageType();

        for (FieldNode field : model.getAllFields()) {
            if (shouldGenerateEnum(field)) {
                generateEnumClass(projectDir, field, packageType);
            }
        }
    }

    private boolean shouldGenerateEnum(FieldNode field) {
        return field.hasEnum88Values() &&
               !field.isFiller() &&
               !mappingDoc.shouldIgnore(field.getName());
    }

    private void generateEnumClass(Path projectDir, FieldNode field,
                                     String packageType) throws IOException {
        String enumName = NamingUtil.toPascalCase(field.getName()) + "Enum";

        // Check for collision
        if (hasCollision(enumName, packageType)) {
            log.warn("Skipping duplicate enum class generation: {} for field {} in package {}",
                    enumName, field.getName(), packageType);
            return;
        }

        generatedEnums.put(enumName, packageType);
        writeEnumFile(projectDir, field, enumName, packageType);
    }

    private boolean hasCollision(String enumName, String packageType) {
        String existingPackage = generatedEnums.get(enumName);
        return existingPackage != null && existingPackage.equals(packageType);
    }

    private void writeEnumFile(Path projectDir, FieldNode field, String enumName,
                                String packageType) throws IOException {
        String packagePath = basePackage.replace('.', '/') + "/model/" + packageType;
        Path enumFile = projectDir.resolve("src/main/java/" + packagePath + "/" + enumName + ".java");

        String content = buildEnumContent(field, enumName, packageType);
        FileWriteUtil.safeWriteString(enumFile, content);

        log.debug("Generated enum: {} in package model.{}", enumName, packageType);
    }

    private String buildEnumContent(FieldNode field, String enumName, String packageType) {
        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(basePackage).append(".model.").append(packageType).append(";\n\n");
        sb.append("import lombok.Getter;\n");
        sb.append("import lombok.RequiredArgsConstructor;\n\n");
        sb.append("/**\n");
        sb.append(" * Generated enum from 88-level values of ").append(field.getName()).append("\n");
        sb.append(" */\n");
        sb.append("@Getter\n");
        sb.append("@RequiredArgsConstructor\n");
        sb.append("public enum ").append(enumName).append(" {\n");

        appendEnumConstants(sb, field);

        sb.append("\n    private final String value;\n");
        sb.append("}\n");

        return sb.toString();
    }

    private void appendEnumConstants(StringBuilder sb, FieldNode field) {
        List<Enum88Node> enums = field.getEnum88Values();
        Set<String> usedConstantNames = new HashSet<>();
        List<String> validConstants = new ArrayList<>();

        // Collect valid constants
        for (Enum88Node enum88 : enums) {
            String constName = enum88.toJavaEnumConstant();
            String value = enum88.getPrimaryValue();

            if (!usedConstantNames.add(constName)) {
                log.warn("Duplicate enum constant name detected: {} (value '{}'). Skipped.",
                        constName, value);
                continue;
            }

            validConstants.add(constName + "(\"" + value + "\")");
        }

        // Append constants
        for (int i = 0; i < validConstants.size(); i++) {
            sb.append("    ").append(validConstants.get(i));
            sb.append(i < validConstants.size() - 1 ? ",\n" : ";\n");
        }
    }

    /**
     * Get the package type where an enum for this field will be generated.
     */
    public String getEnumPackageType(FieldNode field, CopybookModel owningModel) {
        DtoMetadata metadata = dtoMetadataMap.get(owningModel);
        if (metadata != null) {
            return metadata.getPackageType();
        }
        return "layout"; // fallback
    }
}
