package com.mainframe.generator.codegen.dto;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.ImportManager;
import com.mainframe.generator.codegen.util.NamingUtil;
import com.mainframe.generator.mapping.MappingDocument;
import com.mainframe.generator.model.CopybookModel;
import com.mainframe.generator.validation.ValidationConstraintGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;

/**
 * Generates DTO class files from copybook models.
 */
public class DtoClassGenerator {
    private static final Logger log = LoggerFactory.getLogger(DtoClassGenerator.class);

    private final String basePackage;
    private final MappingDocument mappingDoc;
    private final ValidationConstraintGenerator validationGenerator;

    public DtoClassGenerator(String basePackage, MappingDocument mappingDoc) {
        this.basePackage = basePackage;
        this.mappingDoc = mappingDoc;
        this.validationGenerator = new ValidationConstraintGenerator();
    }

    /**
     * Generate a DTO class from metadata.
     */
    public void generateDto(Path projectDir, DtoMetadata metadata,
                             Set<String> nestedClassNames) throws IOException {
        if (metadata.isDeduped()) {
            log.debug("Skipping deduped DTO: {}", metadata.getOriginalClassName());
            return;
        }

        log.info("Generating DTO: {} in package model.{}",
                metadata.getClassName(), metadata.getPackageType());

        String packageName = basePackage + ".model." + metadata.getPackageType();
        String packagePath = packageName.replace('.', '/');
        Path outputPath = projectDir.resolve("src/main/java/" + packagePath + "/" +
                metadata.getClassName() + ".java");

        String content = buildDtoContent(metadata, packageName, nestedClassNames);
        FileWriteUtil.safeWriteString(outputPath, content);
    }

    private String buildDtoContent(DtoMetadata metadata, String packageName,
                                    Set<String> nestedClassNames) {
        StringBuilder sb = new StringBuilder();

        sb.append("package ").append(packageName).append(";\n\n");

        ImportManager importManager = new ImportManager(packageName);
        addStandardImports(importManager);

        sb.append(importManager.generateImports());

        // Generate fields using ImportManager
        String fieldsContent = generateFieldsContent(metadata, importManager, nestedClassNames);

        // Add any additional imports from field generation
        sb.append(importManager.generateImports());
        sb.append("\n");

        sb.append("/**\n");
        sb.append(" * Generated DTO from copybook: ").append(metadata.getOriginalClassName()).append("\n");
        sb.append(" * Total byte length: ").append(metadata.getByteLength()).append("\n");
        sb.append(" */\n");
        appendAnnotations(sb);

        String classDeclaration = buildClassDeclaration(metadata);
        sb.append(classDeclaration);

        sb.append(fieldsContent);
        sb.append("}\n");

        return sb.toString();
    }

    private void addStandardImports(ImportManager importManager) {
        importManager.addImport("lombok.Data");
        importManager.addImport("lombok.NoArgsConstructor");
        importManager.addImport("lombok.AllArgsConstructor");
        importManager.addImport("lombok.Builder");
        importManager.addImport("java.util.List");
        importManager.addImport("java.util.ArrayList");
    }

    private void appendAnnotations(StringBuilder sb) {
        sb.append("@Data\n");
        sb.append("@NoArgsConstructor\n");
        sb.append("@AllArgsConstructor\n");
        sb.append("@Builder\n");
    }

    private String buildClassDeclaration(DtoMetadata metadata) {
        String declaration = "public class " + metadata.getClassName();
        if (metadata.getExtendsClassName() != null) {
            declaration += " extends " + metadata.getExtendsClassName();
        }
        declaration += " {\n\n";
        return declaration;
    }

    private String generateFieldsContent(DtoMetadata metadata, ImportManager importManager,
                                          Set<String> nestedClassNames) {
        StringBuilder fieldsSb = new StringBuilder();

        DtoFieldGenerator fieldGenerator = new DtoFieldGenerator(
                basePackage, metadata.getPackageType(), mappingDoc,
                validationGenerator, importManager);

        CopybookModel model = metadata.getCopybookModel();
        fieldGenerator.generateFields(fieldsSb, model.getChildren(), "    ", nestedClassNames);

        return fieldsSb.toString();
    }
}
