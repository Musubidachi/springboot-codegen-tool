package com.mainframe.generator.codegen.dto;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.copybook.service.CopybookQueryService;
import com.mainframe.generator.codegen.copybook.util.EnumNamingUtil;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.model.input.Enum88Node;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.MappingDocument;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.NamingUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

/**
 * Generates enum classes from COBOL 88-level values.
 *
 * Design:
 * - CopybookModel remains passive (no getAllFields()).
 * - Traversal is done via CopybookQueryService.
 * - Enums are generated into the SAME package as the owning DTO:
 *     <packageRoot>.model.<packageType>
 *
 * Package root is derived from the generated project structure
 * by locating src/main/java/{**}/model.
 */
public class EnumGenerator {

    private static final Logger log = LoggerFactory.getLogger(EnumGenerator.class);

    private final MappingDocument mappingDoc;
    private final Map<CopybookModel, DtoMetadata> dtoMetadataMap;

    private final CopybookQueryService queryService = new CopybookQueryService();

    /** Prevent duplicate enum generation (key = fully qualified class name). */
    private final Set<String> generatedEnumFqcns = new HashSet<>();

    public EnumGenerator(MappingDocument mappingDoc,
                         Map<CopybookModel, DtoMetadata> dtoMetadataMap) {
        this.mappingDoc = Objects.requireNonNull(mappingDoc, "mappingDoc");
        this.dtoMetadataMap = Objects.requireNonNull(dtoMetadataMap, "dtoMetadataMap");
    }

    /** Entry point */
    public void generateEnums(Path projectDir) throws IOException {
        log.info("Generating enum classes...");

        String packageRoot = resolvePackageRoot(projectDir);
        int generated = 0;

        for (Map.Entry<CopybookModel, DtoMetadata> entry : dtoMetadataMap.entrySet()) {
            CopybookModel model = entry.getKey();
            DtoMetadata metadata = entry.getValue();

            if (metadata == null || metadata.isDeduped()) {
                continue; // deduped DTOs do not generate code
            }

            generated += generateEnumsForModel(projectDir, packageRoot, model, metadata);
        }

        log.info("Generated {} enum classes", generated);
    }

    private int generateEnumsForModel(Path projectDir,
                                      String packageRoot,
                                      CopybookModel model,
                                      DtoMetadata metadata) throws IOException {
        int generated = 0;
        String packageType = resolvePackageType(metadata);

        for (FieldNode field : queryService.getAllFields(model.getRootGroup())) {
            if (!shouldGenerateEnum(field)) {
                continue;
            }

            if (generateEnumClass(projectDir, packageRoot, field, packageType)) {
                generated++;
            }
        }
        return generated;
    }

    private boolean shouldGenerateEnum(FieldNode field) {
        return field != null
                && field.hasEnum88Values()
                && !field.isFiller()
                && field.getName() != null
                && !mappingDoc.shouldIgnore(field.getName());
    }

    private boolean generateEnumClass(Path projectDir,
                                      String packageRoot,
                                      FieldNode field,
                                      String packageType) throws IOException {

        String enumName = NamingUtil.toPascalCase(field.getName()) + "Enum";
        String fqcn = packageRoot + ".model." + packageType + "." + enumName;

        if (!generatedEnumFqcns.add(fqcn)) {
            log.debug("Skipping duplicate enum: {}", fqcn);
            return false;
        }

        String content = buildEnumContent(packageRoot, field, enumName, packageType);
        if (content == null) {
            return false;
        }

        Path outputFile = projectDir.resolve(
                "src/main/java/"
                        + packageRoot.replace('.', '/')
                        + "/model/"
                        + packageType
                        + "/"
                        + enumName
                        + ".java"
        );

        FileWriteUtil.safeWriteString(outputFile, content);
        log.debug("Generated enum {}", fqcn);
        return true;
    }

    private String buildEnumContent(String packageRoot,
                                    FieldNode field,
                                    String enumName,
                                    String packageType) {

        List<String> constants = buildEnumConstants(field);
        if (constants.isEmpty()) {
            return null;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(packageRoot).append(".model.").append(packageType).append(";\n\n");
        sb.append("import lombok.Getter;\n");
        sb.append("import lombok.RequiredArgsConstructor;\n\n");
        sb.append("/**\n");
        sb.append(" * Generated enum from 88-level values of ").append(field.getName()).append("\n");
        sb.append(" */\n");
        sb.append("@Getter\n");
        sb.append("@RequiredArgsConstructor\n");
        sb.append("public enum ").append(enumName).append(" {\n");

        for (int i = 0; i < constants.size(); i++) {
            sb.append("    ").append(constants.get(i));
            sb.append(i < constants.size() - 1 ? ",\n" : ";\n");
        }

        sb.append("\n    private final String value;\n");
        sb.append("}\n");

        return sb.toString();
    }

    private List<String> buildEnumConstants(FieldNode field) {
        List<Enum88Node> enums = field.getEnum88Values();
        if (enums == null || enums.isEmpty()) {
            return Collections.emptyList();
        }

        Set<String> usedNames = new HashSet<>();
        List<String> constants = new ArrayList<>();

        for (Enum88Node e : enums) {
            String name = EnumNamingUtil.toJavaEnumConstant(e.getName());
            String value = e.getPrimaryValue();

            if (name == null || value == null || !usedNames.add(name)) {
                continue;
            }

            constants.add(name + "(\"" + escape(value) + "\")");
        }

        return constants;
    }

    private String resolvePackageType(DtoMetadata metadata) {
        if (metadata.getPackageType() != null && !metadata.getPackageType().isBlank()) {
            return metadata.getPackageType();
        }
        return "layout";
    }

    /**
     * Derives the Java package root by locating:
     *   src/main/java/<packageRoot>/model
     */
    private String resolvePackageRoot(Path projectDir) throws IOException {
        Path javaRoot = projectDir.resolve("src/main/java");
        if (!Files.exists(javaRoot)) {
            throw new IOException("Java source root does not exist: " + javaRoot);
        }

        try (Stream<Path> walk = Files.walk(javaRoot)) {
            Path modelDir = walk
                    .filter(Files::isDirectory)
                    .filter(p -> "model".equals(p.getFileName().toString()))
                    .map(Path::getParent)
                    .filter(Objects::nonNull)
                    .findFirst()
                    .orElseThrow(() ->
                            new IOException("Unable to derive package root (no 'model' dir found)")
                    );

            return javaRoot.relativize(modelDir)
                    .toString()
                    .replace('/', '.')
                    .replace('\\', '.');
        }
    }

    private static String escape(String s) {
        return s.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}
