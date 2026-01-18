package com.mainframe.generator.codegen.dto;

import com.mainframe.generator.codegen.util.ImportManager;
import com.mainframe.generator.codegen.util.NamingUtil;
import com.mainframe.generator.mapping.MappingDocument;
import com.mainframe.generator.model.CopybookNode;
import com.mainframe.generator.model.FieldNode;
import com.mainframe.generator.model.GroupNode;
import com.mainframe.generator.validation.ValidationConstraintGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Generates fields for DTO classes.
 */
public class DtoFieldGenerator {
    private static final Logger log = LoggerFactory.getLogger(DtoFieldGenerator.class);

    private final String basePackage;
    private final String packageType;
    private final MappingDocument mappingDoc;
    private final ValidationConstraintGenerator validationGenerator;
    private final ImportManager importManager;

    public DtoFieldGenerator(String basePackage, String packageType,
                             MappingDocument mappingDoc,
                             ValidationConstraintGenerator validationGenerator,
                             ImportManager importManager) {
        this.basePackage = basePackage;
        this.packageType = packageType;
        this.mappingDoc = mappingDoc;
        this.validationGenerator = validationGenerator;
        this.importManager = importManager;
    }

    /**
     * Generate fields for a DTO class.
     */
    public void generateFields(StringBuilder sb, List<CopybookNode> nodes,
                                String indent, Set<String> nestedClassNames) {
        Set<String> usedFieldNames = new HashSet<>();

        for (CopybookNode node : nodes) {
            if (node instanceof FieldNode field) {
                if (shouldIncludeField(field)) {
                    generateField(sb, field, indent, usedFieldNames);
                }
            } else if (node instanceof GroupNode group) {
                if (shouldGenerateGroupAsField(group, nestedClassNames)) {
                    generateGroupField(sb, group, indent);
                }
            }
        }
    }

    private boolean shouldIncludeField(FieldNode field) {
        return !field.isFiller() && !mappingDoc.shouldIgnore(field.getName());
    }

    private boolean shouldGenerateGroupAsField(GroupNode group, Set<String> nestedClassNames) {
        String nestedClassName = NamingUtil.toPascalCase(group.getName()) + "Item";
        return group.getOccursCount() > 1 && nestedClassNames.contains(nestedClassName);
    }

    private void generateField(StringBuilder sb, FieldNode field, String indent, Set<String> usedFieldNames) {
        log.debug("    Generating field: {} ({})", field.getName(), field.inferJavaType());

        String javaType = getJavaType(field);
        String fieldName = getJavaFieldName(field, usedFieldNames);

        addImportsForField(field, javaType);

        // Generate validation annotations
        var constraints = validationGenerator.generateConstraints(field, true);
        for (var constraint : constraints) {
            sb.append(indent).append(constraint.toAnnotation()).append("\n");
        }

        sb.append(indent).append("private ").append(javaType).append(" ").append(fieldName).append(";\n");
    }

    private void generateGroupField(StringBuilder sb, GroupNode group, String indent) {
        String nestedClassName = NamingUtil.toPascalCase(group.getName()) + "Item";
        String fieldName = NamingUtil.toCamelCase(group.getName());
        String javaType = "List<" + nestedClassName + ">";

        importManager.addImport("java.util.List");
        importManager.addImport("jakarta.validation.Valid");

        sb.append(indent).append("@Valid\n");
        sb.append(indent).append("private ").append(javaType).append(" ").append(fieldName).append(";\n");
    }

    private String getJavaType(FieldNode field) {
        // Check for mapping override
        var mapping = mappingDoc.getMappingFor(field.getName());
        if (mapping.isPresent() && mapping.get().getTargetType() != null) {
            return mapping.get().getTargetType();
        }

        // FIX: Check for 88-level (enum) - return enum type
        if (field.hasEnum88Values()) {
            return NamingUtil.toPascalCase(field.getName()) + "Enum";
        }

        return field.inferJavaType();
    }

    private String getJavaFieldName(FieldNode field, Set<String> usedFieldNames) {
        // Check for rename mapping
        var mapping = mappingDoc.getRenamedName(field.getName());
        String fieldName;
        if (mapping.isPresent()) {
            fieldName = NamingUtil.toCamelCase(mapping.get());
        } else {
            fieldName = field.toJavaFieldName();
        }

        // Ensure uniqueness
        String uniqueName = fieldName;
        int suffix = 2;
        while (usedFieldNames.contains(uniqueName)) {
            uniqueName = fieldName + suffix;
            suffix++;
        }
        usedFieldNames.add(uniqueName);

        return uniqueName;
    }

    private void addImportsForField(FieldNode field, String javaType) {
        // FIX: Add enum import from the SAME package
        if (field.hasEnum88Values()) {
            String enumFqn = basePackage + ".model." + packageType + "." + javaType;
            importManager.addImport(enumFqn);
        }

        // Add type-specific imports
        if (javaType.equals("LocalDate") || javaType.equals("LocalDateTime")) {
            importManager.addImport("java.time." + javaType);
        } else if (javaType.equals("BigDecimal")) {
            importManager.addImport("java.math.BigDecimal");
        }
    }
}
