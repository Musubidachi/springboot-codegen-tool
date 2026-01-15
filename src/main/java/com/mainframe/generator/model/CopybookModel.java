package com.mainframe.generator.model;

import lombok.Builder;
import lombok.Data;

import java.util.*;

/**
 * Represents a fully parsed copybook with all its structure.
 */
@Data
@Builder
public class CopybookModel {
    private String name;
    private String sourcePath;
    private GroupNode rootGroup;
    private List<CopyDirectiveNode> copyDirectives;
    private List<RedefinesNode> redefines;
    private Map<String, FieldNode> fieldsByName;
    private Map<String, GroupNode> groupsByName;
    private int totalByteLength;
    private List<String> warnings;
    private List<String> errors;
    
    public static CopybookModelBuilder builder() {
        return new CopybookModelBuilder()
                .copyDirectives(new ArrayList<>())
                .redefines(new ArrayList<>())
                .fieldsByName(new HashMap<>())
                .groupsByName(new HashMap<>())
                .warnings(new ArrayList<>())
                .errors(new ArrayList<>());
    }
    
    public void addWarning(String warning) {
        if (warnings == null) warnings = new ArrayList<>();
        warnings.add(warning);
    }
    
    public void addError(String error) {
        if (errors == null) errors = new ArrayList<>();
        errors.add(error);
    }
    
    public boolean hasErrors() {
        return errors != null && !errors.isEmpty();
    }
    
    public boolean hasWarnings() {
        return warnings != null && !warnings.isEmpty();
    }
    
    /**
     * Get all leaf fields in order.
     */
    public List<FieldNode> getAllFields() {
        List<FieldNode> fields = new ArrayList<>();
        if (rootGroup != null) {
            collectFields(rootGroup, fields);
        }
        return fields;
    }
    
    private void collectFields(CopybookNode node, List<FieldNode> fields) {
        if (node instanceof FieldNode field) {
            fields.add(field);
        } else if (node instanceof GroupNode group) {
            for (CopybookNode child : group.getChildren()) {
                collectFields(child, fields);
            }
        }
    }
    
    /**
     * Get all enum (88-level) definitions.
     */
    public List<Enum88Node> getAllEnums() {
        List<Enum88Node> enums = new ArrayList<>();
        for (FieldNode field : getAllFields()) {
            enums.addAll(field.getEnum88Values());
        }
        return enums;
    }
    
    /**
     * Find a field by its COBOL name.
     */
    public Optional<FieldNode> findField(String name) {
        return Optional.ofNullable(fieldsByName.get(name.toUpperCase()));
    }
    
    /**
     * Find a group by its COBOL name.
     */
    public Optional<GroupNode> findGroup(String name) {
        return Optional.ofNullable(groupsByName.get(name.toUpperCase()));
    }
    
    /**
     * Calculate total byte length of the record.
     */
    public int calculateTotalByteLength() {
        if (rootGroup != null) {
            return rootGroup.calculateByteLength();
        }
        return 0;
    }
}
