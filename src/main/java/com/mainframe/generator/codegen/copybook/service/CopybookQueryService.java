package com.mainframe.generator.codegen.copybook.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

import com.mainframe.generator.codegen.model.input.CopybookNode;
import com.mainframe.generator.codegen.model.input.Enum88Node;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.GroupNode;

import lombok.NoArgsConstructor;

/**
 * Traversal/query utilities for a parsed copybook AST.
 *
 * Stateless: callers provide the root group/node.
 */
@NoArgsConstructor
public class CopybookQueryService {

    /**
     * Get all leaf fields in traversal order.
     */
    public List<FieldNode> getAllFields(GroupNode rootGroup) {
        List<FieldNode> fields = new ArrayList<>();
        if (rootGroup != null) {
            collectFields(rootGroup, fields);
        }
        return fields;
    }

    private void collectFields(CopybookNode node, List<FieldNode> fields) {
        if (node instanceof FieldNode field) {
            fields.add(field);
            return;
        }
        if (node instanceof GroupNode group) {
            for (CopybookNode child : group.getChildren()) {
                collectFields(child, fields);
            }
        }
    }

    /**
     * Get all enum (88-level) definitions in traversal order.
     */
    public List<Enum88Node> getAllEnums(GroupNode rootGroup) {
        List<Enum88Node> enums = new ArrayList<>();
        for (FieldNode field : getAllFields(rootGroup)) {
            enums.addAll(field.getEnum88Values());
        }
        return enums;
    }

    /**
     * Find a field by its COBOL name (case-insensitive).
     * Returns the first match encountered during traversal.
     */
    public Optional<FieldNode> findField(GroupNode rootGroup, String cobolName) {
        if (rootGroup == null || cobolName == null || cobolName.isBlank()) {
            return Optional.empty();
        }
        String needle = cobolName.trim().toUpperCase(Locale.ROOT);
        for (FieldNode field : getAllFields(rootGroup)) {
            if (field.getName() != null && field.getName().toUpperCase(Locale.ROOT).equals(needle)) {
                return Optional.of(field);
            }
        }
        return Optional.empty();
    }

    /**
     * Find a group by its COBOL name (case-insensitive).
     * Returns the first match encountered during traversal.
     */
    public Optional<GroupNode> findGroup(GroupNode rootGroup, String cobolName) {
        if (rootGroup == null || cobolName == null || cobolName.isBlank()) {
            return Optional.empty();
        }
        String needle = cobolName.trim().toUpperCase(Locale.ROOT);
        return findGroupRecursive(rootGroup, needle);
    }

    private Optional<GroupNode> findGroupRecursive(CopybookNode node, String needleUpper) {
        if (node instanceof GroupNode group) {
            if (group.getName() != null && group.getName().toUpperCase(Locale.ROOT).equals(needleUpper)) {
                return Optional.of(group);
            }
            for (CopybookNode child : group.getChildren()) {
                Optional<GroupNode> found = findGroupRecursive(child, needleUpper);
                if (found.isPresent()) return found;
            }
        }
        return Optional.empty();
    }
}
