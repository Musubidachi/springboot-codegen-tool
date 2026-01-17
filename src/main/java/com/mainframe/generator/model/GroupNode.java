// (Full file contents)
package com.mainframe.generator.model;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a group (non-leaf) node in the copybook structure.
 * Groups contain other groups or fields.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class GroupNode extends CopybookNode {
    private List<CopybookNode> children = new ArrayList<>();
    private int occursCount = 1;
    private String occursDepending;
    private boolean isRedefines;
    private String redefinesTarget;
    
    @Builder
    public GroupNode(int level, String name, String originalName, CopybookNode parent,
                     int startOffset, int byteLength, String sourceFile, int sourceLine,
                     List<CopybookNode> children, int occursCount, String occursDepending,
                     boolean isRedefines, String redefinesTarget) {
        this.level = level;
        this.name = name;
        this.originalName = originalName != null ? originalName : name;
        this.parent = parent;
        this.startOffset = startOffset;
        this.byteLength = byteLength;
        this.sourceFile = sourceFile;
        this.sourceLine = sourceLine;
        this.children = children != null ? children : new ArrayList<>();
        this.occursCount = occursCount > 0 ? occursCount : 1;
        this.occursDepending = occursDepending;
        this.isRedefines = isRedefines;
        this.redefinesTarget = redefinesTarget;
    }

    public void addChild(CopybookNode child) {
        children.add(child);
        child.setParent(this);
    }

    @Override
    public void accept(CopybookNodeVisitor visitor) {
        visitor.visit(this);
    }
    
    public int calculateByteLength() {
        // First pass: identify which fields are redefine targets
        java.util.Set<String> redefineTargets = new java.util.HashSet<>();
        for (CopybookNode child : children) {
            if (child instanceof GroupNode group && group.isRedefines()) {
                redefineTargets.add(group.getRedefinesTarget());
            } else if (child instanceof FieldNode field && field.isRedefines()) {
                redefineTargets.add(field.getRedefinesTarget());
            }
        }

        // Second pass: calculate total length
        int total = 0;
        int maxRedefineLength = 0;
        String currentRedefineTarget = null;

        for (CopybookNode child : children) {
            boolean isRedefine = (child instanceof GroupNode group && group.isRedefines()) ||
                    (child instanceof FieldNode field && field.isRedefines());

            int childLength = 0;
            if (child instanceof GroupNode group) {
                childLength = group.calculateByteLength();
            } else if (child instanceof FieldNode field) {
                childLength = field.getByteLength() * field.getOccursCount();
            }

            if (isRedefine) {
                String target = null;
                if (child instanceof GroupNode group) {
                    target = group.getRedefinesTarget();
                } else if (child instanceof FieldNode field) {
                    target = field.getRedefinesTarget();
                }

                if (currentRedefineTarget != null && !currentRedefineTarget.equals(target)) {
                    // Switching to a different redefine set, finalize the previous one
                    total += maxRedefineLength;
                    maxRedefineLength = 0;
                }

                currentRedefineTarget = target;
                maxRedefineLength = Math.max(maxRedefineLength, childLength);
            } else {
                // Non-redefine field
                boolean isTarget = redefineTargets.contains(child.getName());

                if (isTarget) {
                    // This field is a redefine target, so it starts a redefine set
                    if (currentRedefineTarget != null && maxRedefineLength > 0) {
                        // Finalize previous redefine set
                        total += maxRedefineLength;
                    }
                    currentRedefineTarget = child.getName();
                    maxRedefineLength = childLength;
                } else {
                    // Normal field not involved in any redefines
                    if (maxRedefineLength > 0) {
                        total += maxRedefineLength;
                        maxRedefineLength = 0;
                        currentRedefineTarget = null;
                    }
                    total += childLength;
                }
            }
        }

        // Handle trailing redefines
        if (maxRedefineLength > 0) {
            total += maxRedefineLength;
        }

        // Multiply by this group's occursCount only once
        return total * occursCount;
    }
}