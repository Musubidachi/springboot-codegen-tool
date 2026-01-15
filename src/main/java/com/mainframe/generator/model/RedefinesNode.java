package com.mainframe.generator.model;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a REDEFINES clause in COBOL.
 * REDEFINES allows multiple interpretations of the same memory area.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class RedefinesNode extends CopybookNode {
    private String targetName;
    private CopybookNode targetNode;
    private List<CopybookNode> children = new ArrayList<>();
    
    @Builder
    public RedefinesNode(int level, String name, String originalName, String targetName,
                         CopybookNode parent, String sourceFile, int sourceLine) {
        this.level = level;
        this.name = name;
        this.originalName = originalName != null ? originalName : name;
        this.targetName = targetName;
        this.parent = parent;
        this.sourceFile = sourceFile;
        this.sourceLine = sourceLine;
    }
    
    @Override
    public void accept(CopybookNodeVisitor visitor) {
        visitor.visit(this);
    }
    
    public void addChild(CopybookNode child) {
        children.add(child);
        child.setParent(this);
    }
    
    public void resolveTarget(CopybookNode target) {
        this.targetNode = target;
        this.startOffset = target.getStartOffset();
        this.byteLength = target.getByteLength();
    }
}
