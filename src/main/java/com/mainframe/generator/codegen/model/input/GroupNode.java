// (Full file contents)
package com.mainframe.generator.codegen.model.input;


import java.util.ArrayList;
import java.util.List;

import com.mainframe.generator.codegen.copybook.CopybookNodeVisitor;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

/**
 * Represents a group (non-leaf) node in the copybook structure.
 * Groups contain other groups or fields.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class GroupNode extends CopybookNode {
	
    @EqualsAndHashCode.Exclude
    @ToString.Exclude
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
    
}