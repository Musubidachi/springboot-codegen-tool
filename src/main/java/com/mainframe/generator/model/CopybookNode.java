package com.mainframe.generator.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * Base class for all copybook AST nodes.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public abstract class CopybookNode {
    protected int level;
    protected String name;
    protected String originalName;
    protected CopybookNode parent;
    protected int startOffset;
    protected int byteLength;
    protected String sourceFile;
    protected int sourceLine;

    public abstract void accept(CopybookNodeVisitor visitor);
    
    public String getFullPath() {
        if (parent == null || parent.getName() == null) {
            return name;
        }
        return parent.getFullPath() + "." + name;
    }
}
