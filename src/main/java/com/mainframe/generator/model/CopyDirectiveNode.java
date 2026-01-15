package com.mainframe.generator.model;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Represents a COPY directive in COBOL that includes another copybook.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class CopyDirectiveNode extends CopybookNode {
    private String copybookName;
    private String library;
    private boolean resolved;
    private String resolvedPath;
    private GroupNode resolvedContent;
    
    @Builder
    public CopyDirectiveNode(String copybookName, String library, String sourceFile, int sourceLine) {
        this.level = 0; // COPY directives don't have a level
        this.name = copybookName;
        this.copybookName = copybookName;
        this.library = library;
        this.sourceFile = sourceFile;
        this.sourceLine = sourceLine;
    }
    
    @Override
    public void accept(CopybookNodeVisitor visitor) {
        visitor.visit(this);
    }
    
    public void markResolved(String path, GroupNode content) {
        this.resolved = true;
        this.resolvedPath = path;
        this.resolvedContent = content;
    }
}
