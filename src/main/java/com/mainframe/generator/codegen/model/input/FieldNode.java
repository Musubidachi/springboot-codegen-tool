// (Full file contents)
package com.mainframe.generator.codegen.model.input;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

import com.mainframe.generator.codegen.copybook.CopybookNodeVisitor;
import com.mainframe.generator.codegen.copybook.util.PictureClause;

/**
 * Represents a leaf field (elementary item) in the copybook structure.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class FieldNode extends CopybookNode {
    private PictureClause picture;
    private UsageType usage = UsageType.DISPLAY;
    private int occursCount = 1;
    private String occursDepending;
    private boolean isRedefines;
    private String redefinesTarget;
    private String value;
    private List<Enum88Node> enum88Values = new ArrayList<>();
    private boolean isFiller;
    private String javaType;
    private String javaFieldName;
    
    @Builder
    public FieldNode(int level, String name, String originalName, CopybookNode parent,
                     int startOffset, int byteLength, String sourceFile, int sourceLine,
                     PictureClause picture, UsageType usage, int occursCount, String occursDepending,
                     boolean isRedefines, String redefinesTarget, String value,
                     List<Enum88Node> enum88Values, boolean isFiller) {
        this.level = level;
        this.name = name;
        this.originalName = originalName != null ? originalName : name;
        this.parent = parent;
        this.startOffset = startOffset;
        this.byteLength = byteLength;
        this.sourceFile = sourceFile;
        this.sourceLine = sourceLine;
        this.picture = picture;
        this.usage = usage != null ? usage : UsageType.DISPLAY;
        this.occursCount = occursCount > 0 ? occursCount : 1;
        this.occursDepending = occursDepending;
        this.isRedefines = isRedefines;
        this.redefinesTarget = redefinesTarget;
        this.value = value;
        this.enum88Values = enum88Values != null ? enum88Values : new ArrayList<>();
        this.isFiller = isFiller;
    }

    @Override
    public void accept(CopybookNodeVisitor visitor) {
        visitor.visit(this);
    }
    
    public void addEnum88(Enum88Node enum88) {
        enum88Values.add(enum88);
        enum88.setParent(this);
    }
    
    public boolean hasEnum88Values() {
        return !enum88Values.isEmpty();
    }
    
}