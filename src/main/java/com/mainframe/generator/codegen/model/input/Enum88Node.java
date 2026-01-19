package com.mainframe.generator.codegen.model.input;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

import com.mainframe.generator.codegen.copybook.CopybookNodeVisitor;

/**
 * Represents an 88-level condition name in COBOL.
 * These become enum values in the generated Java code.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class Enum88Node extends CopybookNode {
    private List<String> values = new ArrayList<>();
    private String throughValue;
    
    @Builder
    public Enum88Node(String name, String originalName, CopybookNode parent,
                      String sourceFile, int sourceLine, List<String> values, String throughValue) {
        this.level = 88;
        this.name = name;
        this.originalName = originalName != null ? originalName : name;
        this.parent = parent;
        this.sourceFile = sourceFile;
        this.sourceLine = sourceLine;
        this.values = values != null ? values : new ArrayList<>();
        this.throughValue = throughValue;
    }
    
    @Override
    public void accept(CopybookNodeVisitor visitor) {
        visitor.visit(this);
    }
    
    /**
     * Get the primary value for this condition.
     */
    public String getPrimaryValue() {
        return values.isEmpty() ? null : values.get(0);
    }
    
    
    /**
     * Check if this is a range value (THRU/THROUGH).
     */
    public boolean isRange() {
        return throughValue != null;
    }
}
