package com.mainframe.generator.model;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

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

    /**
     * Calculate the byte length for this field.
     */
    public int calculateByteLength() {
        if (picture == null) {
            return 0;
        }
        return picture.getByteLength(usage);
    }
    
    /**
     * Get the inferred Java type for this field.
     */
    public String inferJavaType() {
        if (javaType != null) {
            return javaType;
        }
        if (picture == null) {
            return "String";
        }
        if (hasEnum88Values()) {
            return convertToJavaClassName(name) + "Enum";
        }
        return picture.getJavaType(usage);
    }
    
    /**
     * Convert a COBOL field name to a Java field name.
     */
    public String toJavaFieldName() {
        if (javaFieldName != null) {
            return javaFieldName;
        }
        return convertToJavaFieldName(name);
    }
    
    private String convertToJavaFieldName(String cobolName) {
        if (cobolName == null) return "field";
        String[] parts = cobolName.toLowerCase().split("-");
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < parts.length; i++) {
            if (i == 0) {
                sb.append(parts[i]);
            } else {
                sb.append(Character.toUpperCase(parts[i].charAt(0)));
                if (parts[i].length() > 1) {
                    sb.append(parts[i].substring(1));
                }
            }
        }
        return sb.toString();
    }
    
    private String convertToJavaClassName(String cobolName) {
        if (cobolName == null) return "Field";
        String[] parts = cobolName.split("-");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (!part.isEmpty()) {
                sb.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    sb.append(part.substring(1).toLowerCase());
                }
            }
        }
        return sb.toString();
    }
}
