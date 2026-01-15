package com.mainframe.generator.mapping;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * Represents a single mapping rule from the mapping document.
 */
@Data
@Builder
public class MappingEntry {
    private MappingType type;
    private List<String> sourceFields;
    private String targetName;
    private String targetType;
    private boolean ignore;
    
    public enum MappingType {
        /**
         * Simple rename of a single field.
         */
        RENAME,
        
        /**
         * Combine multiple fields into a single derived field.
         */
        COMBINE,
        
        /**
         * Force a field to be an enum type.
         */
        ENUM,
        
        /**
         * Ignore/skip this field in the generated output.
         */
        IGNORE
    }
    
    public boolean isSingleField() {
        return sourceFields != null && sourceFields.size() == 1;
    }
    
    public String getPrimarySourceField() {
        return sourceFields != null && !sourceFields.isEmpty() ? sourceFields.get(0) : null;
    }
}
