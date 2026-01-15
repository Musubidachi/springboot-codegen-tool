package com.mainframe.generator.model;

/**
 * COBOL USAGE clause types.
 */
public enum UsageType {
    /**
     * Default display format (character or zoned decimal).
     */
    DISPLAY,
    
    /**
     * Binary format (COMP or BINARY).
     */
    BINARY,
    
    /**
     * Packed decimal format (COMP-3).
     */
    PACKED_DECIMAL,
    
    /**
     * Native binary (COMP-5).
     */
    COMP_5,
    
    /**
     * Floating point single precision (COMP-1).
     */
    COMP_1,
    
    /**
     * Floating point double precision (COMP-2).
     */
    COMP_2;
    
    public static UsageType fromCobol(String usage) {
        if (usage == null) {
            return DISPLAY;
        }
        String normalized = usage.toUpperCase().trim();
        return switch (normalized) {
            case "COMP", "COMP-4", "BINARY", "COMPUTATIONAL", "COMPUTATIONAL-4" -> BINARY;
            case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> PACKED_DECIMAL;
            case "COMP-5", "COMPUTATIONAL-5" -> COMP_5;
            case "COMP-1", "COMPUTATIONAL-1" -> COMP_1;
            case "COMP-2", "COMPUTATIONAL-2" -> COMP_2;
            default -> DISPLAY;
        };
    }
}
