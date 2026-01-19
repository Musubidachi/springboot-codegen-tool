package com.mainframe.generator.codegen.model.input;

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
    
}
