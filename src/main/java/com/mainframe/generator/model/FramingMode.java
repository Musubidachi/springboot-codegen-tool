package com.mainframe.generator.model;

/**
 * TCP message framing modes for mainframe communication.
 */
public enum FramingMode {
    /**
     * 2-byte big-endian length prefix before payload.
     */
    LENGTH_PREFIX_2,
    
    /**
     * 4-byte big-endian length prefix before payload.
     */
    LENGTH_PREFIX_4,
    
    /**
     * Fixed-length messages (length derived from copybook layout).
     */
    FIXED
}
