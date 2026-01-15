package com.mainframe.generator.parser;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a token from the COBOL copybook tokenizer.
 */
@Data
@AllArgsConstructor
public class CopybookToken {
    private TokenType type;
    private String value;
    private int line;
    private int column;
    
    public enum TokenType {
        LEVEL_NUMBER,
        FIELD_NAME,
        PIC,
        PICTURE,
        OCCURS,
        TIMES,
        REDEFINES,
        COPY,
        VALUE,
        VALUES,
        USAGE,
        COMP,
        COMP_1,
        COMP_2,
        COMP_3,
        COMP_4,
        COMP_5,
        BINARY,
        PACKED_DECIMAL,
        DISPLAY,
        DEPENDING,
        ON,
        THRU,
        THROUGH,
        IS,
        ARE,
        FILLER,
        PERIOD,
        STRING_LITERAL,
        NUMERIC_LITERAL,
        IDENTIFIER,
        LPAREN,
        RPAREN,
        EOF,
        UNKNOWN
    }
    
    public boolean isUsageType() {
        return type == TokenType.COMP || type == TokenType.COMP_1 || 
               type == TokenType.COMP_2 || type == TokenType.COMP_3 ||
               type == TokenType.COMP_4 || type == TokenType.COMP_5 ||
               type == TokenType.BINARY || type == TokenType.PACKED_DECIMAL ||
               type == TokenType.DISPLAY;
    }
}
