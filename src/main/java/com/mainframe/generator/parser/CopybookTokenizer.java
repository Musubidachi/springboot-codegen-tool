package com.mainframe.generator.parser;

import com.mainframe.generator.parser.CopybookToken.TokenType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tokenizer for COBOL copybook source files.
 */
public class CopybookTokenizer {
    private static final Logger log = LoggerFactory.getLogger(CopybookTokenizer.class);
    
    private static final Map<String, TokenType> KEYWORDS = Map.ofEntries(
        Map.entry("PIC", TokenType.PIC),
        Map.entry("PICTURE", TokenType.PICTURE),
        Map.entry("OCCURS", TokenType.OCCURS),
        Map.entry("TIMES", TokenType.TIMES),
        Map.entry("REDEFINES", TokenType.REDEFINES),
        Map.entry("COPY", TokenType.COPY),
        Map.entry("VALUE", TokenType.VALUE),
        Map.entry("VALUES", TokenType.VALUES),
        Map.entry("USAGE", TokenType.USAGE),
        Map.entry("COMP", TokenType.COMP),
        Map.entry("COMPUTATIONAL", TokenType.COMP),
        Map.entry("COMP-1", TokenType.COMP_1),
        Map.entry("COMPUTATIONAL-1", TokenType.COMP_1),
        Map.entry("COMP-2", TokenType.COMP_2),
        Map.entry("COMPUTATIONAL-2", TokenType.COMP_2),
        Map.entry("COMP-3", TokenType.COMP_3),
        Map.entry("COMPUTATIONAL-3", TokenType.COMP_3),
        Map.entry("COMP-4", TokenType.COMP_4),
        Map.entry("COMPUTATIONAL-4", TokenType.COMP_4),
        Map.entry("COMP-5", TokenType.COMP_5),
        Map.entry("COMPUTATIONAL-5", TokenType.COMP_5),
        Map.entry("BINARY", TokenType.BINARY),
        Map.entry("PACKED-DECIMAL", TokenType.PACKED_DECIMAL),
        Map.entry("DISPLAY", TokenType.DISPLAY),
        Map.entry("DEPENDING", TokenType.DEPENDING),
        Map.entry("ON", TokenType.ON),
        Map.entry("THRU", TokenType.THRU),
        Map.entry("THROUGH", TokenType.THROUGH),
        Map.entry("IS", TokenType.IS),
        Map.entry("ARE", TokenType.ARE),
        Map.entry("FILLER", TokenType.FILLER)
    );
    
    // Pattern for COBOL picture strings (e.g., X(10), 9(5)V99, S9(7)COMP-3)
    private static final Pattern PIC_PATTERN = Pattern.compile(
        "S?[9XAVZP*+-.,()0-9]+",
        Pattern.CASE_INSENSITIVE
    );
    
    private final String source;
    private final String fileName;
    private int pos = 0;
    private int line = 1;
    private int column = 1;
    
    public CopybookTokenizer(String source, String fileName) {
        this.source = preprocessSource(source);
        this.fileName = fileName;
    }
    
    /**
     * Preprocess the source to handle COBOL-specific formatting.
     * Handles sequence numbers, column 7 indicators, and line continuation.
     */
    private String preprocessSource(String src) {
        StringBuilder sb = new StringBuilder();
        String[] lines = src.split("\n");
        boolean previousWasContinuation = false;

        for (String lineContent : lines) {
            // Handle COBOL column conventions:
            // Columns 1-6: Sequence number (ignored)
            // Column 7: Indicator (* = comment, - = continuation, / = page eject comment, D = debug)
            // Columns 8-72: Program area
            // Columns 73-80: Identification area (ignored)

            if (lineContent.length() > 6) {
                char indicator = lineContent.charAt(6);

                // Skip comment lines (* or /)
                if (indicator == '*' || indicator == '/') {
                    continue;
                }

                // Skip debug lines (D in column 7) - treat as comments by default
                if (indicator == 'D' || indicator == 'd') {
                    continue;
                }

                // Get program area (columns 8-72)
                int start = Math.min(7, lineContent.length());
                int end = Math.min(72, lineContent.length());

                if (start < lineContent.length()) {
                    String programArea = lineContent.substring(start, end);

                    // Handle continuation
                    if (indicator == '-') {
                        // Continuation line: remove the last newline if we just added one
                        // and append this line stripped of leading spaces
                        if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\n') {
                            sb.setLength(sb.length() - 1);
                        }
                        sb.append(programArea.stripLeading());
                        previousWasContinuation = true;
                    } else {
                        // Normal line
                        sb.append(programArea);
                        previousWasContinuation = false;
                    }
                }
            } else if (lineContent.trim().isEmpty()) {
                // Empty or very short line - skip
                continue;
            } else {
                // Line without full column layout - append as-is
                sb.append(lineContent);
                previousWasContinuation = false;
            }

            // Add newline (will be removed on next iteration if continuation follows)
            sb.append("\n");
        }

        return sb.toString();
    }
    
    /**
     * Tokenize the entire source file.
     */
    public List<CopybookToken> tokenize() {
        List<CopybookToken> tokens = new ArrayList<>();
        
        while (pos < source.length()) {
            skipWhitespaceAndComments();
            
            if (pos >= source.length()) {
                break;
            }
            
            CopybookToken token = nextToken();
            if (token != null && token.getType() != TokenType.UNKNOWN) {
                tokens.add(token);
            }
        }
        
        tokens.add(new CopybookToken(TokenType.EOF, "", line, column));
        return tokens;
    }
    
    private void skipWhitespaceAndComments() {
        while (pos < source.length()) {
            char c = source.charAt(pos);
            
            if (c == '\n') {
                line++;
                column = 1;
                pos++;
            } else if (Character.isWhitespace(c)) {
                column++;
                pos++;
            } else if (c == '*' && column == 1) {
                // Comment line, skip to end
                while (pos < source.length() && source.charAt(pos) != '\n') {
                    pos++;
                }
            } else {
                break;
            }
        }
    }
    
    private CopybookToken nextToken() {
        if (pos >= source.length()) {
            return new CopybookToken(TokenType.EOF, "", line, column);
        }
        
        char c = source.charAt(pos);
        int startLine = line;
        int startCol = column;
        
        // Period
        if (c == '.') {
            pos++;
            column++;
            return new CopybookToken(TokenType.PERIOD, ".", startLine, startCol);
        }
        
        // Parentheses
        if (c == '(') {
            pos++;
            column++;
            return new CopybookToken(TokenType.LPAREN, "(", startLine, startCol);
        }
        if (c == ')') {
            pos++;
            column++;
            return new CopybookToken(TokenType.RPAREN, ")", startLine, startCol);
        }
        
        // String literal
        if (c == '\'' || c == '"') {
            return readStringLiteral(c, startLine, startCol);
        }
        
        // Number (level number or numeric literal)
        if (Character.isDigit(c)) {
            return readNumber(startLine, startCol);
        }
        
        // Identifier or keyword
        if (Character.isLetter(c) || c == '-') {
            return readIdentifierOrKeyword(startLine, startCol);
        }
        
        // Unknown character, skip it
        pos++;
        column++;
        return new CopybookToken(TokenType.UNKNOWN, String.valueOf(c), startLine, startCol);
    }
    
    private CopybookToken readStringLiteral(char quote, int startLine, int startCol) {
        StringBuilder sb = new StringBuilder();
        pos++; // Skip opening quote
        column++;
        
        while (pos < source.length()) {
            char c = source.charAt(pos);
            
            if (c == quote) {
                pos++;
                column++;
                // Check for escaped quote (doubled)
                if (pos < source.length() && source.charAt(pos) == quote) {
                    sb.append(quote);
                    pos++;
                    column++;
                } else {
                    break;
                }
            } else if (c == '\n') {
                break; // Unterminated string
            } else {
                sb.append(c);
                pos++;
                column++;
            }
        }
        
        return new CopybookToken(TokenType.STRING_LITERAL, sb.toString(), startLine, startCol);
    }
    
    private CopybookToken readNumber(int startLine, int startCol) {
        StringBuilder sb = new StringBuilder();
        
        while (pos < source.length() && (Character.isDigit(source.charAt(pos)) || source.charAt(pos) == '-' || source.charAt(pos) == '+')) {
            sb.append(source.charAt(pos));
            pos++;
            column++;
        }
        
        String value = sb.toString();
        
        // Check if it's a level number (01-88)
        try {
            int num = Integer.parseInt(value);
            if (num >= 1 && num <= 88) {
                return new CopybookToken(TokenType.LEVEL_NUMBER, value, startLine, startCol);
            }
        } catch (NumberFormatException e) {
            // Not a simple integer
        }
        
        return new CopybookToken(TokenType.NUMERIC_LITERAL, value, startLine, startCol);
    }
    
    private CopybookToken readIdentifierOrKeyword(int startLine, int startCol) {
        StringBuilder sb = new StringBuilder();
        
        while (pos < source.length()) {
            char c = source.charAt(pos);
            if (Character.isLetterOrDigit(c) || c == '-' || c == '_') {
                sb.append(c);
                pos++;
                column++;
            } else {
                break;
            }
        }
        
        String value = sb.toString().toUpperCase();
        
        // Check for keywords
        TokenType keywordType = KEYWORDS.get(value);
        if (keywordType != null) {
            return new CopybookToken(keywordType, value, startLine, startCol);
        }
        
        // Check if it looks like a PIC clause value (after PIC/PICTURE keyword)
        if (PIC_PATTERN.matcher(value).matches() && !value.equals("IS")) {
            // This might be a picture string
            return new CopybookToken(TokenType.IDENTIFIER, sb.toString(), startLine, startCol);
        }
        
        return new CopybookToken(TokenType.IDENTIFIER, sb.toString(), startLine, startCol);
    }
}
