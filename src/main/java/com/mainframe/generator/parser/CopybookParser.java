// (Full file contents)
package com.mainframe.generator.parser;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.model.CopyDirectiveNode;
import com.mainframe.generator.model.CopybookModel;
import com.mainframe.generator.model.CopybookNode;
import com.mainframe.generator.model.Enum88Node;
import com.mainframe.generator.model.FieldNode;
import com.mainframe.generator.model.GroupNode;
import com.mainframe.generator.model.PictureClause;
import com.mainframe.generator.model.RedefinesNode;
import com.mainframe.generator.model.UsageType;
import com.mainframe.generator.parser.CopybookToken.TokenType;

/**
 * Parser for COBOL copybook files.
 * Converts tokens into a hierarchical AST model.
 */
public class CopybookParser {
    private static final Logger log = LoggerFactory.getLogger(CopybookParser.class);
    
    private final List<CopybookToken> tokens;
    private final String fileName;
    private int pos = 0;
    private final Deque<CopybookNode> nodeStack = new ArrayDeque<>();
    private final Map<String, FieldNode> fieldsByName = new HashMap<>();
    private final Map<String, GroupNode> groupsByName = new HashMap<>();
    private final List<CopyDirectiveNode> copyDirectives = new ArrayList<>();
    private final List<RedefinesNode> redefinesList = new ArrayList<>();
    private final List<String> warnings = new ArrayList<>();
    private final List<String> errors = new ArrayList<>();
    
    public CopybookParser(List<CopybookToken> tokens, String fileName) {
        this.tokens = tokens;
        this.fileName = fileName;
    }
    
    /**
     * Parse the tokens into a CopybookModel.
     */
    public CopybookModel parse() {
        GroupNode root = GroupNode.builder()
                .level(0)
                .name("ROOT")
                .sourceFile(fileName)
                .build();
        
        nodeStack.push(root);
        
        while (!isAtEnd()) {
            try {
                parseEntry();
            } catch (ParseException e) {
                errors.add(e.getMessage());
                // Try to recover by skipping to next period
                skipToNextEntry();
            }
        }
        
        // Calculate byte offsets
        calculateOffsets(root, 0);
        
        // Build the model
        return CopybookModel.builder()
                .name(extractCopybookName())
                .sourcePath(fileName)
                .rootGroup(root)
                .copyDirectives(copyDirectives)
                .redefines(redefinesList)
                .fieldsByName(fieldsByName)
                .groupsByName(groupsByName)
                .totalByteLength(root.calculateByteLength())
                .warnings(warnings)
                .errors(errors)
                .build();
    }
    
    private void parseEntry() {
        CopybookToken token = peek();
        
        if (token.getType() == TokenType.COPY) {
            parseCopyDirective();
            return;
        }
        
        if (token.getType() == TokenType.LEVEL_NUMBER) {
            int level = Integer.parseInt(token.getValue());
            advance();
            
            if (level == 88) {
                parse88Level();
            } else {
                parseDataItem(level);
            }
            return;
        }
        
        if (token.getType() == TokenType.PERIOD) {
            advance();
            return;
        }
        
        if (token.getType() == TokenType.EOF) {
            return;
        }
        
        // Unexpected token, skip it
        log.debug("Skipping unexpected token: {} at line {}", token.getValue(), token.getLine());
        advance();
    }
    
    private void parseCopyDirective() {
        expect(TokenType.COPY);
        
        CopybookToken nameToken = expect(TokenType.IDENTIFIER);
        String copybookName = nameToken.getValue();
        
        String library = null;
        if (check(TokenType.IDENTIFIER) && peek().getValue().equalsIgnoreCase("OF")) {
            advance();
            library = expect(TokenType.IDENTIFIER).getValue();
        }
        
        expectPeriod();
        
        CopyDirectiveNode directive = CopyDirectiveNode.builder()
                .copybookName(copybookName)
                .library(library)
                .sourceFile(fileName)
                .sourceLine(nameToken.getLine())
                .build();
        
        copyDirectives.add(directive);
        log.debug("Parsed COPY directive: {} at line {}", copybookName, nameToken.getLine());
    }
    
    private void parseDataItem(int level) {
        int startLine = previous().getLine();
        
        // Parse field name
        String fieldName = "FILLER";
        boolean isFiller = false;
        
        if (check(TokenType.FILLER)) {
            advance();
            isFiller = true;
        } else if (check(TokenType.IDENTIFIER)) {
            fieldName = advance().getValue();
        }
        
        log.debug("Parsing data item: level={}, name={} at line {}", level, fieldName, startLine);
        
        // Adjust stack for hierarchical structure
        adjustStackForLevel(level);
        
        // Check for REDEFINES
        String redefinesTarget = null;
        if (check(TokenType.REDEFINES)) {
            advance();
            redefinesTarget = expect(TokenType.IDENTIFIER).getValue();
            warnings.add("REDEFINES detected for " + fieldName + " -> " + redefinesTarget + " at line " + startLine);
        }
        
        // Parse PIC clause if present
        PictureClause picture = null;
        if (check(TokenType.PIC) || check(TokenType.PICTURE)) {
            advance();
            // Skip optional IS
            if (check(TokenType.IS)) {
                advance();
            }
            picture = parsePictureClause();
        }
        
        // Parse USAGE clause (or direct COMP-1 / COMP-2 tokens)
        UsageType usage = UsageType.DISPLAY;
        boolean usagePresent = false;
        if (check(TokenType.USAGE)) {
            advance();
            // Skip optional IS
            if (check(TokenType.IS)) {
                advance();
            }
            usagePresent = true;
        }
        if (peek().isUsageType()) {
            usage = parseUsageType();
            usagePresent = true;
        }
        
        // Parse OCCURS clause (supports both OCCURS <n> [TIMES] and OCCURS <min> TO <max> DEPENDING ON ...)
        int occursCount = 1;
        String occursDepending = null;
        if (check(TokenType.OCCURS)) {
            advance();
            
            // accept a numeric token (NUMERIC_LITERAL or LEVEL_NUMBER)
            if (check(TokenType.NUMERIC_LITERAL) || check(TokenType.LEVEL_NUMBER)) {
                int firstNum = Integer.parseInt(advance().getValue());
                int minOccurs = firstNum, maxOccurs = firstNum;
                
                // Handle optional range: "TO" (tokenized as IDENTIFIER), or THRU/THROUGH tokens
                if ((check(TokenType.IDENTIFIER) && peek().getValue().equalsIgnoreCase("TO"))
                        || check(TokenType.THRU) || check(TokenType.THROUGH)) {
                    advance(); // consume TO/THRU/THROUGH
                    if (check(TokenType.NUMERIC_LITERAL) || check(TokenType.LEVEL_NUMBER)) {
                        maxOccurs = Integer.parseInt(advance().getValue());
                    } else {
                        throw new ParseException("Expected numeric literal after TO/THRU in OCCURS at line " + peek().getLine());
                    }
                }
                
                // Optional TIMES (may or may not be present)
                if (check(TokenType.TIMES)) {
                    advance();
                }
                
                // Optional DEPENDING ON
                if (check(TokenType.DEPENDING)) {
                    advance();
                    if (check(TokenType.ON)) {
                        advance();
                    }
                    occursDepending = expect(TokenType.IDENTIFIER).getValue();
                }
                
                occursCount = Math.max(1, maxOccurs);
                if (minOccurs != maxOccurs) {
                    log.debug("Parsed OCCURS range for {}: min={}, max={} at line {}", fieldName, minOccurs, maxOccurs, startLine);
                } else {
                    log.debug("Parsed OCCURS count for {}: {} at line {}", fieldName, occursCount, startLine);
                }
            } else {
                throw new ParseException("Expected numeric literal after OCCURS at line " + peek().getLine());
            }
        }
        
        // Parse VALUE clause
        String value = null;
        if (check(TokenType.VALUE) || check(TokenType.VALUES)) {
            advance();
            // Skip optional IS/ARE
            if (check(TokenType.IS) || check(TokenType.ARE)) {
                advance();
            }
            if (check(TokenType.STRING_LITERAL) || check(TokenType.NUMERIC_LITERAL)) {
                value = advance().getValue();
            }
        }
        
        // Expect period
        expectPeriod();
        
        // Decide whether this is elementary (field) or group:
        boolean isElementary = picture != null || usagePresent;
        
        // Create appropriate node type
        CopybookNode node;
        
        // Add to parent FIRST (before pushing groups to stack)
        CopybookNode parent = nodeStack.peek();
        
        if (isElementary) {
            // It's a leaf field
            FieldNode field = FieldNode.builder()
                    .level(level)
                    .name(fieldName)
                    .originalName(fieldName)
                    .picture(picture)
                    .usage(usage)
                    .occursCount(occursCount)
                    .occursDepending(occursDepending)
                    .isRedefines(redefinesTarget != null)
                    .redefinesTarget(redefinesTarget)
                    .value(value)
                    .isFiller(isFiller)
                    .sourceFile(fileName)
                    .sourceLine(startLine)
                    .build();
            
            field.setByteLength(field.calculateByteLength());
            
            if (!isFiller) {
                fieldsByName.put(fieldName.toUpperCase(), field);
            }
            
            node = field;
            log.debug("Parsed field: {} PIC {} USAGE {} at line {}", 
                    fieldName, picture != null ? picture.getRawPicture() : "<none>", usage, startLine);
        } else {
            // It's a group
            GroupNode group = GroupNode.builder()
                    .level(level)
                    .name(fieldName)
                    .originalName(fieldName)
                    .occursCount(occursCount)
                    .occursDepending(occursDepending)
                    .isRedefines(redefinesTarget != null)
                    .redefinesTarget(redefinesTarget)
                    .sourceFile(fileName)
                    .sourceLine(startLine)
                    .build();
            
            groupsByName.put(fieldName.toUpperCase(), group);
            node = group;
            log.debug("Parsed group: {} at line {}", fieldName, startLine);
        }
        
        // Add to parent
        if (parent instanceof GroupNode groupParent) {
            groupParent.addChild(node);
        }
        
        // Push groups to stack AFTER adding to parent (so children go into this group)
        if (node instanceof GroupNode group) {
            nodeStack.push(group);
        }
        
        // Handle REDEFINES
        if (redefinesTarget != null) {
            RedefinesNode redefines = RedefinesNode.builder()
                    .level(level)
                    .name(fieldName)
                    .targetName(redefinesTarget)
                    .sourceFile(fileName)
                    .sourceLine(startLine)
                    .build();
            redefinesList.add(redefines);
        }
    }
    
    private void parse88Level() {
        int startLine = previous().getLine();
        
        String conditionName = expect(TokenType.IDENTIFIER).getValue();
        
        expect(TokenType.VALUE);
        // Skip optional IS/ARE
        if (check(TokenType.IS) || check(TokenType.ARE)) {
            advance();
        }
        
        List<String> values = new ArrayList<>();
        String throughValue = null;
        
        // Parse value(s)
        while (check(TokenType.STRING_LITERAL) || check(TokenType.NUMERIC_LITERAL)) {
            values.add(advance().getValue());
            
            // Check for THRU/THROUGH
            if (check(TokenType.THRU) || check(TokenType.THROUGH)) {
                advance();
                if (check(TokenType.STRING_LITERAL) || check(TokenType.NUMERIC_LITERAL)) {
                    throughValue = advance().getValue();
                }
            }
        }
        
        expectPeriod();
        
        Enum88Node enum88 = Enum88Node.builder()
                .name(conditionName)
                .originalName(conditionName)
                .values(values)
                .throughValue(throughValue)
                .sourceFile(fileName)
                .sourceLine(startLine)
                .build();
        
        // Add to parent field
        CopybookNode parent = nodeStack.peek();
        if (parent instanceof GroupNode group && !group.getChildren().isEmpty()) {
            CopybookNode lastChild = group.getChildren().get(group.getChildren().size() - 1);
            if (lastChild instanceof FieldNode field) {
                field.addEnum88(enum88);
                log.debug("Parsed 88-level: {} for field {} at line {}", 
                        conditionName, field.getName(), startLine);
            }
        }
    }
    
    private PictureClause parsePictureClause() {
        StringBuilder picBuilder = new StringBuilder();
        
        // Collect all picture elements until we hit a clause terminator
        while (!isAtEnd() && !check(TokenType.PERIOD) && 
               !check(TokenType.USAGE) && !check(TokenType.OCCURS) &&
               !check(TokenType.VALUE) && !check(TokenType.VALUES) &&
               !check(TokenType.REDEFINES) && !check(TokenType.DEPENDING) &&
               !peek().isUsageType()) {
            
            CopybookToken token = peek();
            TokenType type = token.getType();
            
            // Accept identifiers (like X, A, Z, S), numerics, parentheses, and level numbers as part of PIC
            // Note: LEVEL_NUMBER is needed because numbers 1-88 (like "8" in X(8)) are tokenized as level numbers
            if (type == TokenType.IDENTIFIER || 
                type == TokenType.NUMERIC_LITERAL ||
                type == TokenType.LEVEL_NUMBER ||
                type == TokenType.LPAREN ||
                type == TokenType.RPAREN) {
                picBuilder.append(advance().getValue());
            } else {
                // Stop on unrecognized token types (keywords, etc.)
                break;
            }
        }
        
        String picString = picBuilder.toString();
        log.debug("Parsed PIC string: '{}'", picString);
        
        PictureClause pic = PictureClause.parse(picString);
        if (pic != null) {
            log.debug("  Parsed PIC: totalLength={}, intDigits={}, decDigits={}, numeric={}, alpha={}", 
                    pic.getTotalLength(), pic.getIntegerDigits(), pic.getDecimalDigits(),
                    pic.isNumeric(), pic.isAlphanumeric());
        }
        
        return pic;
    }
    
    private UsageType parseUsageType() {
        CopybookToken token = advance();
        return UsageType.fromCobol(token.getValue());
    }
    
    private void adjustStackForLevel(int level) {
        // Pop nodes from stack until we find a parent with lower level
        while (nodeStack.size() > 1) {
            CopybookNode top = nodeStack.peek();
            if (top.getLevel() >= level) {
                nodeStack.pop();
            } else {
                break;
            }
        }
    }
    
    private int calculateOffsets(CopybookNode node, int currentOffset) {
        node.setStartOffset(currentOffset);

        if (node instanceof FieldNode field) {
            int length = field.getByteLength() * field.getOccursCount();
            node.setByteLength(field.getByteLength());
            return currentOffset + length;
        } else if (node instanceof GroupNode group) {
            // First pass: identify redefine targets
            java.util.Set<String> redefineTargets = new java.util.HashSet<>();
            for (CopybookNode child : group.getChildren()) {
                if ((child instanceof GroupNode g && g.isRedefines()) ||
                    (child instanceof FieldNode f && f.isRedefines())) {
                    String target = getRedefinesTarget(child);
                    if (target != null) {
                        redefineTargets.add(target);
                    }
                }
            }

            int groupStart = currentOffset;
            int currentGroupOffset = groupStart;
            String currentRedefineTarget = null;
            int redefineSetStartOffset = 0;
            int maxRedefineLength = 0;

            for (CopybookNode child : group.getChildren()) {
                boolean isRedefine = child instanceof RedefinesNode ||
                    (child instanceof GroupNode g && g.isRedefines()) ||
                    (child instanceof FieldNode f && f.isRedefines());

                if (isRedefine) {
                    String target = getRedefinesTarget(child);

                    if (currentRedefineTarget != null && !currentRedefineTarget.equals(target)) {
                        // Switching to a different redefine set, finalize the previous one
                        currentGroupOffset = redefineSetStartOffset + maxRedefineLength;
                        maxRedefineLength = 0;
                    }

                    // Calculate offsets for this redefine
                    int redefineEnd = calculateOffsets(child, redefineSetStartOffset);
                    int redefineLength = redefineEnd - redefineSetStartOffset;
                    maxRedefineLength = Math.max(maxRedefineLength, redefineLength);
                    currentRedefineTarget = target;
                } else {
                    // Non-redefine field
                    boolean isTarget = redefineTargets.contains(child.getName());

                    if (isTarget) {
                        // This field is a redefine target, so it starts a redefine set
                        if (currentRedefineTarget != null && maxRedefineLength > 0) {
                            // Finalize previous redefine set
                            currentGroupOffset = redefineSetStartOffset + maxRedefineLength;
                        }

                        redefineSetStartOffset = currentGroupOffset;
                        int childEnd = calculateOffsets(child, currentGroupOffset);
                        int childLength = childEnd - currentGroupOffset;

                        currentRedefineTarget = child.getName();
                        maxRedefineLength = childLength;
                        // Don't advance currentGroupOffset yet
                    } else {
                        // Normal field not involved in redefines
                        if (maxRedefineLength > 0) {
                            currentGroupOffset = redefineSetStartOffset + maxRedefineLength;
                            maxRedefineLength = 0;
                            currentRedefineTarget = null;
                        }
                        currentGroupOffset = calculateOffsets(child, currentGroupOffset);
                    }
                }
            }

            // Handle trailing redefines
            if (maxRedefineLength > 0) {
                currentGroupOffset = redefineSetStartOffset + maxRedefineLength;
            }

            int singleLength = currentGroupOffset - groupStart;
            group.setByteLength(singleLength);

            // Handle OCCURS
            if (group.getOccursCount() > 1) {
                currentOffset = groupStart + (singleLength * group.getOccursCount());
            } else {
                currentOffset = currentGroupOffset;
            }
        }

        return currentOffset;
    }

    private String getRedefinesTarget(CopybookNode node) {
        if (node instanceof GroupNode g && g.isRedefines()) {
            return g.getRedefinesTarget();
        } else if (node instanceof FieldNode f && f.isRedefines()) {
            return f.getRedefinesTarget();
        }
        return null;
    }
    
    private String extractCopybookName() {
        // Extract name from filename
        if (fileName != null) {
            String name = fileName;
            int lastSlash = Math.max(name.lastIndexOf('/'), name.lastIndexOf('\\'));
            if (lastSlash >= 0) {
                name = name.substring(lastSlash + 1);
            }
            int dot = name.lastIndexOf('.');
            if (dot >= 0) {
                name = name.substring(0, dot);
            }
            return name.toUpperCase();
        }
        return "UNKNOWN";
    }
    
    private void skipToNextEntry() {
        while (!isAtEnd() && !check(TokenType.PERIOD) && !check(TokenType.LEVEL_NUMBER)) {
            advance();
        }
        if (check(TokenType.PERIOD)) {
            advance();
        }
    }
    
    // Token navigation helpers
    
    private boolean isAtEnd() {
        return peek().getType() == TokenType.EOF;
    }
    
    private CopybookToken peek() {
        return tokens.get(pos);
    }
    
    private CopybookToken previous() {
        return tokens.get(pos - 1);
    }
    
    private boolean check(TokenType type) {
        if (isAtEnd()) return false;
        return peek().getType() == type;
    }
    
    private CopybookToken advance() {
        if (!isAtEnd()) pos++;
        return previous();
    }
    
    private CopybookToken expect(TokenType type) {
        if (check(type)) {
            return advance();
        }
        throw new ParseException("Expected " + type + " but found " + peek().getType() + 
                " at line " + peek().getLine());
    }
    
    private void expectPeriod() {
        if (check(TokenType.PERIOD)) {
            advance();
        }
        // Period is optional in some contexts, so don't throw an error
    }
    
    public static class ParseException extends RuntimeException {
        public ParseException(String message) {
            super(message);
        }
    }
}