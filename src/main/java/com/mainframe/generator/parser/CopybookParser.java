package com.mainframe.generator.parser;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.copybook.util.PictureClause;
import com.mainframe.generator.codegen.copybook.util.UsageTypeParser;
import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopyDirectiveNode;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.model.input.CopybookNode;
import com.mainframe.generator.codegen.model.input.Enum88Node;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.GroupNode;
import com.mainframe.generator.codegen.model.input.UsageType;
import com.mainframe.generator.codegen.parsing.exception.ParseException;
import com.mainframe.generator.parser.CopybookToken.TokenType;

/**
 * Parser for COBOL copybook files.
 * Converts tokens into a hierarchical AST model.
 *
 * Parsing only:
 * - Builds the AST
 * - Collects COPY directives
 * - Reports diagnostics
 *
 * It does NOT compute byte sizes/offsets, indexes, or totals.
 */
public class CopybookParser {
    private static final Logger log = LoggerFactory.getLogger(CopybookParser.class);

    private final List<CopybookToken> tokens;
    private final String fileName;
    private int pos = 0;

    private final Deque<CopybookNode> nodeStack = new ArrayDeque<>();
    private final List<CopyDirectiveNode> copyDirectives = new ArrayList<>();

    public CopybookParser(List<CopybookToken> tokens, String fileName) {
        this.tokens = tokens;
        this.fileName = fileName;
    }

    public CopybookModel parse(ToolDiagnostics diagnostics) {
        GroupNode root = GroupNode.builder()
                .level(0)
                .name("ROOT")
                .sourceFile(fileName)
                .build();

        nodeStack.push(root);

        while (!isAtEnd()) {
            try {
                parseEntry(diagnostics);
            } catch (ParseException e) {
                diagnostics.getErrors().add(e.getMessage());
                skipToNextEntry();
            }
        }

        return CopybookModel.builder()
                .name(extractCopybookName())
                .sourcePath(fileName)
                .rootGroup(root)
                .copyDirectives(copyDirectives)
                .build();
    }

    private void parseEntry(ToolDiagnostics diagnostics) {
        CopybookToken token = peek();

        if (token.getType() == TokenType.COPY) {
            parseCopyDirective();
            return;
        }

        if (token.getType() == TokenType.LEVEL_NUMBER) {
            int level = Integer.parseInt(token.getValue());
            advance();

            if (level == 88) {
                parse88Level(diagnostics);
            } else {
                parseDataItem(level, diagnostics);
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

    private void parseDataItem(int level, ToolDiagnostics diagnostics) {
        int startLine = previous().getLine();

        String fieldName = "FILLER";
        boolean isFiller = false;

        if (check(TokenType.FILLER)) {
            advance();
            isFiller = true;
        } else if (check(TokenType.IDENTIFIER)) {
            fieldName = advance().getValue();
        }

        adjustStackForLevel(level);

        String redefinesTarget = null;
        if (check(TokenType.REDEFINES)) {
            advance();
            redefinesTarget = expect(TokenType.IDENTIFIER).getValue();
            diagnostics.getWarnings().add("REDEFINES detected for " + fieldName + " -> " + redefinesTarget + " at line " + startLine);
        }

        PictureClause picture = null;
        if (check(TokenType.PIC) || check(TokenType.PICTURE)) {
            advance();
            if (check(TokenType.IS)) {
                advance();
            }
            picture = parsePictureClause();
        }

        UsageType usage = UsageType.DISPLAY;
        boolean usagePresent = false;
        if (check(TokenType.USAGE)) {
            advance();
            if (check(TokenType.IS)) {
                advance();
            }
            usagePresent = true;
        }
        if (!isAtEnd() && peek().isUsageType()) {
            usage = parseUsageType();
            usagePresent = true;
        }

        int occursCount = 1;
        String occursDepending = null;
        if (check(TokenType.OCCURS)) {
            advance();
            if (check(TokenType.NUMERIC_LITERAL) || check(TokenType.LEVEL_NUMBER)) {
                int firstNum = Integer.parseInt(advance().getValue());
                int maxOccurs = firstNum;

                if ((check(TokenType.IDENTIFIER) && peek().getValue().equalsIgnoreCase("TO"))
                        || check(TokenType.THRU) || check(TokenType.THROUGH)) {
                    advance();
                    if (check(TokenType.NUMERIC_LITERAL) || check(TokenType.LEVEL_NUMBER)) {
                        maxOccurs = Integer.parseInt(advance().getValue());
                    } else {
                        throw new ParseException("Expected numeric literal after TO/THRU in OCCURS at line " + peek().getLine());
                    }
                }

                if (check(TokenType.TIMES)) {
                    advance();
                }

                if (check(TokenType.DEPENDING)) {
                    advance();
                    if (check(TokenType.ON)) {
                        advance();
                    }
                    occursDepending = expect(TokenType.IDENTIFIER).getValue();
                }

                occursCount = Math.max(1, maxOccurs);
            } else {
                throw new ParseException("Expected numeric literal after OCCURS at line " + peek().getLine());
            }
        }

        String value = null;
        if (check(TokenType.VALUE) || check(TokenType.VALUES)) {
            advance();
            if (check(TokenType.IS) || check(TokenType.ARE)) {
                advance();
            }
            if (check(TokenType.STRING_LITERAL) || check(TokenType.NUMERIC_LITERAL)) {
                value = advance().getValue();
            }
        }

        expectPeriod();

        boolean isElementary = picture != null || usagePresent;

        CopybookNode parent = nodeStack.peek();
        CopybookNode node;

        if (isElementary) {
            node = FieldNode.builder()
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
        } else {
            node = GroupNode.builder()
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
        }

        if (parent instanceof GroupNode groupParent) {
            groupParent.addChild(node);
        }

        if (node instanceof GroupNode group) {
            nodeStack.push(group);
        }

        log.debug("Parsed {}: {} at line {}", isElementary ? "field" : "group", fieldName, startLine);
    }

    private void parse88Level(ToolDiagnostics diagnostics) {
        int startLine = previous().getLine();

        String conditionName = expect(TokenType.IDENTIFIER).getValue();

        expect(TokenType.VALUE);
        if (check(TokenType.IS) || check(TokenType.ARE)) {
            advance();
        }

        List<String> values = new ArrayList<>();
        String throughValue = null;

        while (check(TokenType.STRING_LITERAL) || check(TokenType.NUMERIC_LITERAL)) {
            values.add(advance().getValue());

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

        CopybookNode parent = nodeStack.peek();
        if (parent instanceof GroupNode group && !group.getChildren().isEmpty()) {
            CopybookNode lastChild = group.getChildren().get(group.getChildren().size() - 1);
            if (lastChild instanceof FieldNode field) {
                field.addEnum88(enum88);
            } else {
                diagnostics.getWarnings().add("88-level " + conditionName + " not attached to a field at line " + startLine);
            }
        } else {
            diagnostics.getWarnings().add("88-level " + conditionName + " encountered with no parent field at line " + startLine);
        }
    }

    private PictureClause parsePictureClause() {
        StringBuilder picBuilder = new StringBuilder();

        while (!isAtEnd() && !check(TokenType.PERIOD) &&
                !check(TokenType.USAGE) && !check(TokenType.OCCURS) &&
                !check(TokenType.VALUE) && !check(TokenType.VALUES) &&
                !check(TokenType.REDEFINES) && !check(TokenType.DEPENDING) &&
                !peek().isUsageType()) {

            CopybookToken token = peek();
            TokenType type = token.getType();

            if (type == TokenType.IDENTIFIER ||
                type == TokenType.NUMERIC_LITERAL ||
                type == TokenType.LEVEL_NUMBER ||
                type == TokenType.LPAREN ||
                type == TokenType.RPAREN) {
                picBuilder.append(advance().getValue());
            } else {
                break;
            }
        }

        String picString = picBuilder.toString();
        log.debug("Parsed PIC string: '{}'", picString);

        return PictureClause.parse(picString);
    }

    private UsageType parseUsageType() {
        CopybookToken token = advance();
        return UsageTypeParser.fromCobol(token.getValue());
    }

    private void adjustStackForLevel(int level) {
        while (nodeStack.size() > 1) {
            CopybookNode top = nodeStack.peek();
            if (top.getLevel() >= level) {
                nodeStack.pop();
            } else {
                break;
            }
        }
    }

    private String extractCopybookName() {
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
    }
}
