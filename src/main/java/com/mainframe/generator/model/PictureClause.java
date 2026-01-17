package com.mainframe.generator.model;

import lombok.Builder;
import lombok.Data;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a parsed COBOL PIC (PICTURE) clause.
 */
@Data
@Builder
public class PictureClause {
    private String rawPicture;
    private boolean signed;
    private boolean numeric;
    private boolean alphanumeric;
    private int integerDigits;
    private int decimalDigits;
    private int totalLength;
    private boolean hasDecimalPoint;
    private boolean hasImpliedDecimal;
    private String expandedPicture;

    // Patterns for parsing PIC clauses
    private static final Pattern REPEAT_PATTERN = Pattern.compile("(\\w)\\((\\d+)\\)");
    private static final Pattern SIGNED_PATTERN = Pattern.compile("^S");
    private static final Pattern NUMERIC_PATTERN = Pattern.compile("^S?9");
    private static final Pattern DECIMAL_PATTERN = Pattern.compile("V");
    private static final Pattern EXPLICIT_DECIMAL = Pattern.compile("\\.");

    /**
     * Parse a raw COBOL PIC clause string.
     */
    public static PictureClause parse(String pic) {
        if (pic == null || pic.isBlank()) {
            return null;
        }

        String normalized = pic.toUpperCase().replaceAll("\\s+", "").replace("PIC", "").replace("PICTURE", "");
        String expanded = expandPicture(normalized);

        boolean signed = SIGNED_PATTERN.matcher(normalized).find();
        boolean numeric = NUMERIC_PATTERN.matcher(normalized).find();
        boolean alphanumeric = !numeric && (expanded.contains("X") || expanded.contains("A"));
        boolean hasImplied = DECIMAL_PATTERN.matcher(normalized).find();
        boolean hasExplicit = EXPLICIT_DECIMAL.matcher(normalized).find();

        int intDigits = 0;
        int decDigits = 0;
        int totalLen = 0;

        if (numeric) {
            // Count digits before and after V (or .)
            String withoutSign = expanded.replaceFirst("^S", "");
            int decimalPos = withoutSign.indexOf('V');
            if (decimalPos < 0) {
                decimalPos = withoutSign.indexOf('.');
            }

            if (decimalPos >= 0) {
                String beforeDecimal = withoutSign.substring(0, decimalPos);
                String afterDecimal = withoutSign.substring(decimalPos + 1);
                intDigits = countDigits(beforeDecimal);
                decDigits = countDigits(afterDecimal);
            } else {
                intDigits = countDigits(withoutSign);
            }
            totalLen = intDigits + decDigits;
        } else {
            // Alphanumeric - count all characters
            totalLen = expanded.length();
        }

        return PictureClause.builder()
                .rawPicture(pic)
                .signed(signed)
                .numeric(numeric)
                .alphanumeric(alphanumeric)
                .integerDigits(intDigits)
                .decimalDigits(decDigits)
                .totalLength(totalLen)
                .hasDecimalPoint(hasExplicit)
                .hasImpliedDecimal(hasImplied)
                .expandedPicture(expanded)
                .build();
    }

    /**
     * Expand repeat notation like X(10) to XXXXXXXXXX.
     */
    private static String expandPicture(String pic) {
        String result = pic;
        Matcher matcher = REPEAT_PATTERN.matcher(result);
        StringBuffer sb = new StringBuffer();
        
        while (matcher.find()) {
            String ch = matcher.group(1);
            int count = Integer.parseInt(matcher.group(2));
            matcher.appendReplacement(sb, ch.repeat(count));
        }
        matcher.appendTail(sb);
        
        return sb.toString();
    }

    private static int countDigits(String s) {
        return (int) s.chars().filter(c -> c == '9' || c == 'Z' || c == '*').count();
    }

    /**
     * Get the Java type that best represents this picture.
     */
    public String getJavaType(UsageType usage) {
        if (!numeric) {
            return "String";
        }

        if (decimalDigits > 0 || hasImpliedDecimal) {
            return "BigDecimal";
        }

        // For integers, choose type based on size
        int totalDigits = integerDigits;
        
        if (usage == UsageType.BINARY || usage == UsageType.COMP_5) {
            // Binary fields have specific byte sizes
            int byteLen = getBinaryByteLength();
            if (byteLen <= 2) return "Short";
            if (byteLen <= 4) return "Integer";
            return "Long";
        }

        // Display or packed decimal integers
        if (totalDigits <= 9) return "Integer";
        if (totalDigits <= 18) return "Long";
        return "BigDecimal";
    }

    /**
     * Calculate byte length for this picture and usage type.
     */
    public int getByteLength(UsageType usage) {
        if (!numeric) {
            return totalLength;
        }

        return switch (usage) {
            case DISPLAY -> totalLength + (signed ? 0 : 0); // Sign embedded in last digit for zoned
            case BINARY, COMP_5 -> getBinaryByteLength();
            case PACKED_DECIMAL -> (totalLength / 2) + 1; // (digits + 1) / 2 rounded up
            case COMP_1 -> 4; // Single precision float
            case COMP_2 -> 8; // Double precision float
        };
    }

    public int getBinaryByteLength() {
        int digits = integerDigits + decimalDigits;
        if (digits <= 4) return 2;
        if (digits <= 9) return 4;
        return 8;
    }
}
