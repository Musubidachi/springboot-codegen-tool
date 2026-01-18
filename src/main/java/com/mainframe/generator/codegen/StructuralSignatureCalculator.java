package com.mainframe.generator.codegen;

import com.mainframe.generator.model.*;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * Calculates a structural signature (hash) for a copybook's DTO structure.
 * Two copybooks with identical structure will have the same signature.
 */
public class StructuralSignatureCalculator {

    /**
     * Calculate a structural signature for a copybook model.
     * The signature includes:
     * - Normalized field/group names
     * - PIC/usage semantics (digits/scale/signed)
     * - OCCURS counts and ODO dependency names
     * - REDEFINES grouping logic
     * - Byte lengths and offsets
     */
    public static String calculateSignature(CopybookModel model) {
        StringBuilder sb = new StringBuilder();
        if (model.getRootGroup() != null) {
            appendNodeSignature(sb, model.getRootGroup(), "");
        }
        return hashString(sb.toString());
    }

    private static void appendNodeSignature(StringBuilder sb, CopybookNode node, String indent) {
        if (node instanceof GroupNode group) {
            // Group signature
            sb.append(indent).append("GROUP:");
            sb.append(normalizeFieldName(group.getName()));
            sb.append(":OCCURS=").append(group.getOccursCount());
            if (group.getOccursDepending() != null) {
                sb.append(":ODO=").append(normalizeFieldName(group.getOccursDepending()));
            }
            if (group.isRedefines()) {
                sb.append(":REDEFINES=").append(normalizeFieldName(group.getRedefinesTarget()));
            }
            sb.append(":OFFSET=").append(group.getStartOffset());
            sb.append(":LENGTH=").append(group.getByteLength());
            sb.append("\n");

            // Recurse into children
            for (CopybookNode child : group.getChildren()) {
                appendNodeSignature(sb, child, indent + "  ");
            }
        } else if (node instanceof FieldNode field) {
            // Field signature
            sb.append(indent).append("FIELD:");
            sb.append(normalizeFieldName(field.getName()));
            sb.append(":PIC=").append(normalizePicture(field.getPictureClause()));
            sb.append(":USAGE=").append(field.getUsage() != null ? field.getUsage() : "DISPLAY");
            sb.append(":OCCURS=").append(field.getOccursCount());
            if (field.getOccursDepending() != null) {
                sb.append(":ODO=").append(normalizeFieldName(field.getOccursDepending()));
            }
            if (field.isRedefines()) {
                sb.append(":REDEFINES=").append(normalizeFieldName(field.getRedefinesTarget()));
            }
            sb.append(":OFFSET=").append(field.getStartOffset());
            sb.append(":LENGTH=").append(field.getByteLength());
            sb.append(":DIGITS=").append(field.getDigits());
            sb.append(":SCALE=").append(field.getScale());
            sb.append(":SIGNED=").append(field.isSigned());
            sb.append("\n");

            // Include enum values if present
            if (field.getEnum88Values() != null && !field.getEnum88Values().isEmpty()) {
                for (Enum88Node enumNode : field.getEnum88Values()) {
                    sb.append(indent).append("  ENUM88:");
                    sb.append(normalizeFieldName(enumNode.getName()));
                    sb.append(":VALUE=").append(enumNode.getValue());
                    sb.append("\n");
                }
            }
        }
    }

    /**
     * Normalize field name for signature comparison.
     * Converts to uppercase and removes hyphens.
     */
    private static String normalizeFieldName(String name) {
        if (name == null) return "NULL";
        return name.toUpperCase().replace("-", "");
    }

    /**
     * Normalize picture clause for signature comparison.
     * Handles null picture clauses (COMP-1, COMP-2).
     */
    private static String normalizePicture(String pic) {
        if (pic == null) return "NULL";
        // Remove spaces and normalize to uppercase
        return pic.toUpperCase().replaceAll("\\s+", "");
    }

    /**
     * Hash a string to produce a compact signature.
     */
    private static String hashString(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(input.getBytes(StandardCharsets.UTF_8));
            // Return first 16 characters of hex hash for compact representation
            return HexFormat.of().formatHex(hash).substring(0, 16);
        } catch (NoSuchAlgorithmException e) {
            // Fallback to simple hash code
            return String.format("%016x", input.hashCode() & 0xFFFFFFFFFFFFFFFFL);
        }
    }

    /**
     * Check if one copybook structure is a prefix/subset of another.
     * Returns true if 'subset' is a safe structural prefix of 'superset'.
     * This is used for inheritance factoring with --infer-inheritance.
     */
    public static boolean isStructuralSubset(CopybookModel subset, CopybookModel superset) {
        if (subset.getRootGroup() == null || superset.getRootGroup() == null) {
            return false;
        }
        return isNodeSubset(subset.getRootGroup(), superset.getRootGroup());
    }

    private static boolean isNodeSubset(CopybookNode subsetNode, CopybookNode supersetNode) {
        // Both must be same type
        if (subsetNode.getClass() != supersetNode.getClass()) {
            return false;
        }

        if (subsetNode instanceof GroupNode subsetGroup && supersetNode instanceof GroupNode supersetGroup) {
            // Groups must have same name, occurs, and redefines semantics
            if (!normalizeFieldName(subsetGroup.getName()).equals(normalizeFieldName(supersetGroup.getName()))) {
                return false;
            }
            if (subsetGroup.getOccursCount() != supersetGroup.getOccursCount()) {
                return false;
            }
            if (subsetGroup.isRedefines() != supersetGroup.isRedefines()) {
                return false;
            }

            // All children of subset must match prefix of superset children
            if (subsetGroup.getChildren().size() > supersetGroup.getChildren().size()) {
                return false;
            }

            for (int i = 0; i < subsetGroup.getChildren().size(); i++) {
                if (!isNodeSubset(subsetGroup.getChildren().get(i), supersetGroup.getChildren().get(i))) {
                    return false;
                }
            }
            return true;

        } else if (subsetNode instanceof FieldNode subsetField && supersetNode instanceof FieldNode supersetField) {
            // Fields must match exactly in structure
            return normalizeFieldName(subsetField.getName()).equals(normalizeFieldName(supersetField.getName()))
                    && normalizePicture(subsetField.getPictureClause()).equals(normalizePicture(supersetField.getPictureClause()))
                    && (subsetField.getUsage() != null ? subsetField.getUsage() : "DISPLAY")
                        .equals(supersetField.getUsage() != null ? supersetField.getUsage() : "DISPLAY")
                    && subsetField.getOccursCount() == supersetField.getOccursCount()
                    && subsetField.getByteLength() == supersetField.getByteLength()
                    && subsetField.getDigits() == supersetField.getDigits()
                    && subsetField.getScale() == supersetField.getScale()
                    && subsetField.isSigned() == supersetField.isSigned()
                    && subsetField.isRedefines() == supersetField.isRedefines();
        }

        return false;
    }
}
