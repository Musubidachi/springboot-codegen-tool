package com.mainframe.generator.codegen.util;

/**
 * Utility for deriving container keys from 01-level COBOL record names.
 *
 * Container keys are normalized deterministically:
 * - Hyphens are replaced with underscores
 * - Name is converted to uppercase
 *
 * Example: ABC-REQUEST-REC -> ABC_REQUEST_REC
 */
public class ContainerKeyUtil {

    private ContainerKeyUtil() {
        // Utility class
    }

    /**
     * Normalizes a COBOL record name to a container key.
     *
     * @param recordName the 01-level record name
     * @return the normalized container key
     */
    public static String normalizeRecordName(String recordName) {
        if (recordName == null || recordName.isEmpty()) {
            return recordName;
        }
        return recordName.replace("-", "_").toUpperCase();
    }

    /**
     * Converts a container key back to a valid Java identifier.
     *
     * @param containerKey the container key
     * @return a valid Java field name
     */
    public static String toJavaFieldName(String containerKey) {
        if (containerKey == null || containerKey.isEmpty()) {
            return containerKey;
        }
        return containerKey.toLowerCase();
    }
}
