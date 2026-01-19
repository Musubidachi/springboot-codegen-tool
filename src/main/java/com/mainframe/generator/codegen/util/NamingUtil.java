package com.mainframe.generator.codegen.util;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import com.mainframe.generator.codegen.model.input.CopybookModel;

/**
 * Utility for consistent Java naming conventions.
 */
public class NamingUtil {

    private NamingUtil() {
        // Utility class
    }

    /**
     * Converts COBOL-NAME or cobol_name to PascalCase.
     */
    public static String toPascalCase(String name) {
        if (name == null || name.isEmpty()) {
            return name;
        }
        return Arrays.stream(name.split("[-_]"))
                .map(NamingUtil::capitalize)
                .collect(Collectors.joining(""));
    }

    /**
     * Converts COBOL-NAME to camelCase.
     */
    public static String toCamelCase(String name) {
        String pascal = toPascalCase(name);
        if (pascal.isEmpty()) {
            return pascal;
        }
        return pascal.substring(0, 1).toLowerCase() + pascal.substring(1);
    }

    /**
     * Converts name to SCREAMING_SNAKE_CASE for enum constants.
     */
    public static String toScreamingSnakeCase(String name) {
        if (name == null || name.isEmpty()) {
            return name;
        }
        // Handle camelCase or PascalCase
        String result = name.replaceAll("([a-z])([A-Z])", "$1_$2");
        // Handle existing hyphens/underscores
        result = result.replaceAll("[-\\s]+", "_");
        return result.toUpperCase();
    }

    /**
     * Disambiguates class name by appending package type or number suffix.
     */
    public static String disambiguateClassName(String baseName, CopybookModel model,
                                                Set<String> usedNames) {
        // Try package-type suffix first
        String withType = baseName + "Dto";
        if (!usedNames.contains(withType)) {
            return withType;
        }

        // Numeric suffix as last resort
        int suffix = 2;
        String candidate;
        do {
            candidate = baseName + suffix;
            suffix++;
        } while (usedNames.contains(candidate));

        return candidate;
    }

    private static String capitalize(String str) {
        if (str.isEmpty()) return str;
        return str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
    }
}
