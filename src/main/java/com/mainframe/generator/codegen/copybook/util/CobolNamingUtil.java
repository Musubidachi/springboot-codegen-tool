package com.mainframe.generator.codegen.copybook.util;

import lombok.experimental.UtilityClass;

import java.util.Locale;

@UtilityClass
public class CobolNamingUtil {

    public static String toJavaClassName(String cobolName) {
        if (cobolName == null || cobolName.isBlank()) return "Type";

        String[] parts = cobolName.trim().toLowerCase(Locale.ROOT).split("-");
        StringBuilder sb = new StringBuilder();

        for (String p : parts) {
            if (p.isBlank()) continue;
            sb.append(Character.toUpperCase(p.charAt(0)));
            if (p.length() > 1) sb.append(p.substring(1));
        }

        return sb.length() == 0 ? "Type" : sb.toString();
    }

    /**
     * Convert a COBOL field name (often hyphen-separated) to a Java field name (camelCase).
     * Example: "CLAIMANT-NAME" -> "claimantName"
     */
    public static String toJavaFieldName(String cobolName) {
        if (cobolName == null || cobolName.isBlank()) return "field";

        String[] parts = cobolName.trim().toLowerCase(Locale.ROOT).split("-");
        StringBuilder sb = new StringBuilder();

        boolean first = true;
        for (String p : parts) {
            if (p.isBlank()) continue;

            if (first) {
                sb.append(p);
                first = false;
            } else {
                sb.append(Character.toUpperCase(p.charAt(0)));
                if (p.length() > 1) sb.append(p.substring(1));
            }
        }

        return sb.length() == 0 ? "field" : sb.toString();
    }
}
