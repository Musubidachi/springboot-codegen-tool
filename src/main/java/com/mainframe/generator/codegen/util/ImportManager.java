package com.mainframe.generator.codegen.util;

import java.util.Set;
import java.util.TreeSet;

/**
 * Manages import statements for generated Java classes.
 */
public class ImportManager {

    private final Set<String> imports = new TreeSet<>();
    private final String currentPackage;

    public ImportManager(String currentPackage) {
        this.currentPackage = currentPackage;
    }

    /**
     * Adds an import for a fully qualified class name.
     * Skips if in same package or java.lang.
     */
    public void addImport(String fullQualifiedName) {
        if (fullQualifiedName == null || fullQualifiedName.isEmpty()) {
            return;
        }

        // Skip java.lang
        if (fullQualifiedName.startsWith("java.lang.") && !fullQualifiedName.contains(".lang.annotation")) {
            return;
        }

        // Skip same package
        String packageName = getPackageName(fullQualifiedName);
        if (packageName.equals(currentPackage)) {
            return;
        }

        imports.add(fullQualifiedName);
    }

    /**
     * Adds multiple imports.
     */
    public void addImports(Iterable<String> fullQualifiedNames) {
        for (String fqn : fullQualifiedNames) {
            addImport(fqn);
        }
    }

    /**
     * Generates import statements as a string.
     */
    public String generateImports() {
        if (imports.isEmpty()) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        for (String imp : imports) {
            sb.append("import ").append(imp).append(";\n");
        }
        return sb.toString();
    }

    private String getPackageName(String fullQualifiedName) {
        int lastDot = fullQualifiedName.lastIndexOf('.');
        if (lastDot < 0) {
            return "";
        }
        return fullQualifiedName.substring(0, lastDot);
    }
}
