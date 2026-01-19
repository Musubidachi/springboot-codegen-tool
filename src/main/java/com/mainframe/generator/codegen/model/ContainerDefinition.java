package com.mainframe.generator.codegen.model;

import com.mainframe.generator.codegen.model.input.CopybookModel;
import lombok.Builder;
import lombok.Data;

/**
 * Defines a mainframe container with its key and associated copybook.
 *
 * Container keys are derived from 01-level COBOL record names and normalized
 * deterministically (e.g., ABC-REQUEST-REC -> ABC_REQUEST_REC).
 */
@Data
@Builder
public class ContainerDefinition {

    /**
     * The normalized container key (e.g., ABC_REQUEST_REC).
     * Derived from the 01-level record name.
     */
    private String containerKey;

    /**
     * The original 01-level record name from the copybook.
     */
    private String recordName;

    /**
     * The parsed copybook model.
     */
    private CopybookModel copybook;

    /**
     * Container type: "request" or "response".
     */
    private String type;

    /**
     * Returns the Java class name for this container's DTO.
     *
     * @return the class name in PascalCase
     */
    public String getClassName() {
        return toPascalCase(recordName);
    }

    /**
     * Returns the byte length of this container.
     *
     * @return the total byte length
     */
    public int getByteLength() {
        return copybook.getTotalByteLength();
    }

    private String toPascalCase(String name) {
        if (name == null || name.isEmpty()) {
            return name;
        }
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : name.toCharArray()) {
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }
        return result.toString();
    }
}
