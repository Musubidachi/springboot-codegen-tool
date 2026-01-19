package com.mainframe.generator.codegen;

import com.mainframe.generator.codegen.model.input.CopybookModel;

import lombok.Builder;
import lombok.Data;

/**
 * Metadata about a generated DTO, including naming, packaging, and deduplication info.
 */
@Data
@Builder
public class DtoMetadata {
    private CopybookModel copybookModel;
    private String className;        // Final Java class name (may be disambiguated)
    private String originalClassName; // Original class name before disambiguation
    private String packageType;      // "request", "response", "shared", "layout"
    private String structuralSignature; // For deduplication detection
    private boolean isWrapper;       // True for ApiRequest/ApiResponse wrappers
    private boolean isDeduped;       // True if this DTO was deduped to a shared DTO
    private String dedupedToClassName; // If deduped, the shared class name
    private String extendsClassName; // If using inheritance, the parent class name
    private int byteLength;          // Total byte length from copybook

    /**
     * Get the full qualified class name for imports.
     */
    public String getFullQualifiedName(String basePackage) {
        return basePackage + ".model." + packageType + "." + className;
    }

    /**
     * Get the serializer class name.
     */
    public String getSerializerClassName() {
        return className + "Serializer";
    }

    /**
     * Get the full qualified serializer class name.
     */
    public String getFullQualifiedSerializerName(String basePackage) {
        return basePackage + ".util." + packageType + "." + getSerializerClassName();
    }
}
