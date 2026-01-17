package com.mainframe.generator.codegen;

import lombok.Builder;
import lombok.Data;

import java.nio.file.Path;

/**
 * Result of the project generation process.
 */
@Data
@Builder
public class GeneratorResult {
    private boolean success;
    private String errorMessage;
    private String failingTest;
    private Path outputPath;
    
    private int copybooksParsed;
    private int dtoClassesGenerated;
    private int requestByteLength;
    private int responseByteLength;

    // New folder-based selection counts
    private int requestModelsCount;
    private int responseModelsCount;
    private int sharedModelsCount;
    private int serializersGenerated;
    private int tortureTestsGenerated;

    private int notNullCount;
    private int sizeCount;
    private int digitsCount;
    private int minMaxCount;
    
    public static GeneratorResult failure(String errorMessage) {
        return GeneratorResult.builder()
                .success(false)
                .errorMessage(errorMessage)
                .build();
    }
    
    public static GeneratorResult testFailure(String testName, String errorMessage) {
        return GeneratorResult.builder()
                .success(false)
                .errorMessage(errorMessage)
                .failingTest(testName)
                .build();
    }
}
