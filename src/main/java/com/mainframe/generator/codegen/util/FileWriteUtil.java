package com.mainframe.generator.codegen.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

/**
 * Utility for safe file operations with automatic directory creation.
 */
public class FileWriteUtil {

    private FileWriteUtil() {
        // Utility class
    }

    /**
     * Writes content to a file, creating parent directories if needed.
     */
    public static void safeWriteString(Path filePath, String content) throws IOException {
        Path parentDir = filePath.getParent();
        if (parentDir != null) {
            Files.createDirectories(parentDir);
        }
        Files.writeString(filePath, content);
    }

    /**
     * Creates directories recursively.
     */
    public static void createDirectories(Path dir) throws IOException {
        Files.createDirectories(dir);
    }

    /**
     * Recursively deletes a directory.
     */
    public static void deleteDirectory(Path directory) throws IOException {
        if (!Files.exists(directory)) {
            return;
        }
        try (Stream<Path> walk = Files.walk(directory)) {
            walk.sorted(Comparator.reverseOrder())
                .forEach(path -> {
                    try {
                        Files.delete(path);
                    } catch (IOException e) {
                        throw new RuntimeException("Failed to delete: " + path, e);
                    }
                });
        }
    }
}
