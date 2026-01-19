package com.mainframe.generator.codegen.copybook.service;

import lombok.NoArgsConstructor;

import java.io.IOException;
import java.nio.file.*;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@NoArgsConstructor
public class CopybookDiscoveryService {

    public List<Path> discoverCopybookFiles(Path primaryDir) throws IOException {
        try (Stream<Path> stream = Files.walk(primaryDir, 1)) {
            return stream.filter(Files::isRegularFile)
                    .filter(this::isCopybookFile)
                    .collect(Collectors.toList());
        }
    }

    private boolean isCopybookFile(Path path) {
        String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
        return name.endsWith(".cpy") || name.endsWith(".cbl") ||
               name.endsWith(".copy") || name.endsWith(".txt") ||
               !name.contains(".");
    }
}
