package com.mainframe.generator.parser;

import com.mainframe.generator.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;

/**
 * Resolves COPY directive dependencies across copybook files.
 */
public class CopybookResolver {
    private static final Logger log = LoggerFactory.getLogger(CopybookResolver.class);
    
    private static final List<String> COPYBOOK_EXTENSIONS = List.of(
            "", ".cpy", ".CPY", ".cbl", ".CBL", ".copy", ".COPY", ".txt"
    );
    
    private final Path primaryDir;
    private final List<Path> externalDirs;
    private final Map<String, CopybookModel> parsedCopybooks = new HashMap<>();
    private final Set<String> currentlyResolving = new HashSet<>();
    private final List<String> missingCopybooks = new ArrayList<>();
    private final List<String> cyclicDependencies = new ArrayList<>();
    
    public CopybookResolver(Path primaryDir, List<Path> externalDirs) {
        this.primaryDir = primaryDir;
        this.externalDirs = externalDirs != null ? externalDirs : List.of();
    }
    
    /**
     * Load and parse all copybooks from the primary directory.
     */
    public List<CopybookModel> loadAllCopybooks() throws IOException {
        List<CopybookModel> models = new ArrayList<>();
        
        try (var stream = Files.walk(primaryDir, 1)) {
            stream.filter(Files::isRegularFile)
                    .filter(this::isCopybookFile)
                    .forEach(path -> {
                        try {
                            CopybookModel model = loadCopybook(path);
                            if (model != null) {
                                models.add(model);
                            }
                        } catch (IOException e) {
                            log.error("Failed to load copybook: {}", path, e);
                        }
                    });
        }
        
        // Resolve all COPY directives
        for (CopybookModel model : models) {
            resolveDependencies(model);
        }
        
        return models;
    }
    
    /**
     * Load and parse a single copybook file.
     */
    public CopybookModel loadCopybook(Path path) throws IOException {
        String fileName = path.getFileName().toString();
        String content = Files.readString(path);
        
        log.info("Parsing copybook: {}", fileName);
        
        CopybookTokenizer tokenizer = new CopybookTokenizer(content, fileName);
        List<CopybookToken> tokens = tokenizer.tokenize();
        
        CopybookParser parser = new CopybookParser(tokens, fileName);
        CopybookModel model = parser.parse();
        
        // Store for later resolution
        String name = extractName(fileName);
        parsedCopybooks.put(name.toUpperCase(), model);
        
        log.info("Parsed copybook {} - {} fields, {} bytes total", 
                name, model.getAllFields().size(), model.calculateTotalByteLength());
        
        return model;
    }
    
    /**
     * Resolve COPY directive dependencies for a model.
     */
    public void resolveDependencies(CopybookModel model) {
        for (CopyDirectiveNode directive : model.getCopyDirectives()) {
            String name = directive.getCopybookName();
            
            // Check for cycles
            if (currentlyResolving.contains(name.toUpperCase())) {
                String cycle = "Cyclic dependency detected: " + name;
                cyclicDependencies.add(cycle);
                model.addError(cycle);
                log.error(cycle);
                continue;
            }
            
            currentlyResolving.add(name.toUpperCase());
            
            try {
                // Try to find and parse the referenced copybook
                Path resolvedPath = findCopybook(name);
                
                if (resolvedPath != null) {
                    CopybookModel referenced = parsedCopybooks.get(name.toUpperCase());
                    if (referenced == null) {
                        referenced = loadCopybook(resolvedPath);
                        resolveDependencies(referenced);
                    }
                    
                    directive.markResolved(resolvedPath.toString(), referenced.getRootGroup());
                    log.info("Resolved COPY {} -> {}", name, resolvedPath);
                } else {
                    String error = String.format(
                            "Missing copybook '%s' referenced from %s line %d. " +
                            "Provide it in --copybook-dir or --external-copybook-dirs",
                            name, directive.getSourceFile(), directive.getSourceLine()
                    );
                    missingCopybooks.add(name);
                    model.addError(error);
                    log.error(error);
                }
            } catch (IOException e) {
                model.addError("Failed to resolve COPY " + name + ": " + e.getMessage());
                log.error("Failed to resolve COPY {}", name, e);
            } finally {
                currentlyResolving.remove(name.toUpperCase());
            }
        }
    }
    
    /**
     * Find a copybook file by name in the search directories.
     */
    public Path findCopybook(String name) {
        // Search primary directory first
        Path found = searchDirectory(primaryDir, name);
        if (found != null) {
            return found;
        }
        
        // Search external directories
        for (Path extDir : externalDirs) {
            found = searchDirectory(extDir, name);
            if (found != null) {
                return found;
            }
        }
        
        return null;
    }
    
    private Path searchDirectory(Path dir, String name) {
        if (!Files.exists(dir) || !Files.isDirectory(dir)) {
            return null;
        }
        
        for (String ext : COPYBOOK_EXTENSIONS) {
            Path candidate = dir.resolve(name + ext);
            if (Files.exists(candidate)) {
                return candidate;
            }
            // Also try lowercase
            candidate = dir.resolve(name.toLowerCase() + ext);
            if (Files.exists(candidate)) {
                return candidate;
            }
        }
        
        return null;
    }
    
    private boolean isCopybookFile(Path path) {
        String name = path.getFileName().toString().toLowerCase();
        return name.endsWith(".cpy") || name.endsWith(".cbl") || 
               name.endsWith(".copy") || name.endsWith(".txt") ||
               !name.contains(".");
    }
    
    private String extractName(String fileName) {
        int dot = fileName.lastIndexOf('.');
        return dot > 0 ? fileName.substring(0, dot) : fileName;
    }
    
    public List<String> getMissingCopybooks() {
        return Collections.unmodifiableList(missingCopybooks);
    }
    
    public List<String> getCyclicDependencies() {
        return Collections.unmodifiableList(cyclicDependencies);
    }
    
    public boolean hasErrors() {
        return !missingCopybooks.isEmpty() || !cyclicDependencies.isEmpty();
    }
    
    public String getErrorSummary() {
        StringBuilder sb = new StringBuilder();
        
        if (!missingCopybooks.isEmpty()) {
            sb.append("Missing copybooks:\n");
            for (String name : missingCopybooks) {
                sb.append("  - ").append(name).append("\n");
            }
            sb.append("\nSuggestion: Provide these copybooks in --copybook-dir or --external-copybook-dirs\n");
        }
        
        if (!cyclicDependencies.isEmpty()) {
            sb.append("Cyclic dependencies:\n");
            for (String cycle : cyclicDependencies) {
                sb.append("  - ").append(cycle).append("\n");
            }
        }
        
        return sb.toString();
    }
}
