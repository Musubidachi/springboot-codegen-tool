package com.mainframe.generator.parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopyDirectiveNode;
import com.mainframe.generator.codegen.model.input.CopybookModel;

/**
 * Discovers, parses, and resolves COPY directive dependencies across copybook files.
 *
 * Diagnostics are written to ToolDiagnostics (not to models).
 */
public class CopybookResolver {
    private static final Logger log = LoggerFactory.getLogger(CopybookResolver.class);

    private static final List<String> COPYBOOK_EXTENSIONS = List.of(
            "", ".cpy", ".CPY", ".cbl", ".CBL", ".copy", ".COPY", ".txt"
    );

    private final Path primaryDir;
    private final List<Path> externalDirs;

    /** Cache of parsed copybooks by normalized name (uppercase, no extension). */
    private final Map<String, CopybookModel> parsedCopybooks = new HashMap<>();

    /** Tracks recursion stack to detect cycles. */
    private final Set<String> currentlyResolving = new HashSet<>();

    public CopybookResolver(Path primaryDir, List<Path> externalDirs) {
        this.primaryDir = Objects.requireNonNull(primaryDir, "primaryDir");
        this.externalDirs = (externalDirs != null) ? externalDirs : List.of();
    }

    /**
     * Load and parse all copybooks from the primary directory (non-recursive),
     * then resolve COPY directives for each parsed model.
     */
    public List<CopybookModel> loadAllCopybooks(ToolDiagnostics diagnostics) throws IOException {
        Objects.requireNonNull(diagnostics, "diagnostics");

        List<CopybookModel> models = new ArrayList<>();

        try (var stream = Files.walk(primaryDir, 1)) {
            stream.filter(Files::isRegularFile)
                    .filter(this::isCopybookFile)
                    .forEach(path -> {
                        try {
                            CopybookModel model = loadCopybook(path, diagnostics);
                            if (model != null) {
                                models.add(model);
                            }
                        } catch (IOException e) {
                            diagnostics.getErrors().add("Failed to load copybook: " + path + " (" + e.getMessage() + ")");
                            log.error("Failed to load copybook: {}", path, e);
                        }
                    });
        }

        // Resolve COPY directives
        for (CopybookModel model : models) {
            resolveDependencies(model, diagnostics);
        }

        return models;
    }

    /**
     * Load and parse a single copybook file. Uses cache if already parsed.
     */
    public CopybookModel loadCopybook(Path path, ToolDiagnostics diagnostics) throws IOException {
        Objects.requireNonNull(path, "path");
        Objects.requireNonNull(diagnostics, "diagnostics");

        String fileName = path.getFileName().toString();
        String nameKey = normalizeName(extractName(fileName));

        // Cache hit
        CopybookModel cached = parsedCopybooks.get(nameKey);
        if (cached != null) {
            return cached;
        }

        String content = Files.readString(path);

        log.info("Parsing copybook: {}", fileName);

        CopybookTokenizer tokenizer = new CopybookTokenizer(content, fileName);
        List<CopybookToken> tokens = tokenizer.tokenize();

        CopybookParser parser = new CopybookParser(tokens, fileName);
        CopybookModel model = parser.parse(diagnostics);

        // Store for later resolution
        parsedCopybooks.put(nameKey, model);

        // Don't call model.getAllFields() / model.calculateTotalByteLength() here (model should be passive).
        log.info("Parsed copybook: {}", fileName);

        return model;
    }

    /**
     * Resolve COPY directive dependencies for a model. Recursively parses missing dependencies.
     */
    public void resolveDependencies(CopybookModel model, ToolDiagnostics diagnostics) {
        Objects.requireNonNull(model, "model");
        Objects.requireNonNull(diagnostics, "diagnostics");

        // If the model doesn't track copy directives, this is a no-op.
        List<CopyDirectiveNode> directives = model.getCopyDirectives();
        if (directives == null || directives.isEmpty()) {
            return;
        }

        for (CopyDirectiveNode directive : directives) {
            String rawName = directive.getCopybookName();
            String nameKey = normalizeName(rawName);

            if (nameKey.isBlank()) {
                diagnostics.getWarnings().add("COPY directive with blank name in " +
                        directive.getSourceFile() + " line " + directive.getSourceLine());
                continue;
            }

            // Cycle detection
            if (currentlyResolving.contains(nameKey)) {
                String msg = "Cyclic dependency detected: COPY " + rawName +
                        " referenced from " + directive.getSourceFile() + " line " + directive.getSourceLine();
                diagnostics.getErrors().add(msg);
                log.error(msg);
                continue;
            }

            currentlyResolving.add(nameKey);
            try {
                Path resolvedPath = findCopybook(rawName);

                if (resolvedPath == null) {
                    String msg = String.format(
                            "Missing copybook '%s' referenced from %s line %d. Provide it in --copybook-dir or --external-copybook-dirs",
                            rawName, directive.getSourceFile(), directive.getSourceLine()
                    );
                    diagnostics.getErrors().add(msg);
                    log.error(msg);
                    continue;
                }

                CopybookModel referenced = parsedCopybooks.get(nameKey);
                if (referenced == null) {
                    referenced = loadCopybook(resolvedPath, diagnostics);
                    resolveDependencies(referenced, diagnostics);
                }

                // Mutating the directive to mark it resolved is OK (directive is part of parse graph).
                directive.markResolved(resolvedPath.toString(), referenced.getRootGroup());
                log.info("Resolved COPY {} -> {}", rawName, resolvedPath);

            } catch (IOException e) {
                String msg = "Failed to resolve COPY " + rawName + " referenced from " +
                        directive.getSourceFile() + " line " + directive.getSourceLine() +
                        " (" + e.getMessage() + ")";
                diagnostics.getErrors().add(msg);
                log.error("Failed to resolve COPY {}", rawName, e);
            } finally {
                currentlyResolving.remove(nameKey);
            }
        }
    }

    /**
     * Find a copybook file by name in the primary + external directories.
     */
    public Path findCopybook(String name) {
        if (name == null || name.isBlank()) return null;

        Path found = searchDirectory(primaryDir, name);
        if (found != null) return found;

        for (Path extDir : externalDirs) {
            found = searchDirectory(extDir, name);
            if (found != null) return found;
        }

        return null;
    }

    private Path searchDirectory(Path dir, String name) {
        if (dir == null || !Files.exists(dir) || !Files.isDirectory(dir)) {
            return null;
        }

        for (String ext : COPYBOOK_EXTENSIONS) {
            Path candidate = dir.resolve(name + ext);
            if (Files.exists(candidate)) return candidate;

            candidate = dir.resolve(name.toLowerCase(Locale.ROOT) + ext);
            if (Files.exists(candidate)) return candidate;
        }

        return null;
    }

    private boolean isCopybookFile(Path path) {
        String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
        return name.endsWith(".cpy") || name.endsWith(".cbl") ||
               name.endsWith(".copy") || name.endsWith(".txt") ||
               !name.contains(".");
    }

    private String extractName(String fileName) {
        int dot = fileName.lastIndexOf('.');
        return (dot > 0) ? fileName.substring(0, dot) : fileName;
    }

    private String normalizeName(String name) {
        if (name == null) return "";
        return name.trim().toUpperCase(Locale.ROOT);
    }
}
