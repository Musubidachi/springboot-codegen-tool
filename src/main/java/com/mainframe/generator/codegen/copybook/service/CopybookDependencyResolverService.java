package com.mainframe.generator.codegen.copybook.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopyDirectiveNode;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class CopybookDependencyResolverService {
    private static final Logger log = LoggerFactory.getLogger(CopybookDependencyResolverService.class);

    private static final List<String> COPYBOOK_EXTENSIONS = List.of(
            "", ".cpy", ".CPY", ".cbl", ".CBL", ".copy", ".COPY", ".txt"
    );

    private final Map<String, CopybookModel> parsedCopybooks = new HashMap<>();
    private final Set<String> currentlyResolving = new HashSet<>();

    public void primeCache(List<Path> parsedPaths, List<CopybookModel> models) {
        // optional helper if you want deterministic naming, otherwise you can skip
    }

    public void registerParsed(String nameKeyUpper, CopybookModel model) {
        parsedCopybooks.put(nameKeyUpper, model);
    }

    public void resolveAll(List<CopybookModel> models,
                           Path primaryDir,
                           List<Path> externalDirs,
                           ToolDiagnostics diagnostics,
                           CopybookParserService parserService) {
        for (CopybookModel model : models) {
            resolveDependencies(model, primaryDir, externalDirs, diagnostics, parserService);
        }
    }

    private void resolveDependencies(CopybookModel model,
                                     Path primaryDir,
                                     List<Path> externalDirs,
                                     ToolDiagnostics diagnostics,
                                     CopybookParserService parserService) {

        List<CopyDirectiveNode> directives = model.getCopyDirectives();
        if (directives == null || directives.isEmpty()) return;

        for (CopyDirectiveNode directive : directives) {
            String rawName = directive.getCopybookName();
            String nameKey = normalizeName(rawName);

            if (nameKey.isBlank()) {
                diagnostics.getWarnings().add("COPY directive with blank name in " +
                        directive.getSourceFile() + " line " + directive.getSourceLine());
                continue;
            }

            if (currentlyResolving.contains(nameKey)) {
                String msg = "Cyclic dependency detected: COPY " + rawName +
                        " referenced from " + directive.getSourceFile() + " line " + directive.getSourceLine();
                diagnostics.getErrors().add(msg);
                log.error(msg);
                continue;
            }

            currentlyResolving.add(nameKey);
            try {
                Path resolvedPath = findCopybook(rawName, primaryDir, externalDirs);

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
                    referenced = parserService.parse(resolvedPath, diagnostics);
                    parsedCopybooks.put(nameKey, referenced);
                    resolveDependencies(referenced, primaryDir, externalDirs, diagnostics, parserService);
                }

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

    private Path findCopybook(String name, Path primaryDir, List<Path> externalDirs) {
        if (name == null || name.isBlank()) return null;

        Path found = searchDirectory(primaryDir, name);
        if (found != null) return found;

        if (externalDirs != null) {
            for (Path extDir : externalDirs) {
                found = searchDirectory(extDir, name);
                if (found != null) return found;
            }
        }
        return null;
    }

    private Path searchDirectory(Path dir, String name) {
        if (dir == null || !Files.exists(dir) || !Files.isDirectory(dir)) return null;

        for (String ext : COPYBOOK_EXTENSIONS) {
            Path candidate = dir.resolve(name + ext);
            if (Files.exists(candidate)) return candidate;

            candidate = dir.resolve(name.toLowerCase(Locale.ROOT) + ext);
            if (Files.exists(candidate)) return candidate;
        }
        return null;
    }

    private String normalizeName(String name) {
        return (name == null) ? "" : name.trim().toUpperCase(Locale.ROOT);
    }
}
