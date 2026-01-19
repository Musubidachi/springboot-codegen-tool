package com.mainframe.generator.codegen.analysis;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.model.selection.CopybookSelection;

/**
 * Folder-based request/response/shared copybook selection.
 *
 * Heuristic mode intentionally removed:
 * - Request/response cannot be inferred reliably without explicit structure/config.
 *
 * Updated for multi-container requirement:
 * - requestModels/responseModels/sharedModels contain ALL containers
 * - requestRoot/responseRoot are "primary" models for defaults (naming/back-compat),
 *   not the only containers.
 */
public class RequestResponseIdentificationService {

    private static final Logger log = LoggerFactory.getLogger(RequestResponseIdentificationService.class);

    /**
     * Identify request/response/shared copybooks using folder structure.
     *
     * Required config:
     * - requestCopybookDir
     * - responseCopybookDir
     *
     * Optional:
     * - sharedCopybookDir
     * - requestRoot/responseRoot (primary selection; otherwise deterministic pick)
     */
    public CopybookSelection identify(GeneratorConfig config,
                                      List<CopybookModel> copybookModels,
                                      ToolDiagnostics diagnostics) {

        Objects.requireNonNull(config, "config");
        Objects.requireNonNull(copybookModels, "copybookModels");
        Objects.requireNonNull(diagnostics, "diagnostics");

        if (copybookModels.isEmpty()) {
            diagnostics.getErrors().add("No copybooks were loaded; cannot identify request/response.");
            throw new IllegalStateException("No copybooks were loaded.");
        }

        if (config.getRequestCopybookDir() == null || config.getResponseCopybookDir() == null) {
            diagnostics.getErrors().add(
                    "Folder-based selection requires both requestCopybookDir and responseCopybookDir to be configured.");
            throw new IllegalStateException("Missing requestCopybookDir/responseCopybookDir.");
        }

        return identifyFromFolders(config, copybookModels, diagnostics);
    }

    private CopybookSelection identifyFromFolders(GeneratorConfig config,
                                                 List<CopybookModel> copybookModels,
                                                 ToolDiagnostics diagnostics) {

        log.info("Using folder-based copybook selection");

        Path requestDir = config.getRequestCopybookDir();
        Path responseDir = config.getResponseCopybookDir();
        Path sharedDir  = config.getSharedCopybookDir();

        // Deterministic iteration for stable output
        List<CopybookModel> sorted = new ArrayList<>(copybookModels);
        sorted.sort(Comparator.comparing(m -> safeUpper(m.getName())));

        Set<CopybookModel> requestModels = new LinkedHashSet<>();
        Set<CopybookModel> responseModels = new LinkedHashSet<>();
        Set<CopybookModel> sharedModels = new LinkedHashSet<>();

        Map<String, CopybookModel> requestMap = new HashMap<>();
        Map<String, CopybookModel> responseMap = new HashMap<>();

        String reqPrefix = normalizeDirPrefix(requestDir);
        String respPrefix = normalizeDirPrefix(responseDir);
        String sharedPrefix = sharedDir != null ? normalizeDirPrefix(sharedDir) : null;

        for (CopybookModel model : sorted) {
            if (model == null || model.getSourcePath() == null) {
                continue;
            }

            String src = normalizePath(model.getSourcePath());

            // Explicit shared folder takes precedence
            if (sharedPrefix != null && src.startsWith(sharedPrefix)) {
                sharedModels.add(model);
                continue;
            }

            if (src.startsWith(reqPrefix)) {
                requestModels.add(model);
                requestMap.put(normalizeName(model.getName()), model);
                continue;
            }

            if (src.startsWith(respPrefix)) {
                responseModels.add(model);
                responseMap.put(normalizeName(model.getName()), model);
            }
        }

        log.info("  Request copybooks: {}", requestModels.size());
        log.info("  Response copybooks: {}", responseModels.size());
        log.info("  Explicit shared copybooks: {}", sharedModels.size());

        // Detect intersection (same logical name appears in both request and response) => shared
        Set<String> intersection = new HashSet<>(requestMap.keySet());
        intersection.retainAll(responseMap.keySet());
        for (String commonName : intersection) {
            CopybookModel reqModel = requestMap.get(commonName);
            CopybookModel respModel = responseMap.get(commonName);

            // Prefer request instance; remove both from their buckets; mark shared
            if (reqModel != null) {
                requestModels.remove(reqModel);
                sharedModels.add(reqModel);
            }
            if (respModel != null) {
                responseModels.remove(respModel);
                // If it’s a different object instance, we still consider it “the same” copybook by name.
                // We don’t add respModel separately to avoid duplicates in shared.
            }
        }
        if (!intersection.isEmpty()) {
            log.info("  Detected {} shared copybooks by intersection: {}", intersection.size(), intersection);
        }

        // Hard requirements: must have at least one request + one response container
        if (requestModels.isEmpty()) {
            diagnostics.getErrors().add("No copybooks found under requestCopybookDir: " + requestDir);
        }
        if (responseModels.isEmpty()) {
            diagnostics.getErrors().add("No copybooks found under responseCopybookDir: " + responseDir);
        }
        if (!diagnostics.getErrors().isEmpty()) {
            throw new IllegalStateException("Request/response folder selection failed. See diagnostics.");
        }

        // Primary root selection (for naming/back-compat only)
        CopybookModel requestRoot = selectPrimary(config.getRequestRoot(), requestModels, sharedModels);
        CopybookModel responseRoot = selectPrimary(config.getResponseRoot(), responseModels, sharedModels);

        if (requestRoot == null) {
            requestRoot = selectFirstDeterministic(requestModels);
        }
        if (responseRoot == null) {
            responseRoot = selectFirstDeterministic(responseModels);
        }

        // Safety fallback (shouldn’t happen given earlier checks)
        if (requestRoot == null || responseRoot == null) {
            diagnostics.getErrors().add("Unable to determine request/response roots after folder selection.");
            throw new IllegalStateException("Unable to determine request/response roots.");
        }

        log.info("  Selected request root: {}", requestRoot.getName());
        log.info("  Selected response root: {}", responseRoot.getName());

        return CopybookSelection.builder()
                .requestRoot(requestRoot)
                .responseRoot(responseRoot)
                .requestModels(requestModels)
                .responseModels(responseModels)
                .sharedModels(sharedModels)
                .build();
    }

    private CopybookModel selectPrimary(String configuredRoot,
                                       Set<CopybookModel> preferred,
                                       Set<CopybookModel> fallbackShared) {
        if (configuredRoot != null && !configuredRoot.isBlank()) {
            CopybookModel found = findCopybookByNameOrFile(preferred, configuredRoot);
            if (found != null) return found;

            found = findCopybookByNameOrFile(fallbackShared, configuredRoot);
            if (found != null) return found;

            log.warn("Configured root '{}' not found in preferred/shared sets.", configuredRoot);
        }
        return null;
    }

    private CopybookModel findCopybookByNameOrFile(Set<CopybookModel> models, String nameOrFile) {
        if (models == null || models.isEmpty() || nameOrFile == null || nameOrFile.isBlank()) {
            return null;
        }

        for (CopybookModel model : models) {
            if (model == null) continue;

            if (model.getName() != null && model.getName().equalsIgnoreCase(nameOrFile)) {
                return model;
            }

            if (model.getSourcePath() != null) {
                Path sourcePath = Path.of(model.getSourcePath());
                String fileName = sourcePath.getFileName().toString();
                if (fileName.equalsIgnoreCase(nameOrFile) || fileName.equalsIgnoreCase(nameOrFile + ".cpy")) {
                    return model;
                }
            }
        }
        return null;
    }

    private CopybookModel selectFirstDeterministic(Set<CopybookModel> models) {
        if (models == null || models.isEmpty()) {
            return null;
        }
        return models.stream()
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(m -> safeUpper(m.getName())))
                .findFirst()
                .orElse(null);
    }

    private String normalizeDirPrefix(Path dir) {
        String d = dir.toString().replace('\\', '/');
        if (!d.endsWith("/")) d += "/";
        return d;
    }

    private String normalizePath(String path) {
        return path.replace('\\', '/');
    }

    private String normalizeName(String name) {
        return safeUpper(name).toLowerCase(Locale.ROOT);
    }

    private String safeUpper(String s) {
        return s == null ? "" : s.toUpperCase(Locale.ROOT);
    }
}
