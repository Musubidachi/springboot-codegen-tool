package com.mainframe.generator.codegen.dto.metadata;

import com.mainframe.generator.codegen.model.input.CopyDirectiveNode;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * Classifies DTOs into packages: request, response, shared, or layout.
 *
 * FIX: Heuristic mode now traverses dependencies and assigns package types correctly.
 */
public class DtoPackageClassifier {
    private static final Logger log = LoggerFactory.getLogger(DtoPackageClassifier.class);

    private final Set<CopybookModel> requestModels;
    private final Set<CopybookModel> responseModels;
    private final Set<CopybookModel> sharedModels;
    private final Map<String, CopybookModel> copybooksByName;

    public DtoPackageClassifier(Set<CopybookModel> requestModels,
                                Set<CopybookModel> responseModels,
                                Set<CopybookModel> sharedModels,
                                List<CopybookModel> allCopybooks) {
        this.requestModels = requestModels;
        this.responseModels = responseModels;
        this.sharedModels = sharedModels;
        this.copybooksByName = buildCopybookMap(allCopybooks);
    }

    /**
     * Classify a copybook model into its package type.
     */
    public String classify(CopybookModel model) {
        if (sharedModels.contains(model)) return "shared";
        if (requestModels.contains(model)) return "request";
        if (responseModels.contains(model)) return "response";
        return "layout";
    }

    /**
     * FIX: Traverse dependencies in heuristic mode and classify them.
     * This ensures nested copybooks are assigned to request/response/shared packages.
     */
    public void classifyHeuristicDependencies(CopybookModel requestRoot,
                                               CopybookModel responseRoot,
                                               Set<CopybookModel> allModelsToGenerate) {
        log.info("Classifying dependencies for heuristic mode...");

        Set<CopybookModel> requestDeps = resolveDependencies(requestRoot);
        Set<CopybookModel> responseDeps = resolveDependencies(responseRoot);

        // Find intersection (shared dependencies)
        Set<CopybookModel> intersection = findIntersection(requestDeps, responseDeps);

        // Classify models
        classifyModels(requestDeps, responseDeps, intersection, allModelsToGenerate);

        logClassificationResults();
    }

    private Set<CopybookModel> resolveDependencies(CopybookModel root) {
        Set<CopybookModel> deps = new LinkedHashSet<>();
        resolveDependenciesRecursive(root, deps, new HashSet<>());
        return deps;
    }

    private void resolveDependenciesRecursive(CopybookModel model,
                                               Set<CopybookModel> deps,
                                               Set<String> visited) {
        if (model == null || visited.contains(model.getName())) {
            return;
        }
        visited.add(model.getName());
        deps.add(model);

        // Find COPY directives in the model
        if (model.getCopyDirectives() != null) {
            for (CopyDirectiveNode copyNode : model.getCopyDirectives()) {
                String copybookName = copyNode.getCopybookName();
                CopybookModel referenced = copybooksByName.get(copybookName.toLowerCase());
                if (referenced != null) {
                    resolveDependenciesRecursive(referenced, deps, visited);
                }
            }
        }
    }

    private Set<CopybookModel> findIntersection(Set<CopybookModel> set1, Set<CopybookModel> set2) {
        Set<CopybookModel> intersection = new HashSet<>(set1);
        intersection.retainAll(set2);
        return intersection;
    }

    private void classifyModels(Set<CopybookModel> requestDeps, Set<CopybookModel> responseDeps,
                                 Set<CopybookModel> intersection, Set<CopybookModel> allModelsToGenerate) {
        // Add shared models
        for (CopybookModel dep : intersection) {
            sharedModels.add(dep);
            allModelsToGenerate.add(dep);
        }

        // Add request-only models
        for (CopybookModel dep : requestDeps) {
            if (!intersection.contains(dep)) {
                requestModels.add(dep);
                allModelsToGenerate.add(dep);
            }
        }

        // Add response-only models
        for (CopybookModel dep : responseDeps) {
            if (!intersection.contains(dep)) {
                responseModels.add(dep);
                allModelsToGenerate.add(dep);
            }
        }
    }

    private void logClassificationResults() {
        log.info("  Request models: {}", requestModels.size());
        log.info("  Response models: {}", responseModels.size());
        log.info("  Shared models: {}", sharedModels.size());
    }

    private Map<String, CopybookModel> buildCopybookMap(List<CopybookModel> allCopybooks) {
        Map<String, CopybookModel> map = new HashMap<>();
        for (CopybookModel model : allCopybooks) {
            map.put(model.getName().toLowerCase(), model);
        }
        return map;
    }
}
