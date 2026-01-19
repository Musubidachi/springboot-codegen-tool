package com.mainframe.generator.codegen.copybook.service;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class CopybookLoadingService {

    private final CopybookDiscoveryService discoveryService;
    private final CopybookParserService parserService;
    private final CopybookDependencyResolverService dependencyResolver;

    public List<CopybookModel> loadAll(Path primaryDir, List<Path> externalDirs, ToolDiagnostics diagnostics) throws IOException {
        List<Path> files = discoveryService.discoverCopybookFiles(primaryDir);

        List<CopybookModel> models = new ArrayList<>();
        for (Path path : files) {
            CopybookModel model = parserService.parse(path, diagnostics);
            models.add(model);

            // register parsed model into resolver cache by filename stem
            String key = stemUpper(path);
            dependencyResolver.registerParsed(key, model);
        }

        dependencyResolver.resolveAll(models, primaryDir, externalDirs, diagnostics, parserService);
        return models;
    }

    private String stemUpper(Path path) {
        String file = path.getFileName().toString();
        int dot = file.lastIndexOf('.');
        String stem = (dot > 0) ? file.substring(0, dot) : file;
        return stem.toUpperCase(Locale.ROOT);
    }
}
