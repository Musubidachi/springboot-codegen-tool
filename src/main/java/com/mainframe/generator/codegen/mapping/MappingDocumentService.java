package com.mainframe.generator.codegen.mapping;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.MappingDocument;

/**
 * Service that loads + parses the mapping document and reports diagnostics.
 *
 * Responsibilities:
 * - Decide whether a mapping doc should be loaded (based on config)
 * - Validate file existence/readability (soft failures -> warnings/errors)
 * - Parse via MappingParser
 * - Copy MappingDocument diagnostics into ToolDiagnostics
 *
 * Non-responsibilities:
 * - It does not interpret mapping entries or apply mappings to DTO generation
 */
public class MappingDocumentService {

    private static final Logger log = LoggerFactory.getLogger(MappingDocumentService.class);

    private final MappingParser mappingParser;

    public MappingDocumentService() {
        this(new MappingParser());
    }

    public MappingDocumentService(MappingParser mappingParser) {
        this.mappingParser = Objects.requireNonNull(mappingParser, "mappingParser");
    }

    /**
     * Load and parse mapping document according to GeneratorConfig.
     *
     * @return MappingDocument (never null)
     */
    public MappingDocument load(GeneratorConfig config, ToolDiagnostics diagnostics) {
        Objects.requireNonNull(config, "config");
        Objects.requireNonNull(diagnostics, "diagnostics");

        Path mappingPath = config.getMappingDoc();

        // No mapping doc configured -> empty doc
        if (mappingPath == null) {
            log.info("No mapping document configured; proceeding with empty mapping rules.");
            return new MappingDocument();
        }

        // Soft validation: missing/unreadable file should not crash generation,
        // but it should be visible to the user.
        if (!Files.exists(mappingPath)) {
            String msg = "Mapping document not found: " + mappingPath;
            diagnostics.getWarnings().add(msg);
            log.warn(msg);
            return new MappingDocument();
        }

        if (!Files.isRegularFile(mappingPath) || !Files.isReadable(mappingPath)) {
            String msg = "Mapping document is not a readable file: " + mappingPath;
            diagnostics.getWarnings().add(msg);
            log.warn(msg);
            return new MappingDocument();
        }

        log.info("Parsing mapping document: {}", mappingPath);

        MappingDocument doc = mappingParser.parse(mappingPath);

        // Bridge mapping doc diagnostics into ToolDiagnostics
        if (doc.hasErrors()) {
            diagnostics.getWarnings().add("Mapping document has errors; generation will continue but mappings may be incomplete.");
            diagnostics.getWarnings().addAll(doc.getErrorsView());
            log.warn("Mapping document has {} errors.", doc.getErrors().size());
        }

        if (!doc.getWarnings().isEmpty()) {
            diagnostics.getWarnings().addAll(doc.getWarningsView());
            log.warn("Mapping document has {} warnings.", doc.getWarnings().size());
        }

        log.info("Mapping document loaded: {} entries ({} combine, {} enum)",
                doc.getAllEntries().size(),
                doc.getCombinedEntries().size(),
                doc.getEnumEntries().size());

        return doc;
    }
}

