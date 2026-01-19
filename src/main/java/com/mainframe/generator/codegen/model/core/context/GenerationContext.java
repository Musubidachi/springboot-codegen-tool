package com.mainframe.generator.codegen.model.core.context;

import java.nio.file.Path;
import java.time.Instant;
import java.util.Optional;

import com.mainframe.generator.codegen.model.ModelIndex;
import com.mainframe.generator.codegen.model.input.MappingDocument;
import com.mainframe.generator.codegen.model.output.OutputPathSet;
import com.mainframe.generator.codegen.model.selection.CopybookIndex;
import com.mainframe.generator.codegen.model.selection.CopybookSelection;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;

@Getter
@Builder(toBuilder = true)
public final class GenerationContext {

    @NonNull
    private final GeneratorConfig config;

    @NonNull
    private final Path projectRoot;

    @NonNull
    private final CopybookIndex copybookIndex;

    @NonNull
    private final CopybookSelection copybookSelection;

    private final MappingDocument mappingDocument;

    @NonNull
    private final ModelIndex modelIndex;

    @NonNull
    private final OutputPathSet outputPaths;

    @NonNull
    private final GenerationFlags flags;

    @NonNull
    private final GenerationStats stats;

    @Builder.Default
    private final Instant startedAt = Instant.now();

    public Optional<MappingDocument> getMappingDocument() {
        return Optional.ofNullable(mappingDocument);
    }
}

