package com.mainframe.generator.codegen.project;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.dto.EnumGenerator;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.model.input.MappingDocument;

/**
 * Orchestrates enum generation (88-level values) for DTOs.
 */
public class EnumGenerationService {

    public void generateEnumClasses(Path projectDir,
                                    MappingDocument mappingDoc,
                                    Map<CopybookModel, DtoMetadata> dtoMetadataMap) throws IOException {

        EnumGenerator enumGenerator = new EnumGenerator(mappingDoc, dtoMetadataMap);
        enumGenerator.generateEnums(projectDir);
    }
}
