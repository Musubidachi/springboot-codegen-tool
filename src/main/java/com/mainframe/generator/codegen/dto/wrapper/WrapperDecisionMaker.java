package com.mainframe.generator.codegen.dto.wrapper;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * FIX: Decides whether to generate wrapper classes based on DTO count,
 * NOT on folder-based selection mode. Wrappers are needed whenever
 * multiple DTOs exist in a category (request/response).
 */
public class WrapperDecisionMaker {
    private static final Logger log = LoggerFactory.getLogger(WrapperDecisionMaker.class);

    /**
     * Determines if ApiRequest wrapper should be generated.
     * FIX: Returns true if multiple non-deduped request DTOs exist,
     * regardless of heuristic vs folder mode.
     */
    public static boolean shouldGenerateRequestWrapper(
            Set<CopybookModel> requestModels,
            Set<CopybookModel> sharedModels,
            Map<CopybookModel, DtoMetadata> dtoMetadataMap) {

        List<DtoMetadata> requestDtos = collectRequestDtos(requestModels, sharedModels, dtoMetadataMap);
        boolean shouldGenerate = requestDtos.size() > 1;

        if (shouldGenerate) {
            log.info("Will generate ApiRequest wrapper ({} DTOs)", requestDtos.size());
        } else {
            log.debug("Skipping ApiRequest wrapper (only {} DTO)", requestDtos.size());
        }

        return shouldGenerate;
    }

    /**
     * Determines if ApiResponse wrapper should be generated.
     */
    public static boolean shouldGenerateResponseWrapper(
            Set<CopybookModel> responseModels,
            Set<CopybookModel> sharedModels,
            Map<CopybookModel, DtoMetadata> dtoMetadataMap) {

        List<DtoMetadata> responseDtos = collectResponseDtos(responseModels, sharedModels, dtoMetadataMap);
        boolean shouldGenerate = responseDtos.size() > 1;

        if (shouldGenerate) {
            log.info("Will generate ApiResponse wrapper ({} DTOs)", responseDtos.size());
        } else {
            log.debug("Skipping ApiResponse wrapper (only {} DTO)", responseDtos.size());
        }

        return shouldGenerate;
    }

    public static List<DtoMetadata> collectRequestDtos(
            Set<CopybookModel> requestModels,
            Set<CopybookModel> sharedModels,
            Map<CopybookModel, DtoMetadata> dtoMetadataMap) {

        return requestModels.stream()
                .map(dtoMetadataMap::get)
                .filter(meta -> meta != null && !meta.isDeduped())
                .collect(Collectors.toList());
    }

    public static List<DtoMetadata> collectResponseDtos(
            Set<CopybookModel> responseModels,
            Set<CopybookModel> sharedModels,
            Map<CopybookModel, DtoMetadata> dtoMetadataMap) {

        return responseModels.stream()
                .map(dtoMetadataMap::get)
                .filter(meta -> meta != null && !meta.isDeduped())
                .collect(Collectors.toList());
    }
}
