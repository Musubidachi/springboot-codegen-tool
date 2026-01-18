package com.mainframe.generator.codegen.dto.metadata;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.model.CopybookModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * Detects and marks structurally identical DTOs for deduplication.
 */
public class DtoDeduplicator {
    private static final Logger log = LoggerFactory.getLogger(DtoDeduplicator.class);

    private final Map<CopybookModel, DtoMetadata> dtoMetadataMap;
    private final Map<String, List<CopybookModel>> signatureToModels;
    private final Set<CopybookModel> sharedModels;
    private final Set<CopybookModel> requestModels;
    private final Set<CopybookModel> responseModels;

    public DtoDeduplicator(Map<CopybookModel, DtoMetadata> dtoMetadataMap,
                           Map<String, List<CopybookModel>> signatureToModels,
                           Set<CopybookModel> sharedModels,
                           Set<CopybookModel> requestModels,
                           Set<CopybookModel> responseModels) {
        this.dtoMetadataMap = dtoMetadataMap;
        this.signatureToModels = signatureToModels;
        this.sharedModels = sharedModels;
        this.requestModels = requestModels;
        this.responseModels = responseModels;
    }

    /**
     * Perform deduplication and return count of deduped DTOs.
     */
    public int deduplicate() {
        int dedupedCount = 0;

        for (Map.Entry<String, List<CopybookModel>> entry : signatureToModels.entrySet()) {
            if (entry.getValue().size() > 1) {
                List<CopybookModel> duplicates = entry.getValue();
                CopybookModel canonical = selectCanonical(duplicates);

                DtoMetadata canonicalMeta = dtoMetadataMap.get(canonical);
                moveToShared(canonical, canonicalMeta);

                dedupedCount += markDuplicates(duplicates, canonical, canonicalMeta, entry.getKey());
            }
        }

        return dedupedCount;
    }

    private CopybookModel selectCanonical(List<CopybookModel> duplicates) {
        return duplicates.stream()
                .min(Comparator.comparing((CopybookModel m) -> packageTypePriority(m))
                        .thenComparing(CopybookModel::getName))
                .orElse(duplicates.get(0));
    }

    private int packageTypePriority(CopybookModel m) {
        DtoMetadata meta = dtoMetadataMap.get(m);
        if ("shared".equals(meta.getPackageType())) return 0;
        if ("request".equals(meta.getPackageType())) return 1;
        if ("response".equals(meta.getPackageType())) return 2;
        return 3; // layout
    }

    private void moveToShared(CopybookModel canonical, DtoMetadata canonicalMeta) {
        if (!"shared".equals(canonicalMeta.getPackageType())) {
            canonicalMeta.setPackageType("shared");
            if (!sharedModels.contains(canonical)) {
                sharedModels.add(canonical);
                requestModels.remove(canonical);
                responseModels.remove(canonical);
            }
        }
    }

    private int markDuplicates(List<CopybookModel> duplicates, CopybookModel canonical,
                               DtoMetadata canonicalMeta, String signature) {
        int count = 0;
        for (CopybookModel dup : duplicates) {
            if (dup != canonical) {
                DtoMetadata dupMeta = dtoMetadataMap.get(dup);
                dupMeta.setDeduped(true);
                dupMeta.setDedupedToClassName(canonicalMeta.getClassName());
                count++;
                log.info("DEDUPED DTO: {} and {} share signature {} -> using shared type {}",
                        canonical.getName(), dup.getName(), signature, canonicalMeta.getClassName());
            }
        }
        return count;
    }
}
