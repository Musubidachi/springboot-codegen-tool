package com.mainframe.generator.codegen.dedup.service;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Service that detects and marks structurally identical DTOs for deduplication.
 *
 * Rules (same as current DtoDeduplicator):
 * - For each structural signature with >1 models, select a canonical DTO.
 * - Canonical selection priority: shared > request > response > layout, then by name.
 * - Canonical is moved to shared package type (and sets membership updated).
 * - Other duplicates are marked deduped and point to canonical class name.
 */
public class DtoDeduplicationService {

    private static final Logger log = LoggerFactory.getLogger(DtoDeduplicationService.class);

    public int deduplicate(Map<CopybookModel, DtoMetadata> dtoMetadataMap,
                           Map<String, List<CopybookModel>> signatureToModels,
                           Set<CopybookModel> sharedModels,
                           Set<CopybookModel> requestModels,
                           Set<CopybookModel> responseModels) {

        if (dtoMetadataMap == null || dtoMetadataMap.isEmpty()) {
            log.info("Deduplication skipped: dtoMetadataMap is empty.");
            return 0;
        }
        if (signatureToModels == null || signatureToModels.isEmpty()) {
            log.info("Deduplication skipped: signatureToModels is empty.");
            return 0;
        }

        int dedupedCount = 0;

        for (Map.Entry<String, List<CopybookModel>> entry : signatureToModels.entrySet()) {
            List<CopybookModel> duplicates = entry.getValue();
            if (duplicates == null || duplicates.size() <= 1) {
                continue;
            }

            String signature = entry.getKey();
            CopybookModel canonical = selectCanonical(duplicates, dtoMetadataMap);
            if (canonical == null) {
                continue;
            }

            DtoMetadata canonicalMeta = dtoMetadataMap.get(canonical);
            if (canonicalMeta == null) {
                continue;
            }

            moveCanonicalToShared(canonical, canonicalMeta, sharedModels, requestModels, responseModels);

            dedupedCount += markDuplicates(duplicates, canonical, canonicalMeta, signature, dtoMetadataMap);
        }

        return dedupedCount;
    }

    private CopybookModel selectCanonical(List<CopybookModel> duplicates,
                                          Map<CopybookModel, DtoMetadata> dtoMetadataMap) {
        return duplicates.stream()
                .filter(m -> m != null && dtoMetadataMap.containsKey(m))
                .min(Comparator.comparing((CopybookModel m) -> packageTypePriority(m, dtoMetadataMap))
                        .thenComparing(CopybookModel::getName, Comparator.nullsLast(String::compareToIgnoreCase)))
                .orElse(null);
    }

    private int packageTypePriority(CopybookModel m, Map<CopybookModel, DtoMetadata> dtoMetadataMap) {
        DtoMetadata meta = dtoMetadataMap.get(m);
        if (meta == null) return 99;

        String pkg = meta.getPackageType();
        if ("shared".equals(pkg)) return 0;
        if ("request".equals(pkg)) return 1;
        if ("response".equals(pkg)) return 2;
        return 3; // layout or unknown
    }

    private void moveCanonicalToShared(CopybookModel canonical,
                                       DtoMetadata canonicalMeta,
                                       Set<CopybookModel> sharedModels,
                                       Set<CopybookModel> requestModels,
                                       Set<CopybookModel> responseModels) {

        if (!"shared".equals(canonicalMeta.getPackageType())) {
            canonicalMeta.setPackageType("shared");

            // Ensure canonical is in shared set and removed from others
            if (sharedModels != null && !sharedModels.contains(canonical)) {
                sharedModels.add(canonical);
            }
            if (requestModels != null) {
                requestModels.remove(canonical);
            }
            if (responseModels != null) {
                responseModels.remove(canonical);
            }
        }
    }

    private int markDuplicates(List<CopybookModel> duplicates,
                               CopybookModel canonical,
                               DtoMetadata canonicalMeta,
                               String signature,
                               Map<CopybookModel, DtoMetadata> dtoMetadataMap) {

        int count = 0;

        for (CopybookModel dup : duplicates) {
            if (dup == null || dup.equals(canonical)) {
                continue;
            }

            DtoMetadata dupMeta = dtoMetadataMap.get(dup);
            if (dupMeta == null) {
                continue;
            }

            dupMeta.setDeduped(true);
            dupMeta.setDedupedToClassName(canonicalMeta.getClassName());
            count++;

            log.info("DEDUPED DTO: {} and {} share signature {} -> using shared type {}",
                    canonical.getName(), dup.getName(), signature, canonicalMeta.getClassName());
        }

        return count;
    }
}
