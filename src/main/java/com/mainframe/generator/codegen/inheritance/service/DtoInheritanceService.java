package com.mainframe.generator.codegen.inheritance.service;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.StructuralSignatureCalculator;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Service that detects inheritance relationships between DTOs based on structural analysis.
 *
 * NOTE: This preserves the current behavior in your DtoInheritanceDetector:
 * - If StructuralSignatureCalculator.isStructuralSubset(subset, superset) is true,
 *   then subset is set to EXTEND superset (inheritanceMap.put(subset, superset)).
 *
 * If you intended the opposite (superset extends subset), flip the establishInheritance args.
 */
public class DtoInheritanceService {

    private static final Logger log = LoggerFactory.getLogger(DtoInheritanceService.class);

    /**
     * Detect inheritance relationships and populate inheritanceMap + update metadata.
     *
     * @return number of inheritance relationships detected
     */
    public int detectAndApply(Map<CopybookModel, DtoMetadata> dtoMetadataMap,
                              Map<CopybookModel, CopybookModel> inheritanceMap) {

        if (dtoMetadataMap == null || dtoMetadataMap.isEmpty()) {
            log.info("Inheritance detection skipped: dtoMetadataMap is empty.");
            return 0;
        }
        if (inheritanceMap == null) {
            throw new IllegalArgumentException("inheritanceMap must not be null");
        }

        log.info("Detecting inheritance relationships...");
        int inheritanceCount = 0;

        List<CopybookModel> models = new ArrayList<>(dtoMetadataMap.keySet());

        for (int i = 0; i < models.size(); i++) {
            for (int j = 0; j < models.size(); j++) {
                if (i == j) continue;

                CopybookModel subset = models.get(i);
                CopybookModel superset = models.get(j);

                if (canInherit(subset, superset, dtoMetadataMap)) {
                    establishInheritance(superset, subset, dtoMetadataMap, inheritanceMap);
                    inheritanceCount++;
                    break; // Only one parent per DTO
                }
            }
        }

        return inheritanceCount;
    }

    private boolean canInherit(CopybookModel subset,
                               CopybookModel superset,
                               Map<CopybookModel, DtoMetadata> dtoMetadataMap) {

        DtoMetadata subsetMeta = dtoMetadataMap.get(subset);
        DtoMetadata supersetMeta = dtoMetadataMap.get(superset);

        if (subsetMeta == null || supersetMeta == null) {
            return false;
        }

        // Skip if already deduped
        if (subsetMeta.isDeduped() || supersetMeta.isDeduped()) {
            return false;
        }

        // Structural prefix check
        return StructuralSignatureCalculator.isStructuralSubset(subset, superset);
    }

    /**
     * Preserve existing behavior: "child" extends "parent"
     * where child=subset and parent=superset.
     */
    private void establishInheritance(CopybookModel child,
                                      CopybookModel parent,
                                      Map<CopybookModel, DtoMetadata> dtoMetadataMap,
                                      Map<CopybookModel, CopybookModel> inheritanceMap) {

        DtoMetadata childMeta = dtoMetadataMap.get(child);
        DtoMetadata parentMeta = dtoMetadataMap.get(parent);

        if (childMeta == null || parentMeta == null) {
            return;
        }

        inheritanceMap.put(child, parent);
        childMeta.setExtendsClassName(parentMeta.getClassName());

        log.info("INHERITANCE: {} extends {} (structural prefix detected)",
                childMeta.getClassName(), parentMeta.getClassName());
    }
}
