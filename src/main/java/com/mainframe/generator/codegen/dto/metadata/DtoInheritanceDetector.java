package com.mainframe.generator.codegen.dto.metadata;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.StructuralSignatureCalculator;
import com.mainframe.generator.codegen.model.input.CopybookModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Detects inheritance relationships between DTOs based on structural analysis.
 */
public class DtoInheritanceDetector {
    private static final Logger log = LoggerFactory.getLogger(DtoInheritanceDetector.class);

    private final Map<CopybookModel, DtoMetadata> dtoMetadataMap;
    private final Map<CopybookModel, CopybookModel> inheritanceMap;

    public DtoInheritanceDetector(Map<CopybookModel, DtoMetadata> dtoMetadataMap,
                                  Map<CopybookModel, CopybookModel> inheritanceMap) {
        this.dtoMetadataMap = dtoMetadataMap;
        this.inheritanceMap = inheritanceMap;
    }

    /**
     * Detect inheritance relationships and return count.
     */
    public int detect() {
        log.info("Detecting inheritance relationships...");
        int inheritanceCount = 0;

        List<CopybookModel> models = new ArrayList<>(dtoMetadataMap.keySet());

        for (int i = 0; i < models.size(); i++) {
            for (int j = 0; j < models.size(); j++) {
                if (i == j) continue;

                CopybookModel subset = models.get(i);
                CopybookModel superset = models.get(j);

                if (canInherit(subset, superset)) {
                    establishInheritance(subset, superset);
                    inheritanceCount++;
                    break; // Only one parent per DTO
                }
            }
        }

        return inheritanceCount;
    }

    private boolean canInherit(CopybookModel subset, CopybookModel superset) {
        DtoMetadata subsetMeta = dtoMetadataMap.get(subset);
        DtoMetadata supersetMeta = dtoMetadataMap.get(superset);

        // Skip if already deduped
        if (subsetMeta.isDeduped() || supersetMeta.isDeduped()) {
            return false;
        }

        // Check if subset is a structural prefix of superset
        return StructuralSignatureCalculator.isStructuralSubset(subset, superset);
    }

    private void establishInheritance(CopybookModel child, CopybookModel parent) {
        DtoMetadata childMeta = dtoMetadataMap.get(child);
        DtoMetadata parentMeta = dtoMetadataMap.get(parent);

        inheritanceMap.put(child, parent);
        childMeta.setExtendsClassName(parentMeta.getClassName());

        log.info("INHERITANCE: {} extends {} (structural prefix detected)",
                childMeta.getClassName(), parentMeta.getClassName());
    }
}
