package com.mainframe.generator.codegen.dto.metadata;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.StructuralSignatureCalculator;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.util.NamingUtil;

import java.util.Set;

/**
 * Builds DtoMetadata for copybook models.
 */
public class DtoMetadataBuilder {
    private final GeneratorConfig config;
    private final DtoPackageClassifier classifier;
    private final Set<String> usedClassNames;
    private final CopybookModel requestRoot;
    private final CopybookModel responseRoot;

    public DtoMetadataBuilder(GeneratorConfig config,
                              DtoPackageClassifier classifier,
                              Set<String> usedClassNames,
                              CopybookModel requestRoot,
                              CopybookModel responseRoot) {
        this.config = config;
        this.classifier = classifier;
        this.usedClassNames = usedClassNames;
        this.requestRoot = requestRoot;
        this.responseRoot = responseRoot;
    }

    /**
     * Build metadata for a single model.
     */
    public DtoMetadata buildForModel(CopybookModel model) {
        String signature = StructuralSignatureCalculator.calculateSignature(model);
        String packageType = classifier.classify(model);
        String className = determineClassName(model, packageType);
        usedClassNames.add(className);

        return DtoMetadata.builder()
                .copybookModel(model)
                .className(className)
                .originalClassName(NamingUtil.toPascalCase(model.getName()))
                .packageType(packageType)
                .structuralSignature(signature)
                .isWrapper(false)
                .isDeduped(false)
                .byteLength(model.calculateTotalByteLength())
                .build();
    }

    private String determineClassName(CopybookModel model, String packageType) {
        // Special handling for root copybooks - ALWAYS use programId naming
        if (isRequestRoot(model)) {
            return NamingUtil.toPascalCase(config.getProgramId()) + "Request";
        }
        if (isResponseRoot(model)) {
            return NamingUtil.toPascalCase(config.getProgramId()) + "Response";
        }

        String baseName = NamingUtil.toPascalCase(model.getName());
        if (!usedClassNames.contains(baseName)) {
            return baseName;
        }

        return NamingUtil.disambiguateClassName(baseName, model, usedClassNames);
    }

    private boolean isRequestRoot(CopybookModel model) {
        return model != null && model.equals(requestRoot);
    }

    private boolean isResponseRoot(CopybookModel model) {
        return model != null && model.equals(responseRoot);
    }
}
