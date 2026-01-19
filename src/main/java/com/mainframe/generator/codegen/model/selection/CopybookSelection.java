package com.mainframe.generator.codegen.model.selection;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.mainframe.generator.codegen.model.input.CopybookModel;

/**
 * Result of selecting/classifying copybooks into request/response/shared buckets.
 */
@Value
@Builder(toBuilder = true)
public class CopybookSelection {

    @NonNull
    CopybookModel requestRoot;

    @NonNull
    CopybookModel responseRoot;

    @NonNull
    @Singular("requestModel")
    Set<CopybookModel> requestModels;

    @NonNull
    @Singular("responseModel")
    Set<CopybookModel> responseModels;

    @NonNull
    @Builder.Default
    Set<CopybookModel> sharedModels = Collections.emptySet();

    /**
     * Derived union of request, response, and shared models.
     */
    public Set<CopybookModel> getAllModels() {
        Set<CopybookModel> all = new HashSet<>();
        all.addAll(requestModels);
        all.addAll(responseModels);
        all.addAll(sharedModels);
        return Collections.unmodifiableSet(all);
    }

    public int totalModelCount() {
        return getAllModels().size();
    }
}
