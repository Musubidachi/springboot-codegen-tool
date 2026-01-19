package com.mainframe.generator.codegen.model.input;

import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

import java.nio.file.Path;
import java.util.*;

/**
 * Represents a fully parsed copybook with all its structure.
 */
@Data
@Builder(toBuilder = true)
public class CopybookModel {
    private String name;
    private String sourcePath;
    private GroupNode rootGroup;
    private List<CopyDirectiveNode> copyDirectives;
    private List<RedefinesNode> redefines;
    private Map<String, FieldNode> fieldsByName;
    private Map<String, GroupNode> groupsByName;
    private int totalByteLength;
    
    public static CopybookModelBuilder builder() {
        return new CopybookModelBuilder()
                .copyDirectives(new ArrayList<>())
                .redefines(new ArrayList<>())
                .fieldsByName(new HashMap<>())
                .groupsByName(new HashMap<>());
    }
    
}
