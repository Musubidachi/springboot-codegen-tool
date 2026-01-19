package com.mainframe.generator.codegen.model.selection;

import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;

import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;

import com.mainframe.generator.codegen.model.input.CopybookModel;

/**
 * Read-only lookup/index for parsed copybooks.
 */
@Value
@Builder(toBuilder = true)
public class CopybookIndex {

    /**
     * Primary name-based lookup.
     */
    @NonNull
    @Singular("byNameEntry")
    Map<String, CopybookModel> byName;

    /**
     * Path-based lookup.
     */
    @NonNull
    @Singular("byPathEntry")
    Map<Path, CopybookModel> byPath;

    /**
     * All copybooks keyed by a stable identifier (e.g. name or path).
     */
    @NonNull
    @Singular("copybook")
    Map<String, CopybookModel> all;

    public Optional<CopybookModel> findByName(String name) {
        if (name == null || name.isBlank()) {
            return Optional.empty();
        }
        return Optional.ofNullable(byName.get(name));
    }

    public Optional<CopybookModel> findByPath(Path path) {
        if (path == null) {
            return Optional.empty();
        }
        return Optional.ofNullable(byPath.get(path));
    }

    /**
     * Convenience lookup: try name first, then path.
     */
    public Optional<CopybookModel> findByNameOrPath(String name, Path path) {
        return findByName(name).or(() -> findByPath(path));
    }
}
