package com.mainframe.generator.codegen.model.input;

import lombok.Getter;

import java.util.*;

/**
 * Represents a parsed mapping document with all mapping rules plus diagnostics.
 */
@Getter
public class MappingDocument {

    private final Map<String, MappingEntry> entriesBySourceField = new HashMap<>();
    private final List<MappingEntry> allEntries = new ArrayList<>();

    // Precomputed views (optional but nice)
    private final List<MappingEntry> combinedEntries = new ArrayList<>();
    private final List<MappingEntry> enumEntries = new ArrayList<>();

    private final List<String> errors = new ArrayList<>();
    private final List<String> warnings = new ArrayList<>();

    public void addEntry(MappingEntry entry) {
        Objects.requireNonNull(entry, "entry");

        allEntries.add(entry);

        // Maintain derived lists
        if (entry.getType() == MappingEntry.MappingType.COMBINE) {
            combinedEntries.add(entry);
        } else if (entry.getType() == MappingEntry.MappingType.ENUM) {
            enumEntries.add(entry);
        }

        for (String sourceField : entry.getSourceFields()) {
            if (sourceField == null || sourceField.isBlank()) {
                addWarning("Mapping entry contains blank source field: " + entry);
                continue;
            }

            String key = sourceField.toUpperCase(Locale.ROOT);

            MappingEntry previous = entriesBySourceField.put(key, entry);
            if (previous != null && previous != entry) {
                addWarning("Duplicate mapping for source field '" + sourceField
                        + "'. Later entry overwrote earlier one.");
            }
        }
    }

    public Optional<MappingEntry> getMappingFor(String fieldName) {
        if (fieldName == null || fieldName.isBlank()) return Optional.empty();
        return Optional.ofNullable(entriesBySourceField.get(fieldName.toUpperCase(Locale.ROOT)));
    }

    public boolean shouldIgnore(String fieldName) {
        return getMappingFor(fieldName).map(MappingEntry::isIgnore).orElse(false);
    }

    public boolean shouldRename(String fieldName) {
        return getMappingFor(fieldName)
                .filter(e -> e.getType() == MappingEntry.MappingType.RENAME)
                .isPresent();
    }

    public Optional<String> getRenamedName(String fieldName) {
        return getMappingFor(fieldName)
                .filter(e -> e.getType() == MappingEntry.MappingType.RENAME)
                .map(MappingEntry::getTargetName);
    }

    /** Unmodifiable view (safe for callers). */
    public List<MappingEntry> getAllEntriesView() {
        return Collections.unmodifiableList(allEntries);
    }

    public List<MappingEntry> getCombinedFieldMappings() {
        return Collections.unmodifiableList(combinedEntries);
    }

    public List<MappingEntry> getEnumMappings() {
        return Collections.unmodifiableList(enumEntries);
    }

    public boolean hasErrors() {
        return !errors.isEmpty();
    }

    public void addError(String error) {
        if (error != null && !error.isBlank()) errors.add(error);
    }

    public void addWarning(String warning) {
        if (warning != null && !warning.isBlank()) warnings.add(warning);
    }

    /** Optional: safe views for diagnostics. */
    public List<String> getErrorsView() {
        return Collections.unmodifiableList(errors);
    }

    public List<String> getWarningsView() {
        return Collections.unmodifiableList(warnings);
    }
}
