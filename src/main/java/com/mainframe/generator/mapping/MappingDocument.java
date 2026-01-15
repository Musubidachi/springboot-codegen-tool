package com.mainframe.generator.mapping;

import lombok.Data;

import java.util.*;

/**
 * Represents a parsed mapping document with all mapping rules.
 */
@Data
public class MappingDocument {
    private final Map<String, MappingEntry> entriesBySourceField = new HashMap<>();
    private final List<MappingEntry> allEntries = new ArrayList<>();
    private final List<String> errors = new ArrayList<>();
    private final List<String> warnings = new ArrayList<>();
    
    public void addEntry(MappingEntry entry) {
        allEntries.add(entry);
        for (String sourceField : entry.getSourceFields()) {
            entriesBySourceField.put(sourceField.toUpperCase(), entry);
        }
    }
    
    public Optional<MappingEntry> getMappingFor(String fieldName) {
        return Optional.ofNullable(entriesBySourceField.get(fieldName.toUpperCase()));
    }
    
    public boolean shouldIgnore(String fieldName) {
        return getMappingFor(fieldName)
                .map(MappingEntry::isIgnore)
                .orElse(false);
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
    
    public List<MappingEntry> getCombinedFieldMappings() {
        return allEntries.stream()
                .filter(e -> e.getType() == MappingEntry.MappingType.COMBINE)
                .toList();
    }
    
    public List<MappingEntry> getEnumMappings() {
        return allEntries.stream()
                .filter(e -> e.getType() == MappingEntry.MappingType.ENUM)
                .toList();
    }
    
    public boolean hasErrors() {
        return !errors.isEmpty();
    }
    
    public void addError(String error) {
        errors.add(error);
    }
    
    public void addWarning(String warning) {
        warnings.add(warning);
    }
}
