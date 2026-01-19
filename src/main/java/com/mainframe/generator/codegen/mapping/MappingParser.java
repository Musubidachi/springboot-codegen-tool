package com.mainframe.generator.codegen.mapping;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.model.input.MappingDocument;
import com.mainframe.generator.codegen.model.input.MappingEntry;

public class MappingParser {
    private static final Logger log = LoggerFactory.getLogger(MappingParser.class);

    // Allow dot notation too (HDR.FIELD-NAME)
    private static final Pattern MAPPING_PATTERN = Pattern.compile(
            "^([A-Z0-9_\\-\\.]+(?:\\s*\\+\\s*[A-Z0-9_\\-\\.]+)*)\\s*=\\s*(.+)$",
            Pattern.CASE_INSENSITIVE
    );

    private static final Pattern TARGET_TYPE_PATTERN = Pattern.compile(
            "^([A-Za-z][A-Za-z0-9]*)(?::(.+))?$"
    );

    public MappingDocument parse(Path mappingFile) {
        MappingDocument doc = new MappingDocument();
        try {
            List<String> lines = Files.readAllLines(mappingFile);
            return parse(lines);
        } catch (IOException e) {
            doc.addError("Failed to read mapping file: " + mappingFile + " (" + e.getMessage() + ")");
            return doc;
        }
    }

    public MappingDocument parse(List<String> lines) {
        MappingDocument doc = new MappingDocument();

        int lineNum = 0;
        int errorCount = 0;

        for (String rawLine : lines) {
            lineNum++;

            String trimmed = rawLine.trim();
            if (trimmed.isEmpty() || trimmed.startsWith("#")) {
                continue;
            }

            try {
                MappingEntry entry = parseLine(trimmed);
                if (entry != null) {
                    doc.addEntry(entry);
                    log.debug("Parsed mapping: {} -> {} ({})",
                            entry.getSourceFields(), entry.getTargetName(), entry.getType());
                }
            } catch (Exception e) {
                errorCount++;
                doc.addError("Line " + lineNum + ": " + e.getMessage() + " | '" + trimmed + "'");
                log.debug("Failed to parse mapping line {}: {}", lineNum, e.getMessage());
            }
        }

        if (errorCount > 0) {
            log.warn("Mapping document parsed with {} errors", errorCount);
        }

        return doc;
    }

    private MappingEntry parseLine(String line) {
        Matcher matcher = MAPPING_PATTERN.matcher(line);
        if (!matcher.matches()) {
            throw new IllegalArgumentException("Invalid mapping format (expected 'SRC = TARGET'): " + line);
        }

        String sourceStr = matcher.group(1).trim();
        String targetStr = matcher.group(2).trim();

        List<String> sourceFields = parseSourceFields(sourceStr);
        boolean isCombined = sourceFields.size() > 1;

        // IGNORE
        if (targetStr.equalsIgnoreCase("IGNORE")) {
            return MappingEntry.builder()
                    .type(MappingEntry.MappingType.IGNORE)
                    .sourceFields(sourceFields)
                    .ignore(true)
                    .build();
        }

        // Parse target name and optional type
        Matcher targetMatcher = TARGET_TYPE_PATTERN.matcher(targetStr);
        if (!targetMatcher.matches()) {
            throw new IllegalArgumentException("Invalid target format: " + targetStr);
        }

        String targetName = targetMatcher.group(1);

        String targetType = targetMatcher.group(2);
        if (targetType != null) {
            targetType = targetType.trim();
            if (targetType.isEmpty()) {
                throw new IllegalArgumentException("Target type was specified but empty (e.g. 'Foo:')");
            }
        }

        MappingEntry.MappingType type;
        if (isCombined) {
            type = MappingEntry.MappingType.COMBINE;
            // Optional strict rule:
            // if (targetType == null) throw new IllegalArgumentException("Combined mapping must specify a type, e.g. A + B = C:LocalDate");
        } else if (targetType != null && "enum".equalsIgnoreCase(targetType)) {
            type = MappingEntry.MappingType.ENUM;
            targetType = null; // Will be generated
        } else {
            type = MappingEntry.MappingType.RENAME;
        }

        return MappingEntry.builder()
                .type(type)
                .sourceFields(sourceFields)
                .targetName(targetName)
                .targetType(targetType)
                .ignore(false)
                .build();
    }

    private List<String> parseSourceFields(String sourceStr) {
        List<String> fields = new ArrayList<>();

        if (sourceStr.contains("+")) {
            String[] parts = sourceStr.split("\\+");
            for (String part : parts) {
                fields.add(part.trim().toUpperCase());
            }
        } else {
            fields.add(sourceStr.trim().toUpperCase());
        }

        return fields;
    }
}
