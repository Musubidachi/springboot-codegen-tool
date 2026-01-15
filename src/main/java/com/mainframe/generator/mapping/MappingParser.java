package com.mainframe.generator.mapping;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser for mapping document files.
 * 
 * Format:
 * - Rename: TMP-BUS-DAY = TemporaryBusinessDay
 * - Combine: TMP-BUS-DT + TMP-BUS-DAY = TemporaryBusinessDate:LocalDate
 * - Enum: STATUS-CODE = StatusCode:enum
 * - Ignore: FILLER = IGNORE
 * - Comments: # comment
 */
public class MappingParser {
    private static final Logger log = LoggerFactory.getLogger(MappingParser.class);
    
    // Pattern for parsing mapping lines
    private static final Pattern MAPPING_PATTERN = Pattern.compile(
            "^([A-Z0-9_\\-]+(?:\\s*\\+\\s*[A-Z0-9_\\-]+)*)\\s*=\\s*(.+)$",
            Pattern.CASE_INSENSITIVE
    );
    
    // Pattern for combined fields
    private static final Pattern COMBINE_PATTERN = Pattern.compile(
            "([A-Z0-9_\\-]+)\\s*\\+",
            Pattern.CASE_INSENSITIVE
    );
    
    // Pattern for target with type
    private static final Pattern TARGET_TYPE_PATTERN = Pattern.compile(
            "^([A-Za-z][A-Za-z0-9]*)(?::(.+))?$"
    );
    
    public MappingDocument parse(Path mappingFile) throws IOException {
        List<String> lines = Files.readAllLines(mappingFile);
        return parse(lines);
    }
    
    public MappingDocument parse(List<String> lines) {
        MappingDocument doc = new MappingDocument();
        
        int lineNum = 0;
        for (String line : lines) {
            lineNum++;
            
            // Skip empty lines and comments
            String trimmed = line.trim();
            if (trimmed.isEmpty() || trimmed.startsWith("#")) {
                continue;
            }
            
            try {
                MappingEntry entry = parseLine(trimmed, lineNum);
                if (entry != null) {
                    doc.addEntry(entry);
                    log.debug("Parsed mapping: {} -> {} ({})", 
                            entry.getSourceFields(), entry.getTargetName(), entry.getType());
                }
            } catch (Exception e) {
                doc.addError("Line " + lineNum + ": " + e.getMessage());
                log.warn("Failed to parse mapping line {}: {}", lineNum, e.getMessage());
            }
        }
        
        return doc;
    }
    
    private MappingEntry parseLine(String line, int lineNum) {
        Matcher matcher = MAPPING_PATTERN.matcher(line);
        if (!matcher.matches()) {
            throw new IllegalArgumentException("Invalid mapping format: " + line);
        }
        
        String sourceStr = matcher.group(1).trim();
        String targetStr = matcher.group(2).trim();
        
        // Parse source fields
        List<String> sourceFields = parseSourceFields(sourceStr);
        boolean isCombined = sourceFields.size() > 1;
        
        // Check for IGNORE
        if (targetStr.equalsIgnoreCase("IGNORE")) {
            return MappingEntry.builder()
                    .type(MappingEntry.MappingType.IGNORE)
                    .sourceFields(sourceFields)
                    .ignore(true)
                    .build();
        }
        
        // Parse target name and type
        Matcher targetMatcher = TARGET_TYPE_PATTERN.matcher(targetStr);
        if (!targetMatcher.matches()) {
            throw new IllegalArgumentException("Invalid target format: " + targetStr);
        }
        
        String targetName = targetMatcher.group(1);
        String targetType = targetMatcher.group(2);
        
        // Determine mapping type
        MappingEntry.MappingType type;
        if (isCombined) {
            type = MappingEntry.MappingType.COMBINE;
        } else if ("enum".equalsIgnoreCase(targetType)) {
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
            // Multiple fields combined
            String[] parts = sourceStr.split("\\+");
            for (String part : parts) {
                fields.add(part.trim().toUpperCase());
            }
        } else {
            fields.add(sourceStr.toUpperCase());
        }
        
        return fields;
    }
}
