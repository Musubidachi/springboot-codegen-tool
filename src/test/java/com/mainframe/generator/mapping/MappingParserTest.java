package com.mainframe.generator.mapping;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for MappingParser.
 */
class MappingParserTest {

    private final MappingParser parser = new MappingParser();

    @Test
    void testParseSimpleRename() {
        List<String> lines = List.of("CUSTOMER-ID = customerId");
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getAllEntries()).hasSize(1);
        MappingEntry entry = doc.getAllEntries().get(0);
        assertThat(entry.getType()).isEqualTo(MappingEntry.MappingType.RENAME);
        assertThat(entry.getSourceFields()).containsExactly("CUSTOMER-ID");
        assertThat(entry.getTargetName()).isEqualTo("customerId");
    }

    @Test
    void testParseCombinedFields() {
        List<String> lines = List.of("DATE-YEAR + DATE-MONTH + DATE-DAY = birthDate:LocalDate");
        
        MappingDocument doc = parser.parse(lines);
        
        MappingEntry entry = doc.getAllEntries().get(0);
        assertThat(entry.getType()).isEqualTo(MappingEntry.MappingType.COMBINE);
        assertThat(entry.getSourceFields()).containsExactly("DATE-YEAR", "DATE-MONTH", "DATE-DAY");
        assertThat(entry.getTargetName()).isEqualTo("birthDate");
        assertThat(entry.getTargetType()).isEqualTo("LocalDate");
    }

    @Test
    void testParseEnumMapping() {
        List<String> lines = List.of("STATUS-CODE = StatusCode:enum");
        
        MappingDocument doc = parser.parse(lines);
        
        MappingEntry entry = doc.getAllEntries().get(0);
        assertThat(entry.getType()).isEqualTo(MappingEntry.MappingType.ENUM);
        assertThat(entry.getTargetName()).isEqualTo("StatusCode");
    }

    @Test
    void testParseIgnore() {
        List<String> lines = List.of("FILLER = IGNORE");
        
        MappingDocument doc = parser.parse(lines);
        
        MappingEntry entry = doc.getAllEntries().get(0);
        assertThat(entry.getType()).isEqualTo(MappingEntry.MappingType.IGNORE);
        assertThat(entry.isIgnore()).isTrue();
    }

    @Test
    void testSkipComments() {
        List<String> lines = List.of(
                "# This is a comment",
                "FIELD-ONE = fieldOne",
                "# Another comment",
                "FIELD-TWO = fieldTwo"
        );
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getAllEntries()).hasSize(2);
    }

    @Test
    void testSkipEmptyLines() {
        List<String> lines = List.of(
                "FIELD-ONE = fieldOne",
                "",
                "   ",
                "FIELD-TWO = fieldTwo"
        );
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getAllEntries()).hasSize(2);
    }

    @Test
    void testGetMappingFor() {
        List<String> lines = List.of(
                "CUSTOMER-ID = customerId",
                "FIELD-TWO = fieldTwo"
        );
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getMappingFor("CUSTOMER-ID")).isPresent();
        assertThat(doc.getMappingFor("customer-id")).isPresent(); // Case insensitive
        assertThat(doc.getMappingFor("NONEXISTENT")).isEmpty();
    }

    @Test
    void testShouldIgnore() {
        List<String> lines = List.of(
                "FILLER = IGNORE",
                "REGULAR-FIELD = regularField"
        );
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.shouldIgnore("FILLER")).isTrue();
        assertThat(doc.shouldIgnore("REGULAR-FIELD")).isFalse();
    }

    @Test
    void testGetRenamedName() {
        List<String> lines = List.of("OLD-NAME = newName");
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getRenamedName("OLD-NAME")).contains("newName");
        assertThat(doc.getRenamedName("OTHER")).isEmpty();
    }

    @Test
    void testInvalidLineFormat() {
        List<String> lines = List.of(
                "VALID-FIELD = validField",
                "invalid line without equals",
                "ANOTHER-FIELD = anotherField"
        );
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getAllEntries()).hasSize(2);
        assertThat(doc.hasErrors()).isTrue();
    }

    @Test
    void testCaseInsensitiveSourceFields() {
        List<String> lines = List.of("customer-name = customerName");
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getMappingFor("CUSTOMER-NAME")).isPresent();
        assertThat(doc.getMappingFor("Customer-Name")).isPresent();
    }

    @Test
    void testCombinedFieldMappings() {
        List<String> lines = List.of(
                "FIELD-A + FIELD-B = combined:String",
                "SINGLE-FIELD = singleField"
        );
        
        MappingDocument doc = parser.parse(lines);
        
        assertThat(doc.getCombinedFieldMappings()).hasSize(1);
        assertThat(doc.getCombinedFieldMappings().get(0).getSourceFields())
                .containsExactly("FIELD-A", "FIELD-B");
    }
}
