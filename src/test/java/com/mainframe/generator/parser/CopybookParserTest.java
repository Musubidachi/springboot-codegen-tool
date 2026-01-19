package com.mainframe.generator.parser;

import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.model.input.Enum88Node;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.GroupNode;
import com.mainframe.generator.codegen.model.input.UsageType;
import com.mainframe.generator.model.*;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for CopybookTokenizer and CopybookParser.
 */
class CopybookParserTest {

    @Test
    void testParseSimpleField() {
        String copybook = """
               01  TEST-RECORD.
                   05  FIELD-ONE           PIC X(10).
                   05  FIELD-TWO           PIC 9(5).
            """;

        CopybookModel model = parse(copybook);

        assertThat(model.getAllFields()).hasSize(2);
        
        FieldNode field1 = model.getAllFields().get(0);
        assertThat(field1.getName()).isEqualTo("FIELD-ONE");
        assertThat(field1.getPicture().isAlphanumeric()).isTrue();
        assertThat(field1.getPicture().getTotalLength()).isEqualTo(10);
        
        FieldNode field2 = model.getAllFields().get(1);
        assertThat(field2.getName()).isEqualTo("FIELD-TWO");
        assertThat(field2.getPicture().isNumeric()).isTrue();
        assertThat(field2.getPicture().getIntegerDigits()).isEqualTo(5);
    }

    @Test
    void testParseSignedNumeric() {
        String copybook = """
               01  TEST-RECORD.
                   05  AMOUNT              PIC S9(7)V99.
            """;

        CopybookModel model = parse(copybook);
        FieldNode field = model.getAllFields().get(0);

        assertThat(field.getPicture().isSigned()).isTrue();
        assertThat(field.getPicture().getIntegerDigits()).isEqualTo(7);
        assertThat(field.getPicture().getDecimalDigits()).isEqualTo(2);
    }

    @Test
    void testParseComp3PackedDecimal() {
        String copybook = """
               01  TEST-RECORD.
                   05  BALANCE             PIC S9(9)V99 COMP-3.
            """;

        CopybookModel model = parse(copybook);
        FieldNode field = model.getAllFields().get(0);

        assertThat(field.getUsage()).isEqualTo(UsageType.PACKED_DECIMAL);
        assertThat(field.calculateByteLength()).isEqualTo(6); // (11 digits + 1) / 2 = 6
    }

    @Test
    void testParseBinaryComp() {
        String copybook = """
               01  TEST-RECORD.
                   05  COUNT-FIELD         PIC 9(4) COMP.
                   05  LARGE-COUNT         PIC 9(9) COMP.
            """;

        CopybookModel model = parse(copybook);

        FieldNode field1 = model.getAllFields().get(0);
        assertThat(field1.getUsage()).isEqualTo(UsageType.BINARY);
        assertThat(field1.calculateByteLength()).isEqualTo(2);

        FieldNode field2 = model.getAllFields().get(1);
        assertThat(field2.calculateByteLength()).isEqualTo(4);
    }

    @Test
    void testParseOccurs() {
        String copybook = """
               01  TEST-RECORD.
                   05  ITEMS OCCURS 10 TIMES.
                       10  ITEM-CODE       PIC X(5).
                       10  ITEM-QTY        PIC 9(3).
            """;

        CopybookModel model = parse(copybook);

        GroupNode group = model.findGroup("ITEMS").orElse(null);
        assertThat(group).isNotNull();
        assertThat(group.getOccursCount()).isEqualTo(10);
    }

    @Test
    void testParse88LevelConditions() {
        String copybook = """
               01  TEST-RECORD.
                   05  STATUS-CODE         PIC X(1).
                       88  STATUS-ACTIVE   VALUE 'A'.
                       88  STATUS-INACTIVE VALUE 'I'.
                       88  STATUS-PENDING  VALUE 'P'.
            """;

        CopybookModel model = parse(copybook);
        FieldNode field = model.getAllFields().get(0);

        assertThat(field.hasEnum88Values()).isTrue();
        assertThat(field.getEnum88Values()).hasSize(3);
        
        Enum88Node enum1 = field.getEnum88Values().get(0);
        assertThat(enum1.getName()).isEqualTo("STATUS-ACTIVE");
        assertThat(enum1.getPrimaryValue()).isEqualTo("A");
    }

    @Test
    void testParseNestedGroups() {
        String copybook = """
               01  CUSTOMER-RECORD.
                   05  CUSTOMER-ID         PIC 9(10).
                   05  CUSTOMER-ADDRESS.
                       10  STREET          PIC X(30).
                       10  CITY            PIC X(20).
                       10  STATE           PIC X(2).
                       10  ZIP             PIC X(10).
                   05  CUSTOMER-PHONE      PIC X(15).
            """;

        CopybookModel model = parse(copybook);

        assertThat(model.getAllFields()).hasSize(6);
        assertThat(model.findGroup("CUSTOMER-ADDRESS")).isPresent();
        
        GroupNode address = model.findGroup("CUSTOMER-ADDRESS").get();
        assertThat(address.getChildren()).hasSize(4);
    }

    @Test
    void testParseFiller() {
        String copybook = """
               01  TEST-RECORD.
                   05  FIELD-ONE           PIC X(10).
                   05  FILLER              PIC X(5).
                   05  FIELD-TWO           PIC X(10).
            """;

        CopybookModel model = parse(copybook);

        // FILLER should be parsed but marked as filler
        List<FieldNode> allFields = model.getAllFields();
        assertThat(allFields).hasSize(3);
        assertThat(allFields.get(1).isFiller()).isTrue();
    }

    @Test
    void testParseRedefinesDetected() {
        String copybook = """
               01  TEST-RECORD.
                   05  DATE-FIELD          PIC 9(8).
                   05  DATE-PARTS REDEFINES DATE-FIELD.
                       10  DATE-YEAR       PIC 9(4).
                       10  DATE-MONTH      PIC 9(2).
                       10  DATE-DAY        PIC 9(2).
            """;

        CopybookModel model = parse(copybook);

        // Should have warnings about REDEFINES
        assertThat(model.hasWarnings()).isTrue();
        assertThat(model.getRedefines()).isNotEmpty();
    }

    @Test
    void testCalculateTotalByteLength() {
        String copybook = """
               01  TEST-RECORD.
                   05  FIELD-ONE           PIC X(10).
                   05  FIELD-TWO           PIC 9(5).
                   05  FIELD-THREE         PIC S9(7)V99 COMP-3.
            """;

        CopybookModel model = parse(copybook);

        // X(10) = 10 bytes
        // 9(5) DISPLAY = 5 bytes
        // S9(7)V99 COMP-3 = (9+1)/2 = 5 bytes
        int expectedLength = 10 + 5 + 5;
        assertThat(model.calculateTotalByteLength()).isEqualTo(expectedLength);
    }

    @Test
    void testFieldOrdering() {
        String copybook = """
               01  TEST-RECORD.
                   05  FIELD-A             PIC X(5).
                   05  FIELD-B             PIC X(5).
                   05  FIELD-C             PIC X(5).
            """;

        CopybookModel model = parse(copybook);
        List<FieldNode> fields = model.getAllFields();

        assertThat(fields.get(0).getName()).isEqualTo("FIELD-A");
        assertThat(fields.get(1).getName()).isEqualTo("FIELD-B");
        assertThat(fields.get(2).getName()).isEqualTo("FIELD-C");

        // Check offsets
        assertThat(fields.get(0).getStartOffset()).isEqualTo(0);
        assertThat(fields.get(1).getStartOffset()).isEqualTo(5);
        assertThat(fields.get(2).getStartOffset()).isEqualTo(10);
    }

    @Test
    void testParseValueClause() {
        String copybook = """
               01  TEST-RECORD.
                   05  STATUS              PIC X(1) VALUE 'A'.
                   05  COUNT               PIC 9(3) VALUE 0.
            """;

        CopybookModel model = parse(copybook);

        FieldNode field1 = model.getAllFields().get(0);
        assertThat(field1.getValue()).isEqualTo("A");

        FieldNode field2 = model.getAllFields().get(1);
        assertThat(field2.getValue()).isEqualTo("0");
    }

    @Test
    void testRedefinesOffsetCalculation() {
        String copybook = """
               01  TEST-RECORD.
                   05  DATE-FIELD          PIC 9(8).
                   05  DATE-PARTS REDEFINES DATE-FIELD.
                       10  DATE-YEAR       PIC 9(4).
                       10  DATE-MONTH      PIC 9(2).
                       10  DATE-DAY        PIC 9(2).
                   05  NEXT-FIELD          PIC X(10).
            """;

        CopybookModel model = parse(copybook);

        FieldNode dateField = model.findField("DATE-FIELD").orElse(null);
        GroupNode dateParts = model.findGroup("DATE-PARTS").orElse(null);
        FieldNode nextField = model.findField("NEXT-FIELD").orElse(null);

        assertThat(dateField).isNotNull();
        assertThat(dateParts).isNotNull();
        assertThat(nextField).isNotNull();

        // REDEFINES should share the same offset as its target
        assertThat(dateField.getStartOffset()).isEqualTo(0);
        assertThat(dateParts.getStartOffset()).isEqualTo(0);

        // Next field should start after DATE-FIELD (8 bytes)
        assertThat(nextField.getStartOffset()).isEqualTo(8);

        // Total record length should be 8 + 10 = 18 (REDEFINES uses max, not sum)
        assertThat(model.calculateTotalByteLength()).isEqualTo(18);
    }

    @Test
    void testRedefinesWithDifferentSizes() {
        String copybook = """
               01  TEST-RECORD.
                   05  SMALL-FIELD         PIC X(5).
                   05  LARGE-REDEF REDEFINES SMALL-FIELD PIC X(20).
                   05  NEXT-FIELD          PIC X(10).
            """;

        CopybookModel model = parse(copybook);

        FieldNode smallField = model.findField("SMALL-FIELD").orElse(null);
        FieldNode largeRedef = model.findField("LARGE-REDEF").orElse(null);
        FieldNode nextField = model.findField("NEXT-FIELD").orElse(null);

        assertThat(smallField).isNotNull();
        assertThat(largeRedef).isNotNull();
        assertThat(nextField).isNotNull();

        // Both should start at offset 0
        assertThat(smallField.getStartOffset()).isEqualTo(0);
        assertThat(largeRedef.getStartOffset()).isEqualTo(0);

        // Next field should start after the larger REDEFINES (20 bytes)
        assertThat(nextField.getStartOffset()).isEqualTo(20);

        // Total should use max of redefines: 20 + 10 = 30
        assertThat(model.calculateTotalByteLength()).isEqualTo(30);
    }

    @Test
    void testOccursDependingOn() {
        String copybook = """
               01  TEST-RECORD.
                   05  ITEM-COUNT          PIC 9(3) COMP.
                   05  ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
                       10  ITEM-CODE       PIC X(5).
                       10  ITEM-QTY        PIC 9(3).
            """;

        CopybookModel model = parse(copybook);

        FieldNode itemCount = model.findField("ITEM-COUNT").orElse(null);
        GroupNode items = model.findGroup("ITEMS").orElse(null);

        assertThat(itemCount).isNotNull();
        assertThat(items).isNotNull();

        // OCCURS DEPENDING ON should be captured
        assertThat(items.getOccursDepending()).isEqualTo("ITEM-COUNT");
        assertThat(items.getOccursCount()).isEqualTo(100); // Max count

        // Verify offset calculation
        assertThat(itemCount.getStartOffset()).isEqualTo(0);
        assertThat(items.getStartOffset()).isEqualTo(2); // After COMP 9(3) = 2 bytes
    }

    @Test
    void testMultipleRedefinesSiblings() {
        String copybook = """
               01  TEST-RECORD.
                   05  BASE-FIELD          PIC X(10).
                   05  REDEF-1 REDEFINES BASE-FIELD PIC X(5).
                   05  REDEF-2 REDEFINES BASE-FIELD PIC X(15).
                   05  REDEF-3 REDEFINES BASE-FIELD PIC X(8).
                   05  NEXT-FIELD          PIC X(5).
            """;

        CopybookModel model = parse(copybook);

        FieldNode baseField = model.findField("BASE-FIELD").orElse(null);
        FieldNode redef1 = model.findField("REDEF-1").orElse(null);
        FieldNode redef2 = model.findField("REDEF-2").orElse(null);
        FieldNode redef3 = model.findField("REDEF-3").orElse(null);
        FieldNode nextField = model.findField("NEXT-FIELD").orElse(null);

        assertThat(baseField).isNotNull();
        assertThat(redef1).isNotNull();
        assertThat(redef2).isNotNull();
        assertThat(redef3).isNotNull();
        assertThat(nextField).isNotNull();

        // All redefines should share offset 0
        assertThat(baseField.getStartOffset()).isEqualTo(0);
        assertThat(redef1.getStartOffset()).isEqualTo(0);
        assertThat(redef2.getStartOffset()).isEqualTo(0);
        assertThat(redef3.getStartOffset()).isEqualTo(0);

        // Next field should start after the largest redefine (15 bytes)
        assertThat(nextField.getStartOffset()).isEqualTo(15);

        // Total should use max of all redefines: 15 + 5 = 20
        assertThat(model.calculateTotalByteLength()).isEqualTo(20);
    }

    private CopybookModel parse(String source) {
        CopybookTokenizer tokenizer = new CopybookTokenizer(source, "test.cpy");
        List<CopybookToken> tokens = tokenizer.tokenize();
        CopybookParser parser = new CopybookParser(tokens, "test.cpy");
        return parser.parse();
    }
}
