package com.mainframe.generator.model;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for PictureClause parsing and type inference.
 */
class PictureClauseTest {

    @Test
    void testParseAlphanumericSimple() {
        PictureClause pic = PictureClause.parse("X(10)");
        
        assertThat(pic.isAlphanumeric()).isTrue();
        assertThat(pic.isNumeric()).isFalse();
        assertThat(pic.getTotalLength()).isEqualTo(10);
    }

    @Test
    void testParseAlphanumericExpanded() {
        PictureClause pic = PictureClause.parse("XXXXX");
        
        assertThat(pic.isAlphanumeric()).isTrue();
        assertThat(pic.getTotalLength()).isEqualTo(5);
    }

    @Test
    void testParseNumericInteger() {
        PictureClause pic = PictureClause.parse("9(5)");
        
        assertThat(pic.isNumeric()).isTrue();
        assertThat(pic.getIntegerDigits()).isEqualTo(5);
        assertThat(pic.getDecimalDigits()).isEqualTo(0);
        assertThat(pic.isSigned()).isFalse();
    }

    @Test
    void testParseNumericSigned() {
        PictureClause pic = PictureClause.parse("S9(7)");
        
        assertThat(pic.isNumeric()).isTrue();
        assertThat(pic.isSigned()).isTrue();
        assertThat(pic.getIntegerDigits()).isEqualTo(7);
    }

    @Test
    void testParseNumericWithDecimal() {
        PictureClause pic = PictureClause.parse("9(5)V99");
        
        assertThat(pic.isNumeric()).isTrue();
        assertThat(pic.getIntegerDigits()).isEqualTo(5);
        assertThat(pic.getDecimalDigits()).isEqualTo(2);
        assertThat(pic.isHasImpliedDecimal()).isTrue();
    }

    @Test
    void testParseSignedDecimal() {
        PictureClause pic = PictureClause.parse("S9(9)V99");
        
        assertThat(pic.isNumeric()).isTrue();
        assertThat(pic.isSigned()).isTrue();
        assertThat(pic.getIntegerDigits()).isEqualTo(9);
        assertThat(pic.getDecimalDigits()).isEqualTo(2);
    }

    @ParameterizedTest
    @CsvSource({
        "X(10), DISPLAY, 10",
        "9(5), DISPLAY, 5",
        "S9(7)V99, DISPLAY, 9",
        // IBM Enterprise COBOL BINARY sizing: 1-4 digits=2, 5-9 digits=4, 10-18 digits=8
        "9(1), BINARY, 2",
        "9(4), BINARY, 2",
        "9(5), BINARY, 4",
        "9(9), BINARY, 4",
        "9(10), BINARY, 8",
        "9(18), BINARY, 8",
        // COMP-3 packed decimal: (digits + 1) / 2
        "9(1), PACKED_DECIMAL, 1",
        "9(3), PACKED_DECIMAL, 2",
        "9(5), PACKED_DECIMAL, 3",
        "S9(9)V99, PACKED_DECIMAL, 6",
        "S9(7)V99, PACKED_DECIMAL, 5",
        "9(15), PACKED_DECIMAL, 8"
    })
    void testByteLengthCalculation(String pic, String usage, int expectedLength) {
        PictureClause clause = PictureClause.parse(pic);
        UsageType usageType = UsageType.valueOf(usage);

        assertThat(clause.getByteLength(usageType)).isEqualTo(expectedLength);
    }

    @Test
    void testJavaTypeInference() {
        // Alphanumeric -> String
        assertThat(PictureClause.parse("X(10)").getJavaType(UsageType.DISPLAY))
                .isEqualTo("String");
        
        // Small integer -> Integer
        assertThat(PictureClause.parse("9(5)").getJavaType(UsageType.DISPLAY))
                .isEqualTo("Integer");
        
        // Large integer -> Long
        assertThat(PictureClause.parse("9(15)").getJavaType(UsageType.DISPLAY))
                .isEqualTo("Long");
        
        // Decimal -> BigDecimal
        assertThat(PictureClause.parse("9(5)V99").getJavaType(UsageType.DISPLAY))
                .isEqualTo("java.math.BigDecimal");
        
        // Binary small -> Short
        assertThat(PictureClause.parse("9(4)").getJavaType(UsageType.BINARY))
                .isEqualTo("Short");
        
        // Binary medium -> Integer
        assertThat(PictureClause.parse("9(9)").getJavaType(UsageType.BINARY))
                .isEqualTo("Integer");
    }

    @Test
    void testExpandedPicture() {
        PictureClause pic = PictureClause.parse("X(3)9(2)");
        
        assertThat(pic.getExpandedPicture()).isEqualTo("XXX99");
    }

    @Test
    void testNullAndEmptyInput() {
        assertThat(PictureClause.parse(null)).isNull();
        assertThat(PictureClause.parse("")).isNull();
        assertThat(PictureClause.parse("   ")).isNull();
    }
}
