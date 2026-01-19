package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.mainframe.generator.codegen.copybook.util.PictureClause;
import com.mainframe.generator.codegen.model.ContainerDefinition;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.model.input.CopybookNode;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.GroupNode;
import com.mainframe.generator.codegen.model.input.UsageType;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.NamingUtil;

/**
 * Generates serializer/deserializer classes for mainframe containers.
 *
 * Serializers handle:
 * - IBM037 (EBCDIC) encoding for alphanumeric fields
 * - Big Endian numerics for COMP fields
 * - Packed decimal for COMP-3 fields
 * - Explicit padding/truncation
 */
public class SerdeGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new SerdeGenerator.
     *
     * @param config the generator configuration
     */
    public SerdeGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates request serializers.
     *
     * @param projectDir the project directory
     * @param containers the request container definitions
     * @throws IOException if file writing fails
     */
    public void generateRequestSerializers(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        generateEbcdicUtils(projectDir);
        for (ContainerDefinition container : containers) {
            generateSerializer(projectDir, container, "request");
        }
    }

    /**
     * Generates response serializers (deserializers).
     *
     * @param projectDir the project directory
     * @param containers the response container definitions
     * @throws IOException if file writing fails
     */
    public void generateResponseSerializers(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        for (ContainerDefinition container : containers) {
            generateSerializer(projectDir, container, "response");
        }
    }

    private void generateEbcdicUtils(Path projectDir) throws IOException {
        String content = """
                package %s.runtime;

                import java.nio.ByteBuffer;
                import java.nio.charset.Charset;

                /**
                 * Utility class for EBCDIC (IBM037) encoding operations.
                 *
                 * All operations use Big Endian byte order.
                 */
                public class EbcdicUtils {

                    private static final Charset IBM037 = Charset.forName("IBM037");
                    private static final byte EBCDIC_SPACE = 0x40;

                    private EbcdicUtils() {
                        // Utility class
                    }

                    /**
                     * Converts a Java String to EBCDIC bytes with padding/truncation.
                     *
                     * @param value the string value
                     * @param length the target byte length
                     * @return EBCDIC bytes padded or truncated to length
                     */
                    public static byte[] stringToEbcdic(String value, int length) {
                        byte[] result = new byte[length];
                        java.util.Arrays.fill(result, EBCDIC_SPACE);

                        if (value != null && !value.isEmpty()) {
                            byte[] encoded = value.getBytes(IBM037);
                            int copyLen = Math.min(encoded.length, length);
                            System.arraycopy(encoded, 0, result, 0, copyLen);
                        }

                        return result;
                    }

                    /**
                     * Converts EBCDIC bytes to a Java String.
                     *
                     * @param bytes the source bytes
                     * @param offset the start offset
                     * @param length the number of bytes
                     * @return the decoded string, trimmed
                     */
                    public static String ebcdicToString(byte[] bytes, int offset, int length) {
                        return new String(bytes, offset, length, IBM037).trim();
                    }

                    /**
                     * Converts an integer to Big Endian bytes.
                     *
                     * @param value the integer value
                     * @param length the target byte length (2 or 4)
                     * @return Big Endian bytes
                     */
                    public static byte[] intToBytes(int value, int length) {
                        ByteBuffer buffer = ByteBuffer.allocate(length);
                        if (length == 2) {
                            buffer.putShort((short) value);
                        } else {
                            buffer.putInt(value);
                        }
                        return buffer.array();
                    }

                    /**
                     * Converts Big Endian bytes to an integer.
                     *
                     * @param bytes the source bytes
                     * @param offset the start offset
                     * @param length the number of bytes
                     * @return the integer value
                     */
                    public static int bytesToInt(byte[] bytes, int offset, int length) {
                        ByteBuffer buffer = ByteBuffer.wrap(bytes, offset, length);
                        if (length == 2) {
                            return buffer.getShort();
                        } else {
                            return buffer.getInt();
                        }
                    }

                    /**
                     * Converts a BigDecimal to packed decimal (COMP-3) format.
                     *
                     * @param value the decimal value
                     * @param length the target byte length
                     * @param scale the decimal scale
                     * @return packed decimal bytes
                     */
                    public static byte[] toPackedDecimal(java.math.BigDecimal value, int length, int scale) {
                        byte[] result = new byte[length];
                        java.util.Arrays.fill(result, (byte) 0x00);

                        if (value == null) {
                            result[length - 1] = 0x0C; // Positive sign
                            return result;
                        }

                        java.math.BigDecimal scaled = value.movePointRight(scale);
                        String digits = scaled.abs().toBigInteger().toString();

                        int maxDigits = length * 2 - 1;
                        if (digits.length() > maxDigits) {
                            digits = digits.substring(digits.length() - maxDigits);
                        }

                        while (digits.length() < maxDigits) {
                            digits = "0" + digits;
                        }

                        for (int i = 0; i < digits.length(); i++) {
                            int digit = digits.charAt(i) - '0';
                            int byteIndex = i / 2;
                            if (i % 2 == 0) {
                                result[byteIndex] = (byte) (digit << 4);
                            } else {
                                result[byteIndex] |= (byte) digit;
                            }
                        }

                        // Set sign nibble
                        int sign = value.signum() >= 0 ? 0x0C : 0x0D;
                        result[length - 1] |= (byte) sign;

                        return result;
                    }

                    /**
                     * Converts packed decimal (COMP-3) bytes to BigDecimal.
                     *
                     * @param bytes the source bytes
                     * @param offset the start offset
                     * @param length the number of bytes
                     * @param scale the decimal scale
                     * @return the BigDecimal value
                     */
                    public static java.math.BigDecimal fromPackedDecimal(byte[] bytes, int offset, int length, int scale) {
                        StringBuilder digits = new StringBuilder();

                        for (int i = 0; i < length - 1; i++) {
                            int high = (bytes[offset + i] >> 4) & 0x0F;
                            int low = bytes[offset + i] & 0x0F;
                            digits.append(high).append(low);
                        }

                        int lastByte = bytes[offset + length - 1];
                        int lastDigit = (lastByte >> 4) & 0x0F;
                        int sign = lastByte & 0x0F;
                        digits.append(lastDigit);

                        java.math.BigDecimal result = new java.math.BigDecimal(digits.toString());
                        result = result.movePointLeft(scale);

                        if (sign == 0x0D || sign == 0x0B) {
                            result = result.negate();
                        }

                        return result;
                    }

                    /**
                     * Converts a zoned decimal string to bytes.
                     *
                     * @param value the numeric string
                     * @param length the target length
                     * @param signed whether the field is signed
                     * @return zoned decimal bytes
                     */
                    public static byte[] zonedDecimal(String value, int length, boolean signed) {
                        byte[] result = new byte[length];
                        java.util.Arrays.fill(result, (byte) 0xF0); // EBCDIC '0'

                        if (value == null || value.isEmpty()) {
                            return result;
                        }

                        String numStr = value.replaceAll("[^0-9-]", "");
                        boolean negative = numStr.startsWith("-");
                        if (negative) {
                            numStr = numStr.substring(1);
                        }

                        int start = length - numStr.length();
                        for (int i = 0; i < numStr.length() && start + i < length; i++) {
                            result[start + i] = (byte) (0xF0 | (numStr.charAt(i) - '0'));
                        }

                        if (signed && length > 0) {
                            int lastByte = result[length - 1] & 0x0F;
                            result[length - 1] = (byte) ((negative ? 0xD0 : 0xC0) | lastByte);
                        }

                        return result;
                    }

                    /**
                     * Converts zoned decimal bytes to a numeric string.
                     *
                     * @param bytes the source bytes
                     * @param offset the start offset
                     * @param length the number of bytes
                     * @return the numeric string
                     */
                    public static String unzonedDecimal(byte[] bytes, int offset, int length) {
                        StringBuilder sb = new StringBuilder();
                        boolean negative = false;

                        for (int i = 0; i < length; i++) {
                            int b = bytes[offset + i] & 0xFF;
                            int zone = (b >> 4) & 0x0F;
                            int digit = b & 0x0F;

                            if (i == length - 1 && (zone == 0x0D || zone == 0x0B)) {
                                negative = true;
                            }

                            if (digit <= 9) {
                                sb.append(digit);
                            }
                        }

                        String result = sb.toString().replaceFirst("^0+", "");
                        if (result.isEmpty()) {
                            result = "0";
                        }

                        return negative ? "-" + result : result;
                    }
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/runtime/EbcdicUtils.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateSerializer(Path projectDir, ContainerDefinition container, String type) throws IOException {
        String className = container.getClassName() + "Serializer";
        String dtoClass = container.getClassName();
        int byteLength = container.getByteLength();
        GroupNode root = container.getCopybook().getRootGroup();

        String serializeBody = generateSerializeBody(root, "dto");
        String deserializeBody = generateDeserializeBody(root, "builder");

        String content = """
                package %s.serde.%s;

                import %s.model.%s.%s;
                import %s.runtime.EbcdicUtils;
                import org.springframework.stereotype.Component;

                import java.math.BigDecimal;

                /**
                 * Serializer for %s container.
                 *
                 * Container key: %s
                 * Total byte length: %d
                 * Encoding: IBM037 (EBCDIC)
                 * Endianness: Big Endian
                 */
                @Component
                public class %s {

                    private static final int BYTE_LENGTH = %d;

                    /**
                     * Serializes the DTO to byte-accurate mainframe format.
                     *
                     * @param dto the DTO to serialize
                     * @return the serialized bytes
                     */
                    public byte[] serialize(%s dto) {
                        byte[] bytes = new byte[BYTE_LENGTH];

                        if (dto == null) {
                            return bytes;
                        }

                %s
                        return bytes;
                    }

                    /**
                     * Deserializes mainframe bytes to a DTO.
                     *
                     * @param bytes the bytes to deserialize
                     * @return the deserialized DTO
                     */
                    public %s deserialize(byte[] bytes) {
                        if (bytes == null || bytes.length < BYTE_LENGTH) {
                            return %s.builder().build();
                        }

                        %s.%sBuilder builder = %s.builder();

                %s
                        return builder.build();
                    }
                }
                """.formatted(
                BASE_PACKAGE, type,
                BASE_PACKAGE, type, dtoClass,
                BASE_PACKAGE,
                container.getRecordName(),
                container.getContainerKey(),
                byteLength,
                className,
                byteLength,
                dtoClass,
                serializeBody,
                dtoClass,
                dtoClass,
                dtoClass, dtoClass, dtoClass,
                deserializeBody
        );

        Path file = projectDir.resolve(
                "src/main/java/com/mainframe/serde/" + type + "/" + className + ".java"
        );
        FileWriteUtil.safeWriteString(file, content);
    }

    private String generateSerializeBody(GroupNode group, String objRef) {
        StringBuilder sb = new StringBuilder();

        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                sb.append(generateFieldSerialize(field, objRef));
            }
        }

        return sb.toString();
    }

    private String generateFieldSerialize(FieldNode field, String objRef) {
        String fieldName = NamingUtil.toCamelCase(field.getName());
        String getter = objRef + ".get" + NamingUtil.toPascalCase(field.getName()) + "()";
        int offset = field.getStartOffset();
        int length = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();

        if (usage == UsageType.PACKED_DECIMAL) {
            int scale = pic != null ? pic.getDecimalDigits() : 0;
            return """
                            // %s (COMP-3, offset=%d, length=%d)
                            if (%s != null) {
                                byte[] packed = EbcdicUtils.toPackedDecimal(%s, %d, %d);
                                System.arraycopy(packed, 0, bytes, %d, %d);
                            }

                    """.formatted(field.getOriginalName(), offset, length, getter, getter, length, scale, offset, length);
        }

        if (usage == UsageType.BINARY) {
            return """
                            // %s (BINARY, offset=%d, length=%d)
                            if (%s != null) {
                                byte[] binary = EbcdicUtils.intToBytes(%s.intValue(), %d);
                                System.arraycopy(binary, 0, bytes, %d, %d);
                            }

                    """.formatted(field.getOriginalName(), offset, length, getter, getter, length, offset, length);
        }

        if (pic != null && pic.isAlphanumeric()) {
            return """
                            // %s (DISPLAY, offset=%d, length=%d)
                            byte[] %sBytes = EbcdicUtils.stringToEbcdic(%s, %d);
                            System.arraycopy(%sBytes, 0, bytes, %d, %d);

                    """.formatted(field.getOriginalName(), offset, length, fieldName, getter, length, fieldName, offset, length);
        }

        if (pic != null && pic.isNumeric()) {
            boolean signed = pic.isSigned();
            return """
                            // %s (ZONED, offset=%d, length=%d)
                            String %sStr = %s != null ? %s.toString() : "";
                            byte[] %sBytes = EbcdicUtils.zonedDecimal(%sStr, %d, %s);
                            System.arraycopy(%sBytes, 0, bytes, %d, %d);

                    """.formatted(field.getOriginalName(), offset, length,
                    fieldName, getter, getter, fieldName, fieldName, length, signed, fieldName, offset, length);
        }

        return "";
    }

    private String generateDeserializeBody(GroupNode group, String builderRef) {
        StringBuilder sb = new StringBuilder();

        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                sb.append(generateFieldDeserialize(field, builderRef));
            }
        }

        return sb.toString();
    }

    private String generateFieldDeserialize(FieldNode field, String builderRef) {
        String fieldName = NamingUtil.toCamelCase(field.getName());
        int offset = field.getStartOffset();
        int length = field.getByteLength();
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();
        String javaType = getJavaType(field);

        if (usage == UsageType.PACKED_DECIMAL) {
            int scale = pic != null ? pic.getDecimalDigits() : 0;
            return """
                            // %s (COMP-3)
                            %s.%s(EbcdicUtils.fromPackedDecimal(bytes, %d, %d, %d));

                    """.formatted(field.getOriginalName(), builderRef, fieldName, offset, length, scale);
        }

        if (usage == UsageType.BINARY) {
            String conversion = switch (javaType) {
                case "Short" -> "(short) EbcdicUtils.bytesToInt(bytes, %d, %d)".formatted(offset, length);
                case "Long" -> "(long) EbcdicUtils.bytesToInt(bytes, %d, %d)".formatted(offset, length);
                default -> "EbcdicUtils.bytesToInt(bytes, %d, %d)".formatted(offset, length);
            };
            return """
                            // %s (BINARY)
                            %s.%s(%s);

                    """.formatted(field.getOriginalName(), builderRef, fieldName, conversion);
        }

        if (pic != null && pic.isAlphanumeric()) {
            return """
                            // %s (DISPLAY - alphanumeric)
                            %s.%s(EbcdicUtils.ebcdicToString(bytes, %d, %d));

                    """.formatted(field.getOriginalName(), builderRef, fieldName, offset, length);
        }

        if (pic != null && pic.isNumeric()) {
            String conversion = switch (javaType) {
                case "BigDecimal" -> "new BigDecimal(EbcdicUtils.unzonedDecimal(bytes, %d, %d))".formatted(offset, length);
                case "Long" -> "Long.parseLong(EbcdicUtils.unzonedDecimal(bytes, %d, %d))".formatted(offset, length);
                case "Short" -> "Short.parseShort(EbcdicUtils.unzonedDecimal(bytes, %d, %d))".formatted(offset, length);
                default -> "Integer.parseInt(EbcdicUtils.unzonedDecimal(bytes, %d, %d))".formatted(offset, length);
            };
            return """
                            // %s (ZONED)
                            %s.%s(%s);

                    """.formatted(field.getOriginalName(), builderRef, fieldName, conversion);
        }

        return "";
    }

    private String getJavaType(FieldNode field) {
        PictureClause pic = field.getPicture();

        if (pic == null) {
            return "String";
        }

        if (pic.isAlphanumeric()) {
            return "String";
        }

        if (pic.isNumeric()) {
            if (pic.getDecimalDigits() > 0) {
                return "BigDecimal";
            }
            int totalDigits = pic.getIntegerDigits() + pic.getDecimalDigits();
            if (totalDigits <= 4) {
                return "Short";
            } else if (totalDigits <= 9) {
                return "Integer";
            } else if (totalDigits <= 18) {
                return "Long";
            } else {
                return "BigDecimal";
            }
        }

        return "String";
    }
}
