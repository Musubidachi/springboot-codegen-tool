package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

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
 * Generates model classes (DTOs) for request and response containers.
 *
 * Generated classes use Lombok annotations for boilerplate reduction.
 * Models are placed in:
 * - model.request for request DTOs
 * - model.response for response DTOs
 */
public class ModelGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new ModelGenerator.
     *
     * @param config the generator configuration
     */
    public ModelGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates request model classes.
     *
     * @param projectDir the project directory
     * @param containers the request container definitions
     * @throws IOException if file writing fails
     */
    public void generateRequestModels(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        for (ContainerDefinition container : containers) {
            generateContainerDto(projectDir, container, "request");
        }
        generateMainframeRequest(projectDir, containers);
    }

    /**
     * Generates response model classes.
     *
     * @param projectDir the project directory
     * @param containers the response container definitions
     * @throws IOException if file writing fails
     */
    public void generateResponseModels(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        for (ContainerDefinition container : containers) {
            generateContainerDto(projectDir, container, "response");
        }
        generateMainframeResponse(projectDir, containers);
    }

    private void generateContainerDto(Path projectDir, ContainerDefinition container, String packageType) throws IOException {
        GroupNode root = container.getCopybook().getRootGroup();
        String className = container.getClassName();
        String fields = generateFields(root);
        String byteLength = String.valueOf(container.getByteLength());

        String content = """
                package %s.model.%s;

                import lombok.AllArgsConstructor;
                import lombok.Builder;
                import lombok.Data;
                import lombok.NoArgsConstructor;
                import jakarta.validation.constraints.*;
                import java.math.BigDecimal;

                /**
                 * DTO for container: %s
                 *
                 * Container key: %s
                 * Byte length: %s
                 */
                @Data
                @Builder
                @NoArgsConstructor
                @AllArgsConstructor
                public class %s {

                %s
                }
                """.formatted(
                BASE_PACKAGE, packageType,
                container.getRecordName(),
                container.getContainerKey(),
                byteLength,
                className,
                fields
        );

        Path file = projectDir.resolve(
                "src/main/java/com/mainframe/model/" + packageType + "/" + className + ".java"
        );
        FileWriteUtil.safeWriteString(file, content);
    }

    private String generateFields(GroupNode group) {
        StringBuilder sb = new StringBuilder();
        for (CopybookNode child : group.getChildren()) {
            if (child instanceof FieldNode field) {
                sb.append(generateField(field));
            } else if (child instanceof GroupNode nested) {
                sb.append(generateNestedGroup(nested));
            }
        }
        return sb.toString();
    }

    private String generateField(FieldNode field) {
        String javaType = getJavaType(field);
        String fieldName = NamingUtil.toCamelCase(field.getName());
        String validation = generateValidation(field);

        return """
                    /**
                     * Field: %s
                     * Offset: %d, Length: %d bytes
                     */
                %s    private %s %s;

                """.formatted(
                field.getOriginalName(),
                field.getStartOffset(),
                field.getByteLength(),
                validation,
                javaType,
                fieldName
        );
    }

    private String generateNestedGroup(GroupNode group) {
        String fieldName = NamingUtil.toCamelCase(group.getName());
        String className = NamingUtil.toPascalCase(group.getName());

        return """
                    /**
                     * Nested group: %s
                     */
                    @Valid
                    private %s %s;

                """.formatted(
                group.getOriginalName(),
                className,
                fieldName
        );
    }

    private String generateValidation(FieldNode field) {
        StringBuilder sb = new StringBuilder();
        PictureClause pic = field.getPicture();

        if (pic != null && pic.isAlphanumeric()) {
            sb.append("    @Size(max = %d)\n".formatted(pic.getTotalLength()));
        }

        if (pic != null && pic.isNumeric()) {
            int intDigits = pic.getIntegerDigits();
            int fracDigits = pic.getDecimalDigits();
            sb.append("    @Digits(integer = %d, fraction = %d)\n".formatted(intDigits, fracDigits));
        }

        return sb.toString();
    }

    private String getJavaType(FieldNode field) {
        PictureClause pic = field.getPicture();
        UsageType usage = field.getUsage();

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

    private void generateMainframeRequest(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        String fields = containers.stream()
                .map(c -> """
                        /**
                         * Container: %s
                         */
                        @Valid
                        private %s %s;
                    """.formatted(
                        c.getContainerKey(),
                        c.getClassName(),
                        NamingUtil.toCamelCase(c.getClassName())
                ))
                .collect(Collectors.joining("\n"));

        String content = """
                package %s.model.request;

                import lombok.AllArgsConstructor;
                import lombok.Builder;
                import lombok.Data;
                import lombok.NoArgsConstructor;
                import jakarta.validation.Valid;

                /**
                 * Aggregate request containing all request containers.
                 */
                @Data
                @Builder
                @NoArgsConstructor
                @AllArgsConstructor
                public class MainframeRequest {

                %s
                }
                """.formatted(BASE_PACKAGE, fields);

        Path file = projectDir.resolve("src/main/java/com/mainframe/model/request/MainframeRequest.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateMainframeResponse(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        String fields = containers.stream()
                .map(c -> """
                        /**
                         * Container: %s
                         */
                        private %s %s;
                    """.formatted(
                        c.getContainerKey(),
                        c.getClassName(),
                        NamingUtil.toCamelCase(c.getClassName())
                ))
                .collect(Collectors.joining("\n"));

        String content = """
                package %s.model.response;

                import lombok.AllArgsConstructor;
                import lombok.Builder;
                import lombok.Data;
                import lombok.NoArgsConstructor;

                /**
                 * Aggregate response containing all response containers.
                 */
                @Data
                @Builder
                @NoArgsConstructor
                @AllArgsConstructor
                public class MainframeResponse {

                %s
                }
                """.formatted(BASE_PACKAGE, fields);

        Path file = projectDir.resolve("src/main/java/com/mainframe/model/response/MainframeResponse.java");
        FileWriteUtil.safeWriteString(file, content);
    }
}
