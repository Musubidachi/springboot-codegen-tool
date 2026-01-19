package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;

import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;

/**
 * Generates Spring configuration classes.
 */
public class ConfigGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new ConfigGenerator.
     *
     * @param config the generator configuration
     */
    public ConfigGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates configuration classes.
     *
     * @param projectDir the project directory
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir) throws IOException {
        generateValidationConfig(projectDir);
        generateCamelConfig(projectDir);
    }

    private void generateValidationConfig(Path projectDir) throws IOException {
        String content = """
                package %s.config;

                import jakarta.validation.Validation;
                import jakarta.validation.Validator;
                import jakarta.validation.ValidatorFactory;
                import org.springframework.context.annotation.Bean;
                import org.springframework.context.annotation.Configuration;

                /**
                 * Configuration for Jakarta Bean Validation.
                 */
                @Configuration
                public class ValidationConfig {

                    /**
                     * Creates the validator bean.
                     *
                     * @return the configured validator
                     */
                    @Bean
                    public Validator validator() {
                        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
                        return factory.getValidator();
                    }
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/config/ValidationConfig.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateCamelConfig(Path projectDir) throws IOException {
        String content = """
                package %s.config;

                import org.apache.camel.CamelContext;
                import org.apache.camel.ProducerTemplate;
                import org.springframework.context.annotation.Bean;
                import org.springframework.context.annotation.Configuration;

                /**
                 * Configuration for Apache Camel.
                 */
                @Configuration
                public class CamelConfig {

                    /**
                     * Creates the producer template bean.
                     *
                     * @param camelContext the Camel context
                     * @return the configured producer template
                     */
                    @Bean
                    public ProducerTemplate producerTemplate(CamelContext camelContext) {
                        return camelContext.createProducerTemplate();
                    }
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/config/CamelConfig.java");
        FileWriteUtil.safeWriteString(file, content);
    }
}
