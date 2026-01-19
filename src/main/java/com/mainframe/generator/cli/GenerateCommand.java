package com.mainframe.generator.cli;

import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.cli.exception.OptionsValidationException;
import com.mainframe.generator.cli.model.GenerateOptions;
import com.mainframe.generator.cli.model.ValidatedGenerateOptions;
import com.mainframe.generator.cli.output.GenerateResultsPrinter;
import com.mainframe.generator.cli.validation.GenerateOptionsValidator;
import com.mainframe.generator.codegen.GeneratorResult;
import com.mainframe.generator.codegen.ProjectGenerator;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;

import picocli.CommandLine.Command;
import picocli.CommandLine.Mixin;

/**
 * Main CLI command for generating Spring Boot mainframe integration projects.
 *
 * This command parses COBOL copybooks and generates a complete Spring Boot
 * project with DTOs, serializers, deserializers, Camel routes, and a REST API.
 */
@Command(
        name = "generate",
        mixinStandardHelpOptions = true,
        version = "springboot-codegen-tool 1.0.0",
        description = "Generates a Spring Boot project for mainframe integration based on COBOL copybooks."
)
public class GenerateCommand implements Callable<Integer> {

    private static final Logger log = LoggerFactory.getLogger(GenerateCommand.class);

    @Mixin
    private GenerateOptions options;

    private final GenerateOptionsValidator validator = new GenerateOptionsValidator();
    private final GenerateResultsPrinter printer = new GenerateResultsPrinter();

    /**
     * Executes the generate command.
     *
     * @return 0 on success, 1 on failure
     */
    @Override
    public Integer call() {
        try {
            configureLogging();
            ValidatedGenerateOptions validated = validator.validate(options);

            printer.printBanner(options, validated);

            if (options.isDryRun()) {
                return handleDryRun(validated);
            }

            GeneratorConfig config = buildConfig(validated);
            GeneratorResult result = new ProjectGenerator(config).generate();

            if (!result.isSuccess()) {
                printer.printFailure(result);
                return 1;
            }

            printer.printSuccess(options, validated, result);
            return 0;

        } catch (OptionsValidationException e) {
            log.error("Invalid options:\n{}", e.getMessage());
            return 1;
        } catch (Exception e) {
            log.error("Generation failed with exception", e);
            return 1;
        }
    }

    private void configureLogging() {
        if (options.isVerbose()) {
            // Set root logger to DEBUG level
            ch.qos.logback.classic.Logger root =
                    (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
            root.setLevel(ch.qos.logback.classic.Level.DEBUG);
            log.debug("Verbose logging enabled");
        }
    }

    private int handleDryRun(ValidatedGenerateOptions validated) {
        log.info("");
        log.info("=================================================");
        log.info("DRY RUN - No files will be written");
        log.info("=================================================");
        log.info("Parsing and planning completed successfully.");
        log.info("Project would be generated at: {}", validated.getProjectPath().toAbsolutePath());
        return 0;
    }

    private GeneratorConfig buildConfig(ValidatedGenerateOptions validated) {
        return GeneratorConfig.builder()
                .serviceName(options.getServiceName())
                .requestCopybookDir(options.getRequestCopybookDir())
                .responseCopybookDir(options.getResponseCopybookDir())
                .outputDir(validated.getNormalizedOutputDir())
                .encoding(options.getEncoding())
                .force(options.isForce())
                .dryRun(options.isDryRun())
                .verbose(options.isVerbose())
                .build();
    }
}
