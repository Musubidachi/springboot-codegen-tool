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

@Command(
        name = "generate",
        mixinStandardHelpOptions = true,
        version = "copybook-spring-camel-gen 1.0.0",
        description = "Generates a Spring Boot + Apache Camel project for mainframe TCP communication based on COBOL copybooks."
)
public class GenerateCommand implements Callable<Integer> {

    private static final Logger log = LoggerFactory.getLogger(GenerateCommand.class);

    @Mixin
    private GenerateOptions options;

    private final GenerateOptionsValidator validator = new GenerateOptionsValidator();
    private final GenerateResultsPrinter printer = new GenerateResultsPrinter();

    @Override
    public Integer call() {
        try {
            ValidatedGenerateOptions validated = validator.validate(options);

            printer.printBanner(options, validated);

            GeneratorConfig config = GeneratorConfig.builder()
                    .projectName(options.getProjectName())
                    .copybookDir(options.getCopybookDir())
                    .mappingDoc(options.getMappingDoc())
                    .externalCopybookDirs(validated.getExternalCopybookDirs())
                    .programId(options.getProgramId())
                    .encoding(options.getEncoding())
                    .tcpHost(options.getTcpHost())
                    .tcpPort(options.getTcpPort())
                    .tcpConnectTimeout(options.getTcpConnectTimeout())
                    .tcpReadTimeout(options.getTcpReadTimeout())
                    .framingMode(options.getFramingMode())
                    .outputDir(validated.getNormalizedOutputDir())
                    .force(options.isForce())
                    .skipTests(options.isSkipTests())
                    .requestCopybookDir(options.getRequestCopybookDir())
                    .responseCopybookDir(options.getResponseCopybookDir())
                    .sharedCopybookDir(options.getSharedCopybookDir())
                    .requestRoot(options.getRequestRoot())
                    .responseRoot(options.getResponseRoot())
                    .inferInheritance(options.isInferInheritance())
                    .testMode(options.isTestMode())
                    .build();

            GeneratorResult result = new ProjectGenerator(config).generate();

            if (!result.isSuccess()) {
                printer.printFailure(result);
                return 1;
            }

            printer.printSuccess(options, validated, result);
            return 0;

        } catch (OptionsValidationException e) {
            // Validation errors are "expected"; keep stack traces out of the console.
            log.error("Invalid options:\n{}", e.getMessage());
            return 1;
        } catch (Exception e) {
            log.error("Generation failed with exception", e);
            return 1;
        }
    }
}
