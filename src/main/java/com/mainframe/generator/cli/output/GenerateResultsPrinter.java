package com.mainframe.generator.cli.output;

import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.cli.model.GenerateOptions;
import com.mainframe.generator.cli.model.ValidatedGenerateOptions;
import com.mainframe.generator.codegen.GeneratorResult;

/**
 * Responsible for printing CLI output for the "generate" command.
 * Handles banner, success, and failure output formatting.
 */
public class GenerateResultsPrinter {

    private static final Logger log = LoggerFactory.getLogger(GenerateResultsPrinter.class);

    /**
     * Prints the startup banner with configuration details.
     *
     * @param options the CLI options
     * @param validated the validated options
     */
    public void printBanner(GenerateOptions options, ValidatedGenerateOptions validated) {
        log.info("=================================================");
        log.info("Spring Boot Mainframe Code Generator");
        log.info("=================================================");
        log.info("Service Name: {}", options.getServiceName());
        log.info("Request Copybook Dir: {}", options.getRequestCopybookDir().toAbsolutePath());
        log.info("Response Copybook Dir: {}", options.getResponseCopybookDir().toAbsolutePath());
        log.info("Output Directory: {}", validated.getProjectPath().toAbsolutePath());
        log.info("Encoding: {} (locked)", options.getEncoding());
        log.info("Dry Run: {}", options.isDryRun());
        log.info("Verbose: {}", options.isVerbose());
        log.info("=================================================");
    }

    /**
     * Prints success message with generation statistics.
     *
     * @param options the CLI options
     * @param validated the validated options
     * @param result the generation result
     */
    public void printSuccess(GenerateOptions options, ValidatedGenerateOptions validated, GeneratorResult result) {
        Path projectPath = validated.getProjectPath();

        log.info("");
        log.info("=================================================");
        log.info("GENERATION SUCCESSFUL");
        log.info("=================================================");
        log.info("Output Path: {}", projectPath.toAbsolutePath());
        log.info("Copybooks Parsed: {}", result.getCopybooksParsed());
        log.info("DTO Classes Generated: {}", result.getDtoClassesGenerated());
        log.info("Request Byte Length: {} bytes", result.getRequestByteLength());
        log.info("Response Byte Length: {} bytes", result.getResponseByteLength());

        printFolderSelectionSummary(result);
        printValidationSummary(result);
        printNextSteps(options, projectPath);

        log.info("=================================================");
    }

    /**
     * Prints failure message with error details.
     *
     * @param result the generation result containing error information
     */
    public void printFailure(GeneratorResult result) {
        log.error("Generation failed: {}", result.getErrorMessage());
        if (result.getFailingTest() != null) {
            log.error("Failing test: {}", result.getFailingTest());
        }
    }

    private void printFolderSelectionSummary(GeneratorResult result) {
        if (result.getRequestModelsCount() > 0 || result.getResponseModelsCount() > 0) {
            log.info("");
            log.info("Copybook Summary:");
            log.info("  Request Models: {}", result.getRequestModelsCount());
            log.info("  Response Models: {}", result.getResponseModelsCount());
            log.info("  Shared Models: {}", result.getSharedModelsCount());
            log.info("  Serializers Generated: {}", result.getSerializersGenerated());
        }
    }

    private void printValidationSummary(GeneratorResult result) {
        log.info("");
        log.info("Validation Constraints Summary:");
        log.info("  @NotNull: {}", result.getNotNullCount());
        log.info("  @Size: {}", result.getSizeCount());
        log.info("  @Digits: {}", result.getDigitsCount());
        log.info("  @Min/@Max: {}", result.getMinMaxCount());
    }

    private void printNextSteps(GenerateOptions options, Path projectPath) {
        String serviceName = options.getServiceName().toLowerCase().replace("-", "");

        log.info("");
        log.info("=================================================");
        log.info("NEXT STEPS");
        log.info("=================================================");
        log.info("1. Build and test the generated project:");
        log.info("   cd {}", projectPath.toAbsolutePath());
        log.info("   mvn clean test");
        log.info("");
        log.info("2. Run the application:");
        log.info("   mvn spring-boot:run");
        log.info("");
        log.info("3. Test the endpoint:");
        log.info("   curl -X POST http://localhost:8080/mainframe/{} \\", serviceName);
        log.info("        -H \"Content-Type: application/json\" \\");
        log.info("        -d @{}/sample-request.json", projectPath.toAbsolutePath());
        log.info("");
        log.info("Note: Transport layer is NoOp by default. Implement MainframeTransport for actual mainframe communication.");
    }
}
