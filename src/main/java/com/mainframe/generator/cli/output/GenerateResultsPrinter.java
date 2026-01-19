package com.mainframe.generator.cli.output;

import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.cli.model.GenerateOptions;
import com.mainframe.generator.cli.model.ValidatedGenerateOptions;
import com.mainframe.generator.codegen.GeneratorResult;
import com.mainframe.generator.codegen.model.input.FramingMode;

/**
 * Responsible only for printing CLI output for the "generate" command.
 * No validation, no execution, no prompting.
 */
public class GenerateResultsPrinter {

    private static final Logger log = LoggerFactory.getLogger(GenerateResultsPrinter.class);

    public void printBanner(GenerateOptions o, ValidatedGenerateOptions v) {
        log.info("=================================================");
        log.info("Copybook Spring Camel Generator");
        log.info("=================================================");
        log.info("Project Name: {}", o.getProjectName());

        // In folder-based mode, copybookDir may be null. Avoid NPE and print relevant paths.
        if (o.getCopybookDir() != null) {
            log.info("Copybook Directory: {}", o.getCopybookDir().toAbsolutePath());
        } else {
            log.info("Copybook Directory: {}", "N/A (folder-based selection)");
        }

        log.info("Mapping Document: {}", o.getMappingDoc() != null ? o.getMappingDoc().toAbsolutePath() : "None");
        log.info("Program ID: {}", o.getProgramId());
        log.info("Encoding: {}", o.getEncoding());
        log.info("TCP Host: {}", o.getTcpHost());
        log.info("TCP Port: {}", o.getTcpPort());
        log.info("Framing Mode: {}", o.getFramingMode());
        log.info("Output Directory: {}", v.getProjectPath().toAbsolutePath());

        if (v.isUsingFolderBasedSelection()) {
            log.info("-------------------------------------------------");
            log.info("Folder-based Copybook Selection:");
            log.info("  Request Dir:  {}", o.getRequestCopybookDir() != null ? o.getRequestCopybookDir().toAbsolutePath() : "None");
            log.info("  Response Dir: {}", o.getResponseCopybookDir() != null ? o.getResponseCopybookDir().toAbsolutePath() : "None");
            log.info("  Shared Dir:   {}", o.getSharedCopybookDir() != null ? o.getSharedCopybookDir().toAbsolutePath() : "None");
            log.info("  Request Root: {}", o.getRequestRoot() != null ? o.getRequestRoot() : "None");
            log.info("  Response Root: {}", o.getResponseRoot() != null ? o.getResponseRoot() : "None");
            log.info("  Infer Inheritance: {}", o.isInferInheritance());
            log.info("  Test Mode: {}", o.isTestMode());
        }

        log.info("=================================================");
    }

    public void printSuccess(GenerateOptions o, ValidatedGenerateOptions v, GeneratorResult result) {
        Path projectPath = v.getProjectPath();

        log.info("");
        log.info("=================================================");
        log.info("GENERATION SUCCESSFUL");
        log.info("=================================================");
        log.info("Output Path: {}", projectPath.toAbsolutePath());
        log.info("Copybooks Parsed: {}", result.getCopybooksParsed());
        log.info("DTO Classes Generated: {}", result.getDtoClassesGenerated());
        log.info("Request Byte Length: {} bytes", result.getRequestByteLength());
        log.info("Response Byte Length: {} bytes", result.getResponseByteLength());

        // Folder-based selection stats (only print if it looks applicable)
        if (result.getRequestModelsCount() > 0 || result.getResponseModelsCount() > 0 || result.getSharedModelsCount() > 0) {
            log.info("");
            log.info("Folder-based Selection Summary:");
            log.info("  Request Models: {}", result.getRequestModelsCount());
            log.info("  Response Models: {}", result.getResponseModelsCount());
            log.info("  Shared Models: {}", result.getSharedModelsCount());
            log.info("  Serializers Generated: {}", result.getSerializersGenerated());
            if (result.getTortureTestsGenerated() > 0) {
                log.info("  Torture Tests Generated: {}", result.getTortureTestsGenerated());
            }
        }

        log.info("");
        log.info("Validation Constraints Summary:");
        log.info("  @NotNull: {}", result.getNotNullCount());
        log.info("  @Size: {}", result.getSizeCount());
        log.info("  @Digits: {}", result.getDigitsCount());
        log.info("  @Min/@Max: {}", result.getMinMaxCount());

        log.info("");
        log.info("TCP Configuration:");
        log.info("  Framing Mode: {}", o.getFramingMode());
        if (o.getFramingMode() == FramingMode.FIXED) {
            log.info("  Response Length: {} bytes", result.getResponseByteLength());
        }

        log.info("");
        printNextSteps(o, projectPath);

        log.info("=================================================");
    }

    public void printFailure(GeneratorResult result) {
        log.error("Generation failed: {}", result.getErrorMessage());
        if (result.getFailingTest() != null) {
            log.error("Failing test: {}", result.getFailingTest());
        }
    }

    private void printNextSteps(GenerateOptions o, Path projectPath) {
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
        log.info("   curl -X POST http://localhost:8080/api/{}/execute \\",
                sanitizeProgramIdForEndpoint(o.getProgramId()));
        log.info("        -H \"Content-Type: application/json\" \\");
        log.info("        -d @{}/sample-request.json", projectPath.toAbsolutePath());
        log.info("");
        log.info("Sample request file: {}/sample-request.json", projectPath.toAbsolutePath());
    }

    /**
     * Matches your current behavior: lower-case and remove dashes.
     * Keeping it local avoids leaking formatting rules into other layers.
     */
    private String sanitizeProgramIdForEndpoint(String programId) {
        if (programId == null) return "program";
        return programId.toLowerCase().replace("-", "");
    }
}

