package com.mainframe.generator.cli;

import com.mainframe.generator.codegen.ProjectGenerator;
import com.mainframe.generator.codegen.GeneratorConfig;
import com.mainframe.generator.codegen.GeneratorResult;
import com.mainframe.generator.model.FramingMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import java.io.Console;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * CLI command for generating Spring Boot + Camel mainframe integration projects.
 */
@Command(
        name = "generate",
        mixinStandardHelpOptions = true,
        version = "copybook-spring-camel-gen 1.0.0",
        description = "Generates a Spring Boot + Apache Camel project for mainframe TCP communication based on COBOL copybooks."
)
public class GenerateCommand implements Callable<Integer> {

    private static final Logger log = LoggerFactory.getLogger(GenerateCommand.class);

    @Option(names = {"--project-name", "-n"}, description = "Name of the generated project")
    private String projectName;

    @Option(names = {"--copybook-dir", "-c"}, description = "Directory containing COBOL copybook files (for heuristic mode)")
    private Path copybookDir;

    @Option(names = {"--mapping-doc", "-m"}, description = "Path to field mapping document")
    private Path mappingDoc;

    @Option(names = {"--external-copybook-dirs", "-e"}, description = "Additional directories for resolving COPY statements (comma-separated)")
    private String externalCopybookDirs;

    // New folder-based copybook selection options
    @Option(names = {"--request-copybook-dir"}, description = "Directory containing request-related copybooks")
    private Path requestCopybookDir;

    @Option(names = {"--response-copybook-dir"}, description = "Directory containing response-related copybooks")
    private Path responseCopybookDir;

    @Option(names = {"--shared-copybook-dir"}, description = "Directory containing shared copybooks (common to both)")
    private Path sharedCopybookDir;

    @Option(names = {"--request-root"}, description = "Identifies which copybook in request set is the root for {ProgramId}Request")
    private String requestRoot;

    @Option(names = {"--response-root"}, description = "Identifies which copybook in response set is the root for {ProgramId}Response")
    private String responseRoot;

    @Option(names = {"--infer-inheritance"}, description = "Enable inheritance factoring when DTO structure A is a prefix of B")
    private boolean inferInheritance;

    @Option(names = {"--test-mode"}, description = "Generate DTO+serializer+tests for all parsed copybooks (torture suite)")
    private boolean testMode;

    @Option(names = {"--program-id", "-p"}, defaultValue = "MAINFRAME-PROG", description = "Mainframe program ID")
    private String programId;

    @Option(names = {"--encoding"}, defaultValue = "cp037", description = "Character encoding for EBCDIC (default: cp037)")
    private String encoding;

    @Option(names = {"--tcp-host"}, defaultValue = "localhost", description = "TCP host for mainframe connection")
    private String tcpHost;

    @Option(names = {"--tcp-port"}, defaultValue = "5000", description = "TCP port for mainframe connection")
    private int tcpPort;

    @Option(names = {"--tcp-connect-timeout-ms"}, defaultValue = "3000", description = "TCP connection timeout in milliseconds")
    private int tcpConnectTimeout;

    @Option(names = {"--tcp-read-timeout-ms"}, defaultValue = "5000", description = "TCP read timeout in milliseconds")
    private int tcpReadTimeout;

    @Option(names = {"--framing"}, defaultValue = "LENGTH_PREFIX_2", description = "TCP framing mode: LENGTH_PREFIX_2, LENGTH_PREFIX_4, or FIXED")
    private FramingMode framingMode;

    @Option(names = {"--output-dir", "-o"}, description = "Output directory (defaults to current directory)")
    private Path outputDir;

    @Option(names = {"--force", "-f"}, description = "Overwrite existing output directory")
    private boolean force;

    @Option(names = {"--skip-tests"}, description = "Skip running tests after generation")
    private boolean skipTests;

    @Override
    public Integer call() {
        try {
            // Interactive prompts for missing required values
            if (projectName == null || projectName.isBlank()) {
                projectName = promptForInput("Enter project name: ");
                if (projectName == null || projectName.isBlank()) {
                    log.error("Project name is required");
                    return 1;
                }
            }

            // Validate copybook directories
            boolean usingFolderBasedSelection = requestCopybookDir != null || responseCopybookDir != null;

            if (!usingFolderBasedSelection) {
                // Heuristic mode: require copybookDir
                if (copybookDir == null) {
                    log.error("Either --copybook-dir or --request-copybook-dir/--response-copybook-dir must be provided");
                    return 1;
                }
                if (!Files.exists(copybookDir) || !Files.isDirectory(copybookDir)) {
                    log.error("Copybook directory does not exist or is not a directory: {}", copybookDir);
                    return 1;
                }
                log.warn("Using heuristic mode for copybook selection. Consider using --request-copybook-dir and --response-copybook-dir for explicit control.");
            } else {
                // Folder-based mode: validate request/response directories
                if (requestCopybookDir != null && (!Files.exists(requestCopybookDir) || !Files.isDirectory(requestCopybookDir))) {
                    log.error("Request copybook directory does not exist or is not a directory: {}", requestCopybookDir);
                    return 1;
                }
                if (responseCopybookDir != null && (!Files.exists(responseCopybookDir) || !Files.isDirectory(responseCopybookDir))) {
                    log.error("Response copybook directory does not exist or is not a directory: {}", responseCopybookDir);
                    return 1;
                }
                if (sharedCopybookDir != null && (!Files.exists(sharedCopybookDir) || !Files.isDirectory(sharedCopybookDir))) {
                    log.error("Shared copybook directory does not exist or is not a directory: {}", sharedCopybookDir);
                    return 1;
                }
            }

            // Validate mapping doc if provided
            if (mappingDoc != null && !Files.exists(mappingDoc)) {
                log.error("Mapping document does not exist: {}", mappingDoc);
                return 1;
            }

            // Set output directory
            if (outputDir == null) {
                outputDir = Path.of(".").toAbsolutePath().normalize();
            }

            Path projectPath = outputDir.resolve(projectName);

            // Check if output exists
            if (Files.exists(projectPath)) {
                if (!force) {
                    log.error("Output directory already exists: {}. Use --force to overwrite.", projectPath);
                    return 1;
                }
                log.warn("Force mode enabled, will overwrite: {}", projectPath);
            }

            // Parse external copybook directories
            List<Path> externalDirs = parseExternalDirs();

            // Build configuration
            GeneratorConfig config = GeneratorConfig.builder()
                    .projectName(projectName)
                    .copybookDir(copybookDir)
                    .mappingDoc(mappingDoc)
                    .externalCopybookDirs(externalDirs)
                    .programId(programId)
                    .encoding(encoding)
                    .tcpHost(tcpHost)
                    .tcpPort(tcpPort)
                    .tcpConnectTimeout(tcpConnectTimeout)
                    .tcpReadTimeout(tcpReadTimeout)
                    .framingMode(framingMode)
                    .outputDir(outputDir)
                    .force(force)
                    .skipTests(skipTests)
                    .requestCopybookDir(requestCopybookDir)
                    .responseCopybookDir(responseCopybookDir)
                    .sharedCopybookDir(sharedCopybookDir)
                    .requestRoot(requestRoot)
                    .responseRoot(responseRoot)
                    .inferInheritance(inferInheritance)
                    .testMode(testMode)
                    .build();

            log.info("=================================================");
            log.info("Copybook Spring Camel Generator");
            log.info("=================================================");
            log.info("Project Name: {}", projectName);
            log.info("Copybook Directory: {}", copybookDir.toAbsolutePath());
            log.info("Mapping Document: {}", mappingDoc != null ? mappingDoc.toAbsolutePath() : "None");
            log.info("Program ID: {}", programId);
            log.info("Encoding: {}", encoding);
            log.info("TCP Host: {}", tcpHost);
            log.info("TCP Port: {}", tcpPort);
            log.info("Framing Mode: {}", framingMode);
            log.info("Output Directory: {}", projectPath.toAbsolutePath());
            log.info("=================================================");

            // Generate project
            ProjectGenerator generator = new ProjectGenerator(config);
            GeneratorResult result = generator.generate();

            if (!result.isSuccess()) {
                log.error("Generation failed: {}", result.getErrorMessage());
                if (result.getFailingTest() != null) {
                    log.error("Failing test: {}", result.getFailingTest());
                }
                return 1;
            }

            // Print results
            printResults(result, projectPath);

            return 0;

        } catch (Exception e) {
            log.error("Generation failed with exception", e);
            return 1;
        }
    }

    private String promptForInput(String prompt) {
        Console console = System.console();
        if (console != null) {
            return console.readLine(prompt);
        }
        // Fallback for non-interactive environments
        System.out.print(prompt);
        try {
            java.util.Scanner scanner = new java.util.Scanner(System.in);
            return scanner.nextLine();
        } catch (Exception e) {
            return null;
        }
    }

    private List<Path> parseExternalDirs() {
        if (externalCopybookDirs == null || externalCopybookDirs.isBlank()) {
            return List.of();
        }
        return Arrays.stream(externalCopybookDirs.split(","))
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .map(Path::of)
                .toList();
    }

    private void printResults(GeneratorResult result, Path projectPath) {
        log.info("");
        log.info("=================================================");
        log.info("GENERATION SUCCESSFUL");
        log.info("=================================================");
        log.info("Output Path: {}", projectPath.toAbsolutePath());
        log.info("Copybooks Parsed: {}", result.getCopybooksParsed());
        log.info("DTO Classes Generated: {}", result.getDtoClassesGenerated());
        log.info("Request Byte Length: {} bytes", result.getRequestByteLength());
        log.info("Response Byte Length: {} bytes", result.getResponseByteLength());

        // Show new folder-based selection stats if applicable
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
        log.info("  Framing Mode: {}", framingMode);
        if (framingMode == FramingMode.FIXED) {
            log.info("  Response Length: {} bytes", result.getResponseByteLength());
        }
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
        log.info("   curl -X POST http://localhost:8080/api/{}/execute \\", programId.toLowerCase().replace("-", ""));
        log.info("        -H \"Content-Type: application/json\" \\");
        log.info("        -d @{}/sample-request.json", projectPath.toAbsolutePath());
        log.info("");
        log.info("Sample request file: {}/sample-request.json", projectPath.toAbsolutePath());
        log.info("=================================================");
    }
}
