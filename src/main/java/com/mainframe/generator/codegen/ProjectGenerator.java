package com.mainframe.generator.codegen;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.copybook.service.CopybookDependencyResolverService;
import com.mainframe.generator.codegen.copybook.service.CopybookDiscoveryService;
import com.mainframe.generator.codegen.copybook.service.CopybookLoadingService;
import com.mainframe.generator.codegen.copybook.service.CopybookParserService;
import com.mainframe.generator.codegen.generator.ApiControllerGenerator;
import com.mainframe.generator.codegen.generator.CamelPipelineGenerator;
import com.mainframe.generator.codegen.generator.ConfigGenerator;
import com.mainframe.generator.codegen.generator.DocumentationGenerator;
import com.mainframe.generator.codegen.generator.ModelGenerator;
import com.mainframe.generator.codegen.generator.RuntimeGenerator;
import com.mainframe.generator.codegen.generator.SerdeGenerator;
import com.mainframe.generator.codegen.generator.TestGenerator;
import com.mainframe.generator.codegen.generator.TransportGenerator;
import com.mainframe.generator.codegen.model.ContainerDefinition;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.codegen.project.ApplicationYamlGenerator;
import com.mainframe.generator.codegen.project.PomGenerator;
import com.mainframe.generator.codegen.project.ProjectStructureService;
import com.mainframe.generator.codegen.util.ContainerKeyUtil;

/**
 * Main project generator that creates a complete Spring Boot project
 * from COBOL copybooks.
 *
 * This generator follows a clean, modular architecture with separate
 * generator classes for each concern.
 */
public class ProjectGenerator {

    private static final Logger log = LoggerFactory.getLogger(ProjectGenerator.class);

    private final GeneratorConfig config;
    private final ToolDiagnostics diagnostics;

    private List<CopybookModel> requestCopybooks;
    private List<CopybookModel> responseCopybooks;
    private List<ContainerDefinition> requestContainers;
    private List<ContainerDefinition> responseContainers;

    /**
     * Creates a new ProjectGenerator with the given configuration.
     *
     * @param config the generator configuration
     */
    public ProjectGenerator(GeneratorConfig config) {
        this.config = config;
        this.diagnostics = new ToolDiagnostics();
    }

    /**
     * Generates the complete Spring Boot project.
     *
     * @return the generation result
     */
    public GeneratorResult generate() {
        try {
            log.info("Starting project generation for service: {}", config.getServiceName());

            parseCopybooks();
            if (diagnostics.hasErrors()) {
                return GeneratorResult.failure(diagnostics.getErrors().toString());
            }

            buildContainerDefinitions();

            Path projectDir = createProjectStructure();
            generateProjectFiles(projectDir);

            log.info("Project generation complete!");
            return buildSuccessResult(projectDir);

        } catch (Exception e) {
            log.error("Generation failed", e);
            return GeneratorResult.failure(e.getMessage());
        }
    }

    private void parseCopybooks() {
        log.info("Parsing copybooks...");

        CopybookLoadingService loadingService = createLoadingService();

        requestCopybooks = loadingService.loadAll(
                config.getRequestCopybookDir(),
                List.of(),
                diagnostics
        );

        responseCopybooks = loadingService.loadAll(
                config.getResponseCopybookDir(),
                List.of(),
                diagnostics
        );

        logCopybookStats();
    }

    private CopybookLoadingService createLoadingService() {
        return new CopybookLoadingService(
                new CopybookDiscoveryService(),
                new CopybookParserService(),
                new CopybookDependencyResolverService()
        );
    }

    private void logCopybookStats() {
        log.info("  Request copybooks: {}", requestCopybooks.size());
        log.info("  Response copybooks: {}", responseCopybooks.size());
    }

    private void buildContainerDefinitions() {
        log.info("Building container definitions from 01-level record names...");

        requestContainers = buildContainers(requestCopybooks, "request");
        responseContainers = buildContainers(responseCopybooks, "response");

        logContainerStats();
    }

    private List<ContainerDefinition> buildContainers(List<CopybookModel> copybooks, String type) {
        List<ContainerDefinition> containers = new ArrayList<>();

        for (CopybookModel copybook : copybooks) {
            String recordName = copybook.getRootGroup().getOriginalName();
            String containerKey = ContainerKeyUtil.normalizeRecordName(recordName);

            containers.add(ContainerDefinition.builder()
                    .containerKey(containerKey)
                    .recordName(recordName)
                    .copybook(copybook)
                    .type(type)
                    .build());

            log.debug("  {} -> {}", recordName, containerKey);
        }

        return containers;
    }

    private void logContainerStats() {
        log.info("  Request containers: {}", requestContainers.size());
        log.info("  Response containers: {}", responseContainers.size());
    }

    private Path createProjectStructure() throws IOException {
        log.info("Creating project structure...");

        ProjectStructureService structureService = new ProjectStructureService();
        return structureService.createProjectStructure(config);
    }

    private void generateProjectFiles(Path projectDir) throws IOException {
        generatePom(projectDir);
        generateApplicationYaml(projectDir);
        generateModels(projectDir);
        generateSerializers(projectDir);
        generateTransport(projectDir);
        generateCamelPipeline(projectDir);
        generateApiController(projectDir);
        generateRuntime(projectDir);
        generateConfig(projectDir);
        generateTests(projectDir);
        generateDocumentation(projectDir);
        generateMainApplication(projectDir);
    }

    private void generatePom(Path projectDir) throws IOException {
        log.info("Generating pom.xml...");

        PomGenerator pomGenerator = new PomGenerator();
        PomGenerator.PomInfo pomInfo = new PomGenerator.PomInfo(
                "com.mainframe",
                config.getServiceName().toLowerCase(),
                "1.0.0-SNAPSHOT",
                config.getServiceName(),
                "Generated Spring Boot mainframe integration project"
        );
        pomGenerator.generatePom(projectDir, pomInfo);
    }

    private void generateApplicationYaml(Path projectDir) throws IOException {
        log.info("Generating application.yml...");

        ApplicationYamlGenerator ymlGenerator = new ApplicationYamlGenerator();
        ApplicationYamlGenerator.AppYamlInfo ymlInfo = new ApplicationYamlGenerator.AppYamlInfo(
                config.getServiceName(),
                config.getServiceName(),
                "com.mainframe"
        );
        ymlGenerator.generate(projectDir, ymlInfo);
    }

    private void generateModels(Path projectDir) throws IOException {
        log.info("Generating model classes...");

        ModelGenerator modelGenerator = new ModelGenerator(config);
        modelGenerator.generateRequestModels(projectDir, requestContainers);
        modelGenerator.generateResponseModels(projectDir, responseContainers);
    }

    private void generateSerializers(Path projectDir) throws IOException {
        log.info("Generating serializers...");

        SerdeGenerator serdeGenerator = new SerdeGenerator(config);
        serdeGenerator.generateRequestSerializers(projectDir, requestContainers);
        serdeGenerator.generateResponseSerializers(projectDir, responseContainers);
    }

    private void generateTransport(Path projectDir) throws IOException {
        log.info("Generating transport layer...");

        TransportGenerator transportGenerator = new TransportGenerator(config);
        transportGenerator.generate(projectDir);
    }

    private void generateCamelPipeline(Path projectDir) throws IOException {
        log.info("Generating Camel pipeline...");

        CamelPipelineGenerator camelGenerator = new CamelPipelineGenerator(config);
        camelGenerator.generate(projectDir, requestContainers, responseContainers);
    }

    private void generateApiController(Path projectDir) throws IOException {
        log.info("Generating API controller...");

        ApiControllerGenerator controllerGenerator = new ApiControllerGenerator(config);
        controllerGenerator.generate(projectDir, requestContainers, responseContainers);
    }

    private void generateRuntime(Path projectDir) throws IOException {
        log.info("Generating runtime utilities...");

        RuntimeGenerator runtimeGenerator = new RuntimeGenerator(config);
        runtimeGenerator.generate(projectDir);
    }

    private void generateConfig(Path projectDir) throws IOException {
        log.info("Generating configuration classes...");

        ConfigGenerator configGenerator = new ConfigGenerator(config);
        configGenerator.generate(projectDir);
    }

    private void generateTests(Path projectDir) throws IOException {
        log.info("Generating tests...");

        TestGenerator testGenerator = new TestGenerator(config);
        testGenerator.generate(projectDir, requestContainers, responseContainers);
    }

    private void generateDocumentation(Path projectDir) throws IOException {
        log.info("Generating documentation...");

        DocumentationGenerator docGenerator = new DocumentationGenerator(config);
        docGenerator.generate(projectDir, requestContainers, responseContainers);
    }

    private void generateMainApplication(Path projectDir) throws IOException {
        log.info("Generating main application class...");

        RuntimeGenerator runtimeGenerator = new RuntimeGenerator(config);
        runtimeGenerator.generateMainApplication(projectDir);
    }

    private GeneratorResult buildSuccessResult(Path projectDir) {
        int requestBytes = requestContainers.stream()
                .mapToInt(c -> c.getCopybook().getTotalByteLength())
                .sum();

        int responseBytes = responseContainers.stream()
                .mapToInt(c -> c.getCopybook().getTotalByteLength())
                .sum();

        return GeneratorResult.builder()
                .success(true)
                .outputPath(projectDir)
                .copybooksParsed(requestCopybooks.size() + responseCopybooks.size())
                .dtoClassesGenerated(requestContainers.size() + responseContainers.size())
                .requestByteLength(requestBytes)
                .responseByteLength(responseBytes)
                .requestModelsCount(requestContainers.size())
                .responseModelsCount(responseContainers.size())
                .serializersGenerated(requestContainers.size() + responseContainers.size())
                .build();
    }
}
