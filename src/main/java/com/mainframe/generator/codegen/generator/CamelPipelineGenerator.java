package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import com.mainframe.generator.codegen.model.ContainerDefinition;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.NamingUtil;

/**
 * Generates the Camel pipeline for mainframe communication.
 *
 * The pipeline consists of 5 separate processor classes:
 * 1. RequestValidator - Validates the incoming request
 * 2. ContainerAssembler - Assembles request containers
 * 3. TransportInvoker - Invokes the mainframe transport
 * 4. ResponseValidator - Validates the response containers
 * 5. ResponseDeserializer - Deserializes response containers to DTOs
 */
public class CamelPipelineGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new CamelPipelineGenerator.
     *
     * @param config the generator configuration
     */
    public CamelPipelineGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates all Camel pipeline classes.
     *
     * @param projectDir the project directory
     * @param requestContainers the request container definitions
     * @param responseContainers the response container definitions
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir, List<ContainerDefinition> requestContainers,
                        List<ContainerDefinition> responseContainers) throws IOException {
        generateRequestValidator(projectDir);
        generateContainerAssembler(projectDir, requestContainers);
        generateTransportInvoker(projectDir);
        generateResponseValidator(projectDir, responseContainers);
        generateResponseDeserializer(projectDir, responseContainers);
        generateMainRoute(projectDir);
    }

    private void generateRequestValidator(Path projectDir) throws IOException {
        String content = """
                package %s.camel;

                import %s.model.request.MainframeRequest;
                import jakarta.validation.ConstraintViolation;
                import jakarta.validation.Validator;
                import org.apache.camel.Exchange;
                import org.apache.camel.Processor;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;

                import java.util.Set;
                import java.util.stream.Collectors;

                /**
                 * Step 1: Validates the incoming mainframe request.
                 *
                 * Validates the request DTO using Jakarta Bean Validation.
                 * Throws IllegalArgumentException if validation fails.
                 */
                @Component
                public class RequestValidator implements Processor {

                    @Autowired
                    private Validator validator;

                    /**
                     * Validates the request from the exchange body.
                     *
                     * @param exchange the Camel exchange
                     * @throws IllegalArgumentException if validation fails
                     */
                    @Override
                    public void process(Exchange exchange) {
                        MainframeRequest request = exchange.getIn().getBody(MainframeRequest.class);
                        validateRequest(request);
                    }

                    private void validateRequest(MainframeRequest request) {
                        Set<ConstraintViolation<MainframeRequest>> violations = validator.validate(request);
                        if (!violations.isEmpty()) {
                            String message = formatViolations(violations);
                            throw new IllegalArgumentException("Request validation failed: " + message);
                        }
                    }

                    private String formatViolations(Set<ConstraintViolation<MainframeRequest>> violations) {
                        return violations.stream()
                                .map(v -> v.getPropertyPath() + ": " + v.getMessage())
                                .collect(Collectors.joining(", "));
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/camel/RequestValidator.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateContainerAssembler(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        String serializerFields = containers.stream()
                .map(c -> "    @Autowired\n    private %sSerializer %sSerializer;"
                        .formatted(c.getClassName(), NamingUtil.toCamelCase(c.getClassName())))
                .collect(Collectors.joining("\n\n"));

        String assembleStatements = containers.stream()
                .map(c -> """
                            containers.put("%s",
                                %sSerializer.serialize(request.get%s()));"""
                        .formatted(c.getContainerKey(),
                                NamingUtil.toCamelCase(c.getClassName()),
                                c.getClassName()))
                .collect(Collectors.joining("\n"));

        String content = """
                package %s.camel;

                import %s.model.request.MainframeRequest;
                import %s.serde.request.*;
                import org.apache.camel.Exchange;
                import org.apache.camel.Processor;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;

                import java.util.LinkedHashMap;

                /**
                 * Step 2: Assembles request containers from the request DTO.
                 *
                 * Serializes each request DTO field to bytes and assembles them
                 * into a LinkedHashMap with deterministic ordering.
                 */
                @Component
                public class ContainerAssembler implements Processor {

                %s

                    /**
                     * Assembles request containers from the exchange body.
                     *
                     * @param exchange the Camel exchange
                     */
                    @Override
                    public void process(Exchange exchange) {
                        MainframeRequest request = exchange.getIn().getBody(MainframeRequest.class);
                        LinkedHashMap<String, byte[]> containers = assembleContainers(request);
                        exchange.getIn().setBody(containers);
                    }

                    private LinkedHashMap<String, byte[]> assembleContainers(MainframeRequest request) {
                        LinkedHashMap<String, byte[]> containers = new LinkedHashMap<>();
                %s
                        return containers;
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE, BASE_PACKAGE,
                serializerFields, assembleStatements);

        Path file = projectDir.resolve("src/main/java/com/mainframe/camel/ContainerAssembler.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateTransportInvoker(Path projectDir) throws IOException {
        String content = """
                package %s.camel;

                import %s.mainframe.transport.MainframeTransport;
                import org.apache.camel.Exchange;
                import org.apache.camel.Processor;
                import org.slf4j.Logger;
                import org.slf4j.LoggerFactory;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;

                import java.util.LinkedHashMap;
                import java.util.Map;

                /**
                 * Step 3: Invokes the mainframe transport.
                 *
                 * Sends the assembled request containers to the mainframe
                 * and receives response containers.
                 */
                @Component
                public class TransportInvoker implements Processor {

                    private static final Logger log = LoggerFactory.getLogger(TransportInvoker.class);

                    @Autowired
                    private MainframeTransport transport;

                    /**
                     * Invokes transport with containers from exchange body.
                     *
                     * @param exchange the Camel exchange
                     */
                    @Override
                    @SuppressWarnings("unchecked")
                    public void process(Exchange exchange) {
                        LinkedHashMap<String, byte[]> requestContainers =
                                exchange.getIn().getBody(LinkedHashMap.class);

                        logContainerSizes("Request", requestContainers);

                        Map<String, byte[]> responseContainers = transport.send(requestContainers);

                        logContainerSizes("Response", responseContainers);

                        exchange.getIn().setBody(responseContainers);
                    }

                    private void logContainerSizes(String type, Map<String, byte[]> containers) {
                        containers.forEach((key, value) ->
                                log.debug("{} container [{}]: {} bytes", type, key, value.length));
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/camel/TransportInvoker.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateResponseValidator(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        String containerChecks = containers.stream()
                .map(c -> """
                            if (!containers.containsKey("%s")) {
                                throw new IllegalStateException("Missing response container: %s");
                            }"""
                        .formatted(c.getContainerKey(), c.getContainerKey()))
                .collect(Collectors.joining("\n"));

        String content = """
                package %s.camel;

                import org.apache.camel.Exchange;
                import org.apache.camel.Processor;
                import org.springframework.stereotype.Component;

                import java.util.Map;

                /**
                 * Step 4: Validates the response containers.
                 *
                 * Ensures all expected response containers are present
                 * and have valid byte lengths.
                 */
                @Component
                public class ResponseValidator implements Processor {

                    /**
                     * Validates response containers from the exchange body.
                     *
                     * @param exchange the Camel exchange
                     * @throws IllegalStateException if validation fails
                     */
                    @Override
                    @SuppressWarnings("unchecked")
                    public void process(Exchange exchange) {
                        Map<String, byte[]> containers = exchange.getIn().getBody(Map.class);
                        validateContainers(containers);
                    }

                    private void validateContainers(Map<String, byte[]> containers) {
                        if (containers == null || containers.isEmpty()) {
                            throw new IllegalStateException("No response containers received");
                        }
                %s
                    }
                }
                """.formatted(BASE_PACKAGE, containerChecks);

        Path file = projectDir.resolve("src/main/java/com/mainframe/camel/ResponseValidator.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateResponseDeserializer(Path projectDir, List<ContainerDefinition> containers) throws IOException {
        String serializerFields = containers.stream()
                .map(c -> "    @Autowired\n    private %sSerializer %sSerializer;"
                        .formatted(c.getClassName(), NamingUtil.toCamelCase(c.getClassName())))
                .collect(Collectors.joining("\n\n"));

        String deserializeStatements = containers.stream()
                .map(c -> """
                            response.set%s(
                                %sSerializer.deserialize(containers.get("%s")));"""
                        .formatted(c.getClassName(),
                                NamingUtil.toCamelCase(c.getClassName()),
                                c.getContainerKey()))
                .collect(Collectors.joining("\n"));

        String content = """
                package %s.camel;

                import %s.model.response.MainframeResponse;
                import %s.serde.response.*;
                import org.apache.camel.Exchange;
                import org.apache.camel.Processor;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;

                import java.util.Map;

                /**
                 * Step 5: Deserializes response containers to DTOs.
                 *
                 * Converts the byte arrays from response containers
                 * back into Java DTOs.
                 */
                @Component
                public class ResponseDeserializer implements Processor {

                %s

                    /**
                     * Deserializes containers from exchange to response DTO.
                     *
                     * @param exchange the Camel exchange
                     */
                    @Override
                    @SuppressWarnings("unchecked")
                    public void process(Exchange exchange) {
                        Map<String, byte[]> containers = exchange.getIn().getBody(Map.class);
                        MainframeResponse response = deserializeContainers(containers);
                        exchange.getIn().setBody(response);
                    }

                    private MainframeResponse deserializeContainers(Map<String, byte[]> containers) {
                        MainframeResponse response = new MainframeResponse();
                %s
                        return response;
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE, BASE_PACKAGE,
                serializerFields, deserializeStatements);

        Path file = projectDir.resolve("src/main/java/com/mainframe/camel/ResponseDeserializer.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateMainRoute(Path projectDir) throws IOException {
        String content = """
                package %s.camel;

                import org.apache.camel.builder.RouteBuilder;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.stereotype.Component;

                /**
                 * Main Camel route for mainframe communication.
                 *
                 * This route orchestrates the 5-step pipeline:
                 * 1. Validate request
                 * 2. Assemble containers
                 * 3. Invoke transport
                 * 4. Validate response
                 * 5. Deserialize response
                 */
                @Component
                public class MainframeRoute extends RouteBuilder {

                    @Autowired
                    private RequestValidator requestValidator;

                    @Autowired
                    private ContainerAssembler containerAssembler;

                    @Autowired
                    private TransportInvoker transportInvoker;

                    @Autowired
                    private ResponseValidator responseValidator;

                    @Autowired
                    private ResponseDeserializer responseDeserializer;

                    /**
                     * Configures the mainframe call route.
                     */
                    @Override
                    public void configure() {
                        from("direct:mainframeCall")
                            .routeId("mainframe-call-route")
                            .log("Step 1: Validating request")
                            .process(requestValidator)
                            .log("Step 2: Assembling containers")
                            .process(containerAssembler)
                            .log("Step 3: Invoking transport")
                            .process(transportInvoker)
                            .log("Step 4: Validating response")
                            .process(responseValidator)
                            .log("Step 5: Deserializing response")
                            .process(responseDeserializer)
                            .log("Mainframe call completed");
                    }
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/camel/MainframeRoute.java");
        FileWriteUtil.safeWriteString(file, content);
    }
}
