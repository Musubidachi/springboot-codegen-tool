package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.mainframe.generator.codegen.model.ContainerDefinition;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;

/**
 * Generates the API controller for the mainframe service.
 *
 * The generated controller exposes a single endpoint:
 * POST /mainframe/{serviceName}
 */
public class ApiControllerGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new ApiControllerGenerator.
     *
     * @param config the generator configuration
     */
    public ApiControllerGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates the API controller and related classes.
     *
     * @param projectDir the project directory
     * @param requestContainers the request container definitions
     * @param responseContainers the response container definitions
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir, List<ContainerDefinition> requestContainers,
                        List<ContainerDefinition> responseContainers) throws IOException {
        generateController(projectDir);
        generateExceptionHandler(projectDir);
    }

    private void generateController(Path projectDir) throws IOException {
        String serviceName = config.getServiceName().toLowerCase().replace("-", "");

        String content = """
                package %s.api;

                import %s.model.request.MainframeRequest;
                import %s.model.response.MainframeResponse;
                import jakarta.validation.Valid;
                import org.apache.camel.ProducerTemplate;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.http.ResponseEntity;
                import org.springframework.web.bind.annotation.*;

                /**
                 * REST controller for mainframe operations.
                 *
                 * Exposes a single endpoint for mainframe service invocation.
                 * Request validation is handled by Jakarta Bean Validation.
                 */
                @RestController
                @RequestMapping("/mainframe")
                public class MainframeController {

                    @Autowired
                    private ProducerTemplate producerTemplate;

                    /**
                     * Invokes the mainframe service.
                     *
                     * @param request the mainframe request
                     * @return the mainframe response
                     */
                    @PostMapping("/%s")
                    public ResponseEntity<MainframeResponse> execute(
                            @Valid @RequestBody MainframeRequest request) {

                        MainframeResponse response = producerTemplate.requestBody(
                            "direct:mainframeCall",
                            request,
                            MainframeResponse.class
                        );

                        return ResponseEntity.ok(response);
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE, BASE_PACKAGE, serviceName);

        Path file = projectDir.resolve("src/main/java/com/mainframe/api/MainframeController.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateExceptionHandler(Path projectDir) throws IOException {
        String content = """
                package %s.api;

                import org.springframework.http.HttpStatus;
                import org.springframework.http.ResponseEntity;
                import org.springframework.validation.FieldError;
                import org.springframework.web.bind.MethodArgumentNotValidException;
                import org.springframework.web.bind.annotation.ExceptionHandler;
                import org.springframework.web.bind.annotation.RestControllerAdvice;

                import java.util.HashMap;
                import java.util.Map;

                /**
                 * Global exception handler for validation and runtime errors.
                 *
                 * Converts exceptions to appropriate HTTP responses with
                 * structured error information.
                 */
                @RestControllerAdvice
                public class GlobalExceptionHandler {

                    /**
                     * Handles validation exceptions.
                     *
                     * @param ex the validation exception
                     * @return error response with field-level details
                     */
                    @ExceptionHandler(MethodArgumentNotValidException.class)
                    public ResponseEntity<Map<String, Object>> handleValidation(
                            MethodArgumentNotValidException ex) {

                        Map<String, Object> response = new HashMap<>();
                        response.put("status", "error");
                        response.put("message", "Validation failed");

                        Map<String, String> errors = new HashMap<>();
                        for (FieldError error : ex.getBindingResult().getFieldErrors()) {
                            errors.put(error.getField(), error.getDefaultMessage());
                        }
                        response.put("errors", errors);

                        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
                    }

                    /**
                     * Handles illegal argument exceptions.
                     *
                     * @param ex the exception
                     * @return error response
                     */
                    @ExceptionHandler(IllegalArgumentException.class)
                    public ResponseEntity<Map<String, Object>> handleIllegalArgument(
                            IllegalArgumentException ex) {

                        Map<String, Object> response = new HashMap<>();
                        response.put("status", "error");
                        response.put("message", ex.getMessage());

                        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
                    }

                    /**
                     * Handles unsupported operation exceptions (e.g., NoOp transport).
                     *
                     * @param ex the exception
                     * @return error response
                     */
                    @ExceptionHandler(UnsupportedOperationException.class)
                    public ResponseEntity<Map<String, Object>> handleUnsupported(
                            UnsupportedOperationException ex) {

                        Map<String, Object> response = new HashMap<>();
                        response.put("status", "error");
                        response.put("message", ex.getMessage());

                        return ResponseEntity.status(HttpStatus.NOT_IMPLEMENTED).body(response);
                    }

                    /**
                     * Handles all other exceptions.
                     *
                     * @param ex the exception
                     * @return error response
                     */
                    @ExceptionHandler(Exception.class)
                    public ResponseEntity<Map<String, Object>> handleGeneral(Exception ex) {

                        Map<String, Object> response = new HashMap<>();
                        response.put("status", "error");
                        response.put("message", ex.getMessage());

                        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
                    }
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve("src/main/java/com/mainframe/api/GlobalExceptionHandler.java");
        FileWriteUtil.safeWriteString(file, content);
    }
}
