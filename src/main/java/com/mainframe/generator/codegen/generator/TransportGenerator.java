package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;

import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;

/**
 * Generates the mainframe transport layer.
 *
 * The transport layer consists of:
 * - MainframeTransport interface with LinkedHashMap-based container API
 * - NoOpMainframeTransport default implementation that throws UnsupportedOperationException
 *
 * Transport configuration and actual implementation are out of scope for this generator.
 */
public class TransportGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new TransportGenerator.
     *
     * @param config the generator configuration
     */
    public TransportGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates the transport layer classes.
     *
     * @param projectDir the project directory
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir) throws IOException {
        generateTransportInterface(projectDir);
        generateNoOpTransport(projectDir);
    }

    private void generateTransportInterface(Path projectDir) throws IOException {
        String content = """
                package %s.mainframe.transport;

                import java.util.LinkedHashMap;
                import java.util.Map;

                /**
                 * Interface for mainframe communication transport.
                 *
                 * The transport receives request containers as a LinkedHashMap to preserve
                 * deterministic ordering, and returns response containers as a Map.
                 *
                 * Container keys are derived from 01-level COBOL record names and normalized
                 * (e.g., ABC-REQUEST-REC -> ABC_REQUEST_REC).
                 *
                 * Implementation of this interface is the responsibility of the consuming
                 * organization. The generator provides only a NoOp default implementation.
                 */
                public interface MainframeTransport {

                    /**
                     * Sends request containers to the mainframe and receives response containers.
                     *
                     * @param requestContainers ordered map of container key to serialized bytes
                     * @return map of container key to response bytes
                     * @throws UnsupportedOperationException if transport is not implemented
                     */
                    Map<String, byte[]> send(LinkedHashMap<String, byte[]> requestContainers);
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve(
                "src/main/java/com/mainframe/mainframe/transport/MainframeTransport.java"
        );
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateNoOpTransport(Path projectDir) throws IOException {
        String content = """
                package %s.mainframe.transport;

                import java.util.LinkedHashMap;
                import java.util.Map;

                import org.springframework.stereotype.Component;

                /**
                 * Default NoOp implementation of MainframeTransport.
                 *
                 * This implementation always throws UnsupportedOperationException.
                 * Replace this with your organization's actual transport implementation.
                 *
                 * Transport configuration and ownership are explicitly out of scope
                 * for this generated code.
                 */
                @Component
                public class NoOpMainframeTransport implements MainframeTransport {

                    private static final String ERROR_MESSAGE =
                            "MainframeTransport not implemented. " +
                            "Replace NoOpMainframeTransport with your organization's " +
                            "actual transport implementation.";

                    /**
                     * Always throws UnsupportedOperationException.
                     *
                     * @param requestContainers the request containers (not used)
                     * @return never returns
                     * @throws UnsupportedOperationException always
                     */
                    @Override
                    public Map<String, byte[]> send(LinkedHashMap<String, byte[]> requestContainers) {
                        throw new UnsupportedOperationException(ERROR_MESSAGE);
                    }
                }
                """.formatted(BASE_PACKAGE);

        Path file = projectDir.resolve(
                "src/main/java/com/mainframe/mainframe/transport/NoOpMainframeTransport.java"
        );
        FileWriteUtil.safeWriteString(file, content);
    }
}
