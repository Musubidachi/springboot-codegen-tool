package com.mainframe.generator.codegen.generator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.mainframe.generator.codegen.model.ContainerDefinition;
import com.mainframe.generator.codegen.model.core.context.GeneratorConfig;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.NamingUtil;

/**
 * Generates unit tests for the generated project.
 *
 * Generated tests include:
 * - Serde roundtrip tests
 * - Byte length/layout tests
 * - Camel route tests with mocked transport
 * - Controller tests
 */
public class TestGenerator {

    private static final String BASE_PACKAGE = "com.mainframe";
    private final GeneratorConfig config;

    /**
     * Creates a new TestGenerator.
     *
     * @param config the generator configuration
     */
    public TestGenerator(GeneratorConfig config) {
        this.config = config;
    }

    /**
     * Generates all test classes.
     *
     * @param projectDir the project directory
     * @param requestContainers request container definitions
     * @param responseContainers response container definitions
     * @throws IOException if file writing fails
     */
    public void generate(Path projectDir, List<ContainerDefinition> requestContainers,
                        List<ContainerDefinition> responseContainers) throws IOException {
        for (ContainerDefinition container : requestContainers) {
            generateSerdeTest(projectDir, container, "request");
        }
        for (ContainerDefinition container : responseContainers) {
            generateSerdeTest(projectDir, container, "response");
        }
        generateCamelRouteTest(projectDir);
        generateControllerTest(projectDir);
    }

    private void generateSerdeTest(Path projectDir, ContainerDefinition container, String type) throws IOException {
        String className = container.getClassName();
        String serializerClass = className + "Serializer";
        int byteLength = container.getByteLength();

        String content = """
                package %s.serde.%s;

                import %s.model.%s.%s;
                import org.junit.jupiter.api.Test;
                import static org.assertj.core.api.Assertions.*;

                /**
                 * Tests for %s serialization.
                 *
                 * Container: %s
                 * Expected byte length: %d
                 */
                class %sTest {

                    private final %s serializer = new %s();

                    /**
                     * Tests that serialize produces correct byte length.
                     */
                    @Test
                    void serialize_producesCorrectLength() {
                        %s dto = %s.builder().build();
                        byte[] bytes = serializer.serialize(dto);
                        assertThat(bytes).hasSize(%d);
                    }

                    /**
                     * Tests that serialize handles null input.
                     */
                    @Test
                    void serialize_handlesNull() {
                        byte[] bytes = serializer.serialize(null);
                        assertThat(bytes).hasSize(%d);
                    }

                    /**
                     * Tests roundtrip serialization.
                     */
                    @Test
                    void roundtrip_preservesData() {
                        %s original = %s.builder().build();
                        byte[] bytes = serializer.serialize(original);
                        %s deserialized = serializer.deserialize(bytes);
                        assertThat(deserialized).isNotNull();
                    }

                    /**
                     * Tests deserialization handles short input.
                     */
                    @Test
                    void deserialize_handlesShortInput() {
                        byte[] shortBytes = new byte[10];
                        %s result = serializer.deserialize(shortBytes);
                        assertThat(result).isNotNull();
                    }
                }
                """.formatted(
                BASE_PACKAGE, type,
                BASE_PACKAGE, type, className,
                className,
                container.getContainerKey(),
                byteLength,
                serializerClass,
                serializerClass, serializerClass,
                className, className, byteLength,
                byteLength,
                className, className, className,
                className
        );

        Path file = projectDir.resolve(
                "src/test/java/com/mainframe/serde/" + type + "/" + serializerClass + "Test.java"
        );
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateCamelRouteTest(Path projectDir) throws IOException {
        String content = """
                package %s.camel;

                import %s.mainframe.transport.MainframeTransport;
                import %s.model.request.MainframeRequest;
                import %s.model.response.MainframeResponse;
                import org.apache.camel.CamelContext;
                import org.apache.camel.ProducerTemplate;
                import org.apache.camel.test.spring.junit5.CamelSpringBootTest;
                import org.junit.jupiter.api.Test;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.boot.test.context.SpringBootTest;
                import org.springframework.boot.test.mock.mockito.MockBean;

                import java.util.HashMap;
                import java.util.LinkedHashMap;
                import java.util.Map;

                import static org.assertj.core.api.Assertions.*;
                import static org.mockito.ArgumentMatchers.any;
                import static org.mockito.Mockito.when;

                /**
                 * Tests for the Camel mainframe route with mocked transport.
                 */
                @SpringBootTest
                @CamelSpringBootTest
                class MainframeRouteTest {

                    @Autowired
                    private ProducerTemplate producerTemplate;

                    @MockBean
                    private MainframeTransport mockTransport;

                    /**
                     * Tests successful route execution with mocked transport.
                     */
                    @Test
                    void mainframeRoute_withMockedTransport_succeeds() {
                        // Given
                        Map<String, byte[]> mockResponse = new HashMap<>();
                        when(mockTransport.send(any(LinkedHashMap.class))).thenReturn(mockResponse);

                        MainframeRequest request = MainframeRequest.builder().build();

                        // When/Then - should not throw
                        assertThatCode(() ->
                            producerTemplate.requestBody("direct:mainframeCall", request)
                        ).doesNotThrowAnyException();
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE, BASE_PACKAGE, BASE_PACKAGE);

        Path file = projectDir.resolve("src/test/java/com/mainframe/camel/MainframeRouteTest.java");
        FileWriteUtil.safeWriteString(file, content);
    }

    private void generateControllerTest(Path projectDir) throws IOException {
        String serviceName = config.getServiceName().toLowerCase().replace("-", "");

        String content = """
                package %s.api;

                import %s.model.request.MainframeRequest;
                import %s.mainframe.transport.MainframeTransport;
                import com.fasterxml.jackson.databind.ObjectMapper;
                import org.junit.jupiter.api.Test;
                import org.springframework.beans.factory.annotation.Autowired;
                import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
                import org.springframework.boot.test.context.SpringBootTest;
                import org.springframework.boot.test.mock.mockito.MockBean;
                import org.springframework.http.MediaType;
                import org.springframework.test.web.servlet.MockMvc;

                import java.util.HashMap;
                import java.util.LinkedHashMap;

                import static org.mockito.ArgumentMatchers.any;
                import static org.mockito.Mockito.when;
                import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
                import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

                /**
                 * Tests for MainframeController.
                 */
                @SpringBootTest
                @AutoConfigureMockMvc
                class MainframeControllerTest {

                    @Autowired
                    private MockMvc mockMvc;

                    @Autowired
                    private ObjectMapper objectMapper;

                    @MockBean
                    private MainframeTransport mockTransport;

                    /**
                     * Tests successful endpoint invocation.
                     */
                    @Test
                    void execute_withValidRequest_returns200() throws Exception {
                        when(mockTransport.send(any(LinkedHashMap.class)))
                            .thenReturn(new HashMap<>());

                        MainframeRequest request = MainframeRequest.builder().build();

                        mockMvc.perform(post("/mainframe/%s")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(request)))
                            .andExpect(status().isOk());
                    }

                    /**
                     * Tests NoOp transport returns 501.
                     */
                    @Test
                    void execute_withNoOpTransport_returns501() throws Exception {
                        when(mockTransport.send(any(LinkedHashMap.class)))
                            .thenThrow(new UnsupportedOperationException("Not implemented"));

                        MainframeRequest request = MainframeRequest.builder().build();

                        mockMvc.perform(post("/mainframe/%s")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(request)))
                            .andExpect(status().isNotImplemented());
                    }
                }
                """.formatted(BASE_PACKAGE, BASE_PACKAGE, BASE_PACKAGE, serviceName, serviceName);

        Path file = projectDir.resolve("src/test/java/com/mainframe/api/MainframeControllerTest.java");
        FileWriteUtil.safeWriteString(file, content);
    }
}
