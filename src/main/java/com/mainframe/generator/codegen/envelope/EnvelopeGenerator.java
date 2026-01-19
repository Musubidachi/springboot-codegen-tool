package com.mainframe.generator.codegen.envelope;

import com.mainframe.generator.codegen.DtoMetadata;
import com.mainframe.generator.codegen.util.FileWriteUtil;
import com.mainframe.generator.codegen.util.ImportManager;
import com.mainframe.generator.codegen.util.NamingUtil;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Generates parent envelope classes for request/response root DTOs.
 *
 * This implementation does NOT require basePackage or configured packages.
 * It locates the existing generated DTO packages on disk under:
 *   src/main/java/{**}/model/{request|response}
 *
 * Then it generates:
 *   {ProgramId}RequestEnvelope
 *   {ProgramId}ResponseEnvelope
 * into those same packages.
 */
public class EnvelopeGenerator {

    private final String programId;

    public EnvelopeGenerator(String programId) {
        this.programId = programId;
    }

    public void generate(Path projectDir, DtoMetadata requestRootDto, DtoMetadata responseRootDto) throws IOException {
        if (requestRootDto != null) {
            generateOne(projectDir, requestRootDto, EnvelopeType.REQUEST);
        }
        if (responseRootDto != null) {
            generateOne(projectDir, responseRootDto, EnvelopeType.RESPONSE);
        }
    }

    public void generateRequestEnvelope(Path projectDir, DtoMetadata requestRootDto) throws IOException {
        generateOne(projectDir, requestRootDto, EnvelopeType.REQUEST);
    }

    public void generateResponseEnvelope(Path projectDir, DtoMetadata responseRootDto) throws IOException {
        generateOne(projectDir, responseRootDto, EnvelopeType.RESPONSE);
    }

    private void generateOne(Path projectDir, DtoMetadata rootDto, EnvelopeType type) throws IOException {
        if (rootDto == null) {
            return;
        }

        String packageType = (type == EnvelopeType.REQUEST) ? "request" : "response";

        PackageLocation pkg = PackageLocator.locateModelPackage(projectDir, packageType)
                .orElseThrow(() -> new IllegalStateException(
                        "Could not locate Java package for model/" + packageType
                                + ". Generate DTOs first or configure packages explicitly."
                ));

        String className = NamingUtil.toPascalCase(programId)
                + (type == EnvelopeType.REQUEST ? "RequestEnvelope" : "ResponseEnvelope");

        Path outputPath = pkg.dir.resolve(className + ".java");
        String content = buildEnvelopeSource(pkg.javaPackage, className, rootDto, pkg.javaPackage, projectDir);

        FileWriteUtil.safeWriteString(outputPath, content);
    }

    private String buildEnvelopeSource(
            String envelopePackage,
            String envelopeClassName,
            DtoMetadata rootDto,
            String envelopePackageForImports,
            Path projectDir
    ) throws IOException {

        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(envelopePackage).append(";\n\n");

        ImportManager imports = new ImportManager(envelopePackageForImports);
        imports.addImport("lombok.Data");
        imports.addImport("lombok.NoArgsConstructor");
        imports.addImport("lombok.AllArgsConstructor");
        imports.addImport("lombok.Builder");
        imports.addImport("jakarta.validation.Valid");
        imports.addImport("com.fasterxml.jackson.annotation.JsonProperty");

        PayloadRef payload = resolvePayloadFqcn(projectDir, rootDto);
        imports.addImport(payload.fqcn);

        sb.append(imports.generateImports()).append("\n");

        sb.append("/**\n");
        sb.append(" * Parent envelope wrapping the root payload DTO.\n");
        sb.append(" */\n");
        sb.append("@Data\n");
        sb.append("@NoArgsConstructor\n");
        sb.append("@AllArgsConstructor\n");
        sb.append("@Builder\n");
        sb.append("public class ").append(envelopeClassName).append(" {\n\n");
        sb.append("    @Valid\n");
        sb.append("    @JsonProperty(\"payload\")\n");
        sb.append("    private ").append(payload.simpleName).append(" payload;\n");
        sb.append("}\n");

        return sb.toString();
    }

    /**
     * Resolve the payload import by locating the DTO class on disk.
     * Handles dedup by pointing to shared DTO.
     */
    private PayloadRef resolvePayloadFqcn(Path projectDir, DtoMetadata dto) throws IOException {
        String packageType = dto.isDeduped() ? "shared" : dto.getPackageType();
        String className = dto.isDeduped() ? dto.getDedupedToClassName() : dto.getClassName();

        if (className == null || className.isBlank()) {
            throw new IllegalStateException("DTO className is missing for envelope payload");
        }

        PackageLocation pkg = PackageLocator.locateModelPackage(projectDir, packageType)
                .orElseThrow(() -> new IllegalStateException(
                        "Could not locate Java package for model/" + packageType
                                + " needed for payload import. Generate DTOs first."
                ));

        return new PayloadRef(pkg.javaPackage + "." + className, className);
    }

    private enum EnvelopeType { REQUEST, RESPONSE }

    private static final class PayloadRef {
        private final String fqcn;
        private final String simpleName;

        private PayloadRef(String fqcn, String simpleName) {
            this.fqcn = fqcn;
            this.simpleName = simpleName;
        }
    }

    /**
     * Locates existing generated packages under src/main/java.
     */
    static final class PackageLocator {

        private PackageLocator() {}

        static Optional<PackageLocation> locateModelPackage(Path projectDir, String packageType) throws IOException {
            Path javaRoot = projectDir.resolve("src/main/java");
            if (!Files.exists(javaRoot)) {
                return Optional.empty();
            }

            // Find a directory ending with /model/{packageType}
            try (Stream<Path> stream = Files.walk(javaRoot)) {
                return stream
                        .filter(Files::isDirectory)
                        .filter(p -> p.getFileName().toString().equals(packageType))
                        .filter(p -> p.getParent() != null && p.getParent().getFileName().toString().equals("model"))
                        .findFirst()
                        .map(dir -> new PackageLocation(dir, toJavaPackage(javaRoot, dir)));
            }
        }

        private static String toJavaPackage(Path javaRoot, Path packageDir) {
            Path rel = javaRoot.relativize(packageDir);
            String pkg = rel.toString().replace('/', '.').replace('\\', '.');
            return pkg;
        }
    }

    static final class PackageLocation {
        final Path dir;
        final String javaPackage;

        PackageLocation(Path dir, String javaPackage) {
            this.dir = dir;
            this.javaPackage = javaPackage;
        }
    }
}
