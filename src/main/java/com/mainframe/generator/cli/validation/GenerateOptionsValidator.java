package com.mainframe.generator.cli.validation;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import com.mainframe.generator.cli.exception.OptionsValidationException;
import com.mainframe.generator.cli.model.GenerateOptions;
import com.mainframe.generator.cli.model.ValidatedGenerateOptions;

/**
 * Validates GenerateOptions and produces ValidatedGenerateOptions.
 *
 * Validation rules:
 * - serviceName must be non-blank
 * - outputDir must be provided
 * - requestCopybookDir must exist and be a directory
 * - responseCopybookDir must exist and be a directory
 */
public class GenerateOptionsValidator {

    /**
     * Validates the provided options and returns validated/derived values.
     *
     * @param options the raw CLI options
     * @return validated options with normalized paths
     * @throws OptionsValidationException if validation fails
     */
    public ValidatedGenerateOptions validate(GenerateOptions options) {
        List<String> errors = new ArrayList<>();

        validateServiceName(options, errors);
        validateOutputDir(options, errors);
        validateRequestCopybookDir(options, errors);
        validateResponseCopybookDir(options, errors);

        if (!errors.isEmpty()) {
            throw new OptionsValidationException(errors);
        }

        return buildValidatedOptions(options);
    }

    private void validateServiceName(GenerateOptions options, List<String> errors) {
        if (isBlank(options.getServiceName())) {
            errors.add("Service name is required (--serviceName).");
        }
    }

    private void validateOutputDir(GenerateOptions options, List<String> errors) {
        if (options.getOutputDir() == null) {
            errors.add("Output directory is required (--outputDir).");
        }
    }

    private void validateRequestCopybookDir(GenerateOptions options, List<String> errors) {
        if (options.getRequestCopybookDir() == null) {
            errors.add("Request copybook directory is required (--requestCopybookDir).");
        } else if (!existsDirectory(options.getRequestCopybookDir())) {
            errors.add("Request copybook directory does not exist or is not a directory: "
                    + options.getRequestCopybookDir());
        }
    }

    private void validateResponseCopybookDir(GenerateOptions options, List<String> errors) {
        if (options.getResponseCopybookDir() == null) {
            errors.add("Response copybook directory is required (--responseCopybookDir).");
        } else if (!existsDirectory(options.getResponseCopybookDir())) {
            errors.add("Response copybook directory does not exist or is not a directory: "
                    + options.getResponseCopybookDir());
        }
    }

    private ValidatedGenerateOptions buildValidatedOptions(GenerateOptions options) {
        Path normalizedOutputDir = options.getOutputDir()
                .toAbsolutePath()
                .normalize();

        Path projectPath = normalizedOutputDir
                .resolve(options.getServiceName())
                .normalize();

        if (Files.exists(projectPath) && !options.isForce()) {
            throw new OptionsValidationException(List.of(
                    "Output directory already exists: " + projectPath + ". Use --force to overwrite."));
        }

        return new ValidatedGenerateOptions(normalizedOutputDir, projectPath);
    }

    private static boolean existsDirectory(Path path) {
        return path != null && Files.exists(path) && Files.isDirectory(path);
    }

    private static boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }
}
