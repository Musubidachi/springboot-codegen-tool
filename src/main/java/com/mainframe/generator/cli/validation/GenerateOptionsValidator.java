package com.mainframe.generator.cli.validation;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.mainframe.generator.cli.exception.OptionsValidationException;
import com.mainframe.generator.cli.model.GenerateOptions;
import com.mainframe.generator.cli.model.ValidatedGenerateOptions;

public class GenerateOptionsValidator {

	public ValidatedGenerateOptions validate(GenerateOptions o) {
		List<String> errors = new ArrayList<>();

		if (isBlank(o.getProjectName())) {
			errors.add("Project name is required (--project-name / -n).");
		}

		boolean usingFolderBasedSelection = o.getRequestCopybookDir() != null || o.getResponseCopybookDir() != null;

		if (!usingFolderBasedSelection) {
			// Heuristic mode requires --copybook-dir
			if (o.getCopybookDir() == null) {
				errors.add("Either --copybook-dir or --request-copybook-dir/--response-copybook-dir must be provided.");
			} else if (!existsDirectory(o.getCopybookDir())) {
				errors.add("Copybook directory does not exist or is not a directory: " + o.getCopybookDir());
			}
		} else {
			// Folder-based mode: validate request/response if provided
			if (o.getRequestCopybookDir() != null && !existsDirectory(o.getRequestCopybookDir())) {
				errors.add("Request copybook directory does not exist or is not a directory: "
						+ o.getRequestCopybookDir());
			}
			if (o.getResponseCopybookDir() != null && !existsDirectory(o.getResponseCopybookDir())) {
				errors.add("Response copybook directory does not exist or is not a directory: "
						+ o.getResponseCopybookDir());
			}
			if (o.getSharedCopybookDir() != null && !existsDirectory(o.getSharedCopybookDir())) {
				errors.add(
						"Shared copybook directory does not exist or is not a directory: " + o.getSharedCopybookDir());
			}

			// Optional but helpful sanity checks
			if (o.getRequestCopybookDir() == null && o.getResponseCopybookDir() == null) {
				errors.add(
						"Folder-based selection requires at least one of --request-copybook-dir or --response-copybook-dir.");
			}
		}

		if (o.getMappingDoc() != null && !Files.exists(o.getMappingDoc())) {
			errors.add("Mapping document does not exist: " + o.getMappingDoc());
		}

		if (o.getTcpPort() <= 0 || o.getTcpPort() > 65535) {
			errors.add("TCP port must be in range 1-65535. Got: " + o.getTcpPort());
		}
		if (o.getTcpConnectTimeout() < 0) {
			errors.add("TCP connect timeout must be >= 0. Got: " + o.getTcpConnectTimeout());
		}
		if (o.getTcpReadTimeout() < 0) {
			errors.add("TCP read timeout must be >= 0. Got: " + o.getTcpReadTimeout());
		}

		// Normalize output dir and compute project path
		Path normalizedOutputDir = (o.getOutputDir() == null ? Path.of(".") : o.getOutputDir()).toAbsolutePath()
				.normalize();

		Path projectPath = normalizedOutputDir.resolve(o.getProjectName() == null ? "" : o.getProjectName())
				.normalize();

		if (Files.exists(projectPath) && !o.isForce()) {
			errors.add("Output directory already exists: " + projectPath + ". Use --force to overwrite.");
		}

		List<Path> externalDirs = parseExternalDirs(o.getExternalCopybookDirs(), errors);

		if (!errors.isEmpty()) {
			throw new OptionsValidationException(errors);
		}

		return new ValidatedGenerateOptions(usingFolderBasedSelection, normalizedOutputDir, projectPath, externalDirs);
	}

	private static boolean existsDirectory(Path p) {
		return p != null && Files.exists(p) && Files.isDirectory(p);
	}

	private static boolean isBlank(String s) {
		return s == null || s.trim().isEmpty();
	}

	private static List<Path> parseExternalDirs(String raw, List<String> errors) {
		if (raw == null || raw.isBlank()) {
			return List.of();
		}

		List<Path> result = Arrays.stream(raw.split(",")).map(String::trim).filter(s -> !s.isEmpty()).map(Path::of)
				.toList();

		// Optional: validate they exist; COPY resolution is nicer when this fails early
		for (Path p : result) {
			if (!existsDirectory(p)) {
				errors.add("External copybook dir does not exist or is not a directory: " + p);
			}
		}

		return result;
	}
}
