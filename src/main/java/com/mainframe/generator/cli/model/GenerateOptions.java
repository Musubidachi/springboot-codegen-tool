package com.mainframe.generator.cli.model;

import java.nio.file.Path;

import com.mainframe.generator.codegen.model.input.FramingMode;

import lombok.Getter;
import picocli.CommandLine.Option;

/**
 * Holds all CLI options for the "generate" command. No validation, no execution
 * logic, no printing.
 */
@Getter
public class GenerateOptions {

	@Option(names = { "--project-name", "-n" }, required = true, description = "Name of the generated project")
	private String projectName;

	@Option(names = { "--copybook-dir",
			"-c" }, description = "Directory containing COBOL copybook files (for heuristic mode)")
	private Path copybookDir;

	@Option(names = { "--mapping-doc", "-m" }, description = "Path to field mapping document")
	private Path mappingDoc;

	@Option(names = { "--external-copybook-dirs",
			"-e" }, description = "Additional directories for resolving COPY statements (comma-separated)")
	private String externalCopybookDirs;

	// Folder-based copybook selection options
	@Option(names = { "--request-copybook-dir" }, description = "Directory containing request-related copybooks")
	private Path requestCopybookDir;

	@Option(names = { "--response-copybook-dir" }, description = "Directory containing response-related copybooks")
	private Path responseCopybookDir;

	@Option(names = { "--shared-copybook-dir" }, description = "Directory containing shared copybooks (common to both)")
	private Path sharedCopybookDir;

	@Option(names = {
			"--request-root" }, description = "Identifies which copybook in request set is the root for {ProgramId}Request")
	private String requestRoot;

	@Option(names = {
			"--response-root" }, description = "Identifies which copybook in response set is the root for {ProgramId}Response")
	private String responseRoot;

	@Option(names = {
			"--infer-inheritance" }, description = "Enable inheritance factoring when DTO structure A is a prefix of B")
	private boolean inferInheritance;

	@Option(names = {
			"--test-mode" }, description = "Generate DTO+serializer+tests for all parsed copybooks (torture suite)")
	private boolean testMode;

	@Option(names = { "--program-id", "-p" }, defaultValue = "MAINFRAME-PROG", description = "Mainframe program ID")
	private String programId;

	@Option(names = {
			"--encoding" }, defaultValue = "cp037", description = "Character encoding for EBCDIC (default: cp037)")
	private String encoding;

	@Option(names = { "--tcp-host" }, defaultValue = "localhost", description = "TCP host for mainframe connection")
	private String tcpHost;

	@Option(names = { "--tcp-port" }, defaultValue = "5000", description = "TCP port for mainframe connection")
	private int tcpPort;

	@Option(names = {
			"--tcp-connect-timeout-ms" }, defaultValue = "3000", description = "TCP connection timeout in milliseconds")
	private int tcpConnectTimeout;

	@Option(names = {
			"--tcp-read-timeout-ms" }, defaultValue = "5000", description = "TCP read timeout in milliseconds")
	private int tcpReadTimeout;

	@Option(names = {
			"--framing" }, defaultValue = "LENGTH_PREFIX_2", description = "TCP framing mode: LENGTH_PREFIX_2, LENGTH_PREFIX_4, or FIXED")
	private FramingMode framingMode;

	@Option(names = { "--output-dir", "-o" }, description = "Output directory (defaults to current directory)")
	private Path outputDir;

	@Option(names = { "--force", "-f" }, description = "Overwrite existing output directory")
	private boolean force;

	@Option(names = { "--skip-tests" }, description = "Skip running tests after generation")
	private boolean skipTests;

	// ---- Getters (no setters needed; picocli sets fields reflectively) ----

}
