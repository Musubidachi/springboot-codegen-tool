package com.mainframe.generator;

import com.mainframe.generator.cli.GenerateCommand;
import picocli.CommandLine;

/**
 * Main entry point for the Copybook Spring Camel Generator.
 * This CLI tool generates Spring Boot projects that integrate with mainframe systems
 * via raw TCP sockets, using Apache Camel for message routing.
 */
public class GeneratorApplication {

    public static void main(String[] args) {
        int exitCode = new CommandLine(new GenerateCommand())
                .setCaseInsensitiveEnumValuesAllowed(true)
                .execute(args);
        System.exit(exitCode);
    }
}
