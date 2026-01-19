package com.mainframe.generator.cli.exception;

import java.util.List;

/**
 * Single exception that can hold multiple validation errors for better UX.
 */
public class OptionsValidationException extends RuntimeException {
    
	private static final long serialVersionUID = 1L;
	private final List<String> errors;

    public OptionsValidationException(List<String> errors) {
        super(String.join(System.lineSeparator(), errors));
        this.errors = List.copyOf(errors);
    }

    public List<String> getErrors() {
        return errors;
    }
}