package com.mainframe.generator.codegen.model.core.context;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;

/**
 * Tool-wide diagnostics (errors/warnings/info) accumulated during a generation run.
 *
 * Pure structure only: no logging, no formatting, no IO.
 */
@Getter
public class ToolDiagnostics {
  private final List<String> errors = new ArrayList<>();
  private final List<String> warnings = new ArrayList<>();
  private final List<String> infos = new ArrayList<>();
  
  public boolean hasErrors() {
	  return this.errors != null && !this.errors.isEmpty();
  }
  
}

