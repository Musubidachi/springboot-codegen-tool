package com.mainframe.generator.codegen.analysis.model;

import java.util.Set;

import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopybookModel;

public record RequestResponseContainerPlan(
	    Set<CopybookModel> requestContainers,
	    Set<CopybookModel> responseContainers,
	    Set<CopybookModel> sharedContainers,
	    ToolDiagnostics diagnostics
	) {}

