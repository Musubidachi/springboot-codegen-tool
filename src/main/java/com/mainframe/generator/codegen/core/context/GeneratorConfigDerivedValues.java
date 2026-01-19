package com.mainframe.generator.codegen.core.context;

public class GeneratorConfigDerivedValues {

	
	/**
     * Get the base package name for generated code.
     */
    public String getBasePackage(String projectName) {
        String name = projectName.toLowerCase()
                .replaceAll("[^a-z0-9]", "")
                .replaceAll("^[0-9]+", "");
        return "com." + name;
    }
    
    /**
     * Get the artifact ID for Maven.
     */
    public String getArtifactId(String projectName) {
        return projectName.toLowerCase().replaceAll("[^a-z0-9-]", "-");
    }
    
    /**
     * Get the program ID in a URL-safe format.
     */
    public String getProgramIdPath(String programId) {
        return programId.toLowerCase().replace("-", "");
    }
	
}
