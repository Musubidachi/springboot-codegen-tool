package com.mainframe.generator.codegen.copybook.util;

import com.mainframe.generator.codegen.model.input.UsageType;

import lombok.experimental.UtilityClass;

@UtilityClass
public class UsageTypeParser {

    public static UsageType fromCobol(String usage) {
        if (usage == null) {
            return UsageType.DISPLAY;
        }
        String normalized = usage.toUpperCase().trim();
        return switch (normalized) {
            case "COMP", "COMP-4", "BINARY", "COMPUTATIONAL", "COMPUTATIONAL-4" -> UsageType.BINARY;
            case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> UsageType.PACKED_DECIMAL;
            case "COMP-5", "COMPUTATIONAL-5" -> UsageType.COMP_5;
            case "COMP-1", "COMPUTATIONAL-1" -> UsageType.COMP_1;
            case "COMP-2", "COMPUTATIONAL-2" -> UsageType.COMP_2;
            default -> UsageType.DISPLAY;
        };
    }
	
}
