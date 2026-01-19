package com.mainframe.generator.codegen.mapper;

import java.util.List;

import com.mainframe.generator.codegen.copybook.util.CobolNamingUtil;
import com.mainframe.generator.codegen.copybook.util.PictureClause;
import com.mainframe.generator.codegen.model.input.Enum88Node;
import com.mainframe.generator.codegen.model.input.UsageType;

import lombok.experimental.UtilityClass;

@UtilityClass
public class CobolToJavaTypeMapper {

    /**
     * Get the inferred Java type for this field.
     */
    public String inferJavaType(String javaType, PictureClause picture, UsageType usage, List<Enum88Node> enumValues, String name) {
        if (javaType != null) {
            return javaType;
        }
        if (picture == null) {
            // No PIC -> fallback based on usage
            return switch (usage) {
                case COMP_1 -> "Float";
                case COMP_2 -> "Double";
                case PACKED_DECIMAL -> "java.math.BigDecimal";
                case BINARY -> "Integer";
                default -> "String";
            };
        }
        if (hasEnum88Values(enumValues)) {
            return CobolNamingUtil.toJavaClassName(name) + "Enum";
        }
        return picture.getJavaType(usage);
    }
    
    public boolean hasEnum88Values(List<Enum88Node> enum88Values) {
        return enum88Values != null && !enum88Values.isEmpty();
    }
	
}
