package com.mainframe.generator.validation;

import com.mainframe.generator.model.*;
import lombok.Builder;
import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Generates Jakarta Bean Validation constraints from copybook metadata.
 */
public class ValidationConstraintGenerator {
    private static final Logger log = LoggerFactory.getLogger(ValidationConstraintGenerator.class);
    
    private int notNullCount = 0;
    private int sizeCount = 0;
    private int digitsCount = 0;
    private int minMaxCount = 0;
    
    /**
     * Generate validation constraints for a field node.
     */
    public List<ValidationConstraint> generateConstraints(FieldNode field, boolean required) {
        List<ValidationConstraint> constraints = new ArrayList<>();
        
        if (field.isFiller()) {
            return constraints; // No validation for FILLER fields
        }
        
        PictureClause pic = field.getPicture();
        if (pic == null) {
            log.warn("Field {} has no PictureClause", field.getName());
            return constraints;
        }
        
        log.debug("Generating constraints for field: {} with PIC: {} (totalLength={})", 
                field.getName(), pic.getRawPicture(), pic.getTotalLength());
        
        String javaType = field.inferJavaType();
        
        // NotNull for required fields
        if (required && !isPrimitive(javaType)) {
            constraints.add(ValidationConstraint.builder()
                    .annotationType("NotNull")
                    .message(field.getName() + " is required")
                    .build());
            notNullCount++;
        }
        
        // String length constraints
        if (pic.isAlphanumeric() || (pic.isNumeric() && "String".equals(javaType))) {
            constraints.add(ValidationConstraint.builder()
                    .annotationType("Size")
                    .attribute("max", pic.getTotalLength())
                    .message(field.getName() + " must not exceed " + pic.getTotalLength() + " characters")
                    .build());
            sizeCount++;
        }
        
        // Numeric constraints
        if (pic.isNumeric() && !pic.isAlphanumeric()) {
            // Digits constraint for numeric fields
            if (javaType.contains("BigDecimal") || javaType.equals("String")) {
                constraints.add(ValidationConstraint.builder()
                        .annotationType("Digits")
                        .attribute("integer", pic.getIntegerDigits())
                        .attribute("fraction", pic.getDecimalDigits())
                        .message(field.getName() + " must have at most " + pic.getIntegerDigits() + 
                                " integer digits and " + pic.getDecimalDigits() + " decimal digits")
                        .build());
                digitsCount++;
            }
            
            // Unsigned constraint
            if (!pic.isSigned()) {
                if (javaType.equals("Integer") || javaType.equals("Long") || 
                    javaType.equals("Short") || javaType.contains("BigDecimal")) {
                    constraints.add(ValidationConstraint.builder()
                            .annotationType("PositiveOrZero")
                            .message(field.getName() + " must be positive or zero")
                            .build());
                    minMaxCount++;
                }
            }
            
            // Min/Max for bounded integer types
            if (javaType.equals("Integer")) {
                long maxValue = (long) Math.pow(10, pic.getIntegerDigits()) - 1;
                if (maxValue <= Integer.MAX_VALUE) {
                    constraints.add(ValidationConstraint.builder()
                            .annotationType("Max")
                            .attribute("value", maxValue)
                            .message(field.getName() + " must not exceed " + maxValue)
                            .build());
                    minMaxCount++;
                }
            } else if (javaType.equals("Short")) {
                long maxValue = Math.min((long) Math.pow(10, pic.getIntegerDigits()) - 1, Short.MAX_VALUE);
                constraints.add(ValidationConstraint.builder()
                        .annotationType("Max")
                        .attribute("value", maxValue)
                        .message(field.getName() + " must not exceed " + maxValue)
                        .build());
                minMaxCount++;
            }
        }
        
        // OCCURS array constraint
        if (field.getOccursCount() > 1) {
            constraints.add(ValidationConstraint.builder()
                    .annotationType("Size")
                    .attribute("max", field.getOccursCount())
                    .message(field.getName() + " must not have more than " + field.getOccursCount() + " elements")
                    .build());
            sizeCount++;
        }
        
        return constraints;
    }
    
    /**
     * Generate constraints for a group (nested object).
     */
    public List<ValidationConstraint> generateGroupConstraints(GroupNode group, boolean required) {
        List<ValidationConstraint> constraints = new ArrayList<>();
        
        if (required) {
            constraints.add(ValidationConstraint.builder()
                    .annotationType("NotNull")
                    .message(group.getName() + " is required")
                    .build());
            notNullCount++;
        }
        
        // Cascade validation to children
        constraints.add(ValidationConstraint.builder()
                .annotationType("Valid")
                .build());
        
        // OCCURS array constraint
        if (group.getOccursCount() > 1) {
            constraints.add(ValidationConstraint.builder()
                    .annotationType("Size")
                    .attribute("max", group.getOccursCount())
                    .message(group.getName() + " must not have more than " + group.getOccursCount() + " elements")
                    .build());
            sizeCount++;
        }
        
        return constraints;
    }
    
    private boolean isPrimitive(String javaType) {
        return javaType.equals("int") || javaType.equals("long") || 
               javaType.equals("short") || javaType.equals("byte") ||
               javaType.equals("double") || javaType.equals("float") ||
               javaType.equals("boolean") || javaType.equals("char");
    }
    
    public int getNotNullCount() { return notNullCount; }
    public int getSizeCount() { return sizeCount; }
    public int getDigitsCount() { return digitsCount; }
    public int getMinMaxCount() { return minMaxCount; }
    
    public void resetCounts() {
        notNullCount = 0;
        sizeCount = 0;
        digitsCount = 0;
        minMaxCount = 0;
    }
    
    /**
     * Represents a validation constraint to be generated.
     */
    @Data
    @Builder
    public static class ValidationConstraint {
        private String annotationType;
        private String message;
        private List<ConstraintAttribute> attributes;
        
        public static class ValidationConstraintBuilder {
            private List<ConstraintAttribute> attributes = new ArrayList<>();
            
            public ValidationConstraintBuilder attribute(String name, Object value) {
                attributes.add(new ConstraintAttribute(name, value));
                return this;
            }
        }
        
        public String toAnnotation() {
            StringBuilder sb = new StringBuilder("@");
            sb.append(annotationType);
            
            List<String> parts = new ArrayList<>();
            
            if (attributes != null && !attributes.isEmpty()) {
                for (ConstraintAttribute attr : attributes) {
                    if (attr.value instanceof String) {
                        parts.add(attr.name + " = \"" + attr.value + "\"");
                    } else {
                        parts.add(attr.name + " = " + attr.value);
                    }
                }
            }
            
            if (message != null && !message.isEmpty()) {
                parts.add("message = \"" + message + "\"");
            }
            
            if (!parts.isEmpty()) {
                sb.append("(").append(String.join(", ", parts)).append(")");
            }
            
            return sb.toString();
        }
    }
    
    @Data
    public static class ConstraintAttribute {
        private final String name;
        private final Object value;
    }
}
