package com.mainframe.generator.codegen.copybook.util;

import java.util.Locale;

import lombok.experimental.UtilityClass;

@UtilityClass
public class EnumNamingUtil {
  public static String toJavaEnumConstant(String cobolName) {
    return cobolName == null ? "VALUE" : cobolName.toUpperCase(Locale.ROOT).replace("-", "_");
  }
}

