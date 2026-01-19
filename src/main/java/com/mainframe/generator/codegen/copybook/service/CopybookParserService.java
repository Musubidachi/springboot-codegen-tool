package com.mainframe.generator.codegen.copybook.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mainframe.generator.codegen.model.core.context.ToolDiagnostics;
import com.mainframe.generator.codegen.model.input.CopybookModel;
import com.mainframe.generator.parser.CopybookParser;
import com.mainframe.generator.parser.CopybookToken;
import com.mainframe.generator.parser.CopybookTokenizer;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class CopybookParserService {
    private static final Logger log = LoggerFactory.getLogger(CopybookParserService.class);

    public CopybookModel parse(Path path, ToolDiagnostics diagnostics) throws IOException {
        String fileName = path.getFileName().toString();
        String content = Files.readString(path);

        log.info("Parsing copybook: {}", fileName);

        CopybookTokenizer tokenizer = new CopybookTokenizer(content, fileName);
        List<CopybookToken> tokens = tokenizer.tokenize();

        CopybookParser parser = new CopybookParser(tokens, fileName);
        return parser.parse(diagnostics);
    }
}
