package com.mainframe.generator.codegen.copybook;

import com.mainframe.generator.codegen.model.input.CopyDirectiveNode;
import com.mainframe.generator.codegen.model.input.Enum88Node;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.GroupNode;
import com.mainframe.generator.codegen.model.input.RedefinesNode;

/**
 * Visitor pattern interface for traversing copybook AST.
 */
public interface CopybookNodeVisitor {
    void visit(GroupNode group);
    void visit(FieldNode field);
    void visit(Enum88Node enum88);
    void visit(CopyDirectiveNode copyDirective);
    void visit(RedefinesNode redefines);
}
