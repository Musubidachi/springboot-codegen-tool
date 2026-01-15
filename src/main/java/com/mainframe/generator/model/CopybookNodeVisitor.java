package com.mainframe.generator.model;

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
