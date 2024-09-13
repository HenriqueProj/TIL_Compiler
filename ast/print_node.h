#ifndef __SIMPLE_AST_PRINT_NODE_H__
#define __SIMPLE_AST_PRINT_NODE_H__

#include <cdk/ast/sequence_node.h>

/* Trocar todos os suquence_node por expression_node, reverte o doc para aquilo que o DM mandou*/
namespace til {

  /**
   * Class for describing print nodes.
   */
  class print_node : public cdk::basic_node {
    cdk::sequence_node *_arguments;
    bool _append_newline = false;

  public:
    print_node(int lineno, cdk::sequence_node *arguments, bool append_newline) :
        cdk::basic_node(lineno), _arguments(arguments), _append_newline(append_newline) {
    }

    cdk::sequence_node *arguments() { return _arguments; }

    bool append_newline(){ return _append_newline;}

    void accept(basic_ast_visitor *sp, int level) { sp->do_print_node(this, level); }

  };

} // til

#endif
