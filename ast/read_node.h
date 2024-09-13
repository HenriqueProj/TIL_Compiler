#ifndef __SIMPLE_AST_READ_NODE_H__
#define __SIMPLE_AST_READ_NODE_H__

#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing read nodes.
   */
  class read_node : public cdk::expression_node {

  public:
    read_node(int lineno) :
        cdk::expression_node(lineno){
    }


    void accept(basic_ast_visitor *sp, int level) { sp->do_read_node(this, level); }

  };

} // til

#endif
