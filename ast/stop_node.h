#ifndef __TIL_AST_STOP_NODE_H__
#define __TIL_AST_STOP_NODE_H__

#include <cdk/ast/basic_node.h>

namespace til {

  /**
   * Class for describing stop nodes.
   */
  class stop_node: public cdk::basic_node {
    int _level;

  public:
    inline stop_node(int lineno, int level) :
        cdk::basic_node(lineno), _level(level) {
    }

    inline int level() { return _level; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stop_node(this, level);
    }
  };

} // til

#endif