#ifndef __SIMPLE_TARGETS_POSTFIX_WRITER_H__
#define __SIMPLE_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include <sstream>
#include <cdk/emitters/basic_postfix_emitter.h>

#include <optional>
#include <set>
#include <stack>
#include <cdk/types/types.h>
namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;

    bool _forceOutsideFunction = false; //whether to require that future declarations be global
    bool _inFunctionArgs = false;
    std::stack<std::string> _functionLabels; // (history of) the current function's call stack
    std::string _currentFunctionRetLabel; // the destination to jump to upon return
    int _offset; // current frame pointer offset (0 indicates no variables are defined)
    std::optional<std::string> _externalFunctionName; // name of the external function to be called, if applicable
    std::set<std::string> _externalFunctionsToDeclare; // set of external functions to be declared
    
    // (history of) labels of the current visiting function's loops; pairs (condition, end)
    std::vector<std::pair<std::string, std::string>> *_currentFunctionLoopLabels;
    bool _visitedFinalInstruction = false; // whether a final instruction has been visited; no others should follow in this block


  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  protected:
    void prepareIDBinaryExpression(cdk::binary_operation_node * const node, int lvl);
    void prepareIDBinaryComparisonExpression(cdk::binary_operation_node * const node, int lvl);
    void acceptCovariantNode(std::shared_ptr<cdk::basic_type> const node_type, cdk::expression_node * const node, int lvl);
    template<size_t P, typename T> void executeLoopControlInstruction(T * const node);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    inline bool inFunction() {
      return !_forceOutsideFunction && !_functionLabels.empty();
    }

    template<class T>
    inline bool isInstanceOf(cdk::basic_node * const node) {
      return dynamic_cast<T*>(node) != nullptr;
    }
    template<class T, class... Rest, typename std::enable_if<sizeof...(Rest) != 0, int>::type = 0>
    inline bool isInstanceOf(cdk::basic_node * const node) {
      return dynamic_cast<T*>(node) != nullptr || isInstanceOf<Rest...>(node);
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

#define THROW_ERROR_FOR_NODE(subject, msg) { \
  std::cerr << subject->lineno() << ": " << msg << std::endl; \
  return; \
}
#define THROW_ERROR(msg) THROW_ERROR_FOR_NODE(node, msg)

  };

} // til

#endif
