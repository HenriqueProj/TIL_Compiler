#ifndef __SIMPLE_TARGETS_XML_WRITER_H__
#define __SIMPLE_TARGETS_XML_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include <cdk/ast/basic_node.h>
#include <cdk/types/types.h>

#include "til_parser.tab.h"

namespace til {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class xml_writer: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;

  public:
    xml_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab) :
        basic_ast_visitor(compiler), _symtab(symtab) {
    }

  public:
    ~xml_writer() {
      os().flush();
    }

  private:
    void openTag(const std::string &tag, int lvl) {
      os() << std::string(lvl, ' ') + "< " + tag + " >" << std::endl;
    }
    void openTag(const cdk::basic_node *node, int lvl) {
      openTag(node->label(), lvl);
    }
    template<class... Attributes>
    void openTagWithAttributes(const std::string &tag, int lvl, bool empty, Attributes&&... attributes) {
      os() << std::string(lvl, ' ') + "< " + tag + ":";

      ( (os() << " " << std::get<0>(attributes) << " = \"" << std::get<1>(attributes) << "\" "), ...);

      if (empty)
        os() << "/> " << std::endl;
      else
        os() << "> " << std::endl;
    }
  
    template<class... Attributes>
    void openTagWithAttributes(const cdk::basic_node *node, int lvl, Attributes&&... attrs) {
      openTagWithAttributes(node->label(), lvl, false, attrs...);
    }
    template<class... Attributes>
    void emptyTagWithAttributes(const cdk::basic_node *node, int lvl, Attributes&&... attrs) {
      openTagWithAttributes(node->label(), lvl, true, attrs...);
    }

    void closeTag(const std::string &tag, int lvl) {
      os() << std::string(lvl, ' ') + "</ " + tag + " >" << std::endl;
    }
    void closeTag(const cdk::basic_node *node, int lvl) {
      closeTag(node->label(), lvl);
    }
    
    void emptyTag(const std::string &tag, int lvl) {
      os() << std::string(lvl, ' ') + "< " + tag + " />" << std::endl;
    }
    void emptyTag(const cdk::basic_node *node, int lvl) {
      emptyTag(node->label(), lvl);
    }

    template<typename T>
    void inlineTag(const std::string &tag, int lvl, T &content) {
      os() << std::string(lvl, ' ') << "< " << tag << " > " << content << " </ " << tag << " >" << std::endl;
    }
    template<typename T>
    void inlineTag(const cdk::basic_node *node, int lvl, T &content) {
      inlineTag(node->label(), lvl, content);
    }   

  protected:
    void do_binary_operation(cdk::binary_operation_node *const node, int lvl);
    void do_unary_operation(cdk::unary_operation_node *const node, int lvl);

    template<typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
      inlineTag(node, lvl, node->value());
    }
    
    inline const char *bool_to_str(bool boolean) {
      return boolean ? "true" : "false";
    }
    
    inline std::string type_to_str(std::shared_ptr<cdk::basic_type> type) {
      if (type == nullptr) {
        return "nullptr";
      }

      auto ftype = std::dynamic_pointer_cast<cdk::functional_type>(type);
      if (ftype != nullptr) {
        return "(" + type_to_str(ftype->input()) + ") returns (" + type_to_str(ftype->output()) + ")";
      }

      auto stype = std::dynamic_pointer_cast<cdk::structured_type>(type);
      if (stype != nullptr) {
        auto result = std::string();
        auto components = stype->components();
        return std::accumulate(components.begin(), components.end(), result,
            [this] (auto a, auto b) {
              return a + (a.empty() ? "" : ", ") + type_to_str(b);
            });
      }

      return cdk::to_string(type);
    }

    inline const char *qualifier_to_str(int qualifier) {
      switch (qualifier) {
        case tEXTERNAL: return "external";
        case tFORWARD: return "forward";
        case tPUBLIC: return "public";
        case tPRIVATE: return "private";
        default: return "[unknown qualifier]";
      };
    }


  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end

  };

} // til

#endif
