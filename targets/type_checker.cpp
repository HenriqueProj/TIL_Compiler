#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>
#include "til_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

bool til::type_checker::deepTypeComparison(std::shared_ptr<cdk::basic_type> left,
      std::shared_ptr<cdk::basic_type> right, bool lax) {
  
  // Type_unspec in at least one of the types
  if (left->name() == cdk::TYPE_UNSPEC || right->name() == cdk::TYPE_UNSPEC) {
    return false;
  
  } else if (left->name() == cdk::TYPE_FUNCTIONAL) {
    // Only one functional type
    if (right->name() != cdk::TYPE_FUNCTIONAL) {
      return false;
    }

    auto left_func = cdk::functional_type::cast(left);
    auto right_func = cdk::functional_type::cast(right);

    // Different lengths of input or output
    if (left_func->input_length() != right_func->input_length()
          || left_func->output_length() != right_func->output_length()) {
      return false;
    }

    // Checks inputs and outputs recursively
    for (size_t i = 0; i < left_func->input_length(); i++) {
      if (!deepTypeComparison(right_func->input(i), left_func->input(i), lax)) {
        return false;
      }
    }
    for (size_t i = 0; i < left_func->output_length(); i++) {
      if (!deepTypeComparison(left_func->output(i), right_func->output(i), lax)) {
        return false;
      }
    }

    return true;
  // Only one functional type
  } else if (right->name() == cdk::TYPE_FUNCTIONAL) {
    return false;
  // Only one pointer type
  } else if (left->name() == cdk::TYPE_POINTER) {
    if (right->name() != cdk::TYPE_POINTER) {
      return false;
    }

    return deepTypeComparison(cdk::reference_type::cast(left)->referenced(),
        cdk::reference_type::cast(right)->referenced(), false);

  // Only one pointer type
  } else if (right->name() == cdk::TYPE_POINTER) {
      return false;
  
  // if lax is true, it can convert the right side from int to double
  } else if (lax && left->name() == cdk::TYPE_DOUBLE) {
    return right->name() == cdk::TYPE_DOUBLE || right->name() == cdk::TYPE_INT;
  // lax == false -> has to be of the same type
  } else {
    return left == right;
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  // Iterates through all nodes
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}
//---------------------------------------------------------------------------


void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}


void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

// Processes unary expressions
void til::type_checker::processUnary(cdk::unary_operation_node *const node, int lvl, bool acceptDoubles) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);

  // Unspec -> creates int
  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 

  else if (!node->argument()->is_typed(cdk::TYPE_INT)
        && !(acceptDoubles && node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("wrong type in argument of unary expression");
  }

  node->type(node->argument()->type());
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  processUnary(node, lvl, true);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  processUnary(node, lvl, true);
}

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  processUnary(node, lvl, false);
}

//---------------------------------------------------------------------------

// Processes binary arithmetic expressions
void til::type_checker::processBinaryArithmetic(cdk::binary_operation_node *const node, int lvl 
  ,bool acceptDoubles, bool acceptOnePointer, bool acceptBothPointers) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);

  // left node int or unspec
  if (node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->type(node->right()->type());
    } 
    // right node unspec
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (acceptOnePointer && node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->right()->type());

      if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
    } 
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }

    if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(node->type());
    }

  } 
  else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } 
  else if (acceptOnePointer && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
    } 
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(node->left()->type());
    } 
    else if (acceptBothPointers && deepTypeComparison(node->left()->type(), node->right()->type(), false)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }

  } 
  else {
    throw std::string("wrong type in left argument of arithmetic binary expression");
  }
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryArithmetic(node, lvl, true, true, false);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryArithmetic(node, lvl, true, true, true);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryArithmetic(node, lvl, true, false, false);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryArithmetic(node, lvl, true, false, false);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryArithmetic(node, lvl, false, false, false);
}

// Processes binary predicate expressions
void til::type_checker::processBinaryPredicate(cdk::binary_operation_node *const node, int lvl, bool acceptDoubles, bool acceptPointers) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    } 
    else if (!node->right()->is_typed(cdk::TYPE_INT)
        && !(acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))
        && !(acceptPointers && node->right()->is_typed(cdk::TYPE_POINTER))) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } 
  else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    } 
    else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } 
  else if (acceptPointers && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER)) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } 
  else if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->left()->type(node->right()->type());
    } 
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } 
  else {
    throw std::string("wrong type in left argument of arithmetic binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, true, false);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, true, false);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, true, false);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, true, false);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, true, false);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, true, false);
}

void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, false, false);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryPredicate(node, lvl, false, false);
}
//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  auto symbol = _symtab.find(node->name());

  if (symbol == nullptr) {
    throw std::string("undeclared variable: '" + node->name() + "'");
  }

  node->type(symbol->type());
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->rvalue()->accept(this, lvl);

  if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
    node->rvalue()->type(node->lvalue()->type());
  } 
  else if (node->rvalue()->is_typed(cdk::TYPE_POINTER) && node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto lref = cdk::reference_type::cast(node->lvalue()->type());
    auto rref = cdk::reference_type::cast(node->rvalue()->type());

  if (rref->referenced()->name() == cdk::TYPE_UNSPEC
        || rref->referenced()->name() == cdk::TYPE_VOID
        || lref->referenced()->name() == cdk::TYPE_VOID) {
    node->rvalue()->type(node->lvalue()->type());
    }
  }

  if (!deepTypeComparison(node->lvalue()->type(), node->rvalue()->type(), true)) {
    throw std::string("Wrong type in right argument of assignment");
  }

  node->type(node->lvalue()->type());
}

//---------------------------------------------------------------------------

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 
  else if (node->argument()->is_typed(cdk::TYPE_POINTER)) {
    auto ref = cdk::reference_type::cast(node->argument()->type());

    if (ref != nullptr && ref->referenced()->name() == cdk::TYPE_UNSPEC) {
      node->argument()->type(cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }
  }
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));

    child->accept(this, lvl);

    if (child->is_typed(cdk::TYPE_UNSPEC)) {
      child->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (!child->is_typed(cdk::TYPE_INT) && !child->is_typed(cdk::TYPE_DOUBLE)
          && !child->is_typed(cdk::TYPE_STRING)) {
      throw std::string("Wrong type for argument " + std::to_string(i + 1) + " of print instruction");
    }
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Wrong type in condition of 'loop'.");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Wrong type in condition of 'if'.");
  }
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Wrong type in condition of 'if_else'.");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_address_node(til::address_node * const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);
  if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto ref = cdk::reference_type::cast(node->lvalue()->type());

    if (ref->referenced()->name() == cdk::TYPE_VOID) {
      // [[void]] = [void]
      node->type(node->lvalue()->type());
      return;
    }
  }
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

//---------------------------------------------------------------------------

void til::type_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 
  else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Wrong type in argument of unary expression.");
  }

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_block_node(til::block_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node * const node, int lvl) {
  if (node->type() == nullptr) { 
    node->initializer()->accept(this, lvl + 2);

    if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
      node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (node->initializer()->is_typed(cdk::TYPE_POINTER)) {
      auto ref = cdk::reference_type::cast(node->initializer()->type());
      
      if (ref->referenced()->name() == cdk::TYPE_UNSPEC) {
        node->initializer()->type(cdk::reference_type::create(4,
            cdk::primitive_type::create(4, cdk::TYPE_INT)));
      }
    } 
    else if (node->initializer()->is_typed(cdk::TYPE_VOID)) {
      throw std::string("Cannot declare variable of type void.");
    }

    node->type(node->initializer()->type());
  } 
  // Node has a type set already
  else {
    
    if (node->initializer() != nullptr) {
      node->initializer()->accept(this, lvl + 2);

      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
          node->initializer()->type(node->type());
        } 
        else {
          // if node->type() is not an int, error will be thrown next
          node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }

      } 
      else if (node->initializer()->is_typed(cdk::TYPE_POINTER) && node->is_typed(cdk::TYPE_POINTER)) {
        auto noderef = cdk::reference_type::cast(node->type());
        auto initref = cdk::reference_type::cast(node->initializer()->type());
        
        if (initref->referenced()->name() == cdk::TYPE_UNSPEC
              || initref->referenced()->name() == cdk::TYPE_VOID
              || noderef->referenced()->name() == cdk::TYPE_VOID) {
          node->initializer()->type(node->type());
        }
      }

      if (!deepTypeComparison(node->type(), node->initializer()->type(), true)) {
        throw std::string("Wrong type in initializer for variable '" + node->identifier() + "'.");
      }
    }
  }

  if (node->qualifier() == tEXTERNAL && !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    throw std::string("Foreign declaration of non-function '" + node->identifier() + "'.");
  }

  auto symbol = make_symbol(node->identifier(), node->type(), node->qualifier());

  if (_symtab.insert(node->identifier(), symbol)) {
    _parent->set_new_symbol(symbol);
    return;
  }

  auto prev = _symtab.find(node->identifier());

  if (prev != nullptr && prev->qualifier() == tFORWARD) {
    if (deepTypeComparison(prev->type(), symbol->type(), false)) {
      _symtab.replace(node->identifier(), symbol);
      _parent->set_new_symbol(symbol);
      return;
    }
  }

  throw std::string("redeclaration of variable '" + node->identifier() + "'");
}
//---------------------------------------------------------------------------

void til::type_checker::do_function_call_node(til::function_call_node * const node, int lvl) {
  ASSERT_UNSPEC;
  
  std::shared_ptr<cdk::functional_type> functype;

  // recursive call
  if (node->function() == nullptr) { 
    auto symbol = _symtab.find("@", 1);
    if (symbol == nullptr) {
      throw std::string("Recursive call outside function.");
    } 
    else if (symbol->is_main()) {
      throw std::string("Recursive call inside begin end block.");
    }

  functype = cdk::functional_type::cast(symbol->type());
  } 
  else {
    node->function()->accept(this, lvl);
    
    if (!node->function()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("Wrong type in function call.");
    }

    functype = cdk::functional_type::cast(node->function()->type());
  }

  if (functype->input()->length() != node->arguments()->size()) {
    throw std::string("Wrong number of arguments in function call.");
  }

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
    arg->accept(this, lvl);

    auto paramtype = functype->input(i);

    if (arg->is_typed(cdk::TYPE_UNSPEC)) {
      if (paramtype->name() == cdk::TYPE_DOUBLE) {
        arg->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      } 
      else {
        arg->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
    } 
    else if (arg->is_typed(cdk::TYPE_POINTER) && paramtype->name() == cdk::TYPE_POINTER) {
      auto paramref = cdk::reference_type::cast(paramtype);
      auto argref = cdk::reference_type::cast(arg->type());

      if (argref->referenced()->name() == cdk::TYPE_UNSPEC
            || argref->referenced()->name() == cdk::TYPE_VOID
            || paramref->referenced()->name() == cdk::TYPE_VOID) {
        arg->type(paramtype);
      }
    }

    if (!deepTypeComparison(paramtype, arg->type(), true)) {
      throw std::string("Wrong type for argument " + std::to_string(i + 1) + " in function call.");
    }
  }

  // May result in node being typed as void
  node->type(functype->output(0));
}
//---------------------------------------------------------------------------

void til::type_checker::do_function_node(til::function_node * const node, int lvl) {
  auto function = til::make_symbol("@", node->type());
  function->is_main(node->is_main());

  if (!_symtab.insert(function->name(), function)) {
    // Can't insert -> already exists in local context
    _symtab.replace(function->name(), function);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_pointer_index_node(til::pointer_index_node * const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->base()->accept(this, lvl + 2);
  
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("Wrong type in pointer index's base -> pointer expected.");
  }

  node->index()->accept(this, lvl + 2);
  if (node->index()->is_typed(cdk::TYPE_UNSPEC)) {
    node->index()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } 
  else if (!node->index()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Wrong type in pointer index's index -> integer expected.");
  }

  auto basetype = cdk::reference_type::cast(node->base()->type());

  if (basetype->referenced()->name() == cdk::TYPE_UNSPEC) {
    basetype = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->base()->type(basetype);
  }

  node->type(basetype->referenced());
}

//---------------------------------------------------------------------------

void til::type_checker::do_next_node(til::next_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_nullptr_node(til::nullptr_node * const node, int lvl) {
  ASSERT_UNSPEC;

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_return_node(til::return_node * const node, int lvl) {
  // Symbol was stored in the previous context
  auto symbol = _symtab.find("@", 1);
  
  if (symbol == nullptr) {
    throw std::string("Return statement outside begin end block.");
  }

  std::shared_ptr<cdk::functional_type> functype = cdk::functional_type::cast(symbol->type());

  auto rettype = functype->output(0);
  auto rettype_name = rettype->name();

  if (node->return_value() == nullptr) {
    if (rettype_name != cdk::TYPE_VOID) {
      throw std::string("No return value specified for non-void function.");
    }
    return;
  }

  if (rettype_name == cdk::TYPE_VOID) {
    throw std::string("Return value specified for void function.");
  }

  node->return_value()->accept(this, lvl + 2);

  if (!deepTypeComparison(rettype, node->return_value()->type(), true)) {
    throw std::string("Wrong type for return expression.");
  }
}


//---------------------------------------------------------------------------

void til::type_checker::do_sizeof_node(til::sizeof_node * const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void til::type_checker::do_stop_node(til::stop_node * const node, int lvl) {
  // EMPTY
}