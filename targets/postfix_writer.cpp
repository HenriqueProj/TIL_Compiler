#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "targets/frame_size_calculator.h"
#include "til_parser.tab.h"

void til::postfix_writer::acceptCovariantNode(std::shared_ptr<cdk::basic_type> const target_type, cdk::expression_node * const node, int lvl) {
  if (target_type->name() != cdk::TYPE_FUNCTIONAL || !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    node->accept(this, lvl);
    if (target_type->name() == cdk::TYPE_DOUBLE && node->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    return;
  }

  auto lfunc_type = cdk::functional_type::cast(target_type);
  auto rfunc_type = cdk::functional_type::cast(node->type());

  bool needsWrap = false;

  // we assume that type_checker already validated that the types are compatible
  if (lfunc_type->output(0)->name() == cdk::TYPE_DOUBLE && rfunc_type->output(0)->name() == cdk::TYPE_INT) {
    needsWrap = true;
  } else {
    for (size_t i = 0; i < lfunc_type->input_length(); i++) {
      if (lfunc_type->input(i)->name() == cdk::TYPE_INT && rfunc_type->input(i)->name() == cdk::TYPE_DOUBLE) {
        needsWrap = true;
        break;
      }
    }
  }

  if (!needsWrap) {
    // if functions have the exact same type, nothing to do here
    node->accept(this, lvl);
    return;
  }

  // the arguments and/or return values require conversion to double
  // consequently, encapsulate the underlying function within another function that performs this conversion
  auto lineno = node->lineno();

  // If the target function is not in the global scope, it is necessary to declare an auxiliary global variable;
  // otherwise, the wrapper function would attempt to access a local variable of the outer function
  // currently being defined, which is out of scope during runtime, resulting in a segmentation fault

  auto aux_global_decl_name = "_wrapper_target_" + std::to_string(_lbl++);
  auto aux_global_decl = new til::declaration_node(lineno, tPRIVATE, rfunc_type, aux_global_decl_name, nullptr);
  auto aux_global_var = new cdk::variable_node(lineno, aux_global_decl_name);

  _forceOutsideFunction = true;
  aux_global_decl->accept(this, lvl);
  _forceOutsideFunction = false;

  // return to previous segment
  if (inFunction()) {
    _pf.TEXT(_functionLabels.top());
  } else {
    _pf.DATA();
  }
  _pf.ALIGN();

  // we can't pass the target function as an initializer to the declaration, as it
  // may be a non-literal expression, so we need to assign it afterwards
  auto aux_global_assignment = new cdk::assignment_node(lineno, aux_global_var, node);
  aux_global_assignment->accept(this, lvl);

  auto aux_global_rvalue = new cdk::rvalue_node(lineno, aux_global_var);

  auto args = new cdk::sequence_node(lineno);
  auto call_args = new cdk::sequence_node(lineno);
  for (size_t i = 0; i < lfunc_type->input_length(); i++) {
    auto arg_name = "_arg" + std::to_string(i);

    auto arg_decl = new til::declaration_node(lineno, tPRIVATE, lfunc_type->input(i), arg_name, nullptr);
    args = new cdk::sequence_node(lineno, arg_decl, args);

    auto arg_rvalue = new cdk::rvalue_node(lineno, new cdk::variable_node(lineno, arg_name));
    call_args = new cdk::sequence_node(lineno, arg_rvalue, call_args);
  }

  auto function_call = new til::function_call_node(lineno, aux_global_rvalue, call_args);
  auto return_node = new til::return_node(lineno, function_call);
  auto block = new til::block_node(lineno, new cdk::sequence_node(lineno), new cdk::sequence_node(lineno, return_node));

  auto wrapping_function = new til::function_node(lineno, args, lfunc_type->output(0), block);

  wrapping_function->accept(this, lvl);
}

void til::postfix_writer::prepareIDBinaryExpression(cdk::binary_operation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
}
void til::postfix_writer::prepareIDBinaryComparisonExpression(cdk::binary_operation_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
}

template<size_t P, typename T>
void til::postfix_writer::executeLoopControlInstruction(T * const node) {
  ASSERT_SAFE_EXPRESSIONS;

  auto level = static_cast<size_t>(node->level());

  if (level == 0) {
    THROW_ERROR("invalid loop control instruction level");
  } else if (_currentFunctionLoopLabels->size() < level) {
    THROW_ERROR("loop control instruction not within sufficient loops (expected at most " +
         std::to_string(_currentFunctionLoopLabels->size()) + ")");
  }

  auto index = _currentFunctionLoopLabels->size() - level;
  auto label = std::get<P>(_currentFunctionLoopLabels->at(index));
  _pf.JMP(label);

  _visitedFinalInstruction = true;
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

void til::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void til::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl = ++_lbl)); 
  node->right()->accept(this, lvl);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void til::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl = ++_lbl));
  node->right()->accept(this, lvl);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (inFunction()) {
    _pf.INT(node->value());
  } else {
    _pf.SINT(node->value());
  }
}

void til::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (inFunction()) {
    _pf.DOUBLE(node->value());
  } else {
    _pf.SDOUBLE(node->value());
  }
}

void til::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  _pf.SSTRING(node->value());

  if (inFunction()) {
    _pf.TEXT(_functionLabels.top());
    _pf.ADDR(mklbl(lbl1));
  } else {
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DNEG();
  } else {
    _pf.NEG();
  }
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine value
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}

void til::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }

  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    // the difference between two pointers is obtained by dividing the size of what they are referencing
    auto lref = cdk::reference_type::cast(node->left()->type());
    _pf.INT(std::max(static_cast<size_t>(1), lref->referenced()->size()));
    _pf.DIV();
  }
}

void til::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}

void til::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}

void til::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void til::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.LT();
}

void til::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.LE();
}

void til::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.GE();
}

void til::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.GT();
}

void til::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->name()); // type_checker already ensured symbol exists

  if (symbol->qualifier() == tEXTERNAL) {
    _externalFunctionName = symbol->name();
  } else if (symbol->global()) {
    _pf.ADDR(node->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);

  if (_externalFunctionName) {
    return; // name passed through this field; nothing in stack to be loaded
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    _pf.LDINT(); // non-ints are int-sized too, except doubles
  }
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  acceptCovariantNode(node->type(), node->rvalue(), lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }
  node->lvalue()->accept(this, lvl); // Location in which value will be stored 

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE(); // store value at address
  } else {
    _pf.STINT(); // store value at address
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);

  if (node->argument()->type()->size() > 0) {
    _pf.TRASH(node->argument()->type()->size());
  }
}

void til::postfix_writer::do_print_node(til::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
    child->accept(this, lvl);
    
    if (child->is_typed(cdk::TYPE_INT)) {
      _externalFunctionsToDeclare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // delete value
    } else if (child->is_typed(cdk::TYPE_DOUBLE)) {
      _externalFunctionsToDeclare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // delete value
    } else if (child->is_typed(cdk::TYPE_STRING)) {
      _externalFunctionsToDeclare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // delete value's address
    }
  }
  if (node->append_newline()) {
    _externalFunctionsToDeclare.insert("println");
    _pf.CALL("println"); // write a newline
  }
}


//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _externalFunctionsToDeclare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
    _externalFunctionsToDeclare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int condLabel, endLabel;

  _pf.ALIGN();
  _pf.LABEL(mklbl(condLabel = ++_lbl));

  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(endLabel = ++_lbl));
  _currentFunctionLoopLabels->push_back(std::make_pair(mklbl(condLabel), mklbl(endLabel)));

  node->block()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;  // For the case that it's not a block_node, but a single instruction
  _currentFunctionLoopLabels->pop_back();

  _pf.JMP(mklbl(condLabel));
  _pf.ALIGN();
  _pf.LABEL(mklbl(endLabel));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _visitedFinalInstruction = false; // For the case that it's not a block_node, but a single instruction
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_else_node(til::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;  // For the case that it's not a block_node, but a single instruction
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _visitedFinalInstruction = false;  // For the case that it's not a block_node, but a single instruction
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_address_node(til::address_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_alloc_node(til::alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto ref = cdk::reference_type::cast(node->type())->referenced();
  node->argument()->accept(this, lvl);
  // void has size 0, but we still want to alloc 1 byte for it
  _pf.INT(std::max(static_cast<size_t>(1), ref->size()));
  _pf.MUL();
  _pf.ALLOC();
  _pf.SP();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_block_node(til::block_node * const node, int lvl) {
  _symtab.push(); // for block-local variables
  node->declarations()->accept(this, lvl + 2);

  _visitedFinalInstruction = false;
  for (size_t i = 0; i < node->instructions()->size(); i++) {
    auto child = node->instructions()->node(i);

    if (_visitedFinalInstruction) {
      THROW_ERROR_FOR_NODE(child, "unreachable code; further instructions found after a final instruction");
    }

    child->accept(this, lvl + 2);
  }
  _visitedFinalInstruction = false;

  _symtab.pop();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_declaration_node(til::declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = new_symbol();
  reset_new_symbol();

  int offset = 0;
  int typesize = node->type()->size(); // in bytes
  if (_inFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } else if (inFunction()) {
    _offset -= typesize;
    offset = _offset;
  } else {
    // global variable
    offset = 0;
  }
  symbol->offset(offset);

  // function local variables have to be handled separately
  if (inFunction()) {
    // nothing to do for arguments and variables without initializer
    if (_inFunctionArgs || node->initializer() == nullptr) {
      return;
    }

    acceptCovariantNode(node->type(), node->initializer(), lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.LOCAL(symbol->offset());
      _pf.STDOUBLE();
    } else {
      _pf.LOCAL(symbol->offset());
      _pf.STINT();
    }

    return;
  }

  if (symbol->qualifier() == tFORWARD || symbol->qualifier() == tEXTERNAL) {
    _externalFunctionsToDeclare.insert(symbol->name());
    return;
  }

  _externalFunctionsToDeclare.erase(symbol->name());

  if (node->initializer() == nullptr) {
    _pf.BSS();
    _pf.ALIGN();

    if (symbol->qualifier() == tPUBLIC) {
      _pf.GLOBAL(symbol->name(), _pf.OBJ());
    }

    _pf.LABEL(symbol->name());
    _pf.SALLOC(typesize);
    return;
  }

  if (!isInstanceOf<cdk::integer_node, cdk::double_node, cdk::string_node,
        til::nullptr_node, til::function_node>(node->initializer())) {
    THROW_ERROR("non-literal initializer for global variable '" + symbol->name() + "'");
  }

  _pf.DATA();
  _pf.ALIGN();

  if (symbol->qualifier() == tPUBLIC) {
    _pf.GLOBAL(symbol->name(), _pf.OBJ());
  }

  _pf.LABEL(symbol->name());

  if (node->is_typed(cdk::TYPE_DOUBLE) && node->initializer()->is_typed(cdk::TYPE_INT)) {
    // We use double and not an integer because of global declarations of doubles
    auto int_node = dynamic_cast<cdk::integer_node*>(node->initializer());
    _pf.SDOUBLE(int_node->value());
  } else {
    node->initializer()->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_call_node(til::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::shared_ptr<cdk::functional_type> func_type;
  if (node->function() == nullptr) { // recursive call; "@"
    auto symbol = _symtab.find("@", 1);
    func_type = cdk::functional_type::cast(symbol->type());
  } else {
    func_type = cdk::functional_type::cast(node->function()->type());
  }

  int args_size = 0;
  // arguments must be visited in reverse order since the first argument is
  // on top of the stack
  for (size_t i = node->arguments()->size(); i > 0; i--) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i - 1));

    args_size += arg->type()->size();
    acceptCovariantNode(func_type->input(i - 1), arg, lvl + 2);
  }

  _externalFunctionName = std::nullopt;
  if (node->function() == nullptr) { // recursive call; "@"
    _pf.ADDR(_functionLabels.top());
  } else {
    node->function()->accept(this, lvl);
  }

  if (_externalFunctionName) {
    _pf.CALL(*_externalFunctionName);
    _externalFunctionName = std::nullopt;
  } else {
    _pf.BRANCH();
  }

  if (args_size > 0) {
    _pf.TRASH(args_size);
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else if (!node->is_typed(cdk::TYPE_VOID)) {
    _pf.LDFVAL32();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_node(til::function_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
 
  std::string functionLabel;
  if (node->is_main()) {
    functionLabel = "_main";
  } else {
    functionLabel = mklbl(++_lbl);
  }
  _functionLabels.push(functionLabel);

  _pf.TEXT(_functionLabels.top());
  _pf.ALIGN();
  
  if (node->is_main()) {
    _pf.GLOBAL("_main", _pf.FUNC());
  }
  _pf.LABEL(_functionLabels.top());

  auto oldOffset = _offset;
  _offset = 8; // function arguments start at offset 8
  _symtab.push();

  _inFunctionArgs = true;
  node->arguments()->accept(this, lvl);
  _inFunctionArgs = false;

  // calculate stack size for local variables
  frame_size_calculator fsc(_compiler, _symtab);
  node->block()->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize());

  auto oldFunctionRetLabel = _currentFunctionRetLabel;
  _currentFunctionRetLabel = mklbl(++_lbl);

  auto oldFunctionLoopLabels = _currentFunctionLoopLabels;
  _currentFunctionLoopLabels = new std::vector<std::pair<std::string, std::string>>();

  _offset = 0; // local variables start at offset 0

  node->block()->accept(this, lvl);

  if (node->is_main()) {
    // return 0 if main has no return statement
    _pf.INT(0);
    _pf.STFVAL32();
  }

  _pf.ALIGN();
  _pf.LABEL(_currentFunctionRetLabel);
  _pf.LEAVE();
  _pf.RET();

  delete _currentFunctionLoopLabels;
  _currentFunctionLoopLabels = oldFunctionLoopLabels;
  _currentFunctionRetLabel = oldFunctionRetLabel;
  _offset = oldOffset;
  _symtab.pop();
  _functionLabels.pop();

  if (node->is_main()) {
    for (auto name : _externalFunctionsToDeclare) {
      _pf.EXTERN(name);
    }
    return;
  }

  if (inFunction()) {
    _pf.TEXT(_functionLabels.top());
    _pf.ADDR(functionLabel);
  } else {
    _pf.DATA();
    _pf.SADDR(functionLabel);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_pointer_index_node(til::pointer_index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_next_node(til::next_node * const node, int lvl) {
    executeLoopControlInstruction<0>(node);
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_nullptr_node(til::nullptr_node * const node, int lvl) {
  if (inFunction()) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_return_node(til::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find("@", 1);
  auto rettype = cdk::functional_type::cast(symbol->type())->output(0);
  auto rettype_name = rettype->name();

  if (rettype_name != cdk::TYPE_VOID) {
    acceptCovariantNode(rettype, node->return_value(), lvl + 2);

    if (rettype_name == cdk::TYPE_DOUBLE) {
      _pf.STFVAL64();
    } else {
      _pf.STFVAL32();
    }
  }

  _pf.JMP(_currentFunctionRetLabel);

  _visitedFinalInstruction = true;
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sizeof_node(til::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _pf.INT(node->argument()->type()->size());
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_stop_node(til::stop_node * const node, int lvl) {
  executeLoopControlInstruction<1>(node);
}

