%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                  i;         
  double               d;        
  std::string          *s;       
  cdk::basic_node      *node; 
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; 
  cdk::lvalue_node     *lvalue;
  til::block_node      *block;
  std::vector<std::shared_ptr<cdk::basic_type>> *type_vec;
};

%token <i> tINTEGER 
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tTYPE_INT tTYPE_DOUBLE tTYPE_STRING tTYPE_VOID

%token tEXTERNAL tFORWARD tPUBLIC tVAR tPRIVATE
%token tBLOCK tIF tLOOP tSTOP tNEXT tRETURN tPRINT tPRINTLN 
%token tREAD tNULL tSET tINDEX tOBJECTS tSIZEOF tFUNCTION 
%token tPROGRAM
%token tOR tAND tEQ tNE tGE tLE

%type <sequence> fdecls decls instrs exprs func_args
%type <node> fdecl program decl instr func_arg ifotherwise
%type <type> type referable_type func_return_type func_type ref_type void_ref_type
%type <type_vec> types
%type <block> decls_instrs blk
%type <expression> expr func_definition
%type <lvalue> lval


%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%


file : fdecls program    { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
     | fdecls            { compiler->ast($1); }
     |        program    { compiler->ast(new cdk::sequence_node(LINE, $1)); }
     | /* empty */       { compiler->ast(new cdk::sequence_node(LINE)); }
     ;
     
fdecls : fdecls fdecl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
       |        fdecl    { $$ = new cdk::sequence_node(LINE, $1); }
       ;

fdecl:  '(' tEXTERNAL type  tIDENTIFIER        ')'    { $$ = new til::declaration_node(LINE, tEXTERNAL, $3, *$4, nullptr); delete $4; }
      | '(' tFORWARD type  tIDENTIFIER        ')'    { $$ = new til::declaration_node(LINE, tFORWARD, $3, *$4, nullptr); delete $4; }
      | '(' tPUBLIC  type  tIDENTIFIER        ')'    { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, nullptr); delete $4; }
      | '(' tPUBLIC  type  tIDENTIFIER  expr  ')'    { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, $5); delete $4; }
      | '(' tPUBLIC  tVAR  tIDENTIFIER  expr  ')'    { $$ = new til::declaration_node(LINE, tPUBLIC, nullptr, *$4, $5); delete $4; }
      | '(' tPUBLIC        tIDENTIFIER  expr  ')'    { $$ = new til::declaration_node(LINE, tPUBLIC, nullptr, *$3, $4); delete $3; }
      |              decl                            { $$ = $1; }
      ;   

type : referable_type    { $$ = $1; }
     | void_ref_type     { $$ = $1; }
     ;

referable_type : tTYPE_INT       { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
               | tTYPE_DOUBLE    { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
               | tTYPE_STRING    { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
               | func_type       { $$ = $1; }
               | ref_type        { $$ = $1; }
               ;

func_type : '(' func_return_type ')'                  { $$ = cdk::functional_type::create($2); }
          | '(' func_return_type '(' types ')' ')'    { $$ = cdk::functional_type::create(*$4, $2); delete $4; }
          ;

func_return_type : type          { $$ = $1; }
                 | tTYPE_VOID    { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
                 ;

types : types type    { $$ = $1; $$->push_back($2); }
      | type          { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(1, $1); }
      ;
          
ref_type :  referable_type '!'     { $$ = cdk::reference_type::create(4, $1); }
         ;

void_ref_type : void_ref_type '!'    { $$ = $1; }
              | tTYPE_VOID '!'       { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)); }
              ;         

program : '(' tPROGRAM decls_instrs ')'    { $$ = new til::function_node(LINE, $3); }
        ;
        
decls_instrs : decls instrs    { $$ = new til::block_node(LINE, $1, $2); }
             | decls           { $$ = new til::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
             |       instrs    { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
             | /* empty */     { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
             ;

decls : decls decl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
      |       decl    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

decl :  '(' type  tIDENTIFIER       ')'  { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, nullptr); delete $3; }
     |  '(' type  tIDENTIFIER expr  ')'  { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
     |  '(' tVAR  tIDENTIFIER expr  ')'  { $$ = new til::declaration_node(LINE, tPRIVATE, nullptr, *$3, $4); delete $3; }
     ;
     
instrs : instrs instr    { $$ = new cdk::sequence_node(LINE, $2, $1); }
       |        instr    { $$ = new cdk::sequence_node(LINE, $1); }
       ;


instr : expr                                  { $$ = new til::evaluation_node(LINE, $1); }
      | '(' tPRINT exprs ')'                  { $$ = new til::print_node(LINE, $3, false); }
      | '(' tPRINTLN  exprs ')'               { $$ = new til::print_node(LINE, $3, true); }
      | '(' tSTOP tINTEGER ')'                { $$ = new til::stop_node(LINE, $3); }
      | '(' tSTOP ')'                         { $$ = new til::stop_node(LINE, 1); }
      | '(' tNEXT tINTEGER ')'                { $$ = new til::next_node(LINE, $3); }
      | '(' tNEXT ')'                         { $$ = new til::next_node(LINE, 1); }
      | '(' tRETURN expr ')'                  { $$ = new til::return_node(LINE, $3); }
      | '(' tRETURN ')'                       { $$ = new til::return_node(LINE, nullptr); }
      | '(' tLOOP expr instr ')'              { $$ = new til::loop_node(LINE, $3, $4); }
      | '(' tIF  expr instr ')'              { $$ = new til::if_node(LINE, $3, $4); }
      | '(' tIF expr instr ifotherwise ')'    { $$ = new til::if_else_node(LINE, $3, $4, $5); }
      | '(' blk              ')'              { $$ = $2; }
      ;

ifotherwise : instr                             { $$ = $1; }
            ;

blk :  tBLOCK decls_instrs                    { $$ = $2; }
    ;


exprs : exprs expr     { $$ = new cdk::sequence_node(LINE, $2, $1); }
      |       expr    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

expr : tINTEGER                        { $$ = new cdk::integer_node(LINE, $1); }  
     | tDOUBLE                         { $$ = new cdk::double_node(LINE, $1); }
     | tSTRING                         { $$ = new cdk::string_node(LINE, *$1);}
     | tNULL                           { $$ = new til::nullptr_node(LINE); }
     | '(' '+' expr ')'                { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '-' expr ')'                { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '~' expr ')'                { $$ = new cdk::not_node(LINE, $3); }
     | '(' '+' expr expr ')'           { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr  expr ')'          { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '*' expr  expr ')'          { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr  expr ')'          { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr  expr ')'          { $$ = new cdk::mod_node(LINE, $3, $4); }
     | '(' '<' expr  expr ')'          { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr  expr ')'          { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr  expr ')'          { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr  expr ')'          { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr  expr ')'          { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr  expr ')'          { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' tAND expr  expr ')'         { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr  expr ')'          { $$ = new cdk::or_node(LINE, $3, $4); }
     | expr '!'                        { $$ = new til::alloc_node(LINE, $1); }
     | '(' tOBJECTS expr ')'           { $$ = new til::alloc_node(LINE, $3); }  
     | '(' tSET lval expr ')'          { $$ = new cdk::assignment_node(LINE, $3, $4); }
     | '(' tSIZEOF expr  ')'           { $$ = new til::sizeof_node(LINE, $3); }
     | lval                            { $$ = new cdk::rvalue_node(LINE, $1); }
     | '(' '?' lval ')'                { $$ = new til::address_node(LINE, $3); }
     | '(' tREAD ')'                          { $$ = new til::read_node(LINE); }
     | '(' expr exprs ')'                 { $$ = new til::function_call_node(LINE, $2, $3); } 
     | '(' expr ')'                       { $$ = new til::function_call_node(LINE, $2, new cdk::sequence_node(LINE)); }  /* useless in parser due to conflicts */
     | '(' '@' exprs ')'                  { $$ = new til::function_call_node(LINE, nullptr, $3); }
     | '(' '@' ')'                        { $$ = new til::function_call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
     | func_definition                 { $$ = $1; }
     ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     | '(' tINDEX expr expr ')'        { $$ = new til::pointer_index_node(LINE, $3, $4);}
     ;

func_definition :'(' tFUNCTION '(' func_return_type  func_args ')' decls_instrs ')'    { $$ = new til::function_node(LINE, $5, $4, $7); }
                |'(' tFUNCTION '(' func_return_type            ')' decls_instrs ')'  { $$ = new til::function_node(LINE, new cdk::sequence_node(LINE), $4, $6); }
                ;

func_args : func_args func_arg    { $$ = new cdk::sequence_node(LINE, $2, $1); }
          | func_arg                  { $$ = new cdk::sequence_node(LINE, $1); }
          ;

func_arg : '(' type tIDENTIFIER ')'   { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, nullptr); delete $3; }
         ;

%%
