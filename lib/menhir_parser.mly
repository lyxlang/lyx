// SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
// SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
//
// SPDX-License-Identifier: GPL-3.0-only

%{
  open Ast
%}

%token KWDEF
%token <bool> BOOL
%token KWIF
%token KWTHEN
%token KWELSE
%token KWLET
%token KWIN
%token KWMATCH
%token KWINT
%token KWFLOAT
%token KWSTRING
%token KWBOOL
%token KWUNIT
%token EQUAL
%token COLON
%token COLONEQUAL
%token UNDERSCORE
%token COMMA
%token ELLIPSIS
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token TRIANGLE
%token PLUSPLUS
%token STARSTAR
%token UNIT
%token ANDAND
%token BARBAR
%token GT
%token GEQ
%token LT
%token LEQ
%token EQUALEQUAL
%token TILDETILDE
%token BANGEQUAL
%token BANGTILDE
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token BANG
%token LEFT
%token RIGHT
%token AMPERSAND
%token BAR
%token HAT
%token ARROW
%token SEMICOLON
%token BACKSLASH
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> UID
%token <string> LID
%token <string> COMMENT
%token EOF

%start <Ast.program> program

%type <Ast.decl> decl
%type <Ast.olid> olid
%type <Ast.param> param
%type <Ast.tuple_param> tuple_param
%type <Ast.list_pat> list_pat
%type <Ast.tuple_pat> tuple_pat
%type <Ast.expr> expr
%type <Ast.expr> expr1
%type <Ast.expr> expr2
%type <Ast.expr> expr3
%type <Ast.expr> expr4
%type <Ast.expr> expr5
%type <Ast.expr> expr6
%type <Ast.expr> expr7
%type <Ast.expr> expr8
%type <Ast.expr> expr9
%type <Ast.expr> expr10
%type <Ast.expr> expr11
%type <Ast.bool_op> bool_op
%type <Ast.comp_op> comp_op
%type <Ast.add_op> add_op
%type <Ast.mul_op> mul_op
%type <Ast.bit_op> bit_op
%type <Ast.un_op> un_op
%type <Ast.bind> bind
%type <Ast.case> case
%type <Ast.pattern> pattern
%type <Ast.variant> variant
%type <Ast.typing> typing
%type <Ast.typing> typing1

%%

let located(x) == ~ = x; { {loc= {start= $startofs; fin= $endofs}; value= x} }

let program := ~ = list(located(decl)); EOF; < >

let ann := COLON; ~ = located(typing); < >

let decl :=
  | KWDEF; id = located(olid); params = list(located(param)); signature = option(ann); EQUAL; body = located(expr); { Decl {id; params; signature; body} }
  | KWDEF; id = located(UID); COLONEQUAL; polys = list(located(LID)); LBRACE; variants = nonempty_list(located(variant)); RBRACE; { DeclADT {id; polys; variants} }
  | KWDEF; id = located(UID); COLONEQUAL; typing = located(typing); { DeclAlias {id; typing} }
  | ~ = COMMENT; < Comment >

let olid :=
  | UNDERSCORE; { Wildcard }
  | ~ = LID; < L >

let param :=
  | ~ = located(LID); < PRLID >
  | ~ = tuple_param; < PRTuple >

let tuple_param := LPAREN; ~ = separated_nonempty_list(COMMA, located(param)); RPAREN; < >

let expr := body = located(expr1); signature = option(ann); { ETyped {body; signature}}

let expr1 :=
  | l = located(expr1); op = located(bool_op); r = located(expr2); { EBoolOp {l; op; r} }
  | ~ = expr2; < >

let expr2 :=
  | l = located(expr2); op = located(comp_op); r = located(expr3); { ECompOp {l; op; r} }
  | ~ = expr3; < >

let expr3 :=
  | l = located(expr3); TRIANGLE; r = located(expr4); { EPipeOp {l; r} }
  | ~ = expr4; < >

let expr4 :=
  | l = located(expr5); PLUSPLUS; r = located(expr4); { EConcatOp {l; r} }
  | ~ = expr5; < >

let expr5 :=
  | l = located(expr5); op = located(add_op); r = located(expr6); { EAddOp {l; op; r} }
  | ~ = expr6; < >

let expr6 :=
  | l = located(expr6); op = located(mul_op); r = located(expr7); { EMulOp {l; op; r} }
  | ~ = expr7; < >

let expr7 :=
  | op = located(un_op); body = located(expr8); { EUnOp {op; body} }
  | ~ = expr8; < >

let expr8 :=
  | l = located(expr8); STARSTAR; r = located(expr9); { EExpOp {l; r} }
  | ~ = expr9; < >

let expr9 :=
  | l = located(expr9); op = located(bit_op); r = located(expr10); { EBitOp {l; op; r} }
  | ~ = expr10; < >

let expr10 :=
  | fn = located(expr10); arg = located(expr11); { EApp {fn; arg} }
  | ~ = expr11; < >

let expr11 :=
  | BACKSLASH; params = nonempty_list(located(param)); ARROW; body = located(expr); { ELambda {params; body} }
  | KWMATCH; ref = located(expr); LBRACE; cases = nonempty_list(located(case)); RBRACE; { EMatch {ref; cases} }
  | KWLET; binds = separated_nonempty_list(SEMICOLON, located(bind)); KWIN; body = located(expr); { ELets {binds; body} }
  | KWIF; predicate = located(expr); KWTHEN; truthy = located(expr); KWELSE; falsy = located(expr); { EIf {predicate; truthy; falsy} }
  | ~ = located(UID); < EUID >
  | ~ = located(LID); < ELID >
  | LPAREN; ~ = separated_list(COMMA, located(expr)); RPAREN; < ETuple >
  | LBRACKET; ~ = separated_list(COMMA, located(expr)); RBRACKET; < EList >
  | UNIT; { EUnit }
  | ~ = located(BOOL); < EBool >
  | ~ = located(STRING); < EString >
  | ~ = located(FLOAT); < EFloat >
  | ~ = located(INT); < EInt >
  | LPAREN; ~ = located(expr); RPAREN; < EParenthesized >

let bool_op :=
  | ANDAND; { OpBoolAnd }
  | BARBAR; { OpBoolOr }

let comp_op :=
  | GT; { OpGt }
  | GEQ; { OpGeq }
  | LT; { OpLt }
  | LEQ; { OpLeq }
  | EQUALEQUAL; { OpEq }
  | TILDETILDE; { OpFeq }
  | BANGEQUAL; { OpNeq }
  | BANGTILDE; { OpNFeq }

let add_op :=
  | PLUS; { OpAdd }
  | MINUS; { OpSub }

let mul_op :=
  | STAR; { OpMul }
  | SLASH; { OpDiv }
  | PERCENT; { OpMod }

let un_op :=
  | PLUS; { UnPlus }
  | MINUS; { UnNeg }
  | BANG; { UnBoolNot }

let bit_op :=
  | LEFT; { OpBitLShift }
  | RIGHT; { OpBitRShift }
  | AMPERSAND; { OpBitAnd }
  | BAR; { OpBitOr }
  | HAT; { OpBitXor }

let bind := id = located(LID); params = list(located(param)); signature = option(ann); EQUAL; body = located(expr); { {id; params; signature; body} }

let case :=
  | pat = located(pattern); ARROW; body = located(expr); SEMICOLON; { Case {pat; body} }
  | pat = located(pattern); KWIF; guard = located(expr); ARROW; body = located(expr); SEMICOLON; { CaseGuard {pat; guard; body} }

let pattern :=
  | l = located(pattern1); SEMICOLON; r = located(pattern); { POr {l; r} }
  | ~ = pattern1; < >

let pattern1 :=
  | ~ = located(INT); < PInt >
  | ~ = located(FLOAT); < PFloat >
  | ~ = located(STRING); < PString >
  | ~ = located(BOOL); < PBool >
  | ~ = located(olid); < POLID >
  | id = located(UID); params = list(located(pattern1)); { PConstructor {id; params} }
  | ~ = located(list_pat); < PList >
  | ~ = located(list_spd_pat); < PListSpd >
  | ~ = located(tuple_pat); < PTuple >
  | LPAREN; ~ = located(pattern); RPAREN; < PParenthesized >

let list_pat := LBRACKET; ~ = separated_list(COMMA, located(pattern)); RBRACKET; < >

let list_spd_pat := LBRACKET; ~ = separated_nonempty_list(COMMA, located(pattern)); ELLIPSIS; RBRACKET; < >

let tuple_pat := LPAREN; ~ = separated_nonempty_list(COMMA, located(pattern)); RPAREN; < >

let variant := id = located(UID); typing = option(located(typing)); SEMICOLON; { {id; typing} }

let typing :=
  | l = located(typing1); ARROW; r = located(typing); { TFunc {l; r} }
  | ~ = typing1; < >

let typing1 :=
  | KWINT; { TInt }
  | KWFLOAT; { TFloat }
  | KWSTRING; { TString }
  | KWBOOL; { TBool }
  | KWUNIT; { TUnit }
  | LBRACKET; ~ = located(typing); RBRACKET; < TList >
  | LPAREN; ~ = separated_nonempty_list(COMMA, located(typing)); RPAREN; < TTuple >
  | ~ = located(LID); < TPoly >
  | id = located(UID); typing = option(located(typing1)); { TConstructor {id; typing} }
  | LPAREN; ~ = located(typing); RPAREN; < TTyping >
