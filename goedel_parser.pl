% author: Henrik Hinzmann

% TODOs
% import export in module must be same name
% No Module may depend upon itself.
% integer and float constant is treated specially (const_decl)
% func_decl declaration must be transparent

% Can be done later
% range_formula
% list
% set

parse_file(File) :-
    format('Parsing Gödel file: ~w~n', [File]),
    see(File),
    read_file(Txt),
    seen,
    reset, %trace,
    program(Txt,[]),
    format('end '), print_line_count.
parse_file(_File) :-
    format('fail '), print_line_count.

read_file(Txt) :-
    get_code(CharCode),
    (CharCode<0 -> Txt=[]
     ; (Txt = [CharCode|T],
       read_file(T)
       )
    ).

% DCG-Grammar

% Forbidden module_names
forbidden_module_name(Name) :-
    ForbiddenNames = ["Integers", "Rationals","Floats", "Numbers", "Lists",
                     "Sets", "Strings", "Tables", "Units", "Flocks", "Syntax",
                     "Programs", "Scripts", "Theories", "IO", "NumbersIO",
                     "FlocksIO", "ProgramsIO", "ScriptsIO", "TheoriesIO"],
    member(Name, ForbiddenNames).
% forbidden user_big_names
forbidden_user_big_name(Name) :-
    ForbiddenNames = ["EXPORT", "CLOSED", "LOCAL", "MODULE", "IMPORT", "LIFT",
                     "THEORY", "BASE", "CONSTRUCTOR", "CONSTANT", "FUNCTION",
                      "PROPOSITION", "PREDICATE", "DELAY", "UNTIL", "GROUND",
                     "NONVAR", "TRUE", "ALL", "SOME", "IF", "THEN", "ELSE"],
    member(Name, ForbiddenNames).
% forbidden user_graphic_names
forbidden_user_graphic_name(Name) :-
    ForbiddenNames = [":", "<-", "->", "<->", "&", "~", "\\/", "|"],
    member(Name, ForbiddenNames).

% Tokens
token --> big_name(_Name).
token --> little_name(_Name).
token --> graphic_name(_Name).
token --> string(_Str).
token --> bracket(_Nr).
token --> comma.
token --> semicolon.
token --> underscore.
token --> terminator.
token --> number(_Nr).
token --> float(_Nr).
token --> identifier(_Ident).
big_name(Name) --> big_letter(Ch), !, opt_name_character(Ch,Name).
little_name(Name) --> little_letter(Ch), !, opt_name_character(Ch,Name).
graphic_name(Name) --> graphic_character(Ch), !, opt_graphic_character(Ch,Name).
string(Str) --> "\"", opt_string_character('',Str), "\"".
% Nr is label number or -1 if no label
bracket(-1) --> "{".
bracket(Nr) --> "}", bracket_label(Nr).
bracket(-1) --> "(".
bracket(-1) --> ")".
bracket(-1) --> "[".
bracket(-1) --> "]".
bracket_label(Nr) --> "_", !, label(Nr).
bracket_label(-1) --> "".
comma --> ",".
semicolon --> ";".
underscore --> "_".
terminator --> ".".
number(Nr) --> zero(Nr1), !, opt_zero, number_end(Nr2), {Nr is Nr1+Nr2}.
number(Nr) --> positive_number(Nr).
number_end(Nr) --> positive_number(Nr), !.
number_end(Nr) --> {Nr=0}.
float(Nr) --> decimal(Dec), !, float_exp(Exp), {atom_concat(Dec,Exp,Tmp), atom_number(Tmp,Nr)}.
float_exp(Exp) --> "E+", !, number(Nr), {atom_concat('E+',Nr,Exp)}.
float_exp(Exp) --> "E-", !, number(Nr), {atom_concat('E-',Nr,Exp)}.
float_exp(Exp) --> {Exp=''}.
decimal(Nr) --> number(Nr1), ".", number(Nr2),{atom_concat(Nr1,'.',Tmp), atom_concat(Tmp,Nr2,Nr)}.
label(Nr) --> positive_number(Nr).
opt_name_character(Prev,Name) --> name_character(Ch), !,
    {atom_concat(Prev,Ch,New)}, opt_name_character(New,Name).
opt_name_character(Prev,Name) --> {Prev=Name}.
name_character(Ch) --> big_letter(Ch), !.
name_character(Ch) --> little_letter(Ch), !.
name_character(Dig) --> digit(Dig), !.
name_character(Ch) --> underscore, {Ch='_'}.
opt_graphic_character(Prev,Name) --> graphic_character(Ch), !,
    {atom_concat(Prev,Ch,New)}, opt_graphic_character(New,Name).
opt_graphic_character(Prev,Name) --> {Prev=Name}.
graphic_character('\\') --> "\\", !.
graphic_character(Ch) --> non_bs_graphic_char(Ch).
non_rsqm_graphic_character('\\') --> "\\", !.
non_rsqm_graphic_character(Ch) --> non_rsqm_non_bs_graphic_char(Ch).
non_rsqm_non_bs_graphic_char('+') --> "+".
non_rsqm_non_bs_graphic_char('-') --> "-".
non_rsqm_non_bs_graphic_char('*') --> "*".
non_rsqm_non_bs_graphic_char('/') --> "/".
non_rsqm_non_bs_graphic_char('^') --> "^".
non_rsqm_non_bs_graphic_char('#') --> "#".
non_rsqm_non_bs_graphic_char('<') --> "<".
non_rsqm_non_bs_graphic_char('>') --> ">".
non_rsqm_non_bs_graphic_char('=') --> "=".
non_rsqm_non_bs_graphic_char('~') --> "~".
non_rsqm_non_bs_graphic_char('&') --> "&".
non_rsqm_non_bs_graphic_char('?') --> "?".
non_rsqm_non_bs_graphic_char('‘') --> "‘". % left single quotation mark
non_rsqm_non_bs_graphic_char('@') --> "@".
non_rsqm_non_bs_graphic_char('!') --> "!".
non_rsqm_non_bs_graphic_char('$') --> "$".
non_rsqm_non_bs_graphic_char(':') --> ":".
non_rsqm_non_bs_graphic_char('|') --> "|".
non_bs_graphic_char(Ch) --> non_rsqm_non_bs_graphic_char(Ch), !.
non_bs_graphic_char('’') --> "’". % right single quotation mark
big_letter('A') --> "A".
big_letter('B') --> "B".
big_letter('C') --> "C".
big_letter('D') --> "D".
big_letter('E') --> "E".
big_letter('F') --> "F".
big_letter('G') --> "G".
big_letter('H') --> "H".
big_letter('I') --> "I".
big_letter('J') --> "J".
big_letter('K') --> "K".
big_letter('L') --> "L".
big_letter('M') --> "M".
big_letter('N') --> "N".
big_letter('O') --> "O".
big_letter('P') --> "P".
big_letter('Q') --> "Q".
big_letter('R') --> "R".
big_letter('S') --> "S".
big_letter('T') --> "T".
big_letter('U') --> "U".
big_letter('V') --> "V".
big_letter('W') --> "W".
big_letter('X') --> "X".
big_letter('Y') --> "Y".
big_letter('Z') --> "Z".
little_letter('a') --> "a".
little_letter('b') --> "b".
little_letter('c') --> "c".
little_letter('d') --> "d".
little_letter('e') --> "e".
little_letter('f') --> "f".
little_letter('g') --> "g".
little_letter('h') --> "h".
little_letter('i') --> "i".
little_letter('j') --> "j".
little_letter('k') --> "k".
little_letter('l') --> "l".
little_letter('m') --> "m".
little_letter('n') --> "n".
little_letter('o') --> "o".
little_letter('p') --> "p".
little_letter('q') --> "q".
little_letter('r') --> "r".
little_letter('s') --> "s".
little_letter('t') --> "t".
little_letter('u') --> "u".
little_letter('v') --> "v".
little_letter('w') --> "w".
little_letter('x') --> "x".
little_letter('y') --> "y".
little_letter('z') --> "z".
positive_number(Nr) --> opt_zero, non_zero(Dig), opt_digit(Dig,Nr).
opt_digit(Prev,Nr) --> digit(Dig), !, {New is Prev*10+Dig}, opt_digit(New,Nr).
opt_digit(Prev,Nr) --> {Prev=Nr}.
digit(Dig) --> zero(Dig), !.
digit(Dig) --> non_zero(Dig).
non_zero(1) --> "1".
non_zero(2) --> "2".
non_zero(3) --> "3".
non_zero(4) --> "4".
non_zero(5) --> "5".
non_zero(6) --> "6".
non_zero(7) --> "7".
non_zero(8) --> "8".
non_zero(9) --> "9".
opt_zero --> zero(_0), !, opt_zero.
opt_zero --> "".
zero(0) --> "0".
opt_string_character(Prev,Str) --> string_character(Ch), !,
    {string_concat(Prev,Ch,New)}, opt_string_character(New,Str).
opt_string_character(Prev,Str) --> {Prev=Str}.
string_character('\\') --> "\\\\", !.
string_character('\"')--> "\\\"", !.
% does not work TODO
string_character(Ch) --> "\\", !, non_esc_character(EscCh), {atom_concat('\\',EscCh,Ch)}.
string_character(Ch) --> non_esc_character(Ch).
layout_item(Ch) --> layout_character(Ch).
layout_item('') --> comment.
layout_character(' ') --> " ", !.
layout_character('\t') --> "\t", !.
layout_character('\r') --> "\r", !, {increment_line_count}.
layout_character('\n') --> "\n", !, {increment_line_count}.
comment_layout_character --> " ", !.
comment_layout_character --> "\t".
comment --> "%", !, opt_comment_character, comment_end.
comment_end --> "\r", !, {increment_line_count}.
comment_end --> "\n", !, {increment_line_count}.
opt_comment_character --> comment_character, !, opt_comment_character.
opt_comment_character --> "".
% end--------------------------------------------------------------------

% splited into comment_character, non_rsqm_character, non_dqm_character
% character --> "\\".
% character --> "\"".
% character --> non_esc_character.
comment_character --> "\\", !.
comment_character --> "\"", !.
comment_character --> comment_non_esc_character.
non_rsqm_character('\\') --> "\\", !.
non_rsqm_character('\"') --> "\"", !.
non_rsqm_character(Ch) --> non_rsqm_non_esc_character(Ch).
non_dqm_character('\\') --> "\\", !.
non_dqm_character(Ch) --> non_esc_character(Ch).
non_esc_character(Ch) --> layout_character(Ch), !.
non_esc_character(Ch) --> non_bs_graphic_char(Ch), !.
non_esc_character(Ch) --> normal_non_esc_character(Ch).
normal_non_esc_character(Ch) --> name_character(Ch), !.
normal_non_esc_character(';') --> semicolon, !.
normal_non_esc_character(',') --> comma, !.
normal_non_esc_character('.') --> terminator, !.
normal_non_esc_character('%') --> "%".
normal_non_esc_character('{') --> "{".
normal_non_esc_character('}') --> "}".
normal_non_esc_character('(') --> "(".
normal_non_esc_character(')') --> ")".
normal_non_esc_character('[') --> "[".
normal_non_esc_character(']') --> "]".
comment_non_esc_character --> comment_layout_character, !.
comment_non_esc_character --> non_bs_graphic_char(_Ch), !.
comment_non_esc_character --> normal_non_esc_character(_Ch).
non_rsqm_non_esc_character(Ch) --> layout_character(Ch), !.
non_rsqm_non_esc_character(Ch) --> non_rsqm_non_bs_graphic_char(Ch), !.
non_rsqm_non_esc_character(Ch) --> normal_non_esc_character(Ch).
identifier(Ident) --> quoted_ident(Ident), !.
identifier(Ident) --> dble_quoted_ident(Ident), !.
identifier(Ident) --> ordinary_ident(Ident).
quoted_ident(Ident) --> "’", !, opt_quote_char('’', Id), "’", {atom_concat(Id,'’',Ident)}.
opt_quote_char(Prev,Ident) --> quote_char(Ch), {atom_concat(Prev,Ch,New)}, opt_quote_char(New,Ident).
opt_quote_char(Prev,Ident) --> {Prev=Ident}.
quote_char('’’') --> "’’".
quote_char(Ch) --> non_rsqm_character(Ch).
dble_quoted_ident(Ident) --> "\"", !, opt_dble_quoted_char('\"', Id), "\"", {atom_concat(Id,'\"',Ident)}.
opt_dble_quoted_char(Prev,Ident) --> dble_quoted_char(Ch),
    {atom_concat(Prev,Ch,New)}, opt_dble_quoted_char(New,Ident).
opt_dble_quoted_char(Prev,Ident) --> {Prev=Ident}.
dble_quoted_char('\"\"') --> "\"\"", !.
dble_quoted_char(Ch) --> non_dqm_character(Ch).
ordinary_ident(Ident) --> ordinary_char(Ch), opt_ordinary_char(Ch,Ident).
opt_ordinary_char(Prev,Ident) --> ordinary_char(Ch), !,
    {atom_concat(Prev,Ch,New)}, opt_ordinary_char(New,Ident).
opt_ordinary_char(Prev,Ident) --> {Prev = Ident}.
ordinary_char(Ch) --> name_character(Ch), !.
ordinary_char(Ch) --> non_rsqm_graphic_character(Ch), !.
ordinary_char(';') --> semicolon, !.
ordinary_char('.') --> terminator, !.
ordinary_char('[') --> "[".
ordinary_char(']') --> "]".
ordinary_char('{') --> "{".
ordinary_char('}') --> "}".

% Programs
program --> goedel_module, !, opt_goedel_module.
opt_goedel_module --> goedel_module, !, opt_goedel_module.
opt_goedel_module --> "".
goedel_module --> export_part(Exp), local_local_part(Loc). % names of parts must be the same %%TODO
goedel_module --> export_part(Exp).
goedel_module --> module_local_part(Loc).
export_part(Exp) --> export_kind, ws, module_name(Name), optws, terminator, optws, opt_export_item.
% local is splitted into module parts (has no export part)
% and local parts (has export part)
% local_part --> local_kind, module_name, terminator, opt_local_item.
local_local_part(Loc) --> "LOCAL", ws, module_name(Name), optws, terminator, optws, opt_local_local_item.
module_local_part(Loc) --> "MODULE", ws, module_name(Name), optws, terminator, optws, opt_module_local_item.
export_kind --> "EXPORT".
% only system module can have export_kind "CLOSED"
% export_kind --> "CLOSED".
opt_module_names([Name|Names]) --> comma, !, optws, module_name(Name), !, opt_module_names(Names).
opt_module_names(Names) --> {Names=[]}.
module_name(Name) --> user_big_name(Name), !, {\+forbidden_module_name(Name)}.

opt_export_item --> export_item, optws, opt_export_item.
opt_export_item --> "".
export_item --> import_decl(_Names), optws, terminator, optws.
export_item --> language_decl, optws, terminator, optws.
export_item --> control_decl, optws, terminator, optws.
opt_module_local_item --> module_local_item, optws, opt_module_local_item.
opt_module_local_item --> "".
module_local_item --> import_decl(_Names), optws, terminator, optws.
module_local_item --> language_decl, optws, terminator, optws.
module_local_item --> control_decl, optws, terminator, optws.
module_local_item --> statement, optws, terminator, optws.
opt_local_local_item --> local_local_item, optws, opt_local_local_item.
opt_local_local_item --> "".
local_local_item --> module_local_item.
local_local_item --> lift_decl(_Names), optws, terminator, optws.
import_decl([Name|Names]) --> "IMPORT", !, ws,  module_name(Name), optws, opt_module_names(Names).
% only if module has export part
lift_decl([Name|Names]) --> "LIFT", !, ws, module_name(Name), optws, opt_module_names(Names).
% No Module may depend upon itself. %%TODO

% Language Declarations
language_decl --> base_decl(Bases).
language_decl --> constructor_decl(Constrs).
language_decl --> constant_decl(Consts).
language_decl --> function_decl(Funcs).
language_decl --> proposition_decl(Props).
language_decl --> predicate_decl(Preds).
%List of Base names
base_decl(Bases) --> "BASE", !, optws, user_name_seq(Names), {Bases=Names}.
% List of constr(Name,Arity)
constructor_decl([Constr|Constrs]) --> "CONSTRUCTOR", !,  optws, constr_decl(Constr), optws,
    opt_constr_decls(Constrs).
opt_constr_decls([Constr|Constrs]) --> comma, !, optws, constr_decl(Constr), optws,
    opt_constr_decls(Constrs).
opt_constr_decls(Constrs) --> {Constrs=[]}.
constr_decl(contr(Name,Nr)) --> user_name(Name), optws, "/", optws, positive_number(Nr), optws.
% List of const(Name,Type)
constant_decl(Consts) --> "CONSTANT", !, optws, const_decl(Consts1), optws, opt_const_decl(Consts2),
    {append(Consts1,Consts2,Consts)}.
opt_const_decl(Consts) --> semicolon, !, optws, const_decl(Consts1), optws, opt_const_decl(Consts2),
    {append(Consts1,Consts2,Consts)}.
opt_const_decl(Consts) --> {Consts=[]}.
const_decl(Consts) --> user_name_seq(Names), ":", optws, type(Type), optws,
    {const_list(Names,Type,Consts)}.
%integer and float constant is treated specially %%TODO
% List of func(Name,Arity,Spec,Prio,List of Types,Rangetype)
function_decl(Funcs) --> "FUNCTION", !, ws, func_decl(Funcs1), optws, opt_func_decls(Funcs2),
    {append(Funcs1,Funcs2,Funcs)}.
opt_func_decls(Funcs) --> semicolon, !, optws, func_decl(Funcs1), optws,  opt_func_decls(Funcs2),
    {append(Funcs1,Funcs2,Funcs)}.
opt_func_decls(Funcs) --> {Funcs=[]}.
% declaration must be transparent %%TODO
func_decl(Funcs) --> user_name_seq(Names),  ":", optws, func_decl_end(Spec,Prio,Types,Range),
    {length(Types,N),func_list(Names,N,Spec,Prio,Types,Range,Funcs)}.
func_decl_end(Spec,Nr,[Type],Range) --> function_spec_1(Spec), !, optws,
    "(", optws, positive_number(Nr), optws, ")", optws, ":", optws,
    type(Type), optws, "->", optws, type(Range), optws.
func_decl_end(Spec,Nr,[Type1|Type2],Range) --> function_spec_2(Spec), !, optws,
    "(", optws, positive_number(Nr), optws, ")", optws, ":", optws,
    type(Type1), optws, "*", optws, type(Type2), optws, "->", optws, type(Range), optws.
func_decl_end('',-1,[Type|Types],Range) --> type(Type), optws, opt_func_types(Types),
    "->", optws, type(Range), optws.
opt_func_types([Type|Types]) --> "*", !, optws, type(Type), optws, opt_func_types(Types).
opt_func_types(Types) --> {Types=[]}.
function_spec_1('Fx') --> "Fx".
function_spec_1('Fy') --> "Fy".
function_spec_1('xF') --> "xF".
function_spec_1('yF') --> "yF".
function_spec_2('xFx') --> "xFx".
function_spec_2('xFy') --> "xFy".
function_spec_2('yFx') --> "yFx".
% List of Prop names
proposition_decl(Props) --> "PROPOSITION", !, optws, user_name_seq(Names), {Props=Names}.
% List of pred(Name,Arity,Spec,List of Type)
predicate_decl(Preds) --> "PREDICATE", !, optws, pred_decl(Preds1), opt_pred_decls(Preds2),
    {append(Preds1,Preds2,Preds)}.
opt_pred_decls(Preds) --> semicolon, !, optws, pred_decl(Preds1), opt_pred_decls(Preds2),
    {append(Preds1,Preds2,Preds)}.
opt_pred_decls(Preds) --> {Preds=[]}.
pred_decl(Preds) --> user_name_seq(Names), ":", optws, pred_decl_end(Spec,Types),
    {length(Types,N), pred_list(Names, N, Spec, Types, Preds)}.
pred_decl_end(Spec,[Type]) --> predicate_spec_1(Spec), !, optws, ":", optws, type(Type), optws.
pred_decl_end(Spec,[Type1,Type2]) --> predicate_spec_2(Spec), !, optws, ":", optws, type(Type1),
    optws, "*", optws, type(Type2), optws.
pred_decl_end('',[Type|Types]) --> type(Type), optws, opt_func_types(Types).
predicate_spec_1('Pz') --> "Pz".
predicate_spec_1('zP') --> "zP".
predicate_spec_2('zPz') --> "zPz".
user_name_seq([Name|Names]) --> user_name(Name), !, optws, opt_user_names(Names).
opt_user_names([Name|Names]) --> comma, !, optws, user_name(Name), !, optws, opt_user_names(Names).
opt_user_names(Names) --> {Names=[]}.
user_name(Name) --> user_big_name(Name), !.
user_name(Name) --> user_graphic_name(Name).
user_big_name(Name) --> big_name(Name), !, {\+forbidden_user_big_name(Name)}.
user_graphic_name(Name) --> graphic_name(Name), !, {\+forbidden_user_graphic_name(Name)}.

% Types
opt_constr_types(C,N) --> comma, type, {C1 is C+1}, opt_constr_types(C1,N).
opt_constr_types(N,N) --> "".
type(_Type) --> parameter.
type(_Type) --> base.
type(_Type) --> constructor(N), "(", type_seq(N), ")".
type_seq(N) --> type, opt_constr_types(1,N).
base --> user_name(_Name). % symbol with this name has to be declared or imported as base
constructor(N) --> user_name(_Name). % symbol with this name has to be declared or imported as constructor
parameter --> little_name(_Name).

% Control Declarations
control_decl --> "DELAY", cont_decl, opt_cont_decls. % only in module where predicate is declared
opt_cont_decls --> cont_decl, opt_cont_decls.
opt_cont_decls --> "".
% atom cannot be a proposition, no atom pair in delay set can have a
% common instance
cont_decl --> goedel_atom(_Atom), "UNTIL", cond.
cond --> cond1, cond_end.
cond_end --> "&", and_seq.
cond_end --> "\\/", or_seq.
cond_end --> "".
cond1 --> "NONVAR", "(", variable(_Var), ")".
cond1 --> "GROUND", "(", variable(_Var), ")".
cond1 --> "true".
cond1 --> "(", cond, ")".
and_seq --> cond1, and_seq_end.
and_seq_end --> "&", and_seq.
and_seq_end --> "".
or_seq --> cond1, or_seq_end.
or_seq_end --> "\\/", or_seq.
or_seq_end --> "".

% Statements
statement --> goedel_atom(_Atom), statement_end.
statement_end --> "<-", body.
statement_end --> "".
body --> "|", body_end.
body --> cformula_0, body_mid.
body --> cformula_2, body_mid.
body --> cformula_f, body_mid.
body_mid --> "|", body_end.
body_mid --> "".
body_end --> cformula_0.
body_end --> cformula_2.
body_end --> cformula_f.
body_end --> "".
cformula_0 --> "(", cformula_0_f, ")".
cformula_0 --> "{", cformula_0_f, "}", cformula_0_end.
cformula_0_end --> "_", label(_Nr).
cformula_0_end --> "".
cformula_0_f --> cformula_0.
cformula_0_f --> cformula_2.
cformula_0_f --> cformula_f.
cformula_2 --> cformula_0, "&", cformula_2_end.
cformula_2 --> formula_0, "&", cformula_2_end.
cformula_2 --> formula_1, "&", cformula_2_end.
cformula_2_end --> cformula_0.
cformula_2_end --> cformula_2.
cformula_2_end --> formula_0.
cformula_2_end --> formula_1.
cformula_2_end --> formula_2.
cformula_f --> formula_0.
cformula_f --> formula_1.
cformula_f --> formula_2.
cformula_f --> formula_3.
cformula_f --> formula_4.
formula_f --> formula_0.
formula_f --> formula_1.
formula_f --> formula_2.
formula_f --> formula_3.
formula_f --> formula_4.
formula_0 --> goedel_atom(_Atom).
%formula_0 --> range_formula.
formula_0 --> "(", formula_f, ")".
formula_1 --> "~", formula_1_end.
formula_1 --> "SOME", "[", variable_seq(_Vars), "]", formula_1_end.
formula_1 --> "ALL", "[", variable_seq(_Vars), "]", formula_1_end.
formula_1_end --> formula_0.
formula_1_end --> formula_1.
formula_2 --> formula_0, "&", formula_2_f1_end.
formula_2 --> formula_1, "&", formula_2_f1_end.
formula_2 --> "IF", formula_2_if_end.
formula_2 --> "THEN", then_part, formula_2_then_end.
formula_2_f1_end --> formula_0.
formula_2_f1_end --> formula_1.
formula_2_f1_end --> formula_2.
formula_2_if_end --> "SOME", "[", variable_seq(_Vars), "]", formula_f.
formula_2_if_end --> formula_f.
formula_2_then_end --> "ELSE", formula_2_f1_end.
formula_2_then_end --> "".
formula_3 --> formula_0, "\\/", formula_3_end.
formula_3 --> formula_1, "\\/", formula_3_end.
formula_3 --> formula_2, "\\/", formula_3_end.
formula_3_end --> formula_0.
formula_3_end --> formula_1.
formula_3_end --> formula_2.
formula_3_end --> formula_3.
formula_4 --> formula_0, formula_4_mid.
formula_4 --> formula_1, formula_4_mid.
formula_4 --> formula_2, formula_4_mid.
formula_4 --> formula_3, formula_4_mid.
formula_4_mid --> "<-", formula_f.
formula_4_mid --> "->", formula_f.
formula_4_mid --> "<->", formula_4_end.
formula_4_end --> formula_0.
formula_4_end --> formula_1.
formula_4_end --> formula_2.
formula_4_end --> formula_3.
then_part --> "IF", formula_f, "THEN", then_part.
then_part --> formula_0, then_part_end.
then_part --> formula_1, then_part_end.
then_part_end --> "&", then_part.
then_part_end --> "".

goedel_atom(not(eq(Term1,Term2))) --> term(Term1), "~=", !, term(Term2).
goedel_atom(eq(Term1,Term2)) --> term(Term1), "=", !, term(Term2).
goedel_atom(true) --> "True", !.
goedel_atom(false) --> "False", !.
goedel_atom(Atom) --> user_name(Name), goedel_atom_end(Name,Atom).
goedel_atom_end(Name,pred(Name,N,[Term|Terms])) --> "(", !, term(Term), opt_terms(1,N,Terms), ")".
goedel_atom_end(Name,prop(Name)) --> "".
/* Can be added later
range_formula --> term, comperator, term, comperator, term.
comperator --> "<".
comperator --> "=<".*/
term(var(Var)) --> variable(Var), !.
term(fl(Fl)) --> float(Fl), !.
term(nr(Nr)) --> number(Nr), !.
term(str(Str)) --> string(Str), !.
%term --> list.
%term --> set.
term(Term) --> "(", !, term(Term), ")".
term(Term) --> user_name(Name), term_end(Name,Term).
term_end(Name,func(Name,N,[Term|Terms])) --> "(", !, term(Term), opt_terms(1,N,Terms), ")".
term_end(Name,cons(Name)) --> "".
opt_terms(C,N,[Term|Terms]) --> comma, !, term(Term), {C1 is C+1}, opt_terms(C1,N,Terms).
opt_terms(N,N,Terms) --> {Terms=[]}.




/* Can be later used for Lists
list --> "[", list_mid, "]".
list_mid --> list_expr.
list_mid --> "".
list_expr --> term, list_expr_mid.
list_expr_mid --> comma, list_expr.
list_expr_mid --> "|", list_expr_end.
list_expr_mid --> "".
list_expr_end --> list.
list_expr_end --> variable(Var).*/
/* Can be later used for Sets
set --> "{", set_mid, "}".
set_mid --> term, ":", formula_f.
set_mid --> set_expr.
set_mid --> "".
set_expr --> term, set_expr_mid.
set_expr_mid --> comma, set_expr.
set_expr_mid --> "|", set_expr_end.
set_expr_mid --> "".
set_expr_end --> set.
set_expr_end --> variable(Var).*/
variable_seq([Name|Vars]) --> little_name(Name), !, variable_seq_opt(Vars).
variable_seq_opt([Name|Vars]) --> comma, !, little_name(Name), variable_seq_opt(Vars).
variable_seq_opt(Vars) --> {Vars=[]}.
variable(Name) --> underscore, !, variable_end(UName), {atom_concat('_',UName,Name)}.
variable(Name) --> little_name(Name).
variable_end(Name) --> little_name(Name), !.
variable_end(Name) --> {Name=''}.

/* ----------------- */
/* helper predicates */
/* ----------------- */

reset :-
    reset_line_count,
    reset_ast.

:- dynamic ast/3.

reset_ast :-
    retractall(ast(_,_,_)).

const_list([H|T],Type,[const(H,Type)|R]) :-
    const_list(T,Type,R).
const_list([],_,[]).

func_list([H|T],N,Spec,Prio,Types,Range,[func(H,N,Spec,Prio,Types,Range)|R]) :-
    func_list(T,N,Spec,Prio,Types,Range,R).
func_list([],_,_,_,_,_,[]).

pred_list([H|T],N,Spec,Types,[pred(H,N,Spec,Types)|R]) :-
    pred_list(T,N,Spec,Types,R).
pred_list([],_,_,_,[]).

/* ------------- */
/* line counting */
/* ------------- */

ws --> layout_item(_Ch), optws.
optws --> layout_item(_Ch), optws.
optws --> "".

:- dynamic cur_line_number/1.
cur_line_number(1).

print_line_count :-
    cur_line_number(N),
    print('at line '), print(N), print(' ').

increment_line_count :-
    retract(cur_line_number(N)),
    N1 is N+1,
    assert(cur_line_number(N1)).

reset_line_count :-
    retractall(cur_line_number(_N)),
    assert(cur_line_number(1)).
