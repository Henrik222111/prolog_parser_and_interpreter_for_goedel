% author: Henrik Hinzmann

% TODOs
% layout character
% import export in module must be same name
% module_name cannot be in forbidden_module_name
% No Module may depend upon itself.
% integer and float constant is treated specially (const_decl)
% func_decl declaration must be transparent
% user_big_name cannot be in forbidden_big_name
% user_graphic_name cannot be in forbidden_graphic_name

parse_file(File) :-
    format('Parsing Gödel file: ~w~n', [File]),
    see(File),
    read_file(Txt),
    seen,
    reset_line_count, %trace,
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
goedel_module --> export_part, local_local_part. % names of parts must be the same %%TODO
goedel_module --> export_part.
goedel_module --> module_local_part.
export_part --> export_kind, ws, module_name, optws, terminator, optws, opt_export_item.
% local is splitted into module parts (has no export part)
% and local parts (has export part)
% local_part --> local_kind, module_name, terminator, opt_local_item.
local_local_part --> "LOCAL", ws, module_name, optws, terminator, optws, opt_local_local_item.
module_local_part --> "MODULE", ws, module_name, optws, terminator, optws, opt_module_local_item.
export_kind --> "EXPORT".
% only system module can have export_kind "CLOSED"
% export_kind --> "CLOSED".
opt_module_names --> comma, optws, module_name, opt_module_names.
opt_module_names --> "".
module_name --> user_big_name. % cannot be in forbidden_module_names %%TODO
opt_export_item --> export_item, optws, opt_export_item.
opt_export_item --> "".
export_item --> import_decl, optws, terminator, optws.
export_item --> language_decl, optws, terminator, optws.
export_item --> control_decl, optws, terminator, optws.
opt_module_local_item --> module_local_item, optws, opt_module_local_item.
opt_module_local_item --> "".
module_local_item --> import_decl, optws, terminator, optws.
module_local_item --> language_decl, optws, terminator, optws.
module_local_item --> control_decl, optws, terminator, optws.
module_local_item --> statement, optws, terminator, optws.
opt_local_local_item --> local_local_item, optws, opt_local_local_item.
opt_local_local_item --> "".
local_local_item --> module_local_item.
local_local_item --> lift_decl, optws, terminator, optws.
import_decl --> "IMPORT", ws,  module_name, optws, opt_module_names.
lift_decl --> "LIFT", ws, module_name, optws, opt_module_names. % only if module has export part
% No Module may depend upon itself. %%TODO

% Language Declarations
language_decl --> base_decl.
language_decl --> constructor_decl.
language_decl --> constant_decl.
language_decl --> function_decl.
language_decl --> proposition_decl.
language_decl --> predicate_decl.
base_decl --> "BASE", optws, user_name_seq.
constructor_decl --> "CONSTRUCTOR", optws, constr_decl, optws, opt_constr_decls.
opt_constr_decls --> comma, optws, constr_decl, optws, opt_constr_decls.
opt_constr_decls --> "".
constr_decl --> user_name, optws, "/", optws, positive_number(_Nr), optws.
constant_decl --> "CONSTANT", optws, const_decl, optws, opt_const_decl.
opt_const_decl --> semicolon, optws, const_decl, optws, opt_const_decl.
opt_const_decl --> "".
const_decl --> user_name_seq, ":", optws, type, optws. %integer and float constant is treated specially %%TODO
function_decl --> "FUNCTION", ws, func_decl, optws, opt_func_decls.
opt_func_decls --> semicolon, optws, func_decl, optws,  opt_func_decls.
opt_func_decls --> "".
% declaration must be transparent %%TODO
func_decl --> user_name_seq,  ":", optws, func_decl_end.
func_decl_end --> function_spec_1, optws, "(", optws, positive_number(_Nr), optws, ")", optws,
    ":", optws, type, optws, "->", optws, type, optws.
func_decl_end --> function_spec_2, optws, "(", optws, positive_number(_Nr), optws, ")", optws,
    ":", optws, type, optws, "*", optws, type, optws, "->", optws, type, optws.
func_decl_end --> type, optws, opt_func_types, "->", optws, type, optws.
opt_func_types --> "*", optws, type, optws, opt_func_types.
opt_func_types --> "".
function_spec_1 --> "Fx".
function_spec_1 --> "Fy".
function_spec_1 --> "xF".
function_spec_1 --> "yF".
function_spec_2 --> "xFx".
function_spec_2 --> "xFy".
function_spec_2 --> "yFx".
proposition_decl --> "PROPOSITION", optws, user_name_seq.
predicate_decl --> "PREDICATE", optws, pred_decl, opt_pred_decls.
opt_pred_decls --> semicolon, optws, pred_decl, opt_pred_decls.
opt_pred_decls --> "".
pred_decl --> user_name_seq, ":", optws, pred_decl_end.
pred_decl_end --> predicate_spec_1, optws, ":", optws, type, optws.
pred_decl_end --> predicate_spec_2, optws, ":", optws, type, optws, "*", optws, type, optws.
pred_decl_end --> type, optws, opt_func_types.
predicate_spec_1 --> "Pz".
predicate_spec_1 --> "zP".
predicate_spec_2 --> "zPz".
user_name_seq --> user_name, optws, opt_user_names.
opt_user_names --> comma, optws, user_name, optws, opt_user_names.
opt_user_names --> "".
user_name --> user_big_name.
user_name --> user_graphic_name.
user_big_name --> big_name(_Name). % cannot be in forbidden_big_names %%TODO
user_graphic_name --> graphic_name(_Name). % cannot be in forbidden_graphic_names %%TODO

% Types
opt_constr_types(C,N) --> comma, type, {C1 is C+1}, opt_constr_types(C1,N).
opt_constr_types(N,N) --> "".
type --> parameter.
type --> base.
type --> constructor(N), "(", type_seq(N), ")".
type_seq(N) --> type, opt_constr_types(1,N).
base --> user_name. % symbol with this name has to be declared or imported as base
constructor(N) --> user_name. % symbol with this name has to be declared or imported as constructor
parameter --> little_name(_Name).

% Control Declarations
control_decl --> "DELAY", cont_decl, opt_cont_decls. % only in module where predicate is declared
opt_cont_decls --> cont_decl, opt_cont_decls.
opt_cont_decls --> "".
% atom cannot be a proposition, no atom pair in delay set can have a
% common instance
cont_decl --> goedel_atom, "UNTIL", cond.
cond --> cond1, cond_end.
cond_end --> "&", and_seq.
cond_end --> "\\/", or_seq.
cond_end --> "".
cond1 --> "NONVAR", "(", variable, ")".
cond1 --> "GROUND", "(", variable, ")".
cond1 --> "true".
cond1 --> "(", cond, ")".
and_seq --> cond1, and_seq_end.
and_seq_end --> "&", and_seq.
and_seq_end --> "".
or_seq --> cond1, or_seq_end.
or_seq_end --> "\\/", or_seq.
or_seq_end --> "".

% Statements
statement --> goedel_atom, statement_end.
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
formula_0 --> goedel_atom.
formula_0 --> range_formula.
formula_0 --> "(", formula_f, ")".
formula_1 --> "~", formula_1_end.
formula_1 --> "SOME", "[", variable_seq, "]", formula_1_end.
formula_1 --> "ALL", "[", variable_seq, "]", formula_1_end.
formula_1_end --> formula_0.
formula_1_end --> formula_1.
formula_2 --> formula_0, "&", formula_2_f1_end.
formula_2 --> formula_1, "&", formula_2_f1_end.
formula_2 --> "IF", formula_2_if_end.
formula_2 --> "THEN", then_part, formula_2_then_end.
formula_2_f1_end --> formula_0.
formula_2_f1_end --> formula_1.
formula_2_f1_end --> formula_2.
formula_2_if_end --> "SOME", "[", variable_seq, "]", formula_f.
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
goedel_atom --> proposition.
goedel_atom --> predicate_no_ind, "(", term, opt_terms(1,N), ")".
goedel_atom --> predicate_ind, term.
goedel_atom --> term, predicate_ind, goedel_atom_end.
goedel_atom_end --> term.
goedel_atom_end --> "".
range_formula --> term, comperator, term, comperator, term.
comperator --> "<".
comperator --> "=<".
term --> term_inf.
term --> term_p.
term_inf --> variable.
term_inf --> constant.
term_inf --> number(_Nr).
term_inf --> float(_Nr).
term_inf --> string(_Str).
term_inf --> list.
term_inf --> set.
term_inf --> "(", term, ")".
term_inf --> function_no_ind, "(", term, opt_terms(1,N), ")".
opt_terms(C,N) --> comma, term, {C1 is C+1}, opt_terms(C1,N).
opt_terms(N,N) --> "".
term_p --> function_ind, term_p. % Fx and Fy
% TODO
% term_p with left recursion
% TODO
% must be arrity N and imported or declared
% indicators have Prio p
constant --> user_name.
function_ind --> user_name.
function_no_ind --> user_name.
proposition --> user_name.
predicate_ind --> user_name.
predicate_no_ind --> user_name.
list --> "[", list_mid, "]".
list_mid --> list_expr.
list_mid --> "".
list_expr --> term, list_expr_mid.
list_expr_mid --> comma, list_expr.
list_expr_mid --> "|", list_expr_end.
list_expr_mid --> "".
list_expr_end --> list.
list_expr_end --> variable.
set --> "{", set_mid, "}".
set_mid --> term, ":", formula_f.
set_mid --> set_expr.
set_mid --> "".
set_expr --> term, set_expr_mid.
set_expr_mid --> comma, set_expr.
set_expr_mid --> "|", set_expr_end.
set_expr_mid --> "".
set_expr_end --> set.
set_expr_end --> variable.
variable_seq --> little_name(_Name), variable_seq_opt.
variable_seq_opt --> comma, little_name(_Name), variable_seq_opt.
variable_seq_opt --> "".
variable --> underscore, variable_end.
variable --> little_name(_Name).
variable_end --> little_name(_Name).
variable_end --> "".

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
