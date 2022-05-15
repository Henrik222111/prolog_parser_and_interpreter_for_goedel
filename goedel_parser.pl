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
token --> big_name.
token --> little_name.
token --> graphic_name.
token --> string.
token --> bracket.
token --> comma.
token --> semicolon.
token --> underscore.
token --> terminator.
token --> number.
token --> float.
token --> identifier.
big_name --> big_letter, opt_name_character.
little_name --> little_letter, opt_name_character.
graphic_name --> graphic_character, opt_graphic_character.
string --> "\"", opt_string_character, "\"".
bracket --> "{".
bracket --> "}", bracket_label.
bracket --> "(".
bracket --> ")".
bracket --> "[".
bracket --> "]".
bracket_label --> "_", label.
bracket_label --> "".
comma --> ",".
semicolon --> ";".
underscore --> "_".
terminator --> ".".
number --> zero, opt_zero.
number --> positive_number.
float --> decimal, float_exp.
float_exp --> "E+", number.
float_exp --> "E-", number.
float_exp --> "".
decimal --> number, ".", number.
label --> positive_number.
opt_name_character --> name_character, opt_name_character.
opt_name_character --> "".
name_character --> big_letter.
name_character --> little_letter.
name_character --> digit.
name_character --> underscore.
opt_graphic_character --> graphic_character, opt_graphic_character.
opt_graphic_character --> "".
graphic_character --> "\\".
graphic_character --> non_bs_graphic_char.
non_rsqm_graphic_character --> "\\".
non_rsqm_graphic_character --> non_rsqm_non_bs_graphic_char.
non_rsqm_non_bs_graphic_char --> "+".
non_rsqm_non_bs_graphic_char --> "-".
non_rsqm_non_bs_graphic_char --> "*".
non_rsqm_non_bs_graphic_char --> "/".
non_rsqm_non_bs_graphic_char --> "^".
non_rsqm_non_bs_graphic_char --> "#".
non_rsqm_non_bs_graphic_char --> "<".
non_rsqm_non_bs_graphic_char --> ">".
non_rsqm_non_bs_graphic_char --> "=".
non_rsqm_non_bs_graphic_char --> "~".
non_rsqm_non_bs_graphic_char --> "&".
non_rsqm_non_bs_graphic_char --> "?".
non_rsqm_non_bs_graphic_char --> "�". % left single quotation mark
non_rsqm_non_bs_graphic_char --> "@".
non_rsqm_non_bs_graphic_char --> "!".
non_rsqm_non_bs_graphic_char --> "$".
non_rsqm_non_bs_graphic_char --> ":".
non_rsqm_non_bs_graphic_char --> "|".
non_bs_graphic_char --> non_rsqm_non_bs_graphic_char.
non_bs_graphic_char --> "�". % right single quotation mark
big_letter --> "A".
big_letter --> "B".
big_letter --> "C".
big_letter --> "D".
big_letter --> "E".
big_letter --> "F".
big_letter --> "G".
big_letter --> "H".
big_letter --> "I".
big_letter --> "J".
big_letter --> "K".
big_letter --> "L".
big_letter --> "M".
big_letter --> "N".
big_letter --> "O".
big_letter --> "P".
big_letter --> "Q".
big_letter --> "R".
big_letter --> "S".
big_letter --> "T".
big_letter --> "U".
big_letter --> "V".
big_letter --> "W".
big_letter --> "X".
big_letter --> "Y".
big_letter --> "Z".
little_letter --> "a".
little_letter --> "b".
little_letter --> "c".
little_letter --> "d".
little_letter --> "e".
little_letter --> "f".
little_letter --> "g".
little_letter --> "h".
little_letter --> "i".
little_letter --> "j".
little_letter --> "k".
little_letter --> "l".
little_letter --> "m".
little_letter --> "n".
little_letter --> "o".
little_letter --> "p".
little_letter --> "q".
little_letter --> "r".
little_letter --> "s".
little_letter --> "t".
little_letter --> "u".
little_letter --> "v".
little_letter --> "w".
little_letter --> "x".
little_letter --> "y".
little_letter --> "z".
positive_number --> opt_zero, non_zero, opt_digit.
opt_digit --> digit, opt_digit.
opt_digit --> "".
digit --> zero.
digit --> non_zero.
non_zero --> "1".
non_zero --> "2".
non_zero --> "3".
non_zero --> "4".
non_zero --> "5".
non_zero --> "6".
non_zero --> "7".
non_zero --> "8".
non_zero --> "9".
opt_zero --> zero, opt_zero.
opt_zero --> "".
zero --> "0".
opt_string_character --> string_character, opt_string_character.
opt_string_character --> "".
string_character --> "\\\\".
string_character --> "\\\"".
string_character --> "\\", non_esc_character.
string_character --> non_esc_character.
layout_item --> layout_character.
layout_item --> comment.
layout_character --> " ".
layout_character --> "\t".
layout_character --> "\r".
layout_character --> "\n". %%TODO
comment_layout_character --> " ".
comment_layout_character --> "\t".
comment --> "%", opt_comment_character, "\r".
comment --> "%", opt_comment_character, "\n".
opt_comment_character --> comment_character, opt_comment_character.
opt_comment_character --> "".
% splited into comment_character, non_rsqm_character, non_dqm_character
% character --> "\\".
% character --> "\"".
% character --> non_esc_character.
comment_character --> "\\".
comment_character --> "\"".
comment_character --> comment_non_esc_character.
non_rsqm_character --> "\\".
non_rsqm_character --> "\"".
non_rsqm_character --> non_rsqm_non_esc_character.
non_dqm_character --> "\\".
non_dqm_character --> non_esc_character.
non_esc_character --> layout_character.
non_esc_character --> non_bs_graphic_char.
non_esc_character --> normal_non_esc_character.
normal_non_esc_character --> name_character.
normal_non_esc_character --> semicolon.
normal_non_esc_character --> comma.
normal_non_esc_character --> terminator.
normal_non_esc_character --> "%".
normal_non_esc_character --> "{".
normal_non_esc_character --> "}".
normal_non_esc_character --> "(".
normal_non_esc_character --> ")".
normal_non_esc_character --> "[".
normal_non_esc_character --> "]".
comment_non_esc_character --> comment_layout_character.
comment_non_esc_character --> non_bs_graphic_char.
comment_non_esc_character --> normal_non_esc_character.
non_rsqm_non_esc_character --> layout_character.
non_rsqm_non_esc_character --> non_rsqm_non_bs_graphic_char.
non_rsqm_non_esc_character --> normal_non_esc_character.
identifier --> quoted_ident.
identifier --> dble_quoted_ident.
identifier --> ordinary_ident.
quoted_ident --> "�", opt_quote_char, "�".
opt_quote_char --> quote_char, opt_quote_char.
opt_quote_char --> "".
quote_char --> "��".
quote_char --> non_rsqm_character.
dble_quoted_ident --> "\"", opt_dble_quoted_char, "\"".
opt_dble_quoted_char --> dble_quoted_char, opt_dble_quoted_char.
opt_dble_quoted_char --> "".
dble_quoted_char --> "\"\"".
dble_quoted_char --> non_dqm_character.
ordinary_ident --> ordinary_char, opt_ordinary_char.
opt_ordinary_char --> ordinary_char, opt_ordinary_char.
opt_ordinary_char --> "".
ordinary_char --> name_character.
ordinary_char --> non_rsqm_graphic_character.
ordinary_char --> semicolon.
ordinary_char --> terminator.
ordinary_char --> "[".
ordinary_char --> "]".
ordinary_char --> "{".
ordinary_char --> "}".

% Programs
program --> goedel_module, opt_goedel_module.
opt_goedel_module --> goedel_module, opt_goedel_module.
opt_goedel_module --> "".
goedel_module --> export_part, local_local_part. % names of parts must be the same %%TODO
goedel_module --> export_part.
goedel_module --> module_local_part.
export_part --> export_kind, module_name, terminator, opt_export_item.
% local is splitted into module parts (has no export part)
% and local parts (has export part)
% local_part --> local_kind, module_name, terminator, opt_local_item.
local_local_part --> "LOCAL", module_name, terminator, opt_local_local_item.
module_local_part --> "MODULE", module_name, terminator, opt_module_local_item.
export_kind --> "EXPORT".
% only system module can have export_kind "CLOSED"
% export_kind --> "CLOSED".
opt_module_names --> comma, module_name, opt_module_names.
opt_module_names --> "".
module_name --> user_big_name. % cannot be in forbidden_module_names %%TODO
opt_export_item --> export_item, opt_export_item.
opt_export_item --> "".
export_item --> import_decl, terminator.
export_item --> language_decl, terminator.
export_item --> control_decl, terminator.
opt_module_local_item --> module_local_item, opt_module_local_item.
opt_module_local_item --> "".
module_local_item --> import_decl, terminator.
module_local_item --> language_decl, terminator.
module_local_item --> control_decl, terminator.
module_local_item --> statement, terminator.
opt_local_local_item --> local_local_item, opt_local_local_item.
opt_local_local_item --> "".
local_local_item --> module_local_item.
local_local_item --> lift_decl, terminator.
import_decl --> "IMPORT", module_name, opt_module_names.
lift_decl --> "LIFT", module_name, opt_module_names. % only if module has export part
% No Module may depend upon itself. %%TODO

% Language Declarations
language_decl --> base_decl.
language_decl --> constructor_decl.
language_decl --> constant_decl.
language_decl --> function_decl.
language_decl --> proposition_decl.
language_decl --> predicate_decl.
base_decl --> "BASE", user_name_seq.
constructor_decl --> "CONSTRUCTOR", constr_decl, opt_constr_decls.
opt_constr_decls --> comma, constr_decl, opt_constr_decls.
opt_constr_decls --> "".
constr_decl --> user_name, "/", positive_number.
constant_decl --> "CONSTANT", const_decl, opt_const_decl.
opt_const_decl --> semicolon, const_decl, opt_const_decl.
opt_const_decl --> "".
const_decl --> user_name_seq, ":", type. %integer and float constant is treated specially %%TODO
function_decl --> "FUNCTION", func_decl, opt_func_decls.
opt_func_decls --> semicolon, func_decl, opt_func_decls.
opt_func_decls --> "".
% declaration must be transparent %%TODO
func_decl --> user_name_seq, ":", func_decl_end.
func_decl_end --> function_spec_1, "(", positive_number, ")",
    ":", type, "->", type.
func_decl_end --> function_spec_2, "(", positive_number, ")",
    ":", type, "*", type, "->", type.
func_decl_end --> type, opt_func_types, "->", type.
function_spec_1 --> "Fx".
function_spec_1 --> "Fy".
function_spec_1 --> "xF".
function_spec_1 --> "yF".
function_spec_2 --> "xFx".
function_spec_2 --> "xFy".
function_spec_2 --> "yFx".
proposition_decl --> "PROPOSITION", user_name_seq.
predicate_decl --> "PREDICATE", pred_decl, opt_pred_decls.
opt_pred_decls --> semicolon, pred_decl, opt_pred_decls.
opt_pred_decls --> "".
pred_decl --> user_name_seq, ":", pred_decl_end.
pred_decl_end --> predicate_spec_1, ":", type.
pred_decl_end --> predicate_spec_2, ":", type, "*", type.
pred_decl_end --> type, opt_func_types.
predicate_spec_1 --> "Pz".
predicate_spec_1 --> "zP".
predicate_spec_2 --> "zPz".
user_name_seq --> user_name, opt_user_names.
opt_user_names --> comma, user_name, opt_user_names.
opt_user_names --> "".
user_name --> user_big_name.
user_name --> user_graphic_name.
user_big_name --> big_name. % cannot be in forbidden_big_names %%TODO
user_graphic_name --> graphic_name. % cannot be in forbidden_graphic_names %%TODO

% Types
opt_func_types --> "*", type, opt_func_types.
opt_func_types --> "".
opt_constr_types(C,N) --> comma, type, {C1 is C+1}, opt_constr_types(C1,N).
opt_constr_types(N,N) --> "".
type --> parameter.
type --> base.
type --> constructor(N), "(", type_seq(N), ")".
type_seq(N) --> type, opt_constr_types(1,N).
base --> user_name. % symbol with this name has to be declared or imported as base
constructor(N) --> user_name. % symbol with this name has to be declared or imported as constructor
parameter --> little_name.

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
cformula_0_end --> "_", label.
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
term_inf --> number.
term_inf --> float.
term_inf --> string.
term_inf --> list.
term_inf --> set.
term_inf --> "(", term, ")".
term_inf --> function_no_ind, "(", term, opt_terms(1,N), ")".
opt_terms(C,N) --> comma, term, {C1 is C+1}, opt_terms(C1,N).
opt_terms(N,N) --> "".
term_p --> function_ind, term_p. %Fx and Fy
%TODO
%term_p with left recursion
%TODO
%must be arrity N and imported or declared
%indicators have Prio p
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
variable_seq --> little_name, variable_seq_opt.
variable_seq_opt --> comma, little_name, variable_seq_opt.
variable_seq_opt --> "".
variable --> underscore, variable_end.
variable --> little_name.
variable_end --> little_name.
variable_end --> "".
