% author: Henrik Hinzmann

% TODOs
% layout character
% import export in module must be same name
% module_name cannot be in forbidden_module_name
% No Module may depend upon itself.
% integer and float constant is treated specially (const_decl)
% func_decl
% pred_decl
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
bracket --> "}_", label.
bracket --> "}".
bracket --> "(".
bracket --> ")".
bracket --> "[".
bracket --> "]".
comma --> ",".
semicolon --> ";".
underscore --> "_".
terminator --> ".".
number --> zero, opt_zero.
number --> positive_number.
float --> decimal, "E+", number.
float --> decimal, "E-", number.
float --> decimal.
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
non_rsqm_non_bs_graphic_char --> "‘". % left single quotation mark
non_rsqm_non_bs_graphic_char --> "@".
non_rsqm_non_bs_graphic_char --> "!".
non_rsqm_non_bs_graphic_char --> "$".
non_rsqm_non_bs_graphic_char --> ":".
non_rsqm_non_bs_graphic_char --> "|".
non_bs_graphic_char --> non_rsqm_non_bs_graphic_char.
non_bs_graphic_char --> "’". % right single quotation mark
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
quoted_ident --> "’", opt_quote_char, "’".
opt_quote_char --> quote_char, opt_quote_char.
opt_quote_char --> "".
quote_char --> "’’".
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
opt_constr_decls --> comma, constr_decls, opt_constr_decls.
opt_constr_decls --> "".
constr_decl --> user_name, "/", positive_number.
constant_decl --> "CONSTANT", const_decl, opt_const_decl.
opt_const_decl --> semicolon, const_decl, opt_const_decl.
opt_const_decl --> "".
const_decl --> user_name_seq, ":", type. %integer and float constant is treated specially %%TODO
function_decl --> "FUNCTION", func_decl, opt_func_decls.
opt_func_decls --> semicolon, func_decl, opt_func_decls.
opt_func_decls --> "".
% func_decl %%TODO
proposition_decl --> "PROPOSITION", user_name_seq.
predicate_decl --> "PREDICATE", pred_decl, opt_pred_decls.
opt_pred_decls --> semicolon, pred_decls, opt_pred_decls.
opt_pred_decls --> "".
% pred_decl %%TODO
user_name_seq --> user_name, opt_user_names.
opt_user_names --> comma, user_name, opt_user_names.
opt_user_names --> "".
user_name --> user_big_name.
user_name --> user_graphic_name.
user_big_name --> big_name. % cannot be in forbidden_big_names %%TODO
user_graphic_name --> graphic_name. % cannot be in forbidden_graphic_names %%TODO
