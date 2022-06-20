% author: Henrik Hinzmann

% Can be done later
% integer and float constant is treated specially (const_decl)
% range_formula
% list
% set
% pruning

% Example files have to be in prolog working directory
parse1 :- parse_file('Chapter_2_M1.loc'), nl, print_ast.
parse2 :- parse_file('Mem.loc'), nl, print_ast.
parse3 :- parse_file('Flounder.loc'), nl, print_ast.
parse4 :- parse_file('Chapter_2_M2.loc'), nl, print_ast.
parsetest :- parse_file('Test.loc'), nl, print_ast.

print_ast :- ast(Name,Exp,Loc),write('ast('), print_quoted(Name), write(','), print_quoted(Exp),
    write(','), print_quoted(Loc), write(')'), nl.

% From Prof. Micheal Leuschel
print_quoted(X) :- write_term(X,
                            [quoted(true),ignore_ops(true),
                             max_depth(0),
                             numbervars(true),portrayed(true)]).

% File has to be in prolog working directory
parse_file(File) :-
    absolute_file_name(File,AF,[]),
    format('Parsing Gödel file: ~w~n', [AF]),
    open(AF,read,Stream),
    set_stream(Stream,newline(posix)),
    set_input(Stream),
    read_file(Txt),
    close(Stream),
    reset,
    program(Txt,[]),
    format('finished parsing '), print_line_count, format('~n'), !,
    check_types,
    reset_stm,
    extract_statements,
    format('Start Lifting'), nl,
    lift_pp,
    format('Finished Lifting'), nl.
parse_file(_File) :-
    (checked('type error') -> fail; format('failed parsing '), print_line_count, format('~n')),
    fail.

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
string_character('\t') --> "\\t", !.
string_character('\r') --> "\\r", !.
string_character('\n') --> "\\n", !.
string_character(Ch) --> non_esc_character(Ch).
layout_item(Ch) --> layout_character(Ch).
layout_item('') --> comment.
layout_character(' ') --> " ", !.
layout_character('\t') --> "\t", !.
layout_character('\n') --> "\r\n", !, {increment_line_count}.
layout_character('\n') --> "\r", !, {increment_line_count}.
layout_character('\n') --> "\n", !, {increment_line_count}.
comment_layout_character --> " ", !.
comment_layout_character --> "\t".
comment --> "%", !, opt_comment_character, comment_end.
comment_end --> "\r", !, {increment_line_count}.
comment_end --> "\n", !, {increment_line_count}.
% Comment at the end of file
comment_end([],[]).
opt_comment_character --> comment_character, !, opt_comment_character.
opt_comment_character --> "".
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
program --> optws, goedel_module.
/* Support for more modules can be added later
program --> goedel_module, !, opt_goedel_module.
opt_goedel_module --> goedel_module, !, opt_goedel_module.
opt_goedel_module --> "".*/
goedel_module --> export_part(ModName), goedel_module_end(ModName).
goedel_module --> module_local_part.
goedel_module_end(ModName) --> local_local_part(ModName), !.
goedel_module_end(_ModName) --> "".
export_part(ModName) --> export_kind, !, ws, module_name(ModName), {add_module(ModName)},
    optws, terminator, optws, opt_export_item(ModName).
% local is splitted into module parts (has no export part)
% and local parts (has export part)
% local_part --> local_kind, module_name, terminator, opt_local_item.
local_local_part(ModName) --> "LOCAL", ws, module_name(Name), {ModName=Name}, optws, terminator,
    optws, opt_local_local_item(ModName).
module_local_part --> "MODULE", ws, module_name(ModName), {add_module(ModName)},
    optws, terminator, optws, opt_module_local_item(ModName).
export_kind --> "EXPORT".
% only system module can have export_kind "CLOSED"
% export_kind --> "CLOSED".
opt_module_names([Name|Names]) --> comma, !, optws, module_name(Name), !, opt_module_names(Names).
opt_module_names(Names) --> {Names=[]}.
module_name(Name) --> user_big_name(Name), !, {\+forbidden_module_name(Name)}.

opt_export_item(ModName) --> export_item(ModName), !, optws, opt_export_item(ModName).
opt_export_item(_ModName) --> "".
export_item(ModName) --> import_decl(Imports), !, {add_exp_imports(ModName,Imports)},
    optws, terminator, optws.
export_item(ModName) --> language_decl(ModName,exp), !, optws, terminator, optws.
export_item(ModName) --> control_decl(Controls), {add_exp_controls(ModName,Controls)},
    optws, terminator, optws.
opt_module_local_item(ModName) --> module_local_item(ModName), !, optws, opt_module_local_item(ModName).
opt_module_local_item(_ModName) --> "".
module_local_item(ModName) --> import_decl(Imports), !, {add_loc_imports(ModName,Imports)},
    optws, terminator, optws.
module_local_item(ModName) --> language_decl(ModName,loc), !, optws, terminator, optws.
module_local_item(ModName) --> control_decl(Controls), !, {add_loc_controls(ModName,Controls)},
    optws, terminator, optws.
module_local_item(ModName) --> statement(Statement), {add_statement(ModName,Statement)},
    optws, terminator, optws.
opt_local_local_item(ModName) --> local_local_item(ModName), !, optws, opt_local_local_item(ModName).
opt_local_local_item(_ModName) --> "".
local_local_item(ModName) --> module_local_item(ModName), !.
local_local_item(ModName) --> lift_decl(Lifts), {add_lifts(ModName,Lifts)}, optws, terminator, optws.
import_decl([Name|Names]) --> "IMPORT", !, ws,  module_name(Name), optws, opt_module_names(Names).
% only if module has export part
lift_decl([Name|Names]) --> "LIFT", !, ws, module_name(Name), optws, opt_module_names(Names).
% No Module may depend upon itself.


% Language Declarations
language_decl(ModName,Part) --> base_decl(Bases), {add_bases(ModName,Part,Bases)}.
language_decl(ModName,Part) --> constructor_decl(Constrs), {add_constrs(ModName,Part,Constrs)}.
language_decl(ModName,Part) --> constant_decl(Consts), {add_consts(ModName,Part,Consts)}.
language_decl(ModName,Part) --> function_decl(Funcs), {add_funcs(ModName,Part,Funcs)}.
language_decl(ModName,Part) --> proposition_decl(Props), {add_props(ModName,Part,Props)}.
language_decl(ModName,Part) --> predicate_decl(Preds), {add_preds(ModName,Part,Preds)}.
% List of Base names
base_decl(Bases) --> "BASE", !, optws, user_name_seq(Names), {Bases=Names}.
% List of constr(Name,Arity)
constructor_decl([Constr|Constrs]) --> "CONSTRUCTOR", !,  optws, constr_decl(Constr), optws,
    opt_constr_decls(Constrs).
opt_constr_decls([Constr|Constrs]) --> comma, !, optws, constr_decl(Constr), optws,
    opt_constr_decls(Constrs).
opt_constr_decls(Constrs) --> {Constrs=[]}.
constr_decl(constr(Name,Nr)) --> user_name(Name), optws, "/", optws, positive_number(Nr), optws.
% List of const(Name,Type)
constant_decl(Consts) --> "CONSTANT", !, optws, const_decl(Consts1), optws, opt_const_decl(Consts2),
    {append(Consts1,Consts2,Consts)}.
opt_const_decl(Consts) --> semicolon, !, optws, const_decl(Consts1), optws, opt_const_decl(Consts2),
    {append(Consts1,Consts2,Consts)}.
opt_const_decl(Consts) --> {Consts=[]}.
const_decl(Consts) --> user_name_seq(Names), ":", optws, type(Type), optws,
    {const_list(Names,Type,Consts)}.
% integer and float constant is treated specially %% can be done later
% List of func(Name,Arity,Spec,Prio,List of Types,Rangetype)
function_decl(Funcs) --> "FUNCTION", !, ws, func_decl(Funcs1), optws, opt_func_decls(Funcs2),
    {append(Funcs1,Funcs2,Funcs)}.
opt_func_decls(Funcs) --> semicolon, !, optws, func_decl(Funcs1), optws,  opt_func_decls(Funcs2),
    {append(Funcs1,Funcs2,Funcs)}.
opt_func_decls(Funcs) --> {Funcs=[]}.
% declaration must be transparent
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
type(par(Name)) --> little_name(Name), !.
type(Type) --> user_name(Name), optws, type_end(Name,Type).
type_end(Name,Type) --> "(", !, optws, type_seq(N,Types), optws, ")", {Type=constr(Name,N,Types)}.
type_end(Name,Type) --> {Type=base(Name)}.
type_seq(N,[Type|Types]) --> type(Type), optws, opt_constr_types(1,N,Types).
opt_constr_types(C,N,[Type|Types]) --> comma, !, optws, type(Type), optws, {C1 is C+1},
    opt_constr_types(C1,N,Types).
opt_constr_types(C,N,Types) --> {N=C, Types=[]}.


% Control Declarations
control_decl([Delay|Delays]) --> "DELAY", optws, cont_decl(Delay), optws, opt_cont_decls(Delays).
% only in module where predicate is declared
opt_cont_decls([Delay|Delays]) --> semicolon, !, optws, cont_decl(Delay), optws, opt_cont_decls(Delays).
opt_cont_decls(Delays) --> {Delays=[]}.
% atom cannot be a proposition, no atom pair in delay set can have a
% common instance
cont_decl(delay(Atom,Cond)) --> goedel_atom(Atom), optws, "UNTIL", optws, !, cond(Cond), optws.
cond(Cond) --> cond1(Cond1), cond_end(Cond1,Cond).
cond_end(Cond1,Cond) --> "&", !, optws, and_seq(Cond1,Cond).
cond_end(Cond1,Cond) --> "\\/", !, optws, or_seq(Cond1,Cond).
cond_end(Cond1,Cond) --> {Cond=Cond1}.
cond1(nvar(Var)) --> "NONVAR", !, optws, "(", optws, variable(Var), optws, ")", optws.
cond1(grnd(Var)) --> "GROUND", !, optws, "(", optws, variable(Var), optws, ")", optws.
cond1(true) --> "TRUE", !, optws.
cond1(Cond) --> "(", optws, cond(Cond), optws, ")", optws.
and_seq(Cond1,Cond) --> cond1(Cond2), optws, and_seq_end(and(Cond1,Cond2),Cond).
and_seq_end(Cond1,Cond) --> "&", !, optws, and_seq(Cond1,Cond).
and_seq_end(Cond1,Cond) --> {Cond=Cond1}.
or_seq(Cond1,Cond) --> cond1(Cond2), optws, or_seq_end(or(Cond1,Cond2),Cond).
or_seq_end(Cond1,Cond) --> "\\/", !, optws, or_seq(Cond1,Cond).
or_seq_end(Cond1,Cond) --> {Cond=Cond1}.


% Statements
statement(Stm) --> optws, goedel_atom(Head), optws, statement_end(Head,Stm).
statement_end(Head,stm(Head,Body)) --> "<-", !, optws, body(Body).
statement_end(Head,Stm) --> {Stm=stm(Head,true)}.
% Pruning cann be added later with "|"
body(Form) --> cformula_2(Form), !.
body(Form) --> cformula_0(Form), !.
body(Form) --> cformula_f(Form), !.
cformula_0(Form) --> "(", !, optws, cformula_0_f(Form), optws, ")".
/* Pruning can be added later
cformula_0(Form) --> "{", optws, cformula_0_f(Form), optws, "}", cformula_0_end(_Label).
cformula_0_end(Nr) --> "_", label(Nr).
cformula_0_end(-1) --> "". */
cformula_0_f(Form) --> cformula_2(Form), !.
cformula_0_f(Form) --> cformula_0(Form), !.
cformula_0_f(Form) --> cformula_f(Form).
cformula_2(Form) --> cformula_0(Form1), !, optws, cformula_2_mid(Form1,Form).
cformula_2(Form) --> formula_1(Form1), !, optws, cformula_2_mid(Form1,Form).
cformula_2(Form) --> formula_0(Form1), optws, cformula_2_mid(Form1,Form).
cformula_2_mid(Form1,Form) --> "&", optws, cformula_2_and(Form1,Form).
cformula_2_and(Form1,Form) --> cformula_0(Form2), optws, cformula_2_mid(and(Form1,Form2),Form), !.
cformula_2_and(Form1,Form) --> formula_2_if(Form2), optws, cformula_2_mid(and(Form1,Form2),Form), !.
cformula_2_and(Form1,Form) --> formula_1(Form2), optws, cformula_2_mid(and(Form1,Form2),Form), !.
cformula_2_and(Form1,Form) --> formula_0(Form2), optws, cformula_2_mid(and(Form1,Form2),Form), !.
cformula_2_and(Form1,Form) --> cformula_2_end(Form2), {Form=and(Form1,Form2)}.
cformula_2_end(Form) --> cformula_0(Form), !.
cformula_2_end(Form) --> formula_2_if(Form), !. % "&" is filtered
cformula_2_end(Form) --> formula_1(Form), !.
cformula_2_end(Form) --> formula_0(Form).
cformula_f(Form) --> formula_4(Form), !.
cformula_f(Form) --> formula_3(Form), !.
cformula_f(Form) --> formula_2(Form), !.
cformula_f(Form) --> formula_1(Form), !.
cformula_f(Form) --> formula_0(Form).
formula_f(Form) --> formula_4(Form), !, optws.
formula_f(Form) --> formula_3(Form), !, optws.
formula_f(Form) --> formula_2(Form), !, optws.
formula_f(Form) --> formula_1(Form), !, optws.
formula_f(Form) --> formula_0(Form), optws.
formula_0(Form) --> "(", !, optws, formula_f(Form), optws, ")", optws.
formula_0(Atom) --> goedel_atom(Atom), optws.
%formula_0 --> range_formula.
formula_1(not(Form)) --> "~", !, optws, formula_1_end(Form).
formula_1(some(Vars,Form)) --> "SOME", !, optws, "[", optws, variable_seq(Vars), optws, "]", optws,
    formula_1_end(Form), optws.
formula_1(all(Vars,Form)) --> "ALL", optws, "[", optws, variable_seq(Vars), optws, "]", optws,
    formula_1_end(Form), optws.
formula_1_end(Form) --> formula_1(Form).
formula_1_end(Form) --> formula_0(Form).
formula_2(Form) --> formula_2_if(Form), !.
formula_2(Form) --> formula_2_nif(Form).
formula_2_if(Form) --> "IF", optws, formula_2_mid(If), optws, "THEN", optws,
    then_part(Then), formula_2_else(If,Then,Form).
formula_2_nif(Form) --> formula_1(Form1), !, optws, formula_2_mid(Form1,Form), optws.
formula_2_nif(Form) --> formula_0(Form1), optws, formula_2_mid(Form1,Form), optws.
formula_2_mid(Form1,Form) --> "&", optws, formula_2_and(Form1,Form).
formula_2_and(Form1,Form) --> formula_1(Form2), optws, formula_2_mid(and(Form1,Form2),Form), !.
formula_2_and(Form1,Form) --> formula_0(Form2), optws, formula_2_mid(and(Form1,Form2),Form), !.
formula_2_and(Form1,Form) --> formula_2_end(Form2), {Form=and(Form1,Form2)}.
formula_2_end(Form) --> formula_2_if(Form), !. % "&" is filtered
formula_2_end(Form) --> formula_1(Form), !.
formula_2_end(Form) --> formula_0(Form).
formula_2_mid(some(Vars,Form)) --> "SOME", !, optws, "[", optws, variable_seq(Vars), optws, "]",
    optws, formula_f(Form).
formula_2_mid(Form) --> formula_f(Form).
formula_2_else(If,Then,if(If,Then,Else)) --> "ELSE", !, optws, formula_2_end(Else).
formula_2_else(If,Then,Form) --> {Form=if(If,Then)}.
formula_3(Form) --> formula_2(Form1), !, optws, formula_3_mid(Form1,Form), optws.
formula_3(Form) --> formula_1(Form1), !, optws, formula_3_mid(Form1,Form), optws.
formula_3(Form) --> formula_0(Form1), optws, formula_3_mid(Form1,Form), optws.
formula_3_mid(Form1,Form) --> "\\/", optws, formula_3_or(Form1,Form).
formula_3_or(Form1,Form) --> formula_2(Form2), optws, formula_3_mid(or(Form1,Form2),Form), !.
formula_3_or(Form1,Form) --> formula_1(Form2), optws, formula_3_mid(or(Form1,Form2),Form), !.
formula_3_or(Form1,Form) --> formula_0(Form2), optws, formula_3_mid(or(Form1,Form2),Form), !.
formula_3_or(Form1,Form) --> formula_3_end(Form2), {Form=or(Form1,Form2)}.
formula_3_end(Form) --> formula_2(Form), !.
formula_3_end(Form) --> formula_1(Form), !.
formula_3_end(Form) --> formula_0(Form).
formula_4(Form) --> formula_3(Form1), !, optws, formula_4_mid(Form1,Form), optws.
formula_4(Form) --> formula_2(Form1), !, optws, formula_4_mid(Form1,Form), optws.
formula_4(Form) --> formula_1(Form1), !, optws, formula_4_mid(Form1,Form), optws.
formula_4(Form) --> formula_0(Form1), optws, formula_4_mid(Form1,Form), optws.
formula_4_rlImpl(Form1,Form) --> formula_3(Form2), optws, formula_4_mid(rlImpl(Form1,Form2),Form), !.
formula_4_rlImpl(Form1,Form) --> formula_2(Form2), optws, formula_4_mid(rlImpl(Form1,Form2),Form), !.
formula_4_rlImpl(Form1,Form) --> formula_1(Form2), optws, formula_4_mid(rlImpl(Form1,Form2),Form), !.
formula_4_rlImpl(Form1,Form) --> formula_0(Form2), optws, formula_4_mid(rlImpl(Form1,Form2),Form), !.
formula_4_rlImpl(Form1,Form) --> formula_4_end(Form2), {Form=rlImpl(Form1,Form2)}.
formula_4_lrImpl(Form1,Form) --> formula_3(Form2), optws, formula_4_mid(lrImpl(Form1,Form2),Form), !.
formula_4_lrImpl(Form1,Form) --> formula_2(Form2), optws, formula_4_mid(lrImpl(Form1,Form2),Form), !.
formula_4_lrImpl(Form1,Form) --> formula_1(Form2), optws, formula_4_mid(lrImpl(Form1,Form2),Form), !.
formula_4_lrImpl(Form1,Form) --> formula_0(Form2), optws, formula_4_mid(lrImpl(Form1,Form2),Form), !.
formula_4_lrImpl(Form1,Form) --> formula_4_end(Form2), {Form=lrImpl(Form1,Form2)}.
formula_4_mid(Form1,equi(Form1,Form2)) --> "<->", !, optws, formula_4_end(Form2).
formula_4_mid(Form1,Form) --> "<-", !, optws, formula_4_rlImpl(Form1,Form).
formula_4_mid(Form1,Form) --> "->", optws, formula_4_lrImpl(Form1,Form).
formula_4_end(Form) --> formula_3(Form), !.
formula_4_end(Form) --> formula_2(Form), !.
formula_4_end(Form) --> formula_1(Form), !.
formula_4_end(Form) --> formula_0(Form).
then_part(Form) --> then_part_if(Form), !.
then_part(Form) --> then_part_nif(Form).
then_part_if(if(If,Then)) --> "IF", optws, formula_f(If), optws,
    "THEN", optws , then_part(Then), optws.
then_part_nif(Form) --> formula_1(Form1), !, optws, then_part_mid(Form1,Form), optws.
then_part_nif(Form) --> formula_0(Form1), ! ,optws, then_part_mid(Form1,Form), optws.
then_part_nif(Form) --> then_part_end(Form), optws.
then_part_mid(Form1,Form) --> "&", optws, then_part_and(Form1,Form).
then_part_and(Form1,Form) --> formula_1(Form2), optws, then_part_mid(and(Form1,Form2),Form), !.
then_part_and(Form1,Form) --> formula_0(Form2), optws, then_part_mid(and(Form1,Form2),Form), !.
then_part_and(Form1,Form) --> then_part_end(Form2), {Form=and(Form1,Form2)}.
then_part_end(Form) --> then_part_if(Form), !.
then_part_end(Form) --> formula_1(Form), !.
then_part_end(Form) --> formula_0(Form).
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
term(Term) --> "(", !, optws, term(Term), optws, ")".
term(Term) --> user_name(Name), optws, term_end(Name,Term).
term_end(Name,func(Name,N,[Term|Terms])) --> "(", !, optws, term(Term), optws, opt_terms(1,N,Terms), ")".
term_end(Name,const(Name)) --> "".
opt_terms(C,N,[Term|Terms]) --> comma, !, optws, term(Term), optws, {C1 is C+1}, opt_terms(C1,N,Terms).
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


/* ------------ */
/* Type Checker */
/* ------------ */
:- dynamic curr/2. % used to track ModuleName and Part while type checking
:- dynamic checked/1. % used to catch type errors

reset_curr :-
    retractall(curr(_,_)).
set_curr(ModName,Part) :-
    retractall(curr(_,_)),
    assert(curr(ModName,Part)).
reset_checked :-
    retractall(checked(_)),
    assert(checked('no type errors')).
fail_checked :-
    retract(checked(_)),
    assert(checked('type error')),
    fail.

check_types :-
    reset_checked,
    reset_curr,
    check_routine,
    (checked('no type errors') -> true; fail).
check_routine :-
    ast(ModName,exp(_,ELang,EContrl),loc(_,_,LLang,LContrl,Stms)),
    ELang=lang(_,_,EConst,EFunc,_,EPred),
    LLang=lang(_,_,LConst,LFunc,_,LPred),
    format('~nStarting type checking Module: ~w~n',[ModName]),
    check_imports(ModName,[]),
    set_curr(ModName,exp),
    check_consts(EConst),
    check_funcs(EFunc),
    check_preds(EPred),
    check_contrls(EContrl),
    set_curr(ModName,loc),
    check_consts(LConst),
    check_funcs(LFunc),
    check_preds(LPred),
    check_contrls(LContrl),
    check_stms(Stms),
    retractall(curr(_,_)),
    format('Finished type checking~n~n'),
    fail.
check_routine.

check_imports([],_) :- !.
check_imports([H|T],C) :-
    !, check_imports(H,C),
    check_imports(T,C).
check_imports(Name,C) :-
    (\+member(Name,C) -> true
     ; (write('Module cant import itself (looped): '), print_quoted(Name), nl, fail_checked)
    ),
    ast(Name,exp(EI,_,_),loc(LI,L,_,_,_)),
    append(EI,LI,Tmp),
    append(Tmp,L,Imports),
    (\+member(Name,Imports) -> true
     ; (write('Module cant import itself: '), print_quoted(Name), nl, fail_checked)
    ),
    check_imports(Imports,[Name|C]).
% Constant cant have only a parameter as type
check_consts([]) :- !.
check_consts([const(_Name,Type)|T]) :- check(Type,type,[],_), check_consts(T).
% Function cant have only a parameter as type
% Function must be transparent
check_funcs([]) :- !.
check_funcs([func(Name,Arity,_,_,Types,Range)|T]) :-
    check_terms(Types,_,[],Params1), check(Range,type,[],Params2),
    (Params1=Params2 -> true
     ; ( write('Function must be transparent: '), print_quoted(func(Name,Arity,Types,Range)),
         nl, fail_checked
       )
    ),
    check_funcs(T).
check_preds([]) :- !.
check_preds([pred(_,_,_,Types)|T]) :- check_terms(Types,_,[],_), check_preds(T).
check_contrls(Contrls) :- check_contrls(Contrls,[]).
check_contrls([],_) :- !.
check_contrls([delay(Atom,Cond)|T],I) :-
    (Atom=pred(Name,Arity,Types) -> true
     ; ( write('Atom in delay condition must a predicate: '), print_quoted(pred(Name,Arity,Types)),
         nl, fail_checked)
    ),
    check(pred(Name,Arity,Types),predicate,[],Vars1),
    check(Cond,predicate,[],Vars2),
    acc_pred_nimp(Name,Arity), % Delay must be in same module as declaration
    (containing(Vars2,Vars1) -> true
     ; ( write('Variables in delay condition must apear in Atom: '), print_quoted(pred(Name,Arity,Types)),
         nl, fail_checked)
    ),
    l_mkng(Types,ITypes,[],_),
    (\+member(pred(Name,Arity,ITypes),I) -> true
     ; ( write('Delays cant have common instance: '), print_quoted(pred(Name,Arity,Types)),
          nl, fail_checked)
    ),
    check_contrls(T,[pred(Name,Arity,ITypes)|I]).
check_stms([]) :- !.
check_stms([H|T]) :- check(H,stm), check_stms(T).

containing([],_) :- !.
containing([H|T],L) :- member(H,L), containing(T,L).

% An adaption of "A small demo of Hindley Milner Type Inference using
% Unification" from Michael Leuschel
% checking declarations
check(par(Name),typePar) --> defined(Name), !.
check(par(Name),typePar) --> !, add(Name). % tracks Parameters
check(constr(Name,Arity,Terms),type) --> !, check_terms(Terms,_Types),
    {acc_constr(Name,Arity) -> true
     ; (write('Could not find Constr: '), print_quoted(constr(Name,Arity)), nl, fail_checked)
    }.
check(base(Name),type) --> !,
    {acc_base(Name) -> true
     ; (write('Could not find Base: '), print_quoted(base(Name)), nl, fail_checked)
    }.
% checking controls
check(nvar(Name),predicate) --> defined(var(Name,_)), !.
check(nvar(Name),predicate) --> !, add(var(Name,_)).
check(grnd(Name),predicate) --> defined(var(Name,_)), !.
check(grnd(Name),predicate) --> !, add(var(Name,_)).
% checking statements
check(stm(Head,Body),stm) --> !, check_head(Head), check(Body,predicate).
check(var(Var),TVar) --> defined(var(Var,Type)), !,
    {TVar=Type -> true
     ; (write('Variable cant have 2 types: '), print_quoted(var(Var,TVar,Type)), nl, fail_checked)
    }.
check(var(Var),TVar) --> !, add(var(Var,TVar)). % creates fresh variable
check(fl(_Fl),base('Float')) --> !, [].
check(nr(_Nr),base('Integer')) --> !, [].
check(str(_Str),base('String')) --> !, [].
check(func(Name,Arity,Terms),T) --> !, check_terms(Terms,Types),
    {acc_func(Name,Arity,Types,T) -> true
     ; (write('Could not find Function: '), print_quoted(func(Name,Arity,Types,T)), nl, fail_checked)
    }.
check(const(Name),T) --> !,
    {acc_const(Name,T) -> true
     ; (write('Could not find Constant: '), print_quoted(const(Name,T)), nl, fail_checked)
    }.
check(true,predicate) --> !, [].
check(false,predicate) --> !, [].
check(and(A,B),predicate) --> !, check(A,predicate), check(B,predicate).
check(or(A,B),predicate) --> !, check(A,predicate), check(B,predicate).
check(not(A),predicate) --> !, check(A,predicate).
check(rlImpl(A,B),predicate) --> !, check(A,predicate), check(B,predicate).
check(lrImpl(A,B),predicate) --> !, check(A,predicate), check(B,predicate).
check(equi(A,B),predicate) --> !, check(A,predicate), check(B,predicate).
check(eq(A,B),predicate) --> !, check(A,TA), check(B,TA).
check(pred(Name,Arity,Terms),predicate) --> !, check_terms(Terms,Types),
    {acc_pred(Name,Arity,Types) -> true
     ; (write('Could not find Predicate: '), print_quoted(pred(Name,Arity,Types)), nl, fail_checked)
    }.
check(prop(Name),predicate) --> !,
    {acc_prop(Name) -> true
     ; (write('Could not find Proposition: '), print_quoted(prop(Name)), nl, fail_checked)
    }.
check(some(_Vars,Expr),predicate) --> !, check(Expr,predicate).
check(all(_Vars,Expr),predicate) --> !, check(Expr,predicate).
check(if(A,B,C),predicate) --> !, check(A,predicate), check(B,predicate), check(C,predicate).
check(if(A,B), predicate) --> !, check(A,predicate), check(B,predicate).
check(Expr,T,Env,_) :- nonvar(T), !, format('Type error: ~w  (Expected: ~w, Env: ~w)~n',[Expr,T,Env]),
    fail_checked.
check(Expr,_,Env,_) :- format('Illegal expression: ~w  (Env: ~w)~n',[Expr,Env]), fail_checked.
check_head(pred(Name,Arity,Terms)) --> !,
    {get_pred(Name,Arity,Types) -> true
     ; (write('Could not find Predicate: '), print_quoted(pred(Name,Arity,Types)), nl, fail_checked)
    },
    check_terms(Terms,Types).
check_head(prop(Name)) --> !,
    {get_prop(Name) -> true
     ; (write('Could not find Proposition: '), print_quoted(prop(Name)), nl, fail_checked)
    }.
check_head(Expr) --> {format('Illegal head expression: ~w~n',[Expr]), fail_checked}.
check_terms([],[]) --> !, [].
check_terms([H|T],[HT|Types]) --> check(H,HT), check_terms(T,Types).

defined(X,Env,Env) :- member(X,Env).
add(X,Env,[X|Env]).

check(Expr,Result) :- check(Expr,Result,[],Env), write('Typing env: '), portray_clause(Env).


/* --------------- */
/* Checking access */
/* --------------- */

% Base is Name
acc_base(Base) :- member(Base,['Float','Integer','String']).
acc_base(Base) :-
    curr(ModName,Part),
    (Part=exp -> exp_acc_base(ModName,Base)
     ; loc_acc_base(ModName,Base)
    ).
exp_acc_base(ModName,Base) :-
    ast(ModName,exp(_,lang(EBase,_,_,_,_,_),_),_),
    member(Base,EBase), !.
exp_acc_base(ModName,Base) :-
    ast(ModName,exp(EImp,_,_),_),
    imp_acc_base(EImp,Base).
loc_acc_base(ModName,Base) :-
    ast(ModName,exp(_,lang(EBase,_,_,_,_,_),_),loc(_,_,lang(LBase,_,_,_,_,_),_,_)),
    append(LBase,EBase,Bases),
    member(Base,Bases), !.
loc_acc_base(ModName,Base) :-
    ast(ModName,exp(EImp,_,_),loc(LImp,Lift,_,_,_)),
    append(LImp,Lift,Tmp),
    append(Tmp,EImp,Imports),
    imp_acc_base(Imports,Base).
imp_acc_base([H|T],Base) :-
    (exp_acc_base(H,Base) -> (true, !)
     ; imp_acc_base(T,Base)
    ).

% Constr is constr(Name,Arity)
acc_constr(Name,Arity) :-
    curr(ModName,Part),
    (Part=exp -> exp_acc_constr(ModName,constr(Name,Arity))
     ; loc_acc_constr(ModName,constr(Name,Arity))
    ).
exp_acc_constr(ModName,Constr) :-
    ast(ModName,exp(_,lang(_,EConstr,_,_,_,_),_),_),
    member(Constr,EConstr), !.
exp_acc_constr(ModName,Constr) :-
    ast(ModName,exp(EImp,_,_),_),
    imp_acc_constr(EImp,Constr).
loc_acc_constr(ModName,Constr) :-
    ast(ModName,exp(_,lang(_,EConstr,_,_,_,_),_),loc(_,_,lang(_,LConstr,_,_,_,_),_,_)),
    append(LConstr,EConstr,Constrs),
    member(Constr,Constrs), !.
loc_acc_constr(ModName,Constr) :-
    ast(ModName,exp(EImp,_,_),loc(LImp,Lift,_,_,_)),
    append(LImp,Lift,Tmp),
    append(Tmp,EImp,Imports),
    imp_acc_constr(Imports,Constr).
imp_acc_constr([H|T],Constr) :-
    (exp_acc_constr(H,Constr) -> (true, !)
     ; imp_acc_constr(T,Constr)
    ).

% Const is Name
acc_const(Const,T) :-
    curr(ModName,Part),
    (Part=exp -> exp_acc_const(ModName,const(Const,PT))
     ; loc_acc_const(ModName,const(Const,PT))
    ),
    lift_params(PT,ITypes,[],_),
    ITypes=T.
% Const is const(Name,Type)
exp_acc_const(ModName,Const) :-
    ast(ModName,exp(_,lang(_,_,EConst,_,_,_),_),_),
    member(Const,EConst), !.
exp_acc_const(ModName,Const) :-
    ast(ModName,exp(EImp,_,_),_),
    imp_acc_const(EImp,Const).
loc_acc_const(ModName,Const) :-
    ast(ModName,exp(_,lang(_,_,EConst,_,_,_),_),loc(_,_,lang(_,_,LConst,_,_,_),_,_)),
    append(LConst,EConst,Consts),
    member(Const,Consts), !.
loc_acc_const(ModName,Const) :-
    ast(ModName,exp(EImp,_,_),loc(LImp,Lift,_,_,_)),
    append(LImp,Lift,Tmp),
    append(Tmp,EImp,Imports),
    imp_acc_const(Imports,Const).
imp_acc_const([H|T],Const) :-
    (exp_acc_const(H,Const) -> (true, !)
     ; imp_acc_const(T,Const)
    ).

% Func is func(Name,Arity,Specifier,Prio,Types,RangeType)
acc_func(Name,Arity,Types,T) :-
    curr(ModName,Part),
    (Part=exp -> exp_acc_func(ModName,func(Name,Arity,_Spec,_Prio,PTypes,PT))
     ; loc_acc_func(ModName,func(Name,Arity,_Spec,_Prio,PTypes,PT))
    ),
    lift_params(PTypes,ITypes,[],Sub),
    lift_params(PT,IT,Sub,_),
    ITypes=Types,
    IT=T.
exp_acc_func(ModName,Func) :-
    ast(ModName,exp(_,lang(_,_,_,EFunc,_,_),_),_),
    member(Func,EFunc), !.
exp_acc_func(ModName,Func) :-
    ast(ModName,exp(EImp,_,_),_),
    imp_acc_func(EImp,Func).
loc_acc_func(ModName,Func) :-
    ast(ModName,exp(_,lang(_,_,_,EFunc,_,_),_),loc(_,_,lang(_,_,_,LFunc,_,_),_,_)),
    append(LFunc,EFunc,Funcs),
    member(Func,Funcs), !.
loc_acc_func(ModName,Func) :-
    ast(ModName,exp(EImp,_,_),loc(LImp,Lift,_,_,_)),
    append(LImp,Lift,Tmp),
    append(Tmp,EImp,Imports),
    imp_acc_func(Imports,Func).
imp_acc_func([H|T],Func) :-
    (exp_acc_func(H,Func) -> (true, !)
     ; imp_acc_func(T,Func)
    ).

% Prop is Name
get_prop(Prop) :-
    curr(ModName,_),
    ast(ModName,exp(_,lang(_,_,_,_,EProp,_),_),loc(_,_,lang(_,_,_,_,LProp,_),_,_)),
    (member(Prop,EProp) -> true
      ; member(Prop,LProp)
    ).
acc_prop(Prop) :-
    curr(ModName,Part),
    (Part=exp -> exp_acc_prop(ModName,Prop)
     ; loc_acc_prop(ModName,Prop)
    ).
exp_acc_prop(ModName,Prop) :-
    ast(ModName,exp(_,lang(_,_,_,_,EProp,_),_),_),
    member(Prop,EProp), !.
exp_acc_prop(ModName,Prop) :-
    ast(ModName,exp(EImp,_,_),_),
    imp_acc_prop(EImp,Prop).
loc_acc_prop(ModName,Prop) :-
    ast(ModName,exp(_,lang(_,_,_,_,EProp,_),_),loc(_,_,lang(_,_,_,_,LProp,_),_,_)),
    append(LProp,EProp,Props),
    member(Prop,Props), !.
loc_acc_prop(ModName,Prop) :-
    ast(ModName,exp(EImp,_,_),loc(LImp,Lift,_,_,_)),
    append(LImp,Lift,Tmp),
    append(Tmp,EImp,Imports),
    imp_acc_prop(Imports,Prop).
imp_acc_prop([H|T],Prop) :-
    (exp_acc_prop(H,Prop) -> (true, !)
     ; imp_acc_prop(T,Prop)
    ).


% Pred is pred(Name,Arity,Specifier,Types)

get_pred(Name,Arity,Types) :-
    curr(ModName,_),
    ast(ModName,exp(_,lang(_,_,_,_,_,EPred),_),loc(_,_,lang(_,_,_,_,_,LPred),_,_)),
    (member(pred(Name,Arity,_Spec1,PTypes),EPred) -> true
      ;  member(pred(Name,Arity,_Spec2,PTypes),LPred)
    ),
    lift_params(PTypes,ITypes,[],_),
    ITypes=Types.
acc_pred(Name,Arity,Types) :-
    curr(ModName,Part),
    (Part=exp -> exp_acc_pred(ModName,pred(Name,Arity,_Spec,PTypes))
     ; loc_acc_pred(ModName,pred(Name,Arity,_Spec,PTypes))
    ),
    lift_params(PTypes,ITypes,[],_),
    ITypes=Types.
exp_acc_pred(ModName,Pred) :-
    ast(ModName,exp(_,lang(_,_,_,_,_,EPred),_),_),
    member(Pred,EPred), !.
exp_acc_pred(ModName,Pred) :-
    ast(ModName,exp(EImp,_,_),_),
    imp_acc_pred(EImp,Pred).
loc_acc_pred(ModName,Pred) :-
    ast(ModName,exp(_,lang(_,_,_,_,_,EPred),_),loc(_,_,lang(_,_,_,_,_,LPred),_,_)),
    append(LPred,EPred,Preds),
    member(Pred,Preds), !.
loc_acc_pred(ModName,Pred) :-
    ast(ModName,exp(EImp,_,_),loc(LImp,Lift,_,_,_)),
    append(LImp,Lift,Tmp),
    append(Tmp,EImp,Imports),
    imp_acc_pred(Imports,Pred).
imp_acc_pred([H|T],Func) :-
    (exp_acc_pred(H,Func) -> (true, !)
     ; imp_acc_pred(T,Func)
    ).
% used to check control declarations
acc_pred_nimp(Name,Arity):-
    curr(ModName,Part),
    (Part=exp -> exp_acc_pred_nimp(ModName,pred(Name,Arity,_Spec,_PTypes))
     ; loc_acc_pred_nimp(ModName,pred(Name,Arity,_Spec,_PTypes))
    ).
exp_acc_pred_nimp(ModName,Pred) :-
    ast(ModName,exp(_,lang(_,_,_,_,_,EPred),_),_),
    member(Pred,EPred), !.
loc_acc_pred_nimp(ModName,Pred) :-
    ast(ModName,exp(_,lang(_,_,_,_,_,EPred),_),loc(_,_,lang(_,_,_,_,_,LPred),_,_)),
    append(LPred,EPred,Preds),
    member(Pred,Preds), !.

% par(Name) must be lifted to prolog variable
% variable can than be unified with bases/constructors
lift_params([],[]) --> !, [].
lift_params([H|T],[IH|IT]) --> !, lift_params(H,IH), lift_params(T,IT).
lift_params(par(Name),X) --> !, param_var(Name,X).
lift_params(base(Name),base(Name)) --> !, [].
lift_params(constr(Name,Arity,Args),constr(Name,Arity,IArgs)) --> !, lift_params(Args,IArgs).
param_var(Name,X,[],[sub(Name,X)]) :- !.
param_var(Name,X,[sub(N,Y)|T1],[sub(N,Y)|T2]) :-
    (Name==N -> (X=Y, T1=T2)
     ; param_var(Name,X,T1,T2)
    ).


/* ----------- */
/* Interpreter */
/* ----------- */

:- dynamic main/1.
set_main(Name) :-
    retractall(main(_)),
    assert(main(Name)).

goal(Goal) :-
    string_codes(Goal,Codes),
    goal(Form,Codes,[]),
    main(Main),
    reset_checked,
    set_curr(Main,loc), % Goal must be in language of main module (loc has access to loc and exp)
    check(Form,predicate,[],_),
    retractall(curr(_,_)),
    (checked('no type errors') -> true; fail),
    interprete(Form).
goal(Form) --> "<-", optws, goal_end(Form).
goal_end(Form) --> ":", optws, body(Form).
goal_end(Form) --> variable_seq(_Vars), optws, ":", optws, body(Form).
goal_end(Form) --> body(Form).
interprete(Form) :-
    mkng(Form,Goal,[],Vars),
    format('Goal is:~n'),
    portray_clause(Goal),
    format('Result is:~n'),
    call(Goal),
    print_vars(Vars).
print_vars([]) :- !.
print_vars([sub(Name,_Value)|T]) :-
    atom_concat('_',_,Name), !,
    print_vars(T).
print_vars([sub(Name,Value)|T]) :-
    print_quoted(Name), write(' = '), print_quoted(Value), format(',~n'),
    print_vars(T).

:-dynamic stm/2.
reset_stm :- retractall(stm(_,_)).

extract_statements :-
    ast(_ModName,_,loc(_,_,_,_,Statements)),
    extract_statements(Statements),
    fail.
extract_statements.
extract_statements([]) :- !.
extract_statements([stm(Head,Body)|T]) :-
    assert(stm(Head,Body)),
    extract_statements(T).



% An adaption of "Lifting Code" from Michael Leuschel:
% demo on how to lift and pretty-print statements
lift_pp :-
    stm(Head,Body),
    mkng(Head,NVHead,[],Sub),
    mkng(Body,NVBody,Sub,_),
    portray_clause((NVHead :- NVBody)),nl,
    assert((NVHead :- NVBody)),
    fail.
lift_pp.

% an adaption of InstanceDemo from Michael Leuschel's script for "Vertiefung Logische Programmierung"

mkng(vars([]),[]) --> [].
mkng(vars([H|T]),[IH|IT]) -->
    lookup_var(H,IH),
    mkng(vars(T),IT).
mkng(some(Vars,Form),LiftForm) -->
    mkng(vars(Vars),_LiftVars),
    mkng(Form,LiftForm).
% Not Working
mkng(all(Vars,Form),LiftForm) -->
    mkng(vars(Vars),_LiftVars),
    mkng(Form,LiftForm).
mkng(if(If,Then),(LiftIf -> LiftThen)) -->
    mkng(If,LiftIf),
    mkng(Then,LiftThen).
mkng(if(If,Then,Else),(LiftIf -> LiftThen ; LiftElse)) -->
    mkng(If,LiftIf),
    mkng(Then,LiftThen),
    mkng(Else,LiftElse).
mkng(rlImpl(A,B),(LiftB -> LiftA)) -->
    mkng(A,LiftA),
    mkng(B,LiftB).
mkng(lrImpl(A,B),(LiftA -> LiftB)) -->
    mkng(A,LiftA),
    mkng(B,LiftB).
mkng(equi(A,B),(LiftA == LiftB)) -->
    mkng(A,LiftA),
    mkng(B,LiftB).
mkng(not(T),\+(LiftT)) --> mkng(T,LiftT).
mkng(eq(A,B),(LiftA = LiftB)) -->
    mkng(A,LiftA),
    mkng(B,LiftB).
mkng(fl(F),F) --> [].
mkng(nr(N),N) --> [].
mkng(str(S),S) --> [].
mkng(true,true) --> [].
mkng(false,false) --> [].
mkng(prop(X),X) --> [].
mkng(const(C),C) --> [].
mkng(var(N),X) -->
	lookup_var(N,X).
mkng(pred(F,Arity,Args),Term) -->
    mkng(func(F,Arity,Args),Term).
mkng(func(F,_,Args),Term) -->
	l_mkng(Args,IArgs),
	{Term =.. [F|IArgs]}.
mkng(and(A,B),(LiftA,LiftB)) -->
    mkng(A,LiftA),
    mkng(B,LiftB).
mkng(or(A,B),(LiftA ; LiftB)) -->
    mkng(A,LiftA),
    mkng(B,LiftB).

l_mkng([],[],Sub,Sub).
l_mkng([H|T],[IH|IT],InSub,OutSub) :-
	mkng(H,IH,InSub,IntSub),
	l_mkng(T,IT,IntSub,OutSub).

lookup_var(N,X,[],[sub(N,X)]) :- !.
lookup_var(N,X,[sub(M,Y)|T],[sub(M,Y)|T1]) :-
   (M==N -> X=Y, T1=T
    ; lookup_var(N,X,T,T1)).


/* ----------------- */
/* helper predicates */
/* ----------------- */

reset :-
    reset_line_count,
    reset_ast,
    reset_checked,
    reset_curr.

:- dynamic ast/3.

reset_ast :-
    retractall(ast(_,_,_)).

add_module(ModName) :-
    % lang(base,constr,const,func,prop,pred)
    Lang=lang([],[],[],[],[],[]),
    % exp(imp,lang,cont)
    Exp=exp([],Lang,[]),
    % loc(imp,lift,lang,cont,stmnt)
    Loc=loc([],[],Lang,[],[]),
    assert(ast(ModName,Exp,Loc)),
    % Parser can only handle 1 module, that allows setting main module here
    set_main(ModName).

% Adding items to AST

add_exp_imports(ModName,Imports) :-
    retract(ast(ModName,exp(Imp,Lang,Cont),Loc)),
    append(Imp,Imports,New),
    assert(ast(ModName,exp(New,Lang,Cont),Loc)).

add_loc_imports(ModName,Imports) :-
    retract(ast(ModName,Exp,loc(Imp,Lift,Lang,Cont,Stmnt))),
    append(Imp,Imports,New),
    assert(ast(ModName,Exp,loc(New,Lift,Lang,Cont,Stmnt))).

add_lifts(ModName,Lifts) :-
    retract(ast(ModName,Exp,loc(Imp,Lift,Lang,Cont,Stmnt))),
    append(Lift,Lifts,New),
    assert(ast(ModName,Exp,loc(Imp,New,Lang,Cont,Stmnt))).

add_exp_controls(ModName,Controls) :-
    retract(ast(ModName,exp(Imp,Lang,Cont),Loc)),
    append(Cont,Controls,New),
    assert(ast(ModName,exp(Imp,Lang,New),Loc)).

add_loc_controls(ModName,Controls) :-
    retract(ast(ModName,Exp,loc(Imp,Lift,Lang,Cont,Stmnt))),
    append(Cont,Controls,New),
    assert(ast(ModName,Exp,loc(Imp,Lift,Lang,New,Stmnt))).

add_statement(ModName,Statement) :-
    retract(ast(ModName,Exp,loc(Imp,Lift,Lang,Cont,Stmnt))),
    append(Stmnt,[Statement],New),
    assert(ast(ModName,Exp,loc(Imp,Lift,Lang,Cont,New))).

% Adding Language Declarations to ast

add_bases(ModName,loc,Bases) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,LConstr,LConst,LFunc,LProp,LPred),
    ELang=lang(EBase,_,_,_,_,_),
    append(LBase,Bases,New),
    append(New,EBase,All),
    is_set(All), !,
    NewLang=lang(New,LConstr,LConst,LFunc,LProp,LPred),
    assert(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,NewLang,LCont,Stmnt))).
add_bases(ModName,exp,Bases) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,_,_,_,_,_),
    ELang=lang(EBase,EConstr,EConst,EFunc,EProp,EPred),
    append(EBase,Bases,New),
    append(New,LBase,All),
    is_set(All), !,
    NewLang=lang(New,EConstr,EConst,EFunc,EProp,EPred),
    assert(ast(ModName,exp(EImp,NewLang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))).
add_bases(ModName,_,_) :- format('Base double declared in module: ~w~n',[ModName]), fail.

add_constrs(ModName,loc,Constrs) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,LConstr,LConst,LFunc,LProp,LPred),
    ELang=lang(_,EConstr,_,_,_,_),
    append(LConstr,Constrs,New),
    append(New,EConstr,All),
    is_set(All), !,
    NewLang=lang(LBase,New,LConst,LFunc,LProp,LPred),
    assert(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,NewLang,LCont,Stmnt))).
add_constrs(ModName,exp,Constrs) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(_,LConstr,_,_,_,_),
    ELang=lang(EBase,EConstr,EConst,EFunc,EProp,EPred),
    append(EConstr,Constrs,New),
    append(New,LConstr,All),
    is_set(All), !,
    NewLang=lang(EBase,New,EConst,EFunc,EProp,EPred),
    assert(ast(ModName,exp(EImp,NewLang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))).
add_constrs(ModName,_,_) :- format('Constructor double declared in module: ~w~n',[ModName]), fail.

add_consts(ModName,loc,Consts) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,LConstr,LConst,LFunc,LProp,LPred),
    ELang=lang(_,_,EConst,_,_,_),
    append(LConst,Consts,New),
    append(New,EConst,All),
    match_consts(All), !,
    NewLang=lang(LBase,LConstr,New,LFunc,LProp,LPred),
    assert(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,NewLang,LCont,Stmnt))).
add_consts(ModName,exp,Consts) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(_,_,LConst,_,_,_),
    ELang=lang(EBase,EConstr,EConst,EFunc,EProp,EPred),
    append(EConst,Consts,New),
    append(New,LConst,All),
    match_consts(All), !,
    NewLang=lang(EBase,EConstr,New,EFunc,EProp,EPred),
    assert(ast(ModName,exp(EImp,NewLang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))).
add_consts(ModName,_,_) :- format('Constant double declared in module: ~w~n',[ModName]), fail.

add_funcs(ModName,loc,Funcs) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,LConstr,LConst,LFunc,LProp,LPred),
    ELang=lang(_,_,_,EFunc,_,_),
    append(LFunc,Funcs,New),
    append(New,EFunc,All),
    match_funcs(All), !,
    NewLang=lang(LBase,LConstr,LConst,New,LProp,LPred),
    assert(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,NewLang,LCont,Stmnt))).
add_funcs(ModName,exp,Funcs) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(_,_,_,LFunc,_,_),
    ELang=lang(EBase,EConstr,EConst,EFunc,EProp,EPred),
    append(EFunc,Funcs,New),
    append(New,LFunc,All),
    match_funcs(All), !,
    NewLang=lang(EBase,EConstr,EConst,New,EProp,EPred),
    assert(ast(ModName,exp(EImp,NewLang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))).
add_funcs(ModName,_,_) :- format('Function double declared in module: ~w~n',[ModName]), fail.

add_props(ModName,loc,Props) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,LConstr,LConst,LFunc,LProp,LPred),
    ELang=lang(_,_,_,_,EProp,_),
    append(LProp,Props,New),
    append(New,EProp,All),
    is_set(All), !,
    NewLang=lang(LBase,LConstr,LConst,LFunc,New,LPred),
    assert(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,NewLang,LCont,Stmnt))).
add_props(ModName,exp,Props) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(_,_,_,_,LProp,_),
    ELang=lang(EBase,EConstr,EConst,EFunc,EProp,EPred),
    append(EProp,Props,New),
    append(New,LProp,All),
    is_set(All), !,
    NewLang=lang(EBase,EConstr,EConst,EFunc,New,EPred),
    assert(ast(ModName,exp(EImp,NewLang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))).
add_props(ModName,_,_) :- format('Proposition double declared in module: ~w~n',[ModName]), fail.

add_preds(ModName,loc,Preds) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(LBase,LConstr,LConst,LFunc,LProp,LPred),
    ELang=lang(_,_,_,_,_,EPred),
    append(LPred,Preds,New),
    append(New,EPred,All),
    match_preds(All), !,
    NewLang=lang(LBase,LConstr,LConst,LFunc,LProp,New),
    assert(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,NewLang,LCont,Stmnt))).
add_preds(ModName,exp,Preds) :-
    retract(ast(ModName,exp(EImp,ELang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))),
    LLang=lang(_,_,_,_,_,LPred),
    ELang=lang(EBase,EConstr,EConst,EFunc,EProp,EPred),
    append(EPred,Preds,New),
    append(New,LPred,All),
    match_preds(All), !,
    NewLang=lang(EBase,EConstr,EConst,EFunc,EProp,New),
    assert(ast(ModName,exp(EImp,NewLang,ECont),loc(LImp,Lift,LLang,LCont,Stmnt))).
add_preds(ModName,_,_) :- format('Predicate double declared in module: ~w~n',[ModName]), fail.

% Match Lists

match_consts([]) :- !.
match_consts([const(Name,_)|T]) :-
    \+member(const(Name,_),T),
    match_consts(T).

match_funcs([]) :- !.
match_funcs([func(Name,Arity,_,_,_,_)|T]) :-
    \+member(func(Name,Arity,_,_,_,_),T),
    match_funcs(T).

match_preds([]) :- !.
match_preds([pred(Name,Arity,_,_)|T]) :-
    \+member(pred(Name,Arity,_,_),T),
    match_preds(T).

% Building Lists

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
optws --> layout_item(_Ch), !, optws.
optws --> "".

:- dynamic cur_line_number/1.
cur_line_number(1).

print_line_count :-
    cur_line_number(N),
    format('at line ~w',[N]).

increment_line_count :-
    retract(cur_line_number(N)),
    N1 is N+1,
    assert(cur_line_number(N1)).

reset_line_count :-
    retractall(cur_line_number(_N)),
    assert(cur_line_number(1)).
