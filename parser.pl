/******************************************************************************/
/* Prolog Lab 3 example - Grammar test bed                                    */
/******************************************************************************/

/******************************************************************************/
/* The Parser                                                                 */
/******************************************************************************/

parser(Tokens, Res) :- (prog(Tokens, Res), Res = [], write('Parse Succeed!')); write(Res), nl, write('Parse Fail!').


/******************************************************************************/
/* The Terminals = "Facts"                                                    */
/******************************************************************************/

program   --> [256].
input     --> [257].
output    --> [258].
var       --> [259].
integer   --> [260].
begin     --> [261].
end       --> [262].
real      --> [263].
boolean   --> [264].
id        --> [270].
assign_op --> [271].  
number    --> [272].
undefined --> [273].
/*EOF       --> [275].*/

lp        --> [40].
rp        --> [41].
mul       --> [42].
add       --> [43].
comma     --> [44].
punkt     --> [46].
colon     --> [58].
scolon    --> [59].
equal     --> [68].


/******************************************************************************/
/* The Rules                                                                  */
/******************************************************************************/

prog --> prog_head, var_part, stat_part.

/******************************************************************************/
/* Program Header                                                             */
/******************************************************************************/
prog_head --> program, id, lp, input, comma, output, rp, scolon.
    

/******************************************************************************/
/* Var_part                                                                   */
/******************************************************************************/

var_part      --> var, var_dec_list.

var_dec_list  --> var_dec.
var_dec_list  --> var_dec, var_dec_list.

var_dec       --> id_list, colon, type, scolon.

id_list       --> id.
id_list       --> id, comma, id_list.

/*https://stackoverflow.com/a/40682737*/

type          --> integer.
type          --> real.
type          --> boolean.

/******************************************************************************/
/* Stat part                                                                  */
/******************************************************************************/

stat_part   --> begin, stat_list, end, punkt.

stat_list   --> stat.
stat_list   --> stat, scolon, stat_list.

stat        --> assign_stat.

assign_stat --> id, assign_op, expr.

expr        --> term, add, expr.
expr        --> term.

term        --> factor, mul, term.
term        --> factor.

factor      --> lp, expr, rp.
factor      --> operand.

operand     --> id.
operand     --> number.

/******************************************************************************/
/* The Lexical Analyser                                                       */
/******************************************************************************/

test_lexer(File, X) :- read_in(File, L), lexer(L, X), write(X).



lexer([],[]).
lexer([H|T], [F|S]) :- match(H, F), lexer(T, S).


/******************************************************************************/
/* The match                                                                  */
/******************************************************************************/

match(L,F) :- L = 'program',  F is 256.
match(L,F) :- L = 'input',    F is 257.
match(L,F) :- L = 'output',   F is 258.
match(L,F) :- L = 'var',      F is 259.
match(L,F) :- L = 'integer',  F is 260.
match(L,F) :- L = 'begin',    F is 261.
match(L,F) :- L = 'end',      F is 262.
match(L,F) :- L = 'boolean',  F is 263.
match(L,F) :- L = 'real',     F is 264.

match(L,F) :- L = '(', F is 40.
match(L,F) :- L = ')', F is 41.
match(L,F) :- L = '*', F is 42.
match(L,F) :- L = '+', F is 43.
match(L,F) :- L = ',', F is 44.
match(L,F) :- L = ';', F is 59.
match(L,F) :- L = ':', F is 58.
match(L,F) :- L = '=', F is 68.
match(L,F) :- L = '.', F is 46.
match(L,F) :- L = ':=', F is 271.

match(L, T) :- name(L,[H|Tail]), char_type(H, digit), match_num(Tail), T is 272. /* kollar numer */
match(L, T) :- name(L,[H|Tail]), char_type(H, alpha), match_alp(Tail), T is 270. /* kollar id */


match_num([]).
match_num([H|T]) :- char_type(H, digit), match_num(T).

match_alp([]).
match_alp([H|T]) :- char_type(H, alpha), match_alp(T).   /*kollar om första teknet är en bokstav*/
match_alp([H|T]) :- char_type(H, digit), match_alp(T).   /*Viktigt kan innehålla en siffra också*/


/******************************************************************************/
/*THE READER                                                                   */
/******************************************************************************/

lab3(File, Result) :- read_in(File, L), write(L), nl, 
                      lexer(L, Tokens),  write(Tokens), nl,
                      parser(Tokens, Result).


parseFiles([]).

parseFiles([H|T]) :-
   write("Testing "), write(H), nl,
   read_in(H,L), lexer(L, Tokens), parser(Tokens, _),  
   nl, write(H), write(" end"), nl, nl,
   parseFiles(T).


/******************************************************************************/
/* read in                                                                    */
/******************************************************************************/

read_in(File,[W|Ws]) :- see(File), get0(C), readword(C, W, C1), restsent(W, C1, Ws), nl, seen.

/******************************************************************************/
/* Given a word and the character after it, read in the rest of the sentence  */
/******************************************************************************/

restsent(W, _, [])         :- W = -1.                /* added EOF handling */
restsent(W, _, [])         :- lastword(W).
restsent(_, C, [W1 | Ws ]) :- readword(C, W1, C1), restsent(W1, C1, Ws).

/******************************************************************************/
/* Create the funtion to joins the charecters ":" and "=" together            */
/******************************************************************************/

readwordaux(C, W, C1, C2) :- C1 = 61, name(W, [C, C1]), get0(C2). 
readwordaux(C, W, C1, C2) :- C1 \= 61, name(W, [C]), C1=C2. 

/******************************************************************************/
/* Read in a single word, given an initial character,                         */
/* and remembering what character came after the word (NB!)                   */
/******************************************************************************/

readword(C, W, _)  :- C = -1, W = C.                    /* added EOF handling */
readword(C, W, C2) :- C = 58, get0(C1), readwordaux(C, W, C1, C2). /* Hanmtering för ":="  */
readword(C, W, C1) :- single_character( C ), name(W, [C]), get0(C1).
readword(C, W, C2) :-
   in_word(C, NewC ),
   get0(C1),
   restword(C1, Cs, C2),
   name(W, [NewC|Cs]).

readword(_, W, C2) :- get0(C1), readword(C1, W, C2).

restword(C, [NewC|Cs], C2) :-
   in_word(C, NewC),
   get0(C1),
   restword(C1, Cs, C2).

restword(C, [ ], C).

/******************************************************************************/
/* These characters form words on their own                                   */
/******************************************************************************/

single_character(40).                  /* ( */
single_character(41).                  /* ) */
single_character(42).                  /* + */
single_character(43).                  /* * */
single_character(44).                  /* , */
single_character(59).                  /* ; */
single_character(58).                  /* : */
single_character(61).                  /* = */
single_character(46).                  /* . */

/******************************************************************************/
/* These characters can appear within a word.                                 */
/* The second in_word clause converts character to lower case                 */
/******************************************************************************/

in_word(C, C) :- C>96, C<123.             /* a b ... z */
in_word(C, L) :- C>64, C<91, L is C+32.   /* A B ... Z */
in_word(C, C) :- C>47, C<58.              /* 1 2 ... 9 */

/******************************************************************************/
/* These words terminate a sentence                                           */
/******************************************************************************/

lastword('.').

/******************************************************************************/
/* all files                                                                  */
/******************************************************************************/

testa :- lab3('testfiles/testok1.pas'  ,  _).

allfiles :- tell('parser.out'), parseFiles([

   
   'testfiles/testok1.pas',
   'testfiles/testok2.pas',
   'testfiles/testok3.pas',
   'testfiles/testok4.pas',
   'testfiles/testok5.pas',
   'testfiles/testok6.pas',
   'testfiles/testok7.pas',

   /*'testfiles/testa.pas',*/
   'testfiles/testb.pas',
   'testfiles/testc.pas',
   'testfiles/testd.pas',
   'testfiles/teste.pas',
   'testfiles/testf.pas',
   'testfiles/testg.pas',
   'testfiles/testh.pas',
   'testfiles/testi.pas',
   'testfiles/testj.pas',
   'testfiles/testk.pas',
   'testfiles/testl.pas',
   'testfiles/testm.pas',
   'testfiles/testn.pas',
   'testfiles/testo.pas',
   'testfiles/testp.pas',
   'testfiles/testq.pas',
   'testfiles/testr.pas',
   'testfiles/tests.pas',
   'testfiles/testt.pas',
   'testfiles/testu.pas',
   'testfiles/testv.pas',
   'testfiles/testw.pas',
   'testfiles/testx.pas',
   'testfiles/testy.pas',
   'testfiles/testz.pas',

   'testfiles/fun1.pas',
   'testfiles/fun2.pas',
   'testfiles/fun3.pas',
   'testfiles/fun4.pas',
   'testfiles/fun5.pas',

   'testfiles/sem1.pas',
   'testfiles/sem2.pas',
   'testfiles/sem3.pas',
   'testfiles/sem4.pas',
   'testfiles/sem5.pas'
]), told.

/******************************************************************************/
/* end of program                                                             */
/******************************************************************************/