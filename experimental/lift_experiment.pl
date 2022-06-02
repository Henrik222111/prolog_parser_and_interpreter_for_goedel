ast('M1',exp([],lang([],[],[],[],[],[]),[]),loc([],[],
lang(['Day','ListOfDay'],[],
[const('Nil',base('ListOfDay')),const('Monday',base('Day')),
 const('Tuesday',base('Day')),const('Wednesday',base('Day')),
 const('Thursday',base('Day')),const('Friday',base('Day')),
 const('Saturday',base('Day')),const('Sunday',base('Day'))],
[func('Cons',2,'',-1,[base('Day'),base('ListOfDay')],base('ListOfDay'))],[],
[pred('Append',3,'',[base('ListOfDay'),base('ListOfDay'),base('ListOfDay')]),
 pred('Append3',4,'',[base('ListOfDay'),base('ListOfDay'),base('ListOfDay'),base('ListOfDay')])]),[],
[stm(pred('Append',3,[cons('Nil'),var(x),var(x)])),
 stm(pred('Append',3,[func('Cons',2,[var(u),var(x)]),var(y),func('Cons',2,[var(u),var(z)])]),
 pred('Append',3,[var(x),var(y),var(z)])),
 stm(pred('Append3',4,[var(x),var(y),var(z),var(u)]),
  and(pred('Append',3,[var(x),var(y),var(w)]),pred('Append',3,[var(w),var(z),var(u)])))])).

% for convenience: extracted statements and added true as virtual body for facts:
stm(pred('Append',3,[cons('Nil'),var(x),var(x)]),pred(true,0,[])).
stm(pred('Append',3,[func('Cons',2,[var(u),var(x)]),var(y),func('Cons',2,[var(u),var(z)])]),
    pred('Append',3,[var(x),var(y),var(z)])).
stm(pred('Append3',4,[var(x),var(y),var(z),var(u)]),
       and(pred('Append',3,[var(x),var(y),var(w)]),pred('Append',3,[var(w),var(z),var(u)]))).
  
% demo on how to lift and pretty-print statements
lift_pp :- stm(Head,Body), 
    mkng(Head,NVHead,[],Sub),
    mkng(Body,NVBody,Sub,_),
    portray_clause((NVHead :- NVBody)),nl,
    assert((NVHead :- NVBody)),
    fail.
lift_pp.

% an adaption of InstanceDemo from Michael Leuschel's script for "Vertiefung Logische Programmierung"
mkng(cons(C),C) --> [].
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

lookup_var(N,X,[],[sub(N,X)]).
lookup_var(N,X,[sub(M,Y)|T],[sub(M,Y)|T1]) :-
   (M==N -> X=Y, T1=T
    ; lookup_var(N,X,T,T1)).
    
    
/*

| ?- lift_pp.
'Append'('Nil', A, A).

'Append'('Cons'(A,B), C, 'Cons'(A,D)) :-
        'Append'(B, C, D).

'Append3'(A, B, C, D) :-
        'Append'(A, B, E),
        'Append'(E, C, D).

yes
| ?- 'Append'('Cons'('Tuesday','Nil'),'Nil',R).
R = 'Cons'('Tuesday','Nil') ? ;
no

*/