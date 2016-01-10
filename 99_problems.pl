make() :-
  consult("99_problems.pl").

/*
    LISTS
*/

/* last_elm */ 
last_elm([H|T], Elm) :-
  length(T, 0),
  Elm = H.
  
last_elm([_|T], Elm) :-
  length(T, N),
  N > 0,
  last_elm(T, Elm).
  
/* second_last_elm */
second_last_elm([H|T], Elm) :-
  length(T, 1),
  Elm = H.
  
second_last_elm([_|T], Elm) :-
  length(T, N),
  N > 1,
  second_last_elm(T, Elm).
  
/* nth_elm */
nth_elm([H|_], N, Elm) :-
  N = 1,
  Elm = H.

nth_elm([_|T], N, Elm) :-
  N > 1,
  Dec is N - 1,
  nth_elm(T, Dec, Elm).
 
/* len */
len([], 0).
 
len([_|T], N) :-
  len(T, Rest),
  N is Rest + 1.
  
/* palindrome */
match([], []).
match([H1|T1], [H2|T2]) :-
  H1 = H2,
  match(T1, T2).

palindrome(List) :-
  reverse(List, Reversed),
  match(List, Reversed).
  
/* flatten */
flatten([], []).

flatten([H|T], Flattened) :-
  is_list(H),
  flatten(H, FlatH),
  flatten(T, Rest),
  append(FlatH, Rest, Flattened).

flatten([H|T], Flattened) :-
  \+is_list(H),
  flatten(T, Rest),
  append([H], Rest, Flattened).
  
/* del_dups */
is_head(H1, [H2|_]) :-
  H1 = H2.

del_dups([], []).

del_dups([H|T], Result) :-
  is_head(H, T),
  del_dups(T, Result).
  
del_dups([H|T], Result) :-
  \+is_head(H, T),
  del_dups(T, Rest),
  Result = [H|Rest].
  
/* pack */
accumulate([], _, [], []).
accumulate(List, X, List, Result) :-
  List = [H|_],
  H \= X,
  Result = [].
accumulate(List, X, Rem, Result) :-
  List = [H|T],
  H = X,
  accumulate(T, X, Rem, Rest),
  Result = [H|Rest].
 
pack([], []).
pack(List, Result) :-
  List = [H|_],
  accumulate(List, H, Rem, Res),
  pack(Rem, Rest),
  Result = [Res|Rest].
