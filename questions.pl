/* house keeping */
make :-
  consult("questions.pl").

/* every empty list is the sublist of anything */
sub_list([], L) :-
  L = []
  ;
  L = [_|_].
  
/* sublist if head is in list and rest is a sublist */
sub_list(Sub, List) :-
  Sub = [H|T],
  member(H, List),
  sub_list(T, List).


/* the difference between [] and anything is [] */
difference([], _, []).

/* if head of L1 not in L2, append and recurse with rest */
difference(L1, L2, L) :-
  L1 = [H|T],
  \+member(H, L2),
  difference(T, L2, Rest),
  L = [H|Rest].

/* if head of L1 in L2, recurse with rest unaltered */
difference(L1, L2, L) :-
  L1 = [H|T],
  member(H, L2),
  difference(T, L2, L).
  
/**************** SIFT ****************/

/* Result is List with elements greater than N removed */
sift(List, N, Result) :-
  List = [H|T],
  H > N,
  sift(T, N, Result).

sift(List, N, Result) :-
  List = [H|T],
  H =< N,
  sift(T, N, Rest),
  Result = [H|Rest].
  
sift([], _, []).

/*************** COMMON ***************/

/* 
    common/3
*/
set_append(Item, Set, Result) :-
  member(Item, Set),
  Result = Set.
  
set_append(Item, Set, Result) :-
  \+member(Item, Set),
  Result = [Item|Set].

/* I is list of elements common to L1 and L2 */
common([H|T], L2, I) :-
  member(H, L2),
  common(T, L2, Rest),
  set_append(H, Rest, I).
  
common([H|T], L2, I) :-
  \+member(H, L2),
  common(T, L2, I).

common([], _, []).

/*************** DELETE ***************/

delete(List, Result) :-
  remove_odd(List, 0, Result).

remove_odd([], _, []).

remove_odd(L, N, Result) :-
  L = [_|T],
  1 is mod(N, 2), 
  Incr is N + 1,
  remove_odd(T, Incr, Result).

remove_odd(L, N, Result) :-
  L = [H|T],
  0 is mod(N, 2),
  Incr is N + 1,
  remove_odd(T, Incr, Rest),
  Result = [H|Rest].
  
/*************** PROCESS ***************/

get_name_age(Person, Name, Age) :-
  Person = (Name, Age).
  
get_name_age_details(Person, Name, Age, Details) :-
  Person = (Name, Age, Details).

match(RecordOne, RecordTwo) :-
  get_name_age(RecordOne, Name, Age),
  get_name_age_details(RecordTwo, Name, Age, _).

process(L1, L2, Consistent, Inconsistent) :-
  L2 = [H|T],
  H = (Name, Age, _),
  Match = (Name, Age),
  member(Match, L1),
  process(L1, T, RestCons, Inconsistent),
  Consistent = [H|RestCons].
  
process(L1, L2, Consistent, Inconsistent) :-
  L2 = [H|T],
  H = (Name, Age, _),
  Match = (Name, Age),
  \+member(Match, L1),
  process(L1, T, Consistent, RestIncons),
  Inconsistent = [H|RestIncons].
  
process(_, [], [], []).

/*************** SPLIT ***************/

%split([], _, [], []).
split(L, N, L, []) :-
  length(L, X),
  X < N.
  
split(L, 0, [], L).

split(L, N, L1, L2) :-
  length(L, Len),
  Len >= N,
  L = [H|T],
  Decr is N-1,
  split(T, Decr, Rest, L2),
  L1 = [H|Rest].

/*************** DROP ***************/

removeNth([], _, [], _).

removeNth(List, N, Res, Count) :-
  List = [H|T],
  (
    Count == N
  ->
    removeNth(T, N, Res, 1)
  ;
    Inc is Count + 1,
    removeNth(T, N, Rest, Inc),
    Res = [H|Rest]
  ).

drop(List, N, Res) :-
  removeNth(List, N, Res, 1).
  
/*********** enrolment ************/ 

enrolment(L, Student, Degree) :-
  L = [H|T],
  H = (Deg, Students),
  (
    member(Student, Students)
  ->
    Degree = Deg
  ;
    enrolment(T, Student, Degree)
  ).
  
/*********** student_list *************/

student_list([], [], []).

student_list(L, Meng, MSc) :-
  L = [H|T],
  H = (Deg, Students),
  Deg == meng,
  student_list(T, Rest, MSc),
  append(Students, Rest, Meng).
  
student_list(L, Meng, MSc) :-
  L = [H|T],
  H = (Deg, Students),
  Deg == msc,
  student_list(T, Meng, Rest),
  append(Students, Rest, MSc).
  
student_list(L, Meng, MSc) :-
  L = [H|T],
  H = (Deg, _),
  Deg \= msc,
  Deg \= meng,
  student_list(T, Meng, MSc).
