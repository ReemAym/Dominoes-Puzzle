buildList(_,_,[]).

buildList(X, Y,[A,B,C,D]) :-
    Tmp is X*Y,
    create_list(0,Tmp,List),
    Bomb1 is(Y*(A-1)+B)-1,
    Bomb2 is(Y*(C-1)+D)-1,
    replace(Bomb1,List,b,List1),
    replace(Bomb2,List1,b,List2),
    search([[List2,null]], [],X,Y).

replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R).

create_list(Item, Size, List) :-
    create_list(Item, Size, [], List).

create_list(_, 0, List, List).
create_list(Item, Size, Acc, List) :-
    Size > 0,
    NewSize is Size - 1,
    create_list(Item, NewSize, [Item|Acc], List).

search([],Closed,X,Y):- printSolutionC(Closed,X,Y),!.

search(Open, Closed,X,Y):-
getState(Open,  [CurrentState,Parent], TmpOpen),
getAllValidChildren([CurrentState,Parent],TmpOpen,Closed,Children,X,Y),
length(Children, N),N == 0 ->!,
append(Closed,[[CurrentState,Parent]], NewClosed),
search(TmpOpen,NewClosed,X,Y).

search(Open, Closed,X,Y):-
getState(Open, CurrentNode, TmpOpen),
getAllValidChildren(CurrentNode,TmpOpen,Closed,Children,X,Y),
addChildren(Children, TmpOpen, NewOpen),
append(Closed, [CurrentNode], NewClosed),
search(NewOpen, NewClosed,X,Y).

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Children,X,Y):-
findall(Next, getNextState(Node, Open, Closed, Next,X,Y), Children).

getNextState([State,_], Open, Closed, [Next,State],X,Y):-
move(State, Next,X,Y),
not(member([Next,_], Open)),
not(member([Next,_], Closed)),
isOkay(Next).

% Implementation of getState and addChildren determine the search alg.
% BFS
getState([CurrentNode|Rest], CurrentNode, Rest).

addChildren(Children, Open, NewOpen):-
append(Open, Children, NewOpen).

% Implementation of printSolution to print the actual solution path
printSolutionC([],_,_):-!.

printSolutionC(Closed,X,Y):-
getState(Closed,[State,_],Rest),
countZero(State,0,Count),
Tmp is (X*Y)-2,
(Count == Tmp -> printSolutionC(Rest,X,Y); write(State),nl,write("-------------------"),nl,printSolutionC(Rest,X,Y)).

move(State, Next,X,Y):-
left(State,Next,X,Y);up(State, Next,X,Y).

left(State,Next,_,Y):-

nth0(Indx, State, 0),
not(0 is Indx mod Y),

NewIndex is Indx - 1,
nth0(NewIndex, State, Element),
not(Element = b),
not(Element = #),
not(Element = u),

% Swap
replace(Indx, State, #, List1),
replace(NewIndex, List1, #, Next).


up(State, Next,_,Y):-

nth0(Indx, State, 0),
Indx>=Y,

NewIndex is Indx - Y,
nth0(NewIndex, State, Element),
not(Element = b),
not(Element = # ),
not(Element = u),

% Swap
replace(Indx, State, u, List1),
replace(NewIndex, List1, u, Next).


isOkay(_):- true.

countZero([], _, 0).
countZero([H|T], Zero, Count) :-
    countZero(T, Zero, Count1),
    (
        H = Zero,
        Count is Count1 + 1
    ;
        H \= Zero,
        Count is Count1
    ).
