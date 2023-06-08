
buildList(_,_,[]).

buildList(X, Y,[A,B,C,D]) :-
    Tmp is X*Y,
    create_list(0,Tmp,List),
    Bomb1 is(((A-1)*Y)+B)-1,
    Bomb2 is(((C-1)*Y)+D)-1 ,
    replace(Bomb1,List,b,List1),
    replace(Bomb2,List1,b,List2),
    search([[List2,null,0,x,0]], [],X,Y,0).

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

search([], Closed,_,_,MaxG):- write("Max number of dominos: "), write(MaxG),nl,printSolution(Closed,MaxG).

search(Open, Closed,X,Y,MaxG):-
getBestState(Open, [CurrentState,Parent,G,H,F], TmpOpen),
getAllValidChildren([CurrentState,Parent,G,H,F],TmpOpen,Closed,Children,X,Y),
length(Children, N),N == 0 ->!,
getMax(G,MaxG,Res),
append(Closed,[[CurrentState,Parent,G,H,F]], NewClosed),
search(TmpOpen,NewClosed,X,Y,Res).

search(Open, Closed,X,Y,MaxG):-
getBestState(Open, CurrentNode, TmpOpen),
getAllValidChildren(CurrentNode,TmpOpen,Closed,Children,X,Y),
addChildren(Children, TmpOpen, NewOpen),
append(Closed, [CurrentNode], NewClosed),
search(NewOpen, NewClosed,X,Y,MaxG).


% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Children,X,Y):-
findall(Next, getNextState(Node,Open,Closed,Next,X,Y),Children).

getNextState([State,_,G,_,_],Open,Closed,[Next,State,NewG,NewH,NewF],X,Y):-
move(State, Next, MoveCost,X,Y),
isOkay(Next),
calculateH(Next,0, NewH),
NewG is G + MoveCost,
NewF is NewG + NewH,
( not(member([Next,_,_,_,_], Open)) ;memberButBetter(Next,Open,NewF) ),
( not(member([Next,_,_,_,_],Closed));memberButBetter(Next,Closed,NewF)).


memberButBetter(Next, List, NewF):-
findall(F, member([Next,_,_,_,F], List), Numbers),
min_list(Numbers, MinOldF),
MinOldF > NewF.

%implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
append(Open, Children, NewOpen).
getBestState(Open, BestChild, Rest):-
findMin(Open, BestChild),
delete(Open, BestChild, Rest).

%implementation of findMin in getBestState determines the search alg.
% Greedy best-first search
findMin([Z], Z):- !.

findMin([Head|T], Min):-
findMin(T, TmpMin),
Head = [_,_,_,_,HeadF],
TmpMin = [_,_,_,_,TmpF],
(TmpF < HeadF -> Min = TmpMin ; Min = Head).


% Implementation of printSolution to print the actual solution path

printSolution([],_):-!.

printSolution(Closed,MaxG):-
getState(Closed,[State,_,G, H, F],Rest),
(MaxG ==  G-> write([State,G, H, F]),nl,printSolution(Rest,MaxG) ;printSolution(Rest,MaxG)).

move(State, Next,1,X,Y):-
up(State, Next,X,Y);left(State, Next,X,Y).

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
replace(NewIndex, List1, #,Next).


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

calculateH([],_, 0):- !.
calculateH([Elem|T1], Elem, Hvalue):-
!, calculateH(T1, Elem, Count),Hvalue is Count + 1.
calculateH([_|T1], Elem, Hvalue):-
calculateH(T1, Elem, Hvalue).



getMax(A,B,Max):-
(A >= B -> Max is  A; Max is B).

getState([[State,_,G, H, F]|Rest],[State,_,G, H, F], Rest).
