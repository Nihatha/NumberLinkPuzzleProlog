%% Format Input string
split_string(Input,Result):-
    split_string(Input,",(", " ", SplitList), % Split input string based on open brackets and space
    delete(SplitList,"",List), 				  % delete empty elements in the list created
    list_elements(List,Result).

%individually check character in list elements and remove unnecessary character 
remove_char(S,C,X) :- atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X).
remove_char(S,_,S).

%Convert list elements from string to number
str_to_number([H|_],N):-atom_number(H,N).

%Process individual elements in the list of strings and extract only numbers as list elements
list_elements([],[]).
list_elements([Head|Tail],Result):-
    remove_char(Head,:,Elem), 				% remove colon from labels
    split_string(Elem,",",")",List), 		% remove closing brackets from string
    list_elements(Tail,TempList),
    str_to_number(List,Number), 
    append([Number],TempList,Result).

%%Create Matrix
%Create empty matrix based on the board configuration provided as an input
%Initially create a list of 'Rows' number of elements and replace each element of the list 
%with a new list of 'Rows' number of elements.
create_matrix(Rows,Iter,Matrix,MatrixList):-
    Limit is Iter-1,
    Limit >0,
    length(List,Rows), 								% create a list of row elements				
    matrix_replace(Matrix,Iter,List,MatrixList),	% replace each element in the list with a new list
    create_matrix(Rows,Limit,MatrixList,MatrixList).

create_matrix(Rows,1,Matrix,MatrixList):-
    length(List,Rows),
    matrix_replace(Matrix,1,List,MatrixList).
   
%Replace matrix list with new list of the same dimension    
matrix_replace([_|Tail], 1, List, [List|Tail]).
matrix_replace([Head|Tail], Iter, List, [Head|Result]):- 
    Iter > 0, 
    NewIter is Iter-1, 
    matrix_replace(Tail, NewIter, List, Result), !.
matrix_replace(List, _, _, List).

%Fill matrix with numbers based on start/end coordinates
fill_matrix([],_).
fill_matrix([Label,Xs,Ys,Xe,Ye|Tail],Matrix):-
    replace(Matrix,Xs,Ys,Label,Result),
    replace(Result,Xe,Ye,Label,Matrix),
    fill_matrix(Tail,Matrix).

% Replace elements in the matrix with numbers based on coordinates    
replace( L , Y , X , Z , R ) :-
  append(RowPfx,[Row|RowSfx],L),     % decompose the list-of-lists into a prefix, a list and a suffix
  RX is X-1,
  length(RowPfx,RX) ,                 % check the prefix length: do we have the desired list?
  append(ColPfx,[_|ColSfx],Row) ,    % decompose that row into a prefix, a column and a suffix
  RY is Y-1,
  length(ColPfx,RY) ,                 % check the prefix length: do we have the desired column?
  append(ColPfx,[Z|ColSfx],RowNew) , % if so, replace the column with its new value
  append(RowPfx,[RowNew|RowSfx],R).   % and assemble the transformed list-of-lists

%%Constraints to solve the puzzle
%Check if coordinates are adjacent as well as not being visited before. 
connected(P1, P2, Matrix, Visited) :- % P1, P2 refers to the starting and ending coordinates
    next(P1, P2),
    maplist(dif(P2), Visited),			% Check if the current coordinates are not visited before
    path(P1, Matrix, Number),
    path(P2, Matrix, Number).
connected(P1, P2, Matrix, Visited) :-
    next(P1, P3),
    maplist(dif(P3), Visited),
    path(P1, Matrix, Number),
    path(P3, Matrix, Number),
    connected(P3, P2, Matrix, [P3|Visited]).

next(p(X,Y1), p(X,Y2)) :- Y2 is Y1+1.
next(p(X,Y1), p(X,Y2)) :- Y2 is Y1-1.
next(p(X1,Y), p(X2,Y)) :- X2 is X1+1.
next(p(X1,Y), p(X2,Y)) :- X2 is X1-1.

path(p(X,Y), Matrix, Number) :-
    nth1(Y, Matrix, Row),
    nth1(X, Row, Number).

% Pass in the first value in the list representing the board dimension
get_rows([H|_],H).

%Call solution predicates for all coordinate points provided through input
call_connected([],_).
call_connected([_,Xs,Ys,Xe,Ye|T],Matrix):- 		%Matrix refers to the list of list that needs to be filled
    connected(p(Xs,Ys), p(Xe,Ye), Matrix, [p(Xs,Ys)]),
    call_connected(T,Matrix).

%%Check if all the grids in the matrix are filled. 
% Called after the matrix is solved to make sure all grids are filled.  
not_empty(H):-
    nonvar(H).
not_empty_list(H):-
    maplist(not_empty,H).


%Print the solved matrix in readable format
print_matrix([]).
print_matrix([H|T]):-
    write(H),nl,
    print_matrix(T).    

%Main predicate organizes all predicates to solve the puzzle
sol(M) :-
    read(Input),						%Request for user input
    split_string(Input,List),			%Split user input to a list of elements
    length(ConfigList,2),				
    append(ConfigList,CordList,List),	%Seperate first 2 elements (config info) of the user input
    get_rows(ConfigList,Rows),			%Extract dimension detail of matrix
    length(Frame,Rows),
    create_matrix(Rows,Rows,Frame,M),	%Create empty matrix (List of lists)
    fill_matrix(CordList,M),		%Fill matrix with numbers in specified coordinates
    print_matrix(M),			%Print grid
    call_connected(CordList,M),nl.	%Solve puzzle.


%% Start predicate to initiate the puzzle
run:-
    sol(M),
    maplist(not_empty_list,M),
    print_matrix(M),
    fail. 

run.