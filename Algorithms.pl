% # Node and Edges of Grapth
edge(a, b).
edge(a, c).
edge(b, d).
edge(b, e).
edge(c, f).
edge(c, g).
edge(d, h).
edge(e, i).
% #-------------------------------------------------------------------------------------------
% # Implementation of Breadth First Search Algorithms -> ( BFS ).
% #------------------------------------------------------------
breadth_first_search(Start, Goal, Path) :- bfs([[Start]], Goal, Path).

bfs([[Node|Path]|_], Node, [Node|Path]).
bfs([Path|Paths], Goal, FinalPath) :-
    extend(Path, NewPaths),
    append(Paths, NewPaths, UpdatedPaths),
    bfs(UpdatedPaths, Goal, FinalPath),!.

extend([Node|Path], NewPaths) :-
    findall([NewNode, Node|Path],
            (edge(Node, NewNode),
             \+ member(NewNode, Path)),
            ExtendedPaths),
    list_to_set(ExtendedPaths, NewPaths).
% #-------------------------------------------------------------------------------------------
% # Implementation of Depth First Search Algorithms -> ( DFS ).
% #------------------------------------------------------------
depth_first_serach(Start, Goal, Path) :- dfs(Start, Goal, [Start], Path),!.
dfs(Node, Node, _, [Node]) :- !.
dfs(Start, Goal, Visited, [Start|Path]) :-
    edge(Start, Next),
    \+ member(Next, Visited),
    dfs(Next, Goal, [Next|Visited], Path).
% #----------------------------------------------------------------------------
squeeze:-get0(C),put(C),dorest(C).
dorest(46):-!.
dorest(32):-get(C),put(C),dorest(C).
dorest(_):-squeeze.
% #----------------------------------------------------------------------------
read_from_file(X,Y):-see('Read.txt'),read(X),read(Y),write(X),nl,write(Y),seen.
read_on_file(X):-tell('Write.txt'),write(X),told.