/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */
maximum_printing_depth(100).

:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).


% Signature: path(Node1, Node2, Path)/3
% Purpose: Path is a path, denoted by a list of nodes, from Node1 to Node2.
path(Y,Y,[Y]).								%1
path(X,Y,[X|Ps]):-edge(X,Z),path(Z,Y,Ps).	%2

% Signature: cycle(Node, Cycle)/2
% Purpose: Cycle is a cyclic path, denoted a list of nodes, from Node1 to Node1.
cycle(X,[X,Y|Cs]):-edge(X,Y),path(Y,X,[Y|Cs]).

% Signature: reverse(Graph1,Graph2)/2
% Purpose: The edges in Graph1 are reversed in Graph2
reverse([],[]).
reverse([[X,Y]|Gs],[[Y,X]|Ts]):-reverse(Gs,Ts).

% Signature: degree(Node, Graph, Degree)/3
% Purpose: Degree is the degree of node Node, denoted by a Church number (as defined in class)
degree(_,[],zero).
degree(V,[[V,_]|Gs],s(D)):-degree(V,Gs,D).
degree(V,[[U,_]|Gs],D):-V\=U,degree(V,Gs,D).











