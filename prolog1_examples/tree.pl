% :- type tree ---> leaf(int) | node(tree, tree).

% tree_sum(+Tree, ?Sum): A Tree fa leveleiben levõ
% értékek összege Sum.
tree_sum(leaf(Value), Value).
tree_sum(node(Left, Right), S) :-
	tree_sum(Left, S1),
	tree_sum(Right, S2),
	S is S1+S2.























% tree_sum2(?Tree, ?Sum): A Tree fa leveleiben levõ
% értékek összege (a plus/3 eljárás szerint) Sum.
tree_sum2(leaf(Value), Value).
tree_sum2(node(Left, Right), S) :-
	plus(SL, SR, S),
	tree_sum2(Left, SL),
	tree_sum2(Right, SR).


% plus(X, Y, Z): X + Y = Z, X > 0, Y > 0, Z =< 5.
plus(1,1,2).
plus(1,2,3).
plus(1,3,4).
plus(1,4,5).
plus(2,1,3).
plus(2,2,4).
plus(2,3,5).
plus(3,1,4).
plus(3,2,5).
plus(4,1,5).


test :-
	tree_sum(node(leaf(1),
		      node(leaf(2),
			   leaf(2))), S),
	write(S), nl, fail.
test.

test2(S) :-
	tree_sum2(T, S),
	write(T), nl, fail.
test2(_).


