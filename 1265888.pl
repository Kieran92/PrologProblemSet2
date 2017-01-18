/*----------------------------------------------------------------------------------------------
*  Name:Kieran Boyle
*  Student Number: 1265888
*  Course: CMPUT 325
*  Lecture Section: B1
*  Lab Section: HO1
*  Assignment Number: 4
-----------------------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------------------------
*Question 1

fourSquares(+N, [-S1, -S2, -S3, -S4])

fourSquares takes in a single postive, non-zero integer and returns the sum of four numbers that
when squared, add up to it. This predicate uses the clpfd library and constrain programmiing to
find the solution. The variables S1-S4 are the values of the numbers that are supposed to sum up
to the final the number N. The predicate follows the assignmnet constrain that S1<S2, S2<S3, S3<S4, 
and S4\=0. lable just ensures that the correct numbers are assigned in the correct places in the 
list.

Examples:

	fourSquares(-20, Var).
		false.

	fourSquares(20, Var).
		Var = [0, 0, 2, 4] ;
		Var = [1, 1, 3, 3] ;
		false.

	fourSquares(1, Var).
		Var = [0, 0, 0, 1].

------------------------------------------------------------------------------------------------*/

:- use_module(library(clpfd)).
fourSquares(N, [S1, S2, S3, S4]):-
	Vars = [S1, S2, S3, S4],
	Vars ins 0..N,
	S1#=<S2, S2#=<S3, S3#=<S4, S4#\=0,
	N #= S1*S1 + S2*S2 + S3*S3 + S4*S4, 
	label(Vars).



/*----------------------------------------------------------------------------------------------
*Question 2

disarm(+Adivisions, +Bdivisions,-Solution)

Each list represents two nations armaments. Each month once country gets rid of two caches of
armaments while the other gets rid of one cache. This continues until there are no armaments left.
Each element of Solution represents one dismantlement, where a dismantlement is a list containing 
two elements: the first element is a list of country A's dismantlements and the second is a list of country B's 
dismantlements. If there is no solution false is returned. Below I call several subfunctions which I will 
describe individually. 


Examples:
	disarm([1,3,3,4,6,10,12],[3,4,7,9,16],S).
		S = [[[1, 3], [4]], [[3, 6], [9]], [[10], [3, 7]], [[4, 12], [16]]].

	disarm([1,2,3,3,8,5,5],[3,6,4,4,10],S).
		S = [[[1, 2], [3]], [[3, 3], [6]], [[8], [4, 4]], [[5, 5], [10]]].

	disarm([1,2,2,3,3,8,5],[3,2,6,4,4,10],S).
		false.

	disarm([1,2,2,3,3,8,5,5,6,7],[3,2,6,4,4,10,1,5,2],S).
		false.


	disarm([1,2,2,116,3,3,5,2,5,8,5,6,6,8,32,2],[3,5,11,4,37,1,4,121,3,3,14],S).
		S = [[[1, 2], [3]], [[2, 2], [4]], [[2, 3], [5]], [[5], [4, 1]], [[6], [3, 3]], [[3, 8], [11]], [[6|...], [...]], [[...|...]|...], [...|...]].

------------------------------------------------------------------------------------------------*/


disarm(ADivisions, BDivisions, Solution):-
	msort(ADivisions, Adiv), msort(BDivisions, Bdiv), 
	reverse(Adiv, AdivReversed), 
	disarmer(AdivReversed,Bdiv,Solution1), 
	bubblesort(Solution1, Solution).
disarm(ADivisions, BDivisions, Solution):-
	msort(ADivisions, Adiv), msort(BDivisions, Bdiv), 
	reverse(Bdiv, BdivReversed), 
	disarmer(Adiv,BdivReversed,Solution1), 
	bubblesort(Solution1, Solution).

/*----------------------------------------------------------------------------------------------
disarmer(+Adivisions, +Bdivisions,-Solution)

Essentially this is the same predicate as disarm but allows more freedom because it accepts sorted inputs so
that the function will run. There is no sorting of the final solution for this part of the question. This
predicate first finds sets that add up to a number from the BDivision or the Adivision if the BDivision case
fails. If a subset is found it is removed from the Adivision or the BDivision. The remainder of the list is 
ran throught the predicate and the potential solution is appended to the final portion of the list. The
solution is returned to be sorted in the  main function. 


Examples:
	disarmer([1,3,3,4,6,10,12],[3,4,7,9,16],S).
		S = [[[1, 3], [4]], [[3, 6], [9]], [[4, 12], [16]], [[10], [3, 7]]].

	disarmer([1,2,2,3,3,8,5],[3,2,6,4,4,10],S).
		false.

	disarmer([1,2,2,3,3,8,5,5,6,7],[3,2,6,4,4,10,1,5,2],S).
		false.

-----------------------------------------------------------------------------------------------*/

disarmer([],[],[]).
disarmer(ADivisions, [H|BDivisions], Solution):-
	set2(Subset,ADivisions, H), 
	subtractor(ADivisions, Subset, X), 
	msort(Subset, SortedSubset),
	S = [SortedSubset, [H]], 
	msort(X, X1), reverse(X1, Xrev),
	disarmer(Xrev, BDivisions, Solution2),
	append([S],Solution2,Solution),!.

disarmer([H|ADivisions], BDivisions, Solution):-
	set2(Subset,BDivisions, H), 
	subtractor(BDivisions, Subset, X), 
	msort(Subset, SortedSubset),
	S = [[H], SortedSubset], 
	msort(X,X1), reverse(X1, Xrev),
	disarmer(ADivisions, Xrev, Solution2),
	append([S], Solution2, Solution),!.



/*----------------------------------------------------------------------------------------------
set2(-L1,+L,+Num)


This predicate takes in a number and finds those size two subsets of a given list which add
up to the input number. First the subset is found, then the lenght of the subset is determined
if the sub set is of size two it is summed compared against the number.

Examples:

	set2(L,[1,2,3], 3).
		L=[1,2]

	set2([1,2],[1,2,3], 3).
		true.

	set2(L,[1,2,3], 6).
		false.

-----------------------------------------------------------------------------------------------*/
set2(L1,L, Num) :- 
	xsubset(L1, L), 
	length(L1,Len), 
	Len =:= 2,
	sumSubset(L1,Num). 


/*----------------------------------------------------------------------------------------------
xsubset(+L,-Set)

This predicate was given in the previous assignment but it finds all subsets of a given list.
-----------------------------------------------------------------------------------------------*/

xsubset([], _). 
xsubset([X|Xs], Set) :- 
	append(_, [X|Set1], Set), 
	xsubset(Xs, Set1).

/*----------------------------------------------------------------------------------------------
sumSubset(+L, Num)

This predicate adds all of the values in a given list up and returns the sum.

Exmaples:

	sumSubset([1,2],3).
		true.

	sumSubset([1,2],R).
		R = 3.

	sumSubset([1,2],%).
		false.


-----------------------------------------------------------------------------------------------*/
sumSubset([],0).
sumSubset([H|T],Return):-
	sumSubset(T,T1),
	Return is H + T1.

/*----------------------------------------------------------------------------------------------
subtractor(+List, +Subset, Rest)

This function takes a subset and removes it from a larger list. It is similar to subtract in the 
built in prolog library except that it only rmoves one occurance of the specific elements

Examples:

	subtractor([1,1,2,3,2,1], [1,2],  [1,3,2,1]).
		true.

	subtractor([1,1,2,3,2,1], [1,2], R).
		R = [1,3,2,1].

	subtractor([1,1,2,3,2,1], [1,2],  [3]).
		false.
-----------------------------------------------------------------------------------------------*/


%removes the subset from the greater list
subtractor(Remainder, [], Remainder).
subtractor(List, [Current|Delete], X) :-
    select(Current, List, Rest),
    subtractor(Rest, Delete, X).

/*----------------------------------------------------------------------------------------------
bubblesort(+L,-R)

THis predicate is a play on the classic bubblesort alogrithm except that it is tuned to sort the
final lists that are given in my function. Essentially the predicate goes through the list comparing
elements and swapping them if one is larger. 

Examples:

	bubblesort([[[1, 3], [4]], [[3, 6], [9]], [[4, 12], [16]], [[10], [3, 7]]],R).
		R = [[[1, 3], [4]], [[3, 6], [9]], [[10], [3, 7]], [[4, 12], [16]]].

	bubblesort([[[1, 3], [4]], [[3, 6], [9]], [[4, 12], [16]], [[10], [3, 7]]], [[[1, 3], [4]], [[3, 6], [9]], [[10], [3, 7]], [[4, 12], [16]]]).
		true.

	bubblesort([[[1, 3], [4]], [[3, 6], [9]], [[4, 12], [16]], [[10], [3, 7]]],[[[1, 3], [4]], [[3, 6], [9]], [[4, 12], [16]], [[10], [3, 7]]]).
		false.
	 

-----------------------------------------------------------------------------------------------*/

bubblesort(L,R):-
 swap(L,L1),!,
 bubblesort(L1,R).
 
bubblesort(R,R).

/*----------------------------------------------------------------------------------------------
swap(+L, +L)

This predicate is used in bubble sort to swap two elements if one is greater than the other.
for moree information on its effect see bubblesort(+L,-R).
-----------------------------------------------------------------------------------------------*/
swap([H,H1|T], [H1,H|T]):-
	comparer(H,H1).
 
swap([Z|T],[Z|T1]):-
 swap(T,T1).

/*----------------------------------------------------------------------------------------------
comparer(+X,+Y)

comparer takes in two lists X and Y and flattens them. The numbers are then summed and compared
against each other. true or false is returned.

Examples:

	comparer([1,[3],4],[1,2,3]).
		true.
	comparer([1,2,3],[1,[3],4]).
		false.
-----------------------------------------------------------------------------------------------*/

comparer(X,Y):- 
	flatten(X,X1),
	flatten(Y,Y1),
	sumSubset(X1,Xsum),
	sumSubset(Y1,Ysum),
	Xsum > Ysum.



%Resources
%http://stackoverflow.com/questions/8418575/prolog-subtract-each-item-only-once
%http://www.javaist.com/blog/2008/11/17/sorting-lists-in-prolog/
%http://stackoverflow.com/questions/9875760/sum-of-elements-in-list-in-prolog
