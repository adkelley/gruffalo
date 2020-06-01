/** <module> Prolog gruffalo narrate module

This is the narration file for the gruffalo planner

@author Alex Kelley
@copyright 2020
@license MIT
@see <http://github.com/adkelley/prolog-gruffalo
*/

:- module(_, [ narrate_solution/1
			 ]).

:- include(gruffalo(include/common)).

:- use_module(util, []).
:- use_module(library(lists), []).

case_stroll(Start, Stop) :-
	format('narrator: A mouse took a stroll through the deep dark ~w and encountered a ~w. ', [Start, Stop]).

case_object(fox) :-
	format('Come back and have lunch in my underground house.~n').
	
case_object(owl) :-
	format('Come back and have tea in my treetop house.~n').

case_object(snake) :-
	format('Come for a feast in my logpile house.~n').

case_object(gruffalo) :-
	format('You will taste good on a slice of bread.~n').

case_invite(Object) :-
  	format('A ~w saw the mouse and the mouse looked good.~n', [Object]),
  	format('~w: Where are you going to, little brown mouse? ', [Object]),
	case_object(Object).

case_decline(Object) :-
	format('mouse: It is terribly kind of you, ~w, but no ', [Object]),
	format('- I am going to have lunch with a gruffalo.~n').

narrate(stroll(Start, End))  :- case_stroll(Start, End).
narrate(invite(Object))  :- case_invite(Object).
narrate(decline(Object)) :- case_decline(Object).
narrate(meet(gruffalo))  :- case_invite(gruffalo).
narrate(_).

narrate_solution([]).
narrate_solution([H|T]) :-
	narrate_solution(T),
	narrate(H).

%narrate_sofar([]).
%narrate_sofar([H|_]) :-
%	narrate(H).
