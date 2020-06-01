/** <module> Prolog gruffalo main module

This is the main file for the gruffalo planner

@author Alex Kelley
@copyright 2020
@license MIT
@see <http://github.com/adkelley/prolog-gruffalo
*/

:- module(_, [ test/1
			 , solve/3
             , version/1
			 ]).

:- include(gruffalo(include/common)).

:- use_module(util, []).
:- use_module(library(lists), []).
:- use_module(narrate, []).


%!  version(?Version) is det.
%
%   True if version is a list representing the major, minor
%   and patch version numbers of this library.

version([0,0,1]).

test(Plan) :-
	solve([at(mouse, woods),at(fox, foxhole),at(owl, treetop),at(snake, logpile),at(gruffalo,rock)], 
		  [decline(owl)],
		  Plan).

solve(State, Goal, Plan) :-
	solve(State, Goal, [], Plan).

solve(State, Goal, Plan, Plan) :-
	lists:subset(Goal, State), nl,
	narrate:narrate_solution(Plan).

solve(State, Goal, Sofar, Plan) :-
%	format('State: ~w, Goal: ~w, Sofar: ~w~n', [State,Goal,Sofar]),
	opn(Op, Precons, Delete, Add),
	\+ lists:member(Op, Sofar),
	lists:subset(Precons, State),
	delete_list(Delete, State, Remainder),
	lists:append(Add, Remainder, NewState),
	solve(NewState, Goal, [Op|Sofar], Plan).


% The gruffalo problem has eight operators.
% 1st arg = name
% 2nd arg = preconditions
% 3rd arg = delete list
% 4th arg = add list.

opn(stroll(X, Y), 
	[at(mouse, X)],
	[at(mouse, X)],
	[at(mouse, Y)]).
	
opn(invite(fox), 
   [at(mouse, foxhole),at(fox, foxhole)],
   [],
   [invite(fox)]).
   
opn(decline(fox),
	[at(mouse, foxhole),at(fox, foxhole),invite(fox)],
	[],
	[decline(fox)]).

opn(invite(owl),
	[at(mouse, treetop),at(owl, treetop)],
	[],
	[invite(owl)]).

opn(decline(owl),
	[at(mouse, treetop),at(owl, treetop),invite(owl)],
	[],
	[decline(owl)]).

opn(invite(snake), 
   [at(mouse, logpile),at(snake, logpile)],
   [],
   [invite(snake)]).
   
opn(decline(snake),
	[at(mouse, logpile),at(snake, logpile),invite(snake)],
	[],
	[decline(snake)]).

opn(meet(gruffalo),
	[at(mouse, rock),at(gruffalo, rock)],
	[],
	[meet(gruffalo)]).

delete_list([], List, List).
delete_list([H|T], List, Final) :-
	remove(H, List, Remainder),
	delete_list(T, Remainder, Final).

remove(X, [X|T], T).
remove(X, [H|T], [H|R]) :-
	remove(X, T, R).
		

