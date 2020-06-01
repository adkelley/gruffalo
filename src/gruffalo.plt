%% gruffalo test file
%
%
:- include(gruffalo(include/common)).

:- use_module(gruffalo, []).

:- begin_tests('gruffalo').

test('test1', [true(Got == Expected)]) :-
    format('~ngruffalo:test1 - library version~n'),
    Expected = [0, 0, 1],
    gruffalo:version(Got).

test('test2', [true(Got == Expected)]) :-
	format('~ngruffalo:test2 - mouse strolls from forest to foxhole~n'),
	Expected = [stroll(woods,foxhole)],
	gruffalo:solve([at(mouse, woods), at(fox, woods)], 
				   [at(mouse, foxhole)], 
				   Got),
	format('~nPlan = ~w~n', [Got]),
	!.

test('test3', [true(Got == Expected)]) :-
	format('~ngruffalo:test3 - fox invites mouse~n'),
	Expected = [invite(fox),stroll(woods, foxhole)],
	gruffalo:solve([at(mouse, woods), at(fox, foxhole)], 
				   [invite(fox)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.

test('test4', [true(Got == Expected)]) :-
	format('~ngruffalo:test4 - mouse declines fox~n'),
	Expected = [decline(fox),invite(fox),stroll(woods, foxhole)],
	gruffalo:solve([at(mouse, woods), at(fox, foxhole)], 
				   [decline(fox)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.

test('test5', [true(Got == Expected)]) :-
	format('~ngruffalo:test5 - owl invites mouse~n'),
	Expected = [invite(owl),stroll(woods,treetop)],
	gruffalo:solve([at(mouse, woods), at(fox, foxhole), at(owl, treetop)], 
				   [invite(owl)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.

test('test6', [true(Got == Expected)]) :-
	format('~ngruffalo:test6 - mouse declines owl~n'),
	Expected = [decline(owl),invite(owl),stroll(woods, treetop)],
	gruffalo:solve([at(mouse, woods), at(owl, treetop)], 
				   [decline(owl)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.

test('test7', [true(Got == Expected)]) :-
	format('~ngruffalo:test7 - snake invites mouse~n'),
	Expected = [invite(snake),stroll(woods,logpile)],
	gruffalo:solve([at(mouse, woods), at(fox, foxhole), at(owl, treetop),at(snake, logpile)], 
				   [invite(snake)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.

test('test8', [true(Got == Expected)]) :-
	format('~ngruffalo:test8 - mouse declines snake~n'),
	Expected = [decline(snake),invite(snake),stroll(woods, logpile)],
	gruffalo:solve([at(mouse, woods), at(snake, logpile)], 
				   [decline(snake)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.

test('test9', [true(Got == Expected)]) :-
	format('~ngruffalo:test9 - mouse meets gruffalo~n'),
	Expected = [meet(gruffalo),stroll(woods, rock)],
	gruffalo:solve([at(mouse, woods), at(gruffalo, rock)], 
				   [meet(gruffalo)], 
				   Got),
	format('Plan = ~w~n', [Got]),
	!.



:- end_tests('gruffalo').
