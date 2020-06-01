/** <module> Interface for Prolog gruffalo template.

Acts as an interface to the system. Configures load paths and provides
predicates for initiating the system.

To configure for your own project, replace 'gruffalo', with the name
of your main program file.

Configures internal load paths in preparation of use_module calls.
Provides predicates for adding an entity to the current database

@author Fixme
@copyright Fixme
@license Fixme
*/

gruffalo_configure_globals :-
    set_test_options([load(always)]).


% gruffalo_configure_load_paths() is det
%
% Configures internal load paths in preparation of use_module calls.

gruffalo_configure_load_paths :-
    prolog_load_context(directory, Root), % Available during compilation
    gruffalo_configure_path(Root, 'src', gruffalo).

gruffalo_configure_path(PathPrefix, PathSuffix, Name) :-
    atomic_list_concat([PathPrefix,PathSuffix], '/', Path),
    asserta(user:file_search_path(Name, Path)).

% Set everything up
:- gruffalo_configure_globals.
:- gruffalo_configure_load_paths.

% documentation - uncomment to run documentation server
%:- doc_server(4000).
%:- portray_text(true).


:- include(gruffalo(include/common)).

gruffalo_load_project_modules :-
    use_module(library(pldoc), []),  % Load first to enable comment processing
	use_module(gruffalo(util), []),
	use_module(gruffalo(narrate), []),
    use_module(gruffalo(gruffalo), []).

gruffalo_load_project_tests :-
    plunit:load_test_files([]).

%% gruffalo_test() is det.
%
%  Loads everything and runs test suite

gruffalo_test :-
    gruffalo_load_project_modules,
    gruffalo_load_project_tests,
    gruffalo_run_test_suite.

gruffalo_run_test_suite :-
    core:format('~n% Run tests ...~n'),
    plunit:run_tests.

%%  gruffalo_cov() is det.
%
%   Loads everything and runs the test suite with coverage analysis.

gruffalo_cov :-
    gruffalo_load_project_modules,
    gruffalo_load_project_tests,
    gruffalo_run_test_suite_with_coverage.

gruffalo_run_test_suite_with_coverage :-
    core:format('~n% Run tests ...~n'),
    plunit:show_coverage(plunit:run_tests).

%% gruffalo_repl() is det.
%
%  Loads everything and enters interactive mode.

gruffalo_repl :-
    gruffalo_load_project_modules,
    gruffalo_load_project_tests.


%% gruffalo_args() is det.
%
%  Loads everything and executes a goal entered by the user from the command line.
%  Format of command line args is Module Name ExtraArgs, where Name is the
%  predicate you wish to run.  See Makefile
%
gruffalo_args :-
    gruffalo_load_project_modules,
    core:current_prolog_flag(argv, [M,Name|ExtraArgs]),
    format('Goal: ~w:~w~n', [M,Name]),
    go(M:Name, ExtraArgs).

go(_,[]).
go(M:Name, [Arg|Args]) :-
    core:apply(M:Name, [Arg, Result]),
    core:format('Argument: ~w, Result: ~w~n', [Arg,Result]),
    go(M:Name, Args).
