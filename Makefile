PROLOG = swipl -O

.PHONY: test
test:
	@ echo "-- Run tests and exit  ..."
	time $(PROLOG) -s load -g gruffalo_test -t halt

.PHONY: cov
cov:
	@ echo "-- Run tests, print test coverage and exit ..."
	$(PROLOG) -s load -g gruffalo_cov -t halt

.PHONY: repl
repl:
	@ echo "-- Load and enter REPL ..."
	$(PROLOG) -s load -g gruffalo_repl

#
# Args="Module Goal ExtraArgs"
#
.PHONY: args
args:
	@ echo "-- Load and execute goal from command line args ..."
	$(PROLOG) -s load -g gruffalo_args -t halt -- $(ARGS)
