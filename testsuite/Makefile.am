ETEST         = etest
TPL_PL        = gen-test-erlang-module
TPL_ERL       = template.erl
COVER_TO_HTML = cover_to_html

TPL_FILES = parsing.erl construction.erl examples.erl

ERL_TESTS = parsing construction examples

if HAVE_YAMLER
TPL_FILES += yamler_compat.erl
ERL_TESTS += yamler_compat
endif

TESTS = dialyzer $(ERL_TESTS) benchmark

TESTS_ENVIRONMENT = ERL_LIBS=$(ERL_LIBS)

EXTRA_DIST = $(TPL_PL) $(TPL_ERL) $(COVER_TO_HTML)			\
	     test_coverage data

CLEANFILES = $(ERL_TESTS) $(TPL_FILES) erl_crash.dump			\
	     cover_*.html cover_*.out cover_*.percent			\
	     index*.html *.coverdata dialyzer.log *.xml

$(ERL_TESTS): $(ETEST)
	@(test ! -e $@ -o $(ETEST) -nt $@ && echo "Create: $@" &&	\
	  cp $(ETEST) $@ && chmod 755 $@) || :
	@mod=$@.erl; rebuild=no;					\
	 if test ! -e $$mod; then					\
	   rebuild="$$mod not found";					\
	 elif test $(srcdir)/$(TPL_PL) -nt $$mod; then			\
	   rebuild="$(TPL_PL) newer";					\
	 elif test $(srcdir)/$(TPL_ERL) -nt $$mod; then			\
	   rebuild="$(TPL_ERL) newer";					\
	 else								\
	   for file in $(srcdir)/data/$@/*.pattern; do			\
	     if test $$file -nt $$mod; then				\
	       rebuild="$$file newer";					\
	     fi;							\
	   done;							\
	 fi;								\
	 if test "$$rebuild" != "no"; then				\
	 echo "Create: $$mod";						\
	   env								\
	    srcdir=$(srcdir)						\
	    builddir=$(builddir)					\
	    top_srcdir=$(top_srcdir)					\
	    top_builddir=$(top_builddir)				\
	    $(srcdir)/$(TPL_PL) $(srcdir)/$(TPL_ERL) $(srcdir)/data/$@	\
	    > $$mod;							\
	 fi

.PHONY: $(ERL_TESTS)

# check-local is executed after check. We use this to output cover
# results and move tests output (cover & JUnit) to a user-specified
# directory.
check-local:
	@$(srcdir)/$(COVER_TO_HTML) $(TESTS)
	@if [ "$$TEST_OUTPUT_DIR" ]; then				\
	  mkdir -p "$$TEST_OUTPUT_DIR";					\
	  cp -a *.html *.xml "$$TEST_OUTPUT_DIR";			\
	fi
