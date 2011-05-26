# $Id: Makefile.am 2925 2010-05-10 15:05:29Z jean.sebastien.pedron $

ETEST   = etest
TPL_PL  = template.pl
TPL_ERL = template.erl

TPL_FILES = t_yaml_parser.erl

ERL_TESTS = t_yaml_parser

TESTS = dialyzer $(ERL_TESTS)

EXTRA_DIST = $(TPL_PL) $(TPL_ERL) $(ERL_FILES) eunit_lib.erl 		\
	     eunit_tty.erl cover_to_html.sh data

CLEANFILES = $(ERL_TESTS) $(TPL_FILES) eunit_lib.beam eunit_tty.beam 	\
	     erl_crash.dump cover_*.html

$(ERL_TESTS): $(ETEST) eunit_lib.erl eunit_tty.erl $(TPL_PL) $(TPL_ERL) data/$@/*.pattern
	@(test ! -e eunit_lib.beam 					\
	  -o $(srcdir)/eunit_lib.erl -nt eunit_lib.beam &&		\
	  echo "Create: eunit_lib" &&					\
	  $(ERLC) $(srcdir)/eunit_lib.erl) || :
	@(test ! -e eunit_tty.beam 					\
	  -o $(srcdir)/eunit_tty.erl -nt eunit_tty.beam &&		\
	  echo "Create: eunit_tty" &&					\
	  $(ERLC) $(srcdir)/eunit_tty.erl) || :
	@(test ! -e $@ -o $(ETEST) -nt $@ && echo "Create: $@" &&	\
	  cp $(ETEST) $@ && chmod 755 $@) || :
	@if test ! -e $@.erl; then					\
	   rebuild=yes;							\
	 elif test $(srcdir)/$(TPL_PL) -nt $@.erl; then			\
	   rebuild=yes;							\
	 elif test $(srcdir)/$(TPL_ERL) -nt $@.erl; then		\
	   rebuild=yes;							\
	 else								\
	   for file in $(srcdir)/data/$@/*.pattern; do			\
	     if test $$file -nt $@.erl; then				\
	       rebuild=yes;						\
	     fi;							\
	   done;							\
	 fi;								\
	 if test "$$rebuild" = "yes"; then				\
	   echo "Create: $@.erl";					\
	   $(srcdir)/$(TPL_PL) $(srcdir)/$(TPL_ERL) $(srcdir)/data/$@ > $@.erl;\
	 fi
