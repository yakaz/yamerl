# $Id: Makefile.am 5090 2010-09-09 08:50:13Z jean.sebastien.pedron $

erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_yaml)/src

dist_erlsrc_DATA = yaml_parser.erl

CLEANFILES = Emakefile

all-local: Emakefile
	@$(ERL) +B -noshell						\
		-pa @top_builddir@/ebin					\
		-eval "case make:all() of				\
		         up_to_date ->					\
			   halt(0);					\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"

# Emakefile is generated automatically with the source files declared above.
Emakefile: Makefile.am Emakefile.in
	@echo "Generate: $@"
	@$(AWK) -v src='$(dist_erlsrc_DATA)'				\
	"{ print }							\
	/% DO NOT EDIT: Modules list is generated/ {			\
		indent = index(\$$0, \"%\");				\
		indent = substr(\$$0, 1, indent - 1);			\
		split(src, files);					\
		files_count = 0;					\
		for (file in files) {					\
			files_count++					\
		}							\
		mods_count = 0;						\
		for (i = 1; i <= files_count; i++) {			\
			if (files[i] !~ /\.erl$$/)			\
				continue;				\
			mods[++mods_count] = files[i]			\
		}							\
		for (i = 1; i <= mods_count; i++) {			\
			mod = mods[i];					\
			sub(/\.erl/, \"\", mod);			\
			if (i != mods_count)				\
				print indent \"'$(srcdir)/\" mod \"',\";\
			else						\
				print indent \"'$(srcdir)/\" mod \"'\"	\
		}							\
	}" Emakefile.in > $@
