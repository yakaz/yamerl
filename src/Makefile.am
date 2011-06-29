erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_yaml)/src

dist_erlsrc_DATA = yaml_app.erl						\
		   yaml_sup.erl						\
		   yaml_errors.erl					\
		   yaml_parser.erl					\
		   yaml_repr.erl					\
		   yaml_node_seq.erl					\
		   yaml_node_map.erl					\
		   yaml_node_str.erl					\
		   yaml_node_null.erl					\
		   yaml_node_bool.erl					\
		   yaml_node_int.erl					\
		   yaml_node_size.erl					\
		   yaml_node_timestamp.erl				\
		   yaml_node_ipaddr.erl					\
		   yaml_node_erlang_atom.erl				\
		   yaml_node_erlang_fun.erl

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
