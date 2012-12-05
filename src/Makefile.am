erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_yamerl)/src

dist_erlsrc_DATA = yamerl_app.erl					\
		   yamerl_sup.erl					\
		   yamerl_errors.erl					\
		   yamerl_parser.erl					\
		   yamerl_constr.erl					\
		   yamerl_node_seq.erl					\
		   yamerl_node_map.erl					\
		   yamerl_node_str.erl					\
		   yamerl_node_str_json.erl				\
		   yamerl_node_null.erl					\
		   yamerl_node_null_json.erl				\
		   yamerl_node_bool.erl					\
		   yamerl_node_bool_json.erl				\
		   yamerl_node_bool_ext.erl				\
		   yamerl_node_int.erl					\
		   yamerl_node_int_json.erl				\
		   yamerl_node_int_ext.erl				\
		   yamerl_node_float.erl				\
		   yamerl_node_float_json.erl				\
		   yamerl_node_float_ext.erl				\
		   yamerl_node_size.erl					\
		   yamerl_node_timestamp.erl				\
		   yamerl_node_ipaddr.erl				\
		   yamerl_node_erlang_atom.erl				\
		   yamerl_node_erlang_fun.erl

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
