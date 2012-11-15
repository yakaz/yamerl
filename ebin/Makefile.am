beamdir = $(ERLANG_INSTALL_LIB_DIR_yamerl)/ebin

APP_NAME  = $(PACKAGE_NAME)
APP_FILES = $(APP_NAME).app $(APP_NAME).appup

BEAM_FILES = *.beam

beam_DATA = $(APP_FILES) $(BEAM_FILES)

CLEANFILES = $(APP_NAME).app $(BEAM_FILES)

# $(APP_NAME).app is generated automatically with all the BEAM files
# present in the ebin/ directory.
$(APP_NAME).app: Makefile.am $(APP_NAME).app.in
	@echo "Generate: $@"
	@src=`ls $(BEAM_FILES) | sort`;					\
	$(AWK) -v src="$$src"						\
	"{ print }							\
	/% DO NOT EDIT: Modules list is generated/ {			\
		indent = index(\$$0, \"%\");				\
		indent = substr(\$$0, 1, indent - 1);			\
		split(src, mods);					\
		mods_count = 0;						\
		for (mod in mods) {					\
			mods_count++					\
		}							\
		for (i = 1; i <= mods_count; i++) {			\
			mod = mods[i];					\
			sub(/\.beam/, \"\", mod);			\
			if (i != mods_count)				\
				print indent mod \",\";			\
			else						\
				print indent mod			\
		}							\
	}" $(APP_NAME).app.in > $@
