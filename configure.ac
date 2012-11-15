dnl ------------------------------------------------------------------
dnl Autoconf initialization.
dnl ------------------------------------------------------------------

AC_INIT([yamerl], [0.3.0])

AC_CONFIG_SRCDIR([src/yamerl_app.erl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

AM_INIT_AUTOMAKE([1.11 -Wall foreign])

AC_PREREQ([2.64])

ECHO=echo
COLORED_ECHO_INIT

dnl ------------------------------------------------------------------
dnl Internal functions for this configure script.
dnl ------------------------------------------------------------------

dnl EMKOPTS is used by Emakefile(s).
append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

dnl Expand shell variables to have a nice output in the final report.
expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}

dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

dnl Debugging option.
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=yes]]]),,
	enable_debug=yes)
if test "x${enable_debug}" = "xyes"; then
	append_to_EMKOPTS "{d, debug}"
fi

AC_ARG_ENABLE([warnings],
	AC_HELP_STRING([--enable-warnings],
	    [treat warnings as errors [[default=yes]]]),,
	enable_warnings=yes)
if test "x${enable_warnings}" = "xyes"; then
	append_to_EMKOPTS "warnings_as_errors"
fi

AC_ARG_ENABLE([export_all],
	AC_HELP_STRING([--enable-export-all],
	    [export all Erlang functions [[default=no]]]),,
	enable_export_all=no)
if test "x${enable_export_all}" = "xyes"; then
	append_to_EMKOPTS "export_all"
fi

dnl Include debug symbols.
append_to_EMKOPTS "debug_info"

dnl Print any warnings.
append_to_EMKOPTS "report_warnings"
append_to_EMKOPTS "{warn_format, 1}"
append_to_EMKOPTS "warn_export_vars"
append_to_EMKOPTS "warn_shadow_vars"
append_to_EMKOPTS "warn_unused_import"

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

echo
COLORED_ECHO([%BErlang environment%b])

dnl Available flags.
AC_ARG_WITH([erlang],
	AC_HELP_STRING([--with-erlang=PREFIX],
	    [prefix where Erlang is installed (optional)]),
	with_erlang=${withval%/},
	with_erlang="")

dnl erl(1) is used to compile Erlang modules.
if test "x${with_erlang}" = "x"; then
	AC_ERLANG_PATH_ERL
	AC_ERLANG_PATH_ERLC
else
	erl_path="${with_erlang}/bin"
	AC_ERLANG_PATH_ERL(, [$erl_path$PATH_SEPARATOR$PATH])
	AC_ERLANG_PATH_ERLC(, [$erl_path$PATH_SEPARATOR$PATH])
fi

if test "x${ERL}" = "x"; then
	AC_MSG_ERROR([
Erlang not found. Fill the ERL variable with erl(1) path or provide
Erlang prefix with --with-erlang.])
fi

dnl escript(1) is used by the testsuite.
AC_ARG_VAR([ESCRIPT], [Erlang/OTP interpreter command [autodetected]])

if test "x${ESCRIPT}" = "x"; then
	if test "x${with_erlang}" = "x"; then
		AC_PATH_PROG([ESCRIPT], [escript],,)
	else
		erl_path="${with_erlang}/bin"
		AC_PATH_PROG([ESCRIPT], [escript],,
		    [$erl_path$PATH_SEPARATOR$PATH])
	fi
else
	AC_MSG_CHECKING([for escript])
	AC_MSG_RESULT([$ESCRIPT])
fi

if test "x${ESCRIPT}" = "x"; then
	AC_MSG_WARN([
escript(1) not found. Fill the ESCRIPT variable with escript(1) path if
you want to use the testsuite.])
fi

dnl dialyzer(1) is used by the testsuite.
AC_ARG_VAR([DIALYZER], [Erlang/OTP discrepancy analyzer [autodetected]])

if test "x${DIALYZER}" = "x"; then
	if test "x${with_erlang}" = "x"; then
		AC_PATH_PROG([DIALYZER], [dialyzer],,)
	else
		erl_path="${with_erlang}/bin"
		AC_PATH_PROG([DIALYZER], [dialyzer],,
		    [$erl_path$PATH_SEPARATOR$PATH])
	fi
else
	AC_MSG_CHECKING([for dialyzer])
	AC_MSG_RESULT([$DIALYZER])
fi

if test "x${DIALYZER}" = "x"; then
	AC_MSG_WARN([
dialyzer(1) not found. Fill the DIALYZER variable with dialyzer(1) path if
you want it to be run during the testsuite.])
fi

dnl Declare ERL_LIBS as precious.
AC_ARG_VAR([ERL_LIBS], [Erlang/OTP applications search path [none]])

dnl Get Erlang $ROOT dir and lib dir.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

dnl Get ERTS version.
ERLANG_CHECK_ERTS
ERLANG_CHECK_RELEASE

dnl Erlang R14B02 (ERTS 5.8.3) is required.
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.8.3],
    [is_erlang_r14b02="yes"],
    [is_erlang_r14b02="no"])
if test "x${is_erlang_r14b02}" = "xno"; then
	AC_MSG_ERROR([
Erlang R14B02 is required but only Erlang $ERLANG_RELEASE was found!])
fi

dnl Determine directories for installation.
if test "x${prefix}" = "xNONE"; then
	dnl Inside Erlang lib directory.
	ERLANG_INSTALL_LIB_DIR="${ERLANG_LIB_DIR}"
else
	dnl Under $prefix
	ERLANG_INSTALL_LIB_DIR="${prefix}"
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

dnl ------------------------------------------------------------------
dnl Erlang applicatons.
dnl ------------------------------------------------------------------

dnl "compiler" install directory.
AC_ARG_WITH([erlang_compiler],
	AC_HELP_STRING([--with-erlang-compiler=PREFIX],
		       [prefix where the "compiler" Erlang application is installed (optional)]),
	with_erlang_compiler=${withval%/},
	with_erlang_compiler="")

ERL_LIBS_save=$ERL_LIBS
if test "x${with_erlang_compiler}" != "x"; then
	ERL_LIBS="${with_erlang_compiler}:${ERL_LIBS}"
fi
export ERL_LIBS
AC_ERLANG_CHECK_LIB([compiler],, [AC_MSG_ERROR([
"compiler" Erlang application not found.

Provide compiler install path with --with-erlang-compiler.])])
ERL_LIBS=$ERL_LIBS_save
export ERL_LIBS

dnl "tools" install directory.
AC_ARG_WITH([erlang_tools],
	AC_HELP_STRING([--with-erlang-tools=PREFIX],
		       [prefix where the "tools" Erlang application is installed (optional)]),
	with_erlang_tools=${withval%/},
	with_erlang_tools="")

ERL_LIBS_save=$ERL_LIBS
if test "x${with_erlang_tools}" != "x"; then
	ERL_LIBS="${with_erlang_tools}:${ERL_LIBS}"
fi
export ERL_LIBS
AC_ERLANG_CHECK_LIB([tools],, [AC_MSG_ERROR([
"tools" Erlang application not found.

Provide tools install path with --with-erlang-tools.])])
ERL_LIBS=$ERL_LIBS_save
export ERL_LIBS

dnl "eunit" install directory.
AC_ARG_WITH([erlang_eunit],
	AC_HELP_STRING([--with-erlang-eunit=PREFIX],
		       [prefix where the "eunit" Erlang application is installed (optional)]),
	with_erlang_eunit=${withval%/},
	with_erlang_eunit="")

ERL_LIBS_save=$ERL_LIBS
if test "x${with_erlang_eunit}" != "x"; then
	ERL_LIBS="${with_erlang_eunit}:${ERL_LIBS}"
fi
export ERL_LIBS
AC_ERLANG_CHECK_LIB([eunit],, [AC_MSG_WARN([
"eunit" Erlang application not found.

Provide eunit install path with --with-erlang-eunit.])])
ERL_LIBS=$ERL_LIBS_save
export ERL_LIBS

dnl ------------------------------------------------------------------
dnl Final substitutions.
dnl ------------------------------------------------------------------

AC_SUBST(EMKOPTS)

AC_SUBST(APP_RELEASE_DATE)
AC_SUBST(APP_DISTSUFFIX)

exp_ERLANG_INSTALL_LIB_DIR_yamerl=`expand_var ERLANG_INSTALL_LIB_DIR_yamerl`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_yamerl)

dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------

echo
AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
	include/Makefile
	src/Makefile
	src/Emakefile.in
	ebin/Makefile
	ebin/yamerl.app.in
	ebin/yamerl.appup
	testsuite/Makefile
	testsuite/etest
	Makefile
])
AC_CONFIG_FILES([
	testsuite/dialyzer
], [chmod +x testsuite/dialyzer])
AC_CONFIG_FILES([
	testsuite/benchmark
], [chmod +x testsuite/benchmark])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
COLORED_ECHO([ %B== ${PACKAGE_NAME} ${PACKAGE_VERSION}${APP_DISTSUFFIX} ==%b])
echo
COLORED_ECHO([Configuration:])
COLORED_ECHO([    Prefix:               ${prefix}])
COLORED_ECHO([    Application dir.:     ${exp_ERLANG_INSTALL_LIB_DIR_yamerl}])
echo
COLORED_ECHO([    Erlang emulator:      ${ERL}])
COLORED_ECHO([    Erlang compiler:      ${ERLC}])
COLORED_ECHO([    Erlang interpreter:   ${ESCRIPT}])
echo
COLORED_ECHO([    Debug:                ${enable_debug}])
COLORED_ECHO([    Warnings as errors:   ${enable_warnings}])
COLORED_ECHO([    Export all:           ${enable_export_all}])
echo
