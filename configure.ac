AC_INIT([HDBC-postgresql package], [1.1])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([HDBC-postgresql.cabal])

AC_PATH_PROG(pgconfig,pg_config)
if test [ -z "$pgconfig" ]; then
    AC_MSG_ERROR([pg_config executable not found. Make sure pg_config is in your path])
else
    AC_MSG_CHECKING([PostgreSQL header dir])
    PGINCDIR=`${pgconfig} --includedir`
    AC_MSG_RESULT([$PGINCDIR])
    AC_MSG_CHECKING([PostgreSQL library dir])
    PGLIBDIR=`${pgconfig} --libdir`
    AC_MSG_RESULT([$PGLIBDIR])
fi

AC_SUBST([PGINCDIR])
AC_SUBST([PGLIBDIR])
AC_CONFIG_FILES([HDBC-postgresql.buildinfo])
AC_OUTPUT
