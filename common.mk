guilemoduledir = $(datadir)/guile/site
gwrapmoduledir = $(guilemoduledir)/g-wrap
gwrapincludedir = $(includedir)/g-wrap

SETGWPATH = "(set! %load-path (append '(\"$(top_builddir)/guile\" \"$(top_srcdir)/guile\" \"$(top_srcdir)\" \"$(top_srcdir)/lib\") %load-path))"
