guilemoduledir = $(datadir)/guile/site
gwrapmoduledir = $(guilemoduledir)/g-wrap
gwrapincludedir = $(includedir)/g-wrap
gwrapshlibdir = $(pkglibdir)/modules

SETGWPATH = "(set! %load-path (append (apply append \
                    (map (lambda (l) \
	                   (let ((ls (symbol->string l))) \
			      (list (string-append \"$(top_builddir)/\" ls) \
				    (string-append \"$(top_srcdir)/\" ls))))  \
	              '(guile scheme48))) \
	              '(\"$(top_srcdir)\" \"$(top_srcdir)/lib\") %load-path))"
