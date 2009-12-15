guilemoduledir = $(datadir)/guile/site
gwrapmoduledir = $(guilemoduledir)/g-wrap
gwrapincludedir = $(includedir)/g-wrap
gwrapshlibdir = $(pkglibdir)/modules

SETGWPATH = "(set! %load-path (append (apply append \
                    (map (lambda (l) \
	                   (let ((ls (symbol->string l))) \
			      (list (string-append \"$(abs_top_builddir)/\" ls) \
				    (string-append \"$(abs_top_srcdir)/\" ls))))  \
	              '(guile scheme48))) \
	              '(\"$(abs_top_srcdir)\" \"$(abs_top_srcdir)/lib\") %load-path))"
