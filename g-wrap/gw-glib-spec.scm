;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrap C pointers as guile-gtk Scheme side objects.
;;;

(define-module (g-wrap gw-glib-spec)
  :use-module (g-wrap))

(use-modules (g-wrap simple-type))

(let ((ws (gw:new-wrapset "gw-glib")))

  (gw:wrapset-set-guile-module! ws '(g-wrap gw-glib))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if (eq? client-wrapset wrapset)
         '()
         (list
          "#include <glib.h>\n"
          "#include <g-wrap-glib.h>\n"))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; gint64
  (gw:wrap-simple-type
   ws '<gw:gint64> "gint64"
   '("gw_glib_gint64_p(" scm-var ")")
   '(c-var " = gw_glib_scm_to_gint64(" scm-var ");\n")
   '(scm-var " = gw_glib_gint64_to_scm(" c-var ");\n"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; gint64
  (let* ((gchars (gw:wrap-type ws '<gw:gchars>)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const gchar *"
          "gchar *"))

    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad <gw:gchars> options form (caller and callee owned!)."
                   options-form))
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw 'gw:bad-typespec
                   "Bad <gw:gchars> options form (must be caller or callee owned!)."
                   options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec
                   "Bad <gw:gchars> options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let ((tmpcstr (gw:gen-c-tmp "cstr")))
        (list
         "if(SCM_FALSEP(" scm-var "))\n"
         "  " c-var " = NULL;\n"
         "else if(SCM_STRINGP(" scm-var "))\n"
         "{\n"
         "   char *" tmpcstr " = gh_scm2newstr(" scm-var ", NULL);\n"
         "  " c-var " = g_strdup(" tmpcstr ");\n"
         "  free(" tmpcstr ");\n"
         "}\n"
         "else\n"
         `(gw:error ,status-var type ,scm-var))))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list
       "  /* we coerce to (char *) here b/c broken guile 1.3.4 prototype */\n"
       "if(" c-var " == NULL) " scm-var " = SCM_BOOL_F;\n"
       "else\n"
       scm-var " = gh_str02scm((char *) " c-var ");\n"))
    
    (define (c-destructor c-var typespec status-var force?)
      (if (or force?
              (memq 'caller-owned (gw:typespec-get-options typespec)))
          (list "if(" c-var ") g_free((void *) " c-var ");\n")
          '()))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))

    (gw:type-set-c-type-name-func! gchars c-type-name-func)
    (gw:type-set-typespec-options-parser! gchars typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! gchars scm->c-ccg)
    (gw:type-set-c->scm-ccg! gchars c->scm-ccg)
    (gw:type-set-c-destructor! gchars c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! gchars pre-call-arg-ccg)
    (gw:type-set-call-ccg! gchars call-ccg)
    (gw:type-set-post-call-arg-ccg! gchars post-call-arg-ccg)
    (gw:type-set-post-call-result-ccg! gchars post-call-result-ccg)
    
    gchars)

  ;; ((glist-of (<gw:GtkWindow*> gw:const) gw:const) win-list)
  (let ((glo (gw:wrap-type ws 'gw:glist-of)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const GList*"
          "GList*"))
    
    ;; if this succeeds, the glist-of typespec-options will be
    ;; (sub-typespec (caller-owned | callee-owned) [const])
    (define (typespec-options-parser options-form wrapset)
      (if (null? options-form)
          (throw 'gw:bad-typespec
                 "Bad gw:glist-of options form (caller and callee owned!)."
                 options-form))
      (let* ((sub-typespec-form (car options-form))
             (glist-options (cdr options-form))
             (sub-typespec (gw:prototype-form->typespec sub-typespec-form
                                                        wrapset))
             (remainder (cdr options-form)))
        
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad gw:glist-of options form (caller and callee owned!)."
                   options-form))
        
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw
             'gw:bad-typespec
             "Bad gw:glist-of options form (must be caller or callee owned!)."
             options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            (cons sub-typespec glist-options)
            (throw 'gw:bad-typespec
                   "Bad gw:glist-of options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (glist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "scm_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-scm->c-ccg (gw:type-get-scm->c-ccg sub-type))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        
        (list
         "{\n"
         "  SCM " tmp-rest-var " = " scm-var ";\n"
         "  " c-var "= NULL;\n"
         "  while(!SCM_NULLP(" tmp-rest-var ")\n"
         "        && (! " `(gw:error? ,status-var) "))\n"
         "  {\n"
         "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "    SCM " tmp-sub-item-scm-var " = SCM_CAR(" tmp-rest-var ");\n"
         "\n"
         (sub-scm->c-ccg tmp-sub-item-c-var
                         tmp-sub-item-scm-var
                         sub-typespec
                         status-var)
         "\n"
         "    if(! " `(gw:error? ,status-var) " )\n"
         "    {\n"
         "       " c-var " = g_list_prepend (" c-var ", (gpointer)" tmp-sub-item-c-var");\n"
         "    }\n"
         "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
         "  }\n"
         "  if(!" `(gw:error? ,status-var) ")\n"
         "  {\n"
         "    " c-var " = g_list_reverse(" c-var ");\n"
         "  }\n"
         "  else\n"
         "  {\n"
         "    GList * " tmp-cursor " = " c-var ";\n"
         "    while(" tmp-cursor ")\n"
         "    {\n"
         "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #t)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "    }\n"
         "    g_list_free(" c-var ");\n"
         "    " c-var " = NULL;\n"
         "  }\n"
         "}\n")))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (glist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "c_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-c->scm-ccg (gw:type-get-c->scm-ccg sub-type)))
        
        (list
         "GList * " tmp-rest-var " = " c-var ";\n"
         scm-var "= SCM_EOL;\n"
         "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
         "{\n"
         "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "  SCM " tmp-sub-item-scm-var ";\n"
         "\n"
         "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-rest-var "->data") ";\n"
         "\n"
         (sub-c->scm-ccg tmp-sub-item-scm-var
                         tmp-sub-item-c-var
                         sub-typespec
                         status-var)
         "\n"
         "  if(! " `(gw:error? ,status-var) " )\n"
         "  {\n"
         "     " scm-var " = scm_cons (" tmp-sub-item-scm-var ", " scm-var ");\n"
         "  }\n"
         "  " tmp-rest-var " = " (string-append tmp-rest-var "->next") ";\n"
         "}\n"
         "if(!" `(gw:error? ,status-var) ")\n"
         "{\n"
         "  " scm-var " = scm_reverse(" scm-var ");\n"
         "}\n")))
    
    (define (c-destructor c-var typespec status-var force?)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        (list
         "{\n"
         "  GList * " tmp-cursor " = " c-var ";\n"
         "  while(" tmp-cursor ")\n"
         "  {\n"
         "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #f)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "  }\n"
         (if (or (memq 'caller-owned (gw:typespec-get-options typespec))
                 force?)
             (list "  if(" c-var ")\n"
                   "  {\n"
                   "    g_list_free(" c-var ");\n"
                   "    " c-var " = NULL;\n"
                   "  }\n")
             '())
         "}\n")))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (gw:type-set-c-type-name-func! glo c-type-name-func)
    (gw:type-set-typespec-options-parser! glo typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! glo scm->c-ccg)
    (gw:type-set-c->scm-ccg! glo c->scm-ccg)
    (gw:type-set-c-destructor! glo c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! glo pre-call-arg-ccg)
    (gw:type-set-call-ccg! glo call-ccg)
    (gw:type-set-post-call-result-ccg! glo post-call-result-ccg)
    (gw:type-set-post-call-arg-ccg! glo post-call-arg-ccg)
    
    glo)

  ;; ((gslist-of (<gw:GtkWindow*> gw:const) gw:const) win-list)
  (let ((glo (gw:wrap-type ws 'gw:gslist-of)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const GSList*"
          "GSList*"))
    
    ;; if this succeeds, the gslist-of typespec-options will be
    ;; (sub-typespec (caller-owned | callee-owned) [const])
    (define (typespec-options-parser options-form wrapset)
      (if (null? options-form)
          (throw 'gw:bad-typespec
                 "Bad gw:gslist-of options form (caller and callee owned!)."
                 options-form))
      (let* ((sub-typespec-form (car options-form))
             (gslist-options (cdr options-form))
             (sub-typespec (gw:prototype-form->typespec sub-typespec-form
                                                        wrapset))
             (remainder (cdr options-form)))
        
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad gw:gslist-of options form (caller and callee owned!)."
                   options-form))
        
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw
             'gw:bad-typespec
             "Bad gw:gslist-of options form (must be caller or callee owned!)."
             options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            (cons sub-typespec gslist-options)
            (throw 'gw:bad-typespec
                   "Bad gw:gslist-of options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (gslist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "scm_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-scm->c-ccg (gw:type-get-scm->c-ccg sub-type))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        
        (list
         "{\n"
         "  SCM " tmp-rest-var " = " scm-var ";\n"
         "  " c-var "= NULL;\n"
         "  while(!SCM_NULLP(" tmp-rest-var ")\n"
         "        && (! " `(gw:error? ,status-var) "))\n"
         "  {\n"
         "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "    SCM " tmp-sub-item-scm-var " = SCM_CAR(" tmp-rest-var ");\n"
         "\n"
         (sub-scm->c-ccg tmp-sub-item-c-var
                         tmp-sub-item-scm-var
                         sub-typespec
                         status-var)
         "\n"
         "    if(! " `(gw:error? ,status-var) " )\n"
         "    {\n"
         "       " c-var " = g_slist_prepend (" c-var ", (gpointer)" tmp-sub-item-c-var");\n"
         "    }\n"
         "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
         "  }\n"
         "  if(!" `(gw:error? ,status-var) ")\n"
         "  {\n"
         "    " c-var " = g_slist_reverse(" c-var ");\n"
         "  }\n"
         "  else\n"
         "  {\n"
         "    GSList * " tmp-cursor " = " c-var ";\n"
         "    while(" tmp-cursor ")\n"
         "    {\n"
         "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #t)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "    }\n"
         "    g_slist_free(" c-var ");\n"
         "    " c-var " = NULL;\n"
         "  }\n"
         "}\n")))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (gslist-options (cdr options))
             (tmp-rest-var (gw:gen-c-tmp "c_rest"))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (tmp-sub-item-scm-var (gw:gen-c-tmp "scm_item"))
             (sub-c->scm-ccg (gw:type-get-c->scm-ccg sub-type)))
        
        (list
         "GSList * " tmp-rest-var " = " c-var ";\n"
         scm-var "= SCM_EOL;\n"
         "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
         "{\n"
         "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "  SCM " tmp-sub-item-scm-var ";\n"
         "\n"
         "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-rest-var "->data") ";\n"
         "\n"
         (sub-c->scm-ccg tmp-sub-item-scm-var
                         tmp-sub-item-c-var
                         sub-typespec
                         status-var)
         "\n"
         "  if(! " `(gw:error? ,status-var) " )\n"
         "  {\n"
         "     " scm-var " = scm_cons (" tmp-sub-item-scm-var ", " scm-var ");\n"
         "  }\n"
         "  " tmp-rest-var " = " (string-append tmp-rest-var "->next") ";\n"
         "}\n"
         "if(!" `(gw:error? ,status-var) ")\n"
         "{\n"
         "  " scm-var " = scm_reverse(" scm-var ");\n"
         "}\n")))
    
    (define (c-destructor c-var typespec status-var force?)
      (let* ((options (gw:typespec-get-options typespec))
             (sub-typespec (car options))
             (sub-type (gw:typespec-get-type sub-typespec))
             (sub-item-c-type (gw:typespec-get-c-type-name sub-typespec))
             (tmp-sub-item-c-var (gw:gen-c-tmp "c_item"))
             (sub-destructor (gw:type-get-c-destructor sub-type))
             
             (tmp-cursor (gw:gen-c-tmp "cursor")))
        (list
         "{\n"
         "  GSList * " tmp-cursor " = " c-var ";\n"
         "  while(" tmp-cursor ")\n"
         "  {\n"
         "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
         "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
         (string-append tmp-cursor "->data") ";\n"
         (if sub-destructor
             (sub-destructor tmp-sub-item-c-var sub-typespec status-var #f)
             '())
         tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
         "  }\n"
         (if (or (memq 'caller-owned (gw:typespec-get-options typespec))
                 force?)
             (list "  if(" c-var ")\n"
                   "  {\n"
                   "    g_slist_free(" c-var ");\n"
                   "    " c-var " = NULL;\n"
                   "  }\n")
             '())
         "}\n")))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (gw:type-set-c-type-name-func! glo c-type-name-func)
    (gw:type-set-typespec-options-parser! glo typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! glo scm->c-ccg)
    (gw:type-set-c->scm-ccg! glo c->scm-ccg)
    (gw:type-set-c-destructor! glo c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! glo pre-call-arg-ccg)
    (gw:type-set-call-ccg! glo call-ccg)
    (gw:type-set-post-call-result-ccg! glo post-call-result-ccg)
    (gw:type-set-post-call-arg-ccg! glo post-call-arg-ccg)
    
    glo)

  ;;(gw:wrap-function
  ;; mod
  ;; 'gnc:glist->list
  ;; '<gw:scm> "gnc_glist_to_scm_list" '((<glib:GList*> glist) (<gw:wct> wct))
  ;; "Convert glist to scheme list of wcp's of type wct.")
  ;;
  ;;(gw:wrap-function
  ;; mod
  ;; 'gnc:list->glist
  ;; '<glib:GList*> "gnc_scm_list_to_glist" '((<gw:scm> wcp-list))
  ;; "Convert scheme list of wcp's to GList*.")
  ;;
  ;;(gw:wrap-function
  ;; mod
  ;; 'gnc:glist-map
  ;; '<gw:scm> "gnc_glist_scm_map" '((<gw:wct> wct) (<gw:scm> thunk) (<glib:GList*> glist))
  ;; "Call thunk on every element of glist after conversion to wcp of type wct, "
  ;; "and return a list of the results.")
  ;;
  ;;(gw:wrap-function
  ;; mod
  ;; 'gnc:glist-for-each
  ;; '<gw:void> "gnc_glist_scm_for_each"
  ;; '((<gw:wct> wct) (<gw:scm> thunk) (<glib:GList*> glist))
  ;; "Call thunk on every element of glist after conversion to wcp of type wct.")
  
  ws)
