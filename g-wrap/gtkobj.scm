;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrap C pointers as guile-gtk Scheme side objects.
;;;

(define-module (g-wrap gtkobj)
  :use-module (g-wrap))

(define wrapsets-initialized (make-hash-table 31))

(define-public (gw:wrap-gtkobj wrapset gtk-obj-name)
  ;; gtk-obj-name would be something like "GtkWindow"

  (define (mixed-case->lower-w-underscores str)
    ;; FooBar -> foo_bar
    ;; FOOBar -> foo_bar
    ;; FIXME GtkCList -> gtk_clist...
    (do ((i 0 (+ i 1))
         (result '()))
        ((= i (string-length str)) (list->string (reverse! result)))
      
      (let ((c (string-ref str i)))
        (cond
         ((char-upper-case? c)
          (if (and (> i 0)
                   (char-lower-case? (string-ref str (- i 1))))
              (set! result (apply list (char-downcase c) #\_ result))
              (set! result (cons (char-downcase c) result))))
         ((char-lower-case? c)
          (if (and (> i 1)
                   (char-upper-case? (string-ref str (- i 1)))
                   (char-upper-case? (string-ref str (- i 2))))
              (set! result (apply list c (car result) #\_ (cdr result)))
              (set! result (cons c result))))
         (else
          (set! result (cons c result)))))))
         
  (let* ((name-sym (string->symbol (string-append "<gw:" gtk-obj-name "*>")))
         (c-type-name (string-append gtk-obj-name "*"))
         (c-const-type-name (string-append "const " gtk-obj-name "*"))
         (c-type-func-call
          (string-append (mixed-case->lower-w-underscores gtk-obj-name)
                         "_get_type()"))
         (gtktype (gw:wrap-type wrapset name-sym)))

    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          c-const-type-name
          c-type-name))
      
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const options-form))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec "Bad wct options form." options-form))))

    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let ((c-type (gw:typespec-get-c-type-name typespec)))
        (list
         "if(!sgtk_is_a_gtkobj(" c-type-func-call ", " scm-var "))\n"
         `(gw:error ,status-var type ,scm-var)
         "else\n"
         (list c-var " = (" c-type ") sgtk_get_gtkobj(" scm-var ");\n"))))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list scm-var " = sgtk_wrap_gtkobj(GTK_OBJECT(" c-var "));\n"))

    (define (pre-call-arg-ccg param status-var)
      (let ((scm-name (gw:param-get-scm-name param))
            (c-name (gw:param-get-c-name param)))
        (list
         (scm->c-ccg c-name scm-name (gw:param-get-typespec param) status-var)
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
        (c->scm-ccg scm-name c-name typespec status-var)))

    (if (not (hashq-ref wrapsets-initialized wrapset #f))
        (begin          
          (gw:wrapset-add-cs-declarations!
           wrapset
           (lambda (wrapset client-wrapset)
             "#include <guile-gtk.h>\n"))
          
          (gw:wrapset-add-cs-initializers!
           wrapset
           (lambda (wrapset client-wrapset status-var)
             "sgtk_init();\n"))
          
          (hashq-set! wrapsets-initialized wrapset #t)))
    
    (gw:type-set-c-type-name-func! gtktype c-type-name-func)
    (gw:type-set-typespec-options-parser! gtktype typespec-options-parser)

    (gw:type-set-scm->c-ccg! gtktype scm->c-ccg)
    (gw:type-set-c->scm-ccg! gtktype c->scm-ccg)
    (gw:type-set-pre-call-arg-ccg! gtktype pre-call-arg-ccg)
    (gw:type-set-call-ccg! gtktype call-ccg)
    (gw:type-set-post-call-result-ccg! gtktype post-call-result-ccg)
    
    gtktype))
