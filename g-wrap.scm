;;;; File: g-wrap.scm
;;;; Copyright (C) 1996, 1997,1998 Christopher Lee
;;;; Copyright (C) 1999, 2000, 2001, 2002 Rob Browning
;;;; 
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;; MA 02139, USA.
;;;; 

(define-module (g-wrap)
  :use-module (g-wrap output-file)
  :use-module (g-wrap sorting)
  ;; FIXME: What does this one do?
  :use-module (g-wrap g-translate))

(use-modules (ice-9 slib))

(if (not (defined? 'simple-format))
    (begin
      (require 'format)
      (export simple-format)
      (define simple-format format)))

(define *available-wrapsets* (make-hash-table 31))

(define gw:*max-fixed-params* 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions

;; Translate any string to a string suitable for use as a C var or func name.
(define-public (gw:any-str->c-sym-str name)
  (define (char->string-replacement char)
    (cond
     ((char=? char #\?) "_p")
     ((char-alphabetic? char) (string char))
     ((char-numeric? char) (string char))
     (else "_")))

  (apply
   string-append
   (map
    char->string-replacement
    (string->list name))))

(define (scm-form-str->safe-c-str name)
  (define (char->string-replacement char)
    (case char
      ((#\") "\\\"")
      ((#\newline) "\\n")
      (else (string char))))
  (apply
   string-append
   (map
    char->string-replacement
    (string->list name))))

(define (gw:write outfile section-sym . text-lst)
  (outfile:add-to-section outfile section-sym text-lst))

(define (gw:trans-write outfile section-sym trans-assq text)
  (gw:write outfile section-sym (translate-str text trans-assq)))

(define (split-at-char char str)
  (let ((len (string-length str)))
    (let loop ((i 0)
	       (start 0)
	       (strings '()))
      (cond
       ((= i len)
	(reverse (cons (substring str start i) strings))) ;; return line
       ((eq? (string-ref str i) char)
	(loop (+ i 1) (+ i 1) (cons (substring str start i) strings)))
       (else
	(loop (+ i 1) start strings))))))

(define (gen-c-comment input-text)
  (let ((text
         (apply
          string-append
          (map (lambda (str)
                 (split-at-char #\newline (flatten-string str)))
               input-text))))
    (cond
     ((null? text) '())
     (else
      (let loop ((txt (cdr text))
		 (out (list (list "/* " (car text) "\n"))))
	(cond 
	 ((null? txt) (reverse (cons " */\n" out))) ;; return line
	 (else
	  (loop (cdr txt)
		(cons (list " * " (car txt) "\n") out)))))))))

(define-public (gw:any? pred some-list)
  (if (null? some-list)
      #f
      (or (pred (car some-list))
          (gw:any? pred (cdr some-list))))) 

(define-public (gw:every? pred some-list)
  (if (null? some-list)
      #t
      (and (pred (car some-list))
           (gw:every? pred (cdr some-list)))))            

(define (symlist? obj)
  (gw:every? symbol? obj))

(define-public gw:gen-c-tmp
  (let ((tmp-counter 0))
    (lambda (name)
      (let ((result
             (string-append "gw__tmp" (number->string tmp-counter) "_" name)))
        (set! tmp-counter (+ tmp-counter 1))
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The top-level g-wrap wrapset data-structure
;;;
;;; cs-initializers (wrapset client-only?) => string-tree.

(define *gw-wrapset-rtd*
  (make-record-type "gw:wrapset"
                    '(name
                      wrapped-types
                      wrapsets-depended-on
                      types-used

                      ch-declarations-funcs
                      cs-before-includes-funcs ;; pre-header-ccg
                      cs-declarations-funcs    ;; declarations-ccg
                      cs-definitions-funcs     ;; definitions-ccg
                      cs-initializers-funcs    ;; init-ccg

                      cs-wrapper-declarations-funcs
                      cs-wrapper-definitions-funcs
                      cs-wrapper-initializers-funcs

                      guile-module
                      guile-module-exports
                      )))

;;; initializers-funcs (wrapset client-wrapset status-var)
;;;   valid errors --> misc memory range type

;;; definitions-funcs (wrapset client-wrapset)

(define-macro (resolve-wrapset! ws func-name)
  `(if (string? ,ws)
       (let ((actual-wrapset (hash-ref *available-wrapsets* ,ws #f)))
         (if actual-wrapset
             (set! ,ws actual-wrapset)
             (error (string-append ,func-name
                                   " - wrapset \""
                                   ,ws "\" does not exist."))))))

(define-public gw:wrapset-get-name
  (record-accessor *gw-wrapset-rtd* 'name))

(define gw:wrapset-set-wrapped-types!
  (record-modifier *gw-wrapset-rtd* 'wrapped-types))
(define gw:wrapset-get-wrapped-types
  (record-accessor *gw-wrapset-rtd* 'wrapped-types))

(define gw:wrapset-set-wrapsets-depended-on!
  (record-modifier *gw-wrapset-rtd* 'wrapsets-depended-on))
(define gw:wrapset-get-wrapsets-depended-on
  (record-accessor *gw-wrapset-rtd* 'wrapsets-depended-on))

(define gw:wrapset-set-types-used!
  (record-modifier *gw-wrapset-rtd* 'types-used))
(define gw:wrapset-get-types-used
  (record-accessor *gw-wrapset-rtd* 'types-used))

(define gw:wrapset-get-ch-declarations-funcs
  (record-accessor *gw-wrapset-rtd* 'ch-declarations-funcs))
(define gw:wrapset-get-cs-before-includes-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-before-includes-funcs))
(define gw:wrapset-get-cs-declarations-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-declarations-funcs))
(define gw:wrapset-get-cs-definitions-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-definitions-funcs))
(define gw:wrapset-get-cs-initializers-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-initializers-funcs))

(define gw:wrapset-get-cs-wrapper-declarations-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-wrapper-declarations-funcs))
(define gw:wrapset-get-cs-wrapper-definitions-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-wrapper-definitions-funcs))
(define gw:wrapset-get-cs-wrapper-initializers-funcs
  (record-accessor *gw-wrapset-rtd* 'cs-wrapper-initializers-funcs))

(define gw:wrapset-set-ch-declarations-funcs!
  (record-modifier *gw-wrapset-rtd* 'ch-declarations-funcs))
(define gw:wrapset-set-cs-before-includes-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-before-includes-funcs))
(define gw:wrapset-set-cs-declarations-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-declarations-funcs))
(define gw:wrapset-set-cs-definitions-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-definitions-funcs))
(define gw:wrapset-set-cs-initializers-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-initializers-funcs))

(define gw:wrapset-set-cs-wrapper-declarations-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-wrapper-declarations-funcs))
(define gw:wrapset-set-cs-wrapper-definitions-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-wrapper-definitions-funcs))
(define gw:wrapset-set-cs-wrapper-initializers-funcs!
  (record-modifier *gw-wrapset-rtd* 'cs-wrapper-initializers-funcs))




(define-public gw:wrapset-add-ch-declarations!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'ch-declarations-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-ch-declarations!")
      (set-it! wrapset (cons func (gw:wrapset-get-ch-declarations-funcs wrapset))))))
  
(define-public gw:wrapset-add-cs-before-includes!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-before-includes-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-before-includes!")
      (set-it! wrapset
               (cons func (gw:wrapset-get-cs-before-includes-funcs wrapset))))))

(define-public gw:wrapset-add-cs-declarations!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-declarations-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-declarations!")
      (set-it! wrapset (cons func (gw:wrapset-get-cs-declarations-funcs wrapset))))))

(define-public gw:wrapset-add-cs-definitions!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-definitions-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-definitions!")
      (set-it! wrapset (cons func (gw:wrapset-get-cs-definitions-funcs wrapset))))))

(define-public gw:wrapset-add-cs-initializers!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-initializers-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-initializers!")
      (set-it! wrapset (cons func (gw:wrapset-get-cs-initializers-funcs wrapset))))))

(define-public gw:wrapset-add-cs-wrapper-declarations!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-wrapper-declarations-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-wrapper-declarations!")
      (set-it! wrapset (cons func (gw:wrapset-get-cs-wrapper-declarations-funcs wrapset))))))

(define-public gw:wrapset-add-cs-wrapper-definitions!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-wrapper-definitions-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-wrapper-definitions!")
      (set-it! wrapset (cons func (gw:wrapset-get-cs-wrapper-definitions-funcs wrapset))))))

(define-public gw:wrapset-add-cs-wrapper-initializers!
  (let ((set-it! (record-modifier *gw-wrapset-rtd* 'cs-wrapper-initializers-funcs)))
    (lambda (wrapset func)
      (resolve-wrapset! wrapset "gw:wrapset-add-cs-wrapper-initializers!")
      (set-it! wrapset (cons func (gw:wrapset-get-cs-wrapper-initializers-funcs wrapset))))))

(define-public gw:wrapset-set-guile-module!
  (record-modifier *gw-wrapset-rtd* 'guile-module))
(define-public gw:wrapset-get-guile-module
  (record-accessor *gw-wrapset-rtd* 'guile-module))

;; FIXME: we probably shouldn't be doing things this way, but until
;; g-wrap gathers and retains function signature data at runtime,
;; we're kinda stuck.
(define gw:wrapset-set-guile-module-exports!
  (record-modifier *gw-wrapset-rtd* 'guile-module-exports))

(define gw:wrapset-get-guile-module-exports
  (record-accessor *gw-wrapset-rtd* 'guile-module-exports))

(define-public (gw:wrapset-add-guile-module-export! wrapset scheme-sym)
  (gw:wrapset-set-guile-module-exports!
   wrapset
   (cons scheme-sym
         (gw:wrapset-get-guile-module-exports wrapset))))
   
(define gw:make-wrapset
  ;; Just create the record and set all the fields to #f.
  (let ((constructor (record-constructor *gw-wrapset-rtd* '(name))))
    (lambda (name)
      (let ((result (constructor name)))
        ;; initial, non-#f values.
        (gw:wrapset-set-wrapped-types! result '())
        (gw:wrapset-set-wrapsets-depended-on! result '())
        (gw:wrapset-set-types-used! result (make-hash-table 31))
        (gw:wrapset-set-ch-declarations-funcs! result '())
        (gw:wrapset-set-cs-before-includes-funcs! result '())
        (gw:wrapset-set-cs-declarations-funcs! result '())
        (gw:wrapset-set-cs-definitions-funcs! result '())
        (gw:wrapset-set-cs-initializers-funcs! result '())

        (gw:wrapset-set-cs-wrapper-declarations-funcs! result '())
        (gw:wrapset-set-cs-wrapper-definitions-funcs! result '())
        (gw:wrapset-set-cs-wrapper-initializers-funcs! result '())

        (gw:wrapset-set-guile-module-exports! result '())
        result))))

(define-public (gw:wrapset-uses-type? wrapset type)
  (hashq-ref (gw:wrapset-get-types-used wrapset) type #f))


(define (add-wrapset-types-ccg-output wrapset port ccg-key)

  (define (ws-run-type-ccgs-for-client wrapset client-wrapset)
    (simple-format #t " for-client ~A ~A\n"
                   (gw:wrapset-get-name wrapset)
                   (gw:wrapset-get-name client-wrapset))
    ;; Call the init funcs for all this wrapset's types for the
    ;; client-wrapset.
    (for-each
     (lambda (type)
       (if (gw:wrapset-uses-type? client-wrapset type)
           (let ((ccg (hashq-ref type ccg-key #f)))
             (if ccg (flatten-display (ccg type client-wrapset) port)))))
     (map cdr (gw:wrapset-get-wrapped-types wrapset))))
        
  (define (ws-run-type-ccgs-for-provider provider)
    (simple-format #t " for-parent ~A\n" (gw:wrapset-get-name provider))
    (for-each
     (lambda (type)
       (let ((ccg (hashq-ref type ccg-key #f)))
         (if ccg
             (begin
               ;; call ccg once if the wrapset wrapset uses it's own type
               (if (gw:wrapset-uses-type? provider type)
                   (flatten-display (ccg type provider) port))
               ;; call ccg once no matter what.
               (flatten-display (ccg type #f) port)))))
     (map cdr (gw:wrapset-get-wrapped-types provider))))

  (simple-format #t "add-ws-t ~A ~A ~A\n"
                 (gw:wrapset-get-name wrapset)
                 port
                 ccg-key)

  ;; Run all the output funcs from wrapsets this one depends on.
  (for-each
   (lambda (depended-on-wrapset)
     (ws-run-type-ccgs-for-client depended-on-wrapset wrapset))
   (gw:wrapset-get-wrapsets-depended-on wrapset))
  
  ;; Run this wrapset's output funcs for itself.
  (ws-run-type-ccgs-for-provider wrapset))

(define (add-wrapset-types-initializer-ccg-output wrapset port ccg-key)

  (let* ((status-var (gw:gen-c-tmp "err_status"))
         (err-misc-msg-var (gw:gen-c-tmp "err_misc_msg"))
         (err-data-var (gw:gen-c-tmp "err_data"))
         (wrapset-name (gw:wrapset-get-name wrapset))
         (wrapset-name-c-sym (gw:any-str->c-sym-str wrapset-name))
         (wrapset-init-func (string-append "gw_init_wrapset_"
                                           wrapset-name-c-sym)))
    
    (define (output-initializer-code func type client-wrapset)
      (let ((code (func type client-wrapset status-var)))
        
        (if (not (null? code))
            (begin
              (flatten-display
               (gw:expand-special-forms code #f '(type range memory misc))
               port)
              (flatten-display
               (list
                "if (" status-var " != GW__ERR_NONE)"
                "  gw__handle_wrapper_error (" status-var ",\n"
                "                            \"" wrapset-init-func "\",\n"
                "                            0,\n"
                "                            " err-misc-msg-var ",\n"
                "                            " err-data-var ");\n")
               port)))))
    
    (define (ws-run-type-ccgs-for-client wrapset client-wrapset)
      ;; Call the init funcs for all this wrapset's types for the
      ;; client-wrapset.
      (for-each
       (lambda (type)
         (if (gw:wrapset-uses-type? client-wrapset type)
             (let ((ccg (hashq-ref type ccg-key #f)))
               (if ccg (output-initializer-code ccg type client-wrapset)))))
       (map cdr (gw:wrapset-get-wrapped-types wrapset))))
        
    (define (ws-run-type-ccgs-for-provider provider)
      (for-each
       (lambda (type)
         (let ((ccg (hashq-ref type ccg-key #f)))
           (if ccg
               (begin
                 ;; call ccg once if the wrapset wrapset uses it's own type
                 (if (gw:wrapset-uses-type? provider type)
                     (output-initializer-code ccg type provider))
                 ;; call ccg once no matter what.
                 (output-initializer-code ccg type #f)))))
       (map cdr (gw:wrapset-get-wrapped-types provider))))
    
    ;; Run all the output funcs from wrapsets this one depends on.
    (display "{\n" port)
    (display (string-append
              "   enum GW__ErrorStatus " status-var " = GW__ERR_NONE;\n") port)
    (display (string-append "   SCM " err-data-var " = SCM_UNSPECIFIED;\n") port)
    (display (string-append "   char *" err-misc-msg-var " = NULL;\n") port)
    (display (string-append "   (void) " status-var ";\n") port)
    (display (string-append "   (void) " err-data-var ";\n") port)
    (display (string-append "   (void) " err-misc-msg-var ";\n") port)

    (for-each
     (lambda (depended-on-wrapset)
       (ws-run-type-ccgs-for-client depended-on-wrapset wrapset))
     (gw:wrapset-get-wrapsets-depended-on wrapset))
    
    ;; Run this wrapset's output funcs for itself.
    (ws-run-type-ccgs-for-provider wrapset)
    
    (display "}\n" port)))

(define (gw:generate-error-handler wrapset port)
  (display "\
enum GW__ErrorStatus
{
  GW__ERR_NONE,
  GW__ERR_MISC,
  GW__ERR_MEMORY,
  GW__ERR_RANGE,
  GW__ERR_TYPE,
  GW__ERR_ARGC,
  GW__ERR_ARG_RANGE,
  GW__ERR_ARG_TYPE
};
 
static void
gw__handle_wrapper_error(enum GW__ErrorStatus status,
                         const char *func_name,
                         unsigned int arg_pos,
                         const char *misc_msg,
                         SCM scm_data) __attribute__ ((noreturn));

static void
gw__handle_wrapper_error(enum GW__ErrorStatus status,
                         const char *func_name,
                         unsigned int arg_pos,
                         const char *misc_msg,
                         SCM scm_data)
{
  static SCM out_of_range_key = SCM_BOOL_F;
  static SCM wrong_type_key = SCM_BOOL_F;

  if(SCM_FALSEP(out_of_range_key))
    out_of_range_key = scm_permanent_object(scm_c_make_keyword(\"out-of-range\"));
  if(SCM_FALSEP(wrong_type_key))
    wrong_type_key = scm_permanent_object(scm_c_make_keyword(\"wrong-type\"));

  switch(status) {
  case GW__ERR_NONE:
    scm_misc_error(func_name,
                   \"asked to handle error when there wasn't one\",
                   SCM_EOL);
    break;
  case GW__ERR_MISC:
    /* scm_data is a list of format args for misc_msg */
    scm_misc_error(func_name, misc_msg, scm_data); break;
  case GW__ERR_MEMORY:
    scm_memory_error(func_name); break;
  case GW__ERR_RANGE:
    scm_error (out_of_range_key,
	       func_name,
	       \"Out of range: ~S\",
               scm_cons (scm_data, SCM_EOL),
	       SCM_BOOL_F);
    break;
  case GW__ERR_TYPE:
    scm_error(wrong_type_key,
              func_name,
              \"Wrong type: \",
              scm_cons (scm_data, SCM_EOL),
              SCM_BOOL_F);
    break;
  case GW__ERR_ARGC:
    scm_wrong_num_args(scm_makfrom0str(func_name)); break;
  case GW__ERR_ARG_RANGE:
    /* scm_data is the bad arg */
    scm_out_of_range(func_name, scm_data); break;
  case GW__ERR_ARG_TYPE:
    /* scm_data is the bad arg */
    scm_wrong_type_arg(func_name, arg_pos, scm_data); break;
  default:
    scm_misc_error(func_name,
                   \"asked to handle nonexistent gw:error type: ~S\",
                   scm_cons(scm_long2num(status), SCM_EOL));    
    break;
  };
  exit(1);
}
"
port))
  

(define-public (gw:new-wrapset wrapset-name)

  (let ((wrapset (gw:make-wrapset wrapset-name))
        (existing-wrapset (hash-ref *available-wrapsets* wrapset-name)))

    (if existing-wrapset
        (error (string-append 
                "gw:new-wrapset: wrapset "
                wrapset-name " already exists.") existing-wrapset)
        (hash-set! *available-wrapsets* wrapset-name wrapset))

    wrapset))

(define (guile-module-name->c-registration-strlist name-symlist)
  (separate-by (map symbol->string name-symlist) " "))

(define (guile-module-name->c-sym-name-strlist name-symlist)
  (separate-by
   (map (lambda (s) (gw:any-str->c-sym-str (symbol->string s)))
        name-symlist)
   "_"))

(define (gw:wrapset-add-type! wrapset type-obj)
  (gw:wrapset-set-wrapped-types!
   wrapset 
   (cons
    (cons (gw:type-get-name type-obj)
          type-obj)
    (gw:wrapset-get-wrapped-types wrapset))))

(define (gw:wrapset-lookup-type-and-mark-usage main-wrapset type-name-sym)
  (let* ((types (gw:wrapset-get-wrapped-types main-wrapset))
         (type-in-main-wrapset (assq type-name-sym types)))
    (if type-in-main-wrapset
        (begin
          (hashq-set! (gw:wrapset-get-types-used main-wrapset)
                      (cdr type-in-main-wrapset)
                      (cdr type-in-main-wrapset))
          (cdr type-in-main-wrapset))
        (let loop ((places-to-look
                    (gw:wrapset-get-wrapsets-depended-on main-wrapset)))

          (if (null? places-to-look)
              (error "get-type: type not found:" type-name-sym))

          (let* ((wrapset (car places-to-look))
                 (types (gw:wrapset-get-wrapped-types wrapset))
                 (type-cc (assq type-name-sym types)))
            (if (not type-cc)
                (loop (cdr places-to-look))
                (begin
                  (hashq-set! 
                   (gw:wrapset-get-types-used main-wrapset)
                   (cdr type-cc)
                   (cdr type-cc))
                  (cdr type-cc))))))))

(define-public (gw:available-wrapsets)
  *available-wrapsets*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typespecs -- i.e. what you get from (<gw:foo> const caller-owned)
;;; the type, and whatever options it recognizes.  Evenutally we'll
;;; have types handle the form->typespec parsing themselves...

(define-public (gw:make-typespec type options)
  (list type options 'is-a-gw:typespec))

(define-public (gw:typespec-get-type ts) (car ts))
(define-public (gw:typespec-get-options ts) (cadr ts))
(define-public (gw:typespec-get-c-type-name ts)
  (let ((name-func (gw:type-get-c-type-name-func (gw:typespec-get-type ts))))
    (name-func ts)))

(define-public (gw:prototype-form->typespec form wrapset)
  (define (default-options-parser options wrapset)
    ;; default is to allow 'foo only, not '(foo)
    (if (list? form)
        (throw 'gw:bad-typespec #f
               (simple-format
                #f
                "bad typespec ~S -- a typespec may only be a symbol by default"
                form)
               form)
        #f))
  
  (let* ((type-sym (if (list? form) (car form) form))
         (type (gw:wrapset-lookup-type-and-mark-usage wrapset type-sym))
         (parse-options (or (gw:type-get-typespec-options-parser type)
                            default-options-parser))
         (options (parse-options (if (list? form) (cdr form) '()) wrapset)))
    (gw:make-typespec type options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function spec parameter data
;;;
;;; FIXME: this is leftover stuff that should probably be reworked.

(define (make-param name typespec number)
  (let* ((c-name (string-append "gw__c_arg" (number->string number)))
         (scm-name 
          (if (< number gw:*max-fixed-params*)
              (string-append "gw__scm_arg" (number->string number))
              (string-append
               "gw__scm_extras[" (number->string
                                  (- number
                                     gw:*max-fixed-params*)) "]"))))
    
    (vector 'param
            name
            c-name
            scm-name
            typespec
            number)))

(define-public (gw:param-get-name x) (vector-ref x 1))
(define-public (gw:param-get-c-name x) (vector-ref x 2))
(define-public (gw:param-get-scm-name x) (vector-ref x 3))
(define-public (gw:param-get-typespec x) (vector-ref x 4))
(define-public (gw:param-get-type x)
  (gw:typespec-get-type (gw:param-get-typespec x)))
(define-public (gw:param-get-options x)
  (gw:typespec-get-options (gw:param-get-typespec x)))

(define-public (gw:param-get-number x) (vector-ref x 5))

(define-public (gw:param-get-c-type-name x)
  (gw:typespec-get-c-type-name (gw:param-get-typespec x)))

(define (param-specs->params param-specs wrapset)
  (let loop ((remainder param-specs) (n 0))
    (if (null? remainder)
        '()
        (let* ((param-spec (car remainder))
               (spec-name (cadr param-spec))
               (typespec (gw:prototype-form->typespec (car param-spec) wrapset)))
          (cons
           (make-param spec-name typespec n)
           (loop (cdr remainder) (+ n 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function spec return value data
;;;
;;; FIXME: this is leftover stuff that should probably be reworked.

;; A return-type can be a symbol (naming the type) or a list of the form
;; '(type [ options ... ]).
(define (result-spec->result spec wrapset)
  (let* ((typespec (gw:prototype-form->typespec spec wrapset))
         (type (gw:typespec-get-type typespec)))
    (cons typespec 'gw:result)))

(define-public (gw:result-get-c-name r) "gw__c_result")
(define-public (gw:result-get-scm-name r) "gw__scm_result")
(define-public (gw:result-get-typespec r) (car r))
(define-public (gw:result-get-type r) (gw:typespec-get-type (car r)))
(define-public (gw:result-get-options r) (gw:typespec-get-options (car r)))

(define-public (gw:result-get-c-type-name x)
  (gw:typespec-get-c-type-name (car x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic type data
;;;
;;; This data structure is what all other types are based on.
;;; Basically, in order to specify any other wrapped type, you create
;;; one of these and put the right things in its fields.
;;;
;;; It is a hash, and all keys starting with gw: are reserved by
;;; g-wrap, the others may be used by anyone building a new, specific
;;; kind of type on top of one of these. (thinking about changing this
;;; to have a separate child hash-table, or eliminating it
;;; altogether...)

;;;
;;; what happens with arg options?
;;;
;;; relevant hash table entries:
;;; 
;;; gw:name -- something like '<gw:int>
;;; gw:wrapset -- pointer to the wrapset providing the type.
;;;
;;; gw:global-declarations-ccg (type client-wrapset)
;;; gw:global-definitions-ccg (type client-wrapset)
;;; gw:global-initializations-ccg (type client-wrapset)
;;;
;;; gw:c-type-name-func (typespec) => "Foo *" or "const Foo*", etc...
;;; gw:typespec-options-parser (options-form) => typespec-options
;;;    throws 'gw:bad-typespec, etc.  options-form will always be a
;;;    list, even if original typespec-form was just the type symbol.
;;;    If there are any sub-types in typespec, then this type must
;;;    access them by calling (gw:wrapset-lookup-type-and-mark-foreign
;;;    wrapset type-sym)
;;;
;;; gw:scm->c-ccg (c-var scm-var typespec status-var)
;;; gw:c->scm-ccg (scm-var c-var typespec status-var)
;;; gw:c-destructor (c-var typespec status-var force?)
;;;
;;; gw:pre-call-arg-ccg (param status-var)
;;;
;;;   Normally must (at least) declare C param variable, check the
;;;   type of the incoming scm param, and if not OK, "call" (gw:error
;;;   status-var arg-type).  If type is OK, then must convert scm arg
;;;   to C and assign to param variable.  [Code generated will always
;;;   be put in new scope.]
;;;
;;; gw:pre-call-result-ccg (result status-var)
;;;
;;;   [Code generated will always be put in new scope.]
;;;
;;; gw:call-ccg (result func-call-code status-var)
;;;   Normally must (at least) assign func-call-code (a string) to C result var.
;;;
;;; gw:post-call-result-ccg (result status-var)
;;;
;;;   Normally must at least convert the C result and assign it to the
;;;   scm result.  Scope for matching result above will be closed after
;;;   this.
;;;
;;; gw:post-call-arg-ccg (param status-var)
;;;   Optional.  Scope for matching param above will be closed after this.
;;;
;;; If a bad arg is found, processing will stop, but each of the
;;; "post-call" chunks will be run for each matching pre-call chunk
;;; that has already been run.

(define-public (gw:wrap-type wrapset name-sym)
  (let ((result (make-hash-table 17)))
    (resolve-wrapset! wrapset "gw:wrap-type")
    (hashq-set! result 'gw:name name-sym)    
    (hashq-set! result 'gw:wrapset wrapset)
    (gw:wrapset-add-type! wrapset result)
    result))

(define-public (gw:type-get-name t)
  (hashq-ref t 'gw:name))

(define-public (gw:type-get-wrapset t)
  (hashq-ref t 'gw:wrapset))

(define-public (gw:type-set-c-type-name-func! t func)
  (hashq-set! t 'gw:c-type-name-func func))
(define-public (gw:type-get-c-type-name-func t)
  (hashq-ref t 'gw:c-type-name-func))

(define-public (gw:type-set-typespec-options-parser! t func)
  (hashq-set! t 'gw:typespec-options-parser func))
(define-public (gw:type-get-typespec-options-parser t)
  (hashq-ref t 'gw:typespec-options-parser))

(define-public (gw:type-set-global-initializations-ccg! t generator)
  (hashq-set! t 'gw:global-initializations-ccg generator))
(define-public (gw:type-set-global-declarations-ccg! t generator)
  (hashq-set! t 'gw:global-declarations-ccg generator))
(define-public (gw:type-set-global-definitions-ccg! t generator)
  (hashq-set! t 'gw:global-definitions-ccg generator))

(define-public (gw:type-set-c->scm-ccg! t func)
  (hashq-set! t 'gw:c->scm-ccg func))
(define-public (gw:type-set-scm->c-ccg! t func)
  (hashq-set! t 'gw:scm->c-ccg func))
(define-public (gw:type-set-c-destructor! t func)
  (hashq-set! t 'gw:c-destructor func))

(define-public (gw:type-get-c->scm-ccg t)
  (hashq-ref t 'gw:c->scm-ccg #f))
(define-public (gw:type-get-scm->c-ccg t)
  (hashq-ref t 'gw:scm->c-ccg #f))
(define-public (gw:type-get-c-destructor t)
  (hashq-ref t 'gw:c-destructor #f))


(define-public (gw:type-set-pre-call-result-ccg! t generator)
  (hashq-set! t 'gw:pre-call-result-ccg generator))
(define-public (gw:type-set-pre-call-arg-ccg! t generator)
  (hashq-set! t 'gw:pre-call-arg-ccg generator))
(define-public (gw:type-set-call-ccg! t generator)
  (hashq-set! t 'gw:call-ccg generator))
(define-public (gw:type-set-post-call-arg-ccg! t generator)
  (hashq-set! t 'gw:post-call-arg-ccg generator))
(define-public (gw:type-set-post-call-result-ccg! t generator)
  (hashq-set! t 'gw:post-call-result-ccg generator))

(define-public (gw:type-declare-scm-result-var?! t val)
  (hashq-set! t 'gw:declare-scm-result-var? val))
(define-public (gw:type-declare-scm-result-var? t)
  (hashq-ref t 'gw:declare-scm-result-var? #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation code.

(define type-index-generator #f)
(define function-index-generator #f)
(define constant-index-generator #f)

(define (make-index-generator kind)
  (let ((elements '())
	(category #f)
	(title #f))
    (let ((add-element (lambda (el)
			  (set! elements (cons el elements))))
	  (elements->html
	   (lambda ()
	     (if (null? elements)
		 (list "<p>(no " category " defined)</p>\n")
		 (list
		  "<h3>" title "</h3>"
		  (separate-by
		   (map 
		    (lambda (item)
		      (list 
		       "<a href=\"#" category "-" item "\">" item "</a>"))
                    (sort (map
                           (lambda (x)
                             (if (symbol? x)
                                 (symbol->string x)
                                 x))
                           elements)
                          string<?))
		   " |\n")
		  "\n")))))
      (let ((info
	     (assq kind '((constants "constants" "Constants")
			  (functions "functions" "Functions")
			  (types     "types"     "Types")))))
	(if (not info)
	    (error 
	     "make-index-generator: use constants, functions or types"))
	(set! category (cadr info))
	(set! title (caddr info)))

      (lambda (dispatch-command)
	(case dispatch-command
	  ((add) add-element)
	  ((output) elements->html)
	  (else
	   (error "index-generator: don't know command " dispatch-command))))
      )))
	  
(define (gwrap-c-doc-type wrapset type description)
  (let ((output (gw:wrapset-get-doc-file wrapset)))
    (outfile:add-to-section
     output 'types 
     (list
      "  <dt> <a name=\"types-" type "\">"
      "<strong>type <em>" type "</em></strong></a>\n"
      "  <dd> " description "\n"))))

(define (gwrap-c-doc-function wrapset scheme-name declaration description)
  (let ((output (gw:wrapset-get-doc-file wrapset)))
    (outfile:add-to-section 
     output 'functions
     (list
      "  <dt> <a name=\"functions-" scheme-name "\">"
      "<strong>" declaration "</strong></a>\n"
      "  <dd> " description "\n"))))

(define (gwrap-c-doc-constant wrapset constant type description)
  (let ((output (gw:wrapset-get-doc-file wrapset)))
    (outfile:add-to-section 
     output 'constants
     (list
      "  <dt> <a name=\"constants-" constant "\"><strong>constant <tt>"
      constant 
      "</tt> (type <em>" type "</em></strong>)</a>\n"
      "  <dd> " description "\n"))))

(define (generate-html-doc-file wrapset)
  (let* ((wrapset-name (gw:wrapset-get-name wrapset))
         (doc-file-name (string-append wrapset-name ".html")))
    ;; ???
    (call-with-output-file doc-file-name
      (lambda (port)

        (define doc-file-tmpl "\
<html> <head>\n<title>Documentation for %wrapset-name%</title>
</head>
<body text=\"#000000\" bgcolor=\"#ffffff\">
<h1> Documentation for %wrapset-name%</h1>
<!-- Generated by G-Wrap -->

<h2>Index</h2>
%type-index%
%fn-index%
%const-index%

<h2>Types</h2>
<dl>
%types%
</dl>

<h2>Constants</h2>
<dl>
%constants%
</dl>

<h2>Functions</h2>
<dl>
%functions%
</dl>
")

        ;;(set! type-index-generator (make-index-generator 'types))
        ;;(set! function-index-generator (make-index-generator 'functions))
        ;;(set! constant-index-generator (make-index-generator 'constants))
        ;;(gw:wrapset-set-doc-file!
        ;; wrapset
        ;; (text->outfile
        ;;  (string-append wrapset-name ".html")
        ;;  doc-file-tmpl
        ;;  `((type-index . ,type-index-generator)
        ;;    (fn-index . ,function-index-generator)
        ;;    (const-index . ,constant-index-generator))))

        ;; scheme-sym is function scm-sym-name.
        ;;(if function-index-generator
        ;;    ((function-index-generator 'add) scheme-sym))
        ;;
        ;;(gwrap-c-doc-function wrapset
        ;;                      scheme-sym
        ;;                      (caar description) 
        ;;                      (list "<em>" (cdar description) "</em><br>\n"
        ;;                            (cdr description)))
        
        ;;(outfile:close (gw:wrapset-get-doc-file wrapset))
        #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrapping code.

(define (generate-scm-file wrapset)
  (let* ((wrapset-name (gw:wrapset-get-name wrapset))
         (wrapset-name-c-sym (gw:any-str->c-sym-str wrapset-name))
         (wrapset-scm-file-name (string-append wrapset-name ".scm"))
         (guile-module (gw:wrapset-get-guile-module wrapset))
         (guile-module-exports (gw:wrapset-get-guile-module-exports wrapset)))
    
    (call-with-output-file wrapset-scm-file-name
      (lambda (port)
        
        (define (dsp-list lst)
          (for-each (lambda (s) (display s port)) lst))
        
        (dsp-list
         (list
          ";; Generated by G-Wrap: an experimental Guile C API-wrapper engine.\n"
          "\n"
          "(define-module " guile-module ")\n"
          "\n"))

        (for-each
         (lambda (sym)
           (write `(export ,sym) port)
           (newline port))
         (reverse guile-module-exports))

        (dsp-list
         (list
          "(cond\n"
          " ((or (string=? (version) \"1.3\")\n"
          "      (string=? (version) \"1.3.4\"))\n"
          "  (dynamic-call \"gw_init_wrapset_" wrapset-name-c-sym "\"\n"
          "                (dynamic-link \"lib" wrapset-name ".so\")))\n"
          " (else\n"
          "  (dynamic-call \"gw_init_wrapset_" wrapset-name-c-sym "\"\n"
          "                (dynamic-link \"lib" wrapset-name "\"))))\n"))))))

(define (generate-c-header-file wrapset)
  (let* ((wrapset-name (gw:wrapset-get-name wrapset))
         (wrapset-header-name (string-append wrapset-name ".h"))
         (wrapset-name-c-sym (gw:any-str->c-sym-str wrapset-name))
         (wrapset-header-def-sym (make-header-def-sym wrapset-header-name)))
         
    (call-with-output-file wrapset-header-name
      (lambda (port)
        (for-each
         (lambda (s) (display s port))
         (list
          "/* Generated by G-Wrap: an experimental C->Guile wrapper engine */\n"
          "\n"
          "#ifndef " wrapset-header-def-sym "\n"
          "#define " wrapset-header-def-sym "\n"
          "\n"
          "#ifdef __cplusplus\n"
          "extern \"C\" {\n"
          "#endif\n"
          "\n"
          "void gw_init_wrapset_" wrapset-name-c-sym "(void);\n"
          "\n"
          "#ifdef __cplusplus\n"
          "}\n"
          "#endif\n"
          "#endif\n"))))))


(define-public (gw:inline-scheme . code-chunks)
  (map
   (lambda (chunk)
     (list "gh_eval_str(\""
           (scm-form-str->safe-c-str
            (call-with-output-string
             (lambda (port)
               (write chunk port))))
           "\");\n"))
   code-chunks))

(define (run-wrapset-output-funcs wrapset funcs-getter port)
  (list
   
   ;; Run all the output funcs from wrapsets this one depends on.
   (map
    (lambda (depended-on-wrapset)
      (map
       (lambda (func)
         (flatten-display (func depended-on-wrapset wrapset) port))
       (reverse (funcs-getter depended-on-wrapset))))
    (gw:wrapset-get-wrapsets-depended-on wrapset))
   
   ;; Run this wrapset's output funcs for itself.
   (map
    (lambda (func)
      (flatten-display (func wrapset #f) port))
    (reverse (funcs-getter wrapset)))))

(define (run-wrapset-initializer-output-funcs wrapset funcs-getter port)
  (let* ((status-var (gw:gen-c-tmp "status_var"))
         (err-misc-msg-var (gw:gen-c-tmp "err_misc_msg"))
         (err-data-var (gw:gen-c-tmp "err_data"))
         (wrapset-name (gw:wrapset-get-name wrapset))
         (wrapset-name-c-sym (gw:any-str->c-sym-str wrapset-name))
         (wrapset-init-func (string-append "gw_init_wrapset_"
                                           wrapset-name-c-sym)))
    
    (define (output-initializer-code func provider-wrapset client-wrapset)
      (let ((code (func provider-wrapset client-wrapset status-var)))

        (if (not (null? code))
            (begin
              (flatten-display
               (gw:expand-special-forms code #f '(type range memory misc))
               port)
              (flatten-display
               (list
                "if (" status-var " != GW__ERR_NONE)"
                "  gw__handle_wrapper_error (" status-var ",\n"
                "                            \"" wrapset-init-func "\",\n"
                "                            0,\n"
                "                            " err-misc-msg-var ",\n"
                "                            " err-data-var ");\n")
               port)))))

    (list

     (display "{\n" port)
     (display (string-append
               "   enum GW__ErrorStatus " status-var " = GW__ERR_NONE;\n") port)
     (display (string-append "   SCM " err-data-var " = SCM_UNSPECIFIED;\n")
              port)
     (display (string-append "   char *" err-misc-msg-var " = NULL;\n") port)
     (display (string-append "   (void) " status-var ";\n") port)
     (display (string-append "   (void) " err-data-var ";\n") port)
     (display (string-append "   (void) " err-misc-msg-var ";\n") port)
     
     
     ;; Run all the output funcs from wrapsets this one depends on.
     (map
      (lambda (depended-on-wrapset)
        (map
         (lambda (func)
           (output-initializer-code func depended-on-wrapset wrapset))
         (reverse (funcs-getter depended-on-wrapset))))
      (gw:wrapset-get-wrapsets-depended-on wrapset))
     
     ;; Run this wrapset's output funcs for itself.
     (map
      (lambda (func)
        (output-initializer-code func wrapset #f))
      (reverse (funcs-getter wrapset)))
     
     (display "}\n" port))))

(define (generate-c-source-file wrapset)
  (let* ((wrapset-name (gw:wrapset-get-name wrapset))
         (wrapset-header-name (string-append wrapset-name ".h"))
         (wrapset-source-name (string-append wrapset-name ".c"))
         (wrapset-name-c-sym (gw:any-str->c-sym-str wrapset-name))
         (gm (gw:wrapset-get-guile-module wrapset))
         (gm-reg (if gm (guile-module-name->c-registration-strlist gm) "")))

    (call-with-output-file wrapset-source-name
      (lambda (port)

        (define (dsp-list lst)
          (for-each (lambda (s) (display s port)) lst))

        (dsp-list
         (list
          "/* Generated by G-Wrap: an experimental C->Guile wrapper engine */\n"
          "\n"))
        
        (run-wrapset-output-funcs wrapset
                                  gw:wrapset-get-cs-before-includes-funcs
                                  port)

        (dsp-list
         (list
          "#include <guile/gh.h>\n"
          "#include <libguile.h>\n"
          "#include <string.h>\n"
          "#include <g-wrap-compatibility.h>\n"
          "\n"
          "#include \"" wrapset-header-name "\"\n"))

        ;; wrapset-before-includes

        ;; wrapset-declarations
        ;; wrapset-type-declarations (i.e. global-declarations-ccg)
        ;; wrapset-wrapper-declarations

        ;; wrapset-definitions
        ;; wrapset-type-definitions (i.e. global-definitions-ccg)
        ;; wrapset-wrapper-definitions

        ;; wrapset-initializers
        ;; wrapset-type-initializers  (i.e. global-initializations-ccg) ???
        ;; wrapset-wrapper-initializers

        (run-wrapset-output-funcs wrapset
                                  gw:wrapset-get-cs-declarations-funcs
                                  port)
        (add-wrapset-types-ccg-output wrapset port 'gw:global-declarations-ccg)
        (run-wrapset-output-funcs wrapset
                                  gw:wrapset-get-cs-wrapper-declarations-funcs
                                  port)
  


        (gw:generate-error-handler wrapset port)

        (run-wrapset-output-funcs wrapset
                                  gw:wrapset-get-cs-definitions-funcs
                                  port)
        (add-wrapset-types-ccg-output wrapset port 'gw:global-definitions-ccg)
        (run-wrapset-output-funcs wrapset
                                  gw:wrapset-get-cs-wrapper-definitions-funcs
                                  port)

        (dsp-list
         (list
          "void\n"
          "gw_init_wrapset_" wrapset-name-c-sym "() {\n"
          "  static int gw_wrapset_initialized = 0;\n"
          "\n"
          "  if(!gw_wrapset_initialized)\n"
          "  {\n"
          "    gh_eval_str(\"(use-modules (g-wrap runtime))\");\n"
          "    gh_eval_str(\"(gw:wrapset-register-runtime \\\"" wrapset-name "\\\")\");\n"
          "\n"))

        (for-each
         (lambda (depended-on-wrapset)
           (let ((depmod (gw:wrapset-get-guile-module depended-on-wrapset)))
             (flatten-display
              (if depmod
                  (gw:inline-scheme `(use-modules ,depmod))
                  `("gw_init_wrapset_" ,wrapset-name-c-sym "();\n"))
              port)))
         (gw:wrapset-get-wrapsets-depended-on wrapset))
        
        (run-wrapset-initializer-output-funcs
         wrapset
         gw:wrapset-get-cs-initializers-funcs
         port)

        (add-wrapset-types-initializer-ccg-output
         wrapset
         port
         'gw:global-initializations-ccg)

        (run-wrapset-initializer-output-funcs
         wrapset
         gw:wrapset-get-cs-wrapper-initializers-funcs
         port)

        (dsp-list
         (list
          "    gw_wrapset_initialized = 1;\n"
          "    (void) gw__handle_wrapper_error;\n"
          "  }\n"
          "}\n"))))))

(define-public (gw:generate-wrapset wrapset . options)
  (resolve-wrapset! wrapset "gw:generate-wrapset")
  
  ;; There are guile wrapper specific bits here (as elsewhere) that
  ;; will have to be factored out when we go back to supporting other
  ;; wrapper langauges.
  (let* (;; options
         (api-language (or (assq-ref options 'api-language) 'c))
         (wrapper-language (or (assq-ref options 'wrapper-language) 'guile))
         ;; other
         (guile-module (gw:wrapset-get-guile-module wrapset)))
    
    (if (not (eq? wrapper-language 'guile))
        (error "g-wrap: can't generate wrappers for requested language: "
               wrapper-language))
    
    (generate-c-header-file wrapset)
    (generate-c-source-file wrapset)
    (generate-html-doc-file wrapset)
    (if guile-module (generate-scm-file wrapset))))
    

(define-public (gw:wrapset-depends-on wrapset depended-on-wrapset)
  (resolve-wrapset! wrapset "gw:wrapset-depends-on")
  (resolve-wrapset! depended-on-wrapset "gw:wrapset-depends-on")
  
  (gw:wrapset-set-wrapsets-depended-on!
   wrapset 
   (cons 
    depended-on-wrapset
    (gw:wrapset-get-wrapsets-depended-on wrapset))))

(define (make-header-def-sym filename)
  (string-append "__"
                 (str-translate (string-upcase! (string-copy filename))
                                "- ." (vector "_" "_" "_"))
                 "__"))


;;(gw:error? status-var ...)
;;(gw:error? status-var alloc bad-arg)
;;(gw:error status-var alloc)

;; arg-type arg-range memory misc

;; (gw:wrap-value m 'gtk:green '<gw:int> "GTK_GREEN")

;; (gw:error status-var type ...)
(define (gw:expand-gw-error args param allowed-errors top-form)
  ;; args will be something like (status-var err-sym)

  (if (or (null? args) (null? (cdr args)))
      (error "not enough args to gw:error"))
  (if (not (memq (cadr args) allowed-errors))
      (scm-error 'misc-error "gw:expand-gw-error"
                 "gw:error type \"~A\" not allowed in ~S"
                 (list (cadr args) top-form) 
                 #f))

  (let ((status-var (car args)))
    (set! args (cdr args))
    (list
     "{\n"
     
     (case (car args)
       ((misc)
        ;; (list 'gw:error 'misc msg format-args)
        (if (not (= 3 (length args))) (error "bad call to (gw:error 'misc ...)"))
        (list
         "   " status-var " = GW__ERR_MISC;\n"
         "    gw__error_msg = " (list-ref args 1) ";\n"
         "    gw__error_data = " (list-ref args 2) ";\n"))
       ((memory)
        ;; (list 'gw:error 'memory) 
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'memory ...)"))
        (list
         "   " status-var " = GW__ERR_ARG_MEMORY;\n"))
       ((range)
        ;; (list 'gw:error 'range scm-item-out-of-range)
        (if (not (= 2 (length args)))
            (error "bad call to (gw:error 'range ...)"))
        (list
         "   " status-var " = GW__ERR_ARG_TYPE;\n"
         "    gw__error_data = " (cadr args) ";\n"))
       ((type)
        ;; (list 'gw:error 'type scm-bad-type-item)
        (if (not (= 2 (length args)))
            (error "bad call to (gw:error 'type ...)"))
        (list
         "   " status-var " = GW__ERR_ARG_TYPE;\n"
         "    gw__error_data = " (cadr args) ";\n"))
       ((argc)
        ;; (list 'gw:error 'argc)
        (if (not (= 1 (length args))) (error "bad call to (gw:error 'argc ...)"))
        (list
         "   " status-var " = GW__ERR_ARGC;\n"))
       ((arg-type)
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'arg-type ...)"))
        (list
         "   " status-var " = GW__ERR_ARG_TYPE;\n"
         "    gw__error_data = " (gw:param-get-scm-name param) ";\n"))
       ((arg-range)
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'arg-range ...)"))
        (list
         "   " status-var " = GW__ERR_ARG_RANGE;\n"
         "    gw__error_data = " (gw:param-get-scm-name param) ";\n"))
       (else
        (error "unexpected error type in gw:error")))
     
     (if param
         (list "   goto gw__post_call_arg_" (gw:param-get-number param) ";\n")
         "")
         
     "}\n")))

;; arg-type arg-range memory misc

(define (gw:expand-special-forms tree param allowed-errors)
  (define (gw:expand-helper tree param allowed-errors top-form)
    (cond
     ((null? tree) tree)
     ((list? tree)
      (case (car tree)
        ((gw:error?)
         (cond
          ((= 2 (length tree))
           (let ((status-var (list-ref tree 1)))
             (list "(" status-var " != GW__ERR_NONE)")))
          ((= 3 (length tree))
           (let ((status-var (list-ref tree 1))
                 (err-sym
                  (case (list-ref tree 2)
                    ((misc) "GW__ERR_MISC")
                    ((memory) "GW__ERR_MEMORY")
                    ((range) "GW__ERR_RANGE")
                    ((type) "GW__ERR_TYPE")
                    ((argc) "GW__ERR_ARGC")
                    ((arg-range) "GW__ARG_RANGE")
                    ((arg-type) "GW__ARG_TYPE")
                    (else (error "improper error type given to gw:error?: "
                                 (list-ref tree 2))))))
             (list "(" status-var " == " err-sym ")")))
          (else
           (error "improper use of gw:error?"))))
        ((gw:error)
         (gw:expand-gw-error (cdr tree) param allowed-errors top-form))
        (else
         (map
          (lambda (elt) (gw:expand-helper elt param allowed-errors top-form))
          tree))))
     (else tree)))
  (gw:expand-helper tree param allowed-errors tree))

(define (make-c-call-param-list params)  
  (cond ((null? params) '())
	(else
	 (cons
	  (list 
	   (gw:param-get-c-name (car params))
	   (if (null? (cdr params))
	       ""
	       ", "))
	  (make-c-call-param-list (cdr params))))))

(define (make-c-wrapper-param-declarations param-list)
  (let loop ((params param-list)
	     (index  0))
    (cond ((null? params) 
	   '())
	  ((and (= index gw:*max-fixed-params*))
	   "SCM gw__restargs ")
	  (else
	   (cons
	    (list
	     "SCM " (gw:param-get-scm-name (car params)) 
	     (if (null? (cdr params))
		 " "
		 ", "))
	    (loop (cdr params) (+ index 1)))))))

(define (gw:_generate-wrapped-func-definitions_ wrapset
                                                scheme-sym
                                                result
                                                c-name
                                                params
                                                description
                                                wrapper-name
                                                wrapper-namestr)
  
  (let ((param-decl (make-c-wrapper-param-declarations params))
        (fn-c-wrapper wrapper-name)
        (fn-c-string  wrapper-namestr)
        (nargs (length params))
        (status-var "gw__error_status"))
    
    (list
     "static char * " fn-c-string " = \"" scheme-sym "\";\n"
     "static SCM " fn-c-wrapper "  (" param-decl ") {\n"
     "  SCM gw__scm_result = SCM_UNSPECIFIED;\n"
     "  enum GW__ErrorStatus gw__error_status = GW__ERR_NONE;\n"
     "  SCM gw__error_data = SCM_UNSPECIFIED;\n"
     "  unsigned int gw__arg_pos = 0;\n"
     "  const char *gw__error_misc_msg = NULL;\n"

     (if (gw:type-declare-scm-result-var? (gw:result-get-type result))
         (list (gw:result-get-c-type-name result) " "
               (gw:result-get-c-name result) ";\n")
         '())
     
     (if (> nargs gw:*max-fixed-params*)
         (list "  SCM gw__scm_extras[" (- nargs gw:*max-fixed-params*) "];\n")
         '())
     
     "\n"
     
     (map
      (lambda (x)
        (list
         (gw:param-get-c-type-name x) " " (gw:param-get-c-name x) ";\n"))
      params)
     
     (map 
      (lambda (param)
        (let ((pre-call-ccg
               (hashq-ref (gw:param-get-type param) 'gw:pre-call-arg-ccg #f)))
          (list
           "/* ARG " (gw:param-get-number param) " */\n"
           "gw__arg_pos++;\n"
           (if (> (gw:param-get-number param) gw:*max-fixed-params*)
               (list
                "if (SCM_NULLP (gw__restargs)) " status-var " = GW__ERR_ARGC;\n"
                "else {\n"
                "  " (gw:param-get-scm-name param) " = SCM_CAR(gw__restargs);\n"
                "    gw__restargs = SCM_CDR (gw__restargs);\n"
                "}\n")
               '())
           "if (" status-var " != GW__ERR_NONE)"
           " goto " (if (zero? (gw:param-get-number param))
                        "gw__wrapper_exit;\n"
                        (list "gw__post_call_arg_"
                              (- (gw:param-get-number param) 1) ";\n"))
           "\n{\n"
           (if pre-call-ccg
               (gw:expand-special-forms
                (pre-call-ccg param status-var)
                param
                '(memory misc type range arg-type arg-range))
               "  /* no pre-call arg code requested! */\n"))))
      params)
     
     (let ((pre-call-result-ccg
            (hashq-ref (gw:result-get-type result) 'gw:pre-call-result-ccg #f)))
       (list
        "if (" status-var " == GW__ERR_NONE)\n"
        "{\n"
        (if pre-call-result-ccg
            (gw:expand-special-forms (pre-call-result-ccg result status-var)
                                     #f
                                     '(memory misc type range))
            "  /* no pre-call result code requested! */\n")))
     
     
     (let ((call-ccg (hashq-ref (gw:result-get-type result) 'gw:call-ccg #f))
           (func-call-code (list c-name " (" (make-c-call-param-list params) ")")))
       (if call-ccg
           (list
            "if (" status-var " != GW__ERR_NONE)"
            " goto " (if (zero? nargs)
                         "gw__wrapper_exit;\n"
                         (list "gw__post_call_arg_" (- nargs 1) ";\n"))
            "SCM_DEFER_INTS;\n"
            (gw:expand-special-forms (call-ccg result func-call-code status-var)
                                     #f
                                     '(memory misc type range))
            "SCM_ALLOW_INTS;\n")
           "/* no function call requested! */\n"))
     
     (let ((post-call-ccg (hashq-ref (gw:result-get-type result)
                                     'gw:post-call-result-ccg #f)))
       (list
        (if post-call-ccg
            (list
             "{\n"
             (gw:expand-special-forms (post-call-ccg result status-var)
                                      #f
                                      '(memory misc type range))
             "}\n")
            "  /* no post-call result code requested */\n")
        "}\n"))
     
     ;; insert the post-call args code in the opposite order
     ;; of the pre-call code
     (map 
      (lambda (param)
        (let ((post-call-ccg
               (hashq-ref (gw:param-get-type param) 'gw:post-call-arg-ccg #f)))
          (list
           "  gw__post_call_arg_" (gw:param-get-number param) ":\n"
           (if post-call-ccg
               (list
                "{\n"
                (gw:expand-special-forms (post-call-ccg param status-var)
                                         #f
                                         '(memory misc type range))
                "}\n")
               "  /* no post-call arg code requested! */\n")
           "  { /* shut up warnings if no code */ int x = x; }\n"
           "}\n")))
      (reverse params))
     
     " gw__wrapper_exit:\n"
     "  if(gw__error_status != GW__ERR_NONE)\n"
     "    gw__handle_wrapper_error(gw__error_status,\n"
     "                             " fn-c-string ",\n"
     "                             gw__arg_pos,\n"
     "                             gw__error_misc_msg,\n"
     "                             gw__error_data);\n"
     "  return gw__scm_result;\n"
     "}\n")))


(define (gw:_generate-wrapped-func-initializers_ scm-sym
                                                 c-name
                                                 nargs
                                                 description
                                                 wrapper-name
                                                 wrapper-namestr)
  (let ((use-extra-params? (> nargs gw:*max-fixed-params*))
        (fn-c-wrapper wrapper-name)
        (fn-c-string  wrapper-namestr)
        (fn-doc (flatten-string description)))
        ;;(fn-doc (str-translate (flatten-string description)
        ;;                       "\n\""
        ;;                       (vector "\\n\\\n" "\\\""))))

    (list
     "    gh_new_procedure(" fn-c-string ",\n"
     "                     (SCM (*) ()) " fn-c-wrapper ",\n"
     "                     " (if use-extra-params?
                                 gw:*max-fixed-params*
                                 nargs) ",\n"
     "                     0,\n" 
     "                     " (if use-extra-params? "1" "0") ");\n"
     "\n"
     ;;(gw:inline-scheme `(gw:add-description ,scm-sym ,fn-doc))
     )))

;;     "    gw_add_description(scm_cons(SCM_CAR(scm_intern0(" fn-c-string ")), "
;;     "                                gh_str02scm(\"" fn-doc "\")));\n")))


(define-public (gw:wrap-function 
                wrapset
                scheme-sym
                result-spec
                c-name
                param-specs
                .
                new-description)

  (resolve-wrapset! wrapset "gw:wrap-function")

  (let* ((params (param-specs->params param-specs wrapset))
         (wrapper-name (gw:gen-c-tmp (string-append c-name "_wrapper")))
         (wrapper-namestr  (gw:gen-c-tmp (string-append c-name "_namestr")))
         (result (result-spec->result result-spec wrapset))
         (description
          (list
           (param-specs->description-head 
            scheme-sym (gw:result-get-type result) param-specs)
           new-description))
         (nargs (length params)))
         
    
    (gw:wrapset-add-guile-module-export! wrapset scheme-sym)

    (gw:wrapset-add-cs-wrapper-definitions!
     wrapset
     (lambda (wrapset client-wrapset)
       (if client-wrapset
           '()
           (gw:_generate-wrapped-func-definitions_ wrapset
                                                   scheme-sym
                                                   result
                                                   c-name
                                                   params
                                                   description
                                                   wrapper-name
                                                   wrapper-namestr))))
    (gw:wrapset-add-cs-wrapper-initializers!
     wrapset
     (lambda (wrapset client-wrapset status-var)
       (if client-wrapset
           '()
           (gw:_generate-wrapped-func-initializers_ scheme-sym
                                                    c-name
                                                    nargs
                                                    description
                                                    wrapper-name
                                                    wrapper-namestr))))))
        

(define (param-specs->description-head scheme-sym ret-type param-list)
  (list
   (list 
    "(" scheme-sym (map (lambda (x) (list " " (cadr x))) param-list) ")\n")
   (if (null? param-list)
       ""
       (list (separate-by
	      (map (lambda (x) (list (cadr x) " is a " (car x))) param-list)
	      ", ")
	     ".\n"))
   (if (eq? 'void (gw:type-get-name ret-type))
       " No return value.\n"
       (list " Returns " (gw:type-get-name ret-type) ".\n"))))


;;; need gw:wrap-value and gw:wrap-variable?
;;; should gw:wrap-variable produce getter/setter object?  i.e.
;;;   ((car my-c-var)) -> value
;;;   ((cdr my-c-var) value) -> unspecified -- stores value.
(define-public (gw:wrap-value
                wrapset
                scheme-sym
                typespec-form
                c-value ;; (c-var typespec status-var)
                .
                description)
  
  (resolve-wrapset! wrapset "gw:wrap-value")
  
  (gw:wrapset-add-guile-module-export! wrapset scheme-sym)

  (let* ((scm-var (gw:gen-c-tmp "scm_wrapped_value"))
         (typespec (gw:prototype-form->typespec typespec-form wrapset))
         (c->scm (gw:type-get-c->scm-ccg (gw:typespec-get-type typespec))))
  
  (gw:wrapset-add-cs-wrapper-initializers!
   wrapset
   (lambda (wrapset client-wrapset status-var)
     (if client-wrapset
         '()
         (let ((convert-value-code (c->scm scm-var c-value typespec status-var)))
           
           (list
            "{\n"
            "   SCM " scm-var ";\n"
            "\n"
            convert-value-code
            "if(!" `(gw:error? ,status-var) ")"
            "  gh_define(\"" (symbol->string scheme-sym) "\"," scm-var ");\n"
            "}\n")))))))

(use-modules (g-wrap enumeration))
(export gw:wrap-enumeration)
(export gw:enum-add-value!)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Junk.

;;         (orig-doc new-description) ;; new-description is last arg
;;                                    ;; to wrap-function call...
;; If we decide we want to generate headers again...
;     (let ((gen-header-file (gw:wrapset-get-generated-header-file wrapset)))
;       (if gen-header-file
;           (let ((subs
;                  `((doc    ,(gen-c-comment orig-doc))
;                    (ret    ,(gw:result-get-proper-c-type-name result))
;                    (fnname ,c-name)
;                    (args   ,(separate-by 
;                              (map 
;                               (lambda (param)
;                                 (list (gw:param-get-proper-c-type-name param)
;                                       " " (gw:param-get-name param)))
;                               params)
;                              ", ")))))
;             (gw:trans-write gen-header-file 'declarations subs
;                             "%doc%%ret% %fnname% (%args%);\n\n"))))

; (define-public (new-constant sym-name type varb . options)
;   (set! sym-name (prefix-name sym-name))
;   (let ((description (fn-option options 'doc (lambda () '()))))
;     (gwrap-c-doc-constant sym-name type description))
;   (if constant-index-generator
;       ((constant-index-generator 'add) sym-name))
;   (gwrap-c-output-c
;    'type-inits
;    "  scm_sysintern (\"" sym-name "\", "
;    (make-conversion-to-scm (get-type type) varb)
;    ");\n"))


