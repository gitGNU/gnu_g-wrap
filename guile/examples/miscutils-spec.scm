(define-module (examples miscutils-spec)
  #:use-module (oop goops)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard))

(define-class <timespec64-type> (<gw-type>))

(define-class <miscutils-wrapset> (<gw-guile-wrapset>)
  #:id 'miscutils
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <miscutils-wrapset>))
  (list
   (next-method)
   "#include \"miscutils-guile.h\"\n"))

(define-method (initialize (ws <miscutils-wrapset>) initargs)
  (next-method ws (append '(#:module (gw-miscutils)) initargs))

  (add-type! ws (make <timespec64-type> #:name 'timespec64))
  
  (wrap-function!
   ws
   #:name 'join-strings
   #:returns '(mchars caller-owned)
   #:c-name "join_strings"
   #:arguments '(((mchars caller-owned) a) ((mchars caller-owned) b))
   #:description "Return a string consisting of a followed by b.")
  
  (wrap-function!
   ws
   #:name 'seconds-since-dow
   #:returns 'double
   #:c-name "seconds_since_dow"
   #:arguments '((unsigned-int day-of-week))
   #:description "Given day-of-week (ranging 1-7), return elapsed time since then.")

  (wrap-function!
   ws
   #:name 'elapsed-time
   #:returns 'timespec64
   #:c-name "elapsed_time"
   #:arguments '((timespec64 start) (timespec64 finish))
   #:description "Return the elapsed time between @var{start} and @var{finish}."))


(define-method (c-type-name (type <timespec64-type>) (typespec <gw-typespec>))
  "Timespec64")

(define-method (unwrap-value-cg (type <timespec64-type>)
                                (value <gw-value>) error-var)
  (list
   "if (SCM_FALSEP (msu_scm_timespec64_p(" (scm-var value) ")))"
   `(gw:error ,error-var type ,(wrapped-var value))
   "else\n"  
   "  " (var value) " = msu_timespec64_to_c(" (scm-var value) ");\n"))

(define-method (wrap-value-cg (type <timespec64-type>)
                              (value <gw-value>) error-var)
  (list
   (scm-var value) " = msu_timespec64_to_scm(" (var value) ");\n"))
