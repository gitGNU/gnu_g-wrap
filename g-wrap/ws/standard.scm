(define-module (g-wrap ws standard)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)

  #:export (<gw-standard-wrapset>))

(define-class <gw-standard-wrapset> (<gw-wrapset>)
  (use-limits? #:init-value #f))

(define-method (add-type! (wrapset <gw-standard-wrapset>)
                          (type <gw-ranged-integer-type>))
  (next-method)
  (slot-set! wrapset 'use-limits? #t))

(define-method (initialize (wrapset <gw-standard-wrapset>) initargs)

  (define (before-includes lang)
    (if (slot-ref wrapset 'use-limits?)
        (list "#define _GNU_SOURCE\n")
        '()))

  (define (global-declarator lang)
    (if (slot-ref wrapset 'use-limits?)
        (list "#include <limits.h>\n")
        '()))
  
  (next-method)

  (add-cs-before-includes! wrapset before-includes)
  (add-client-cs-before-includes! wrapset before-includes)
  (add-cs-global-declarator! wrapset global-declarator)
  (add-client-cs-global-declarator! wrapset global-declarator)
  
  (add-type! wrapset
             (make <gw-ctype-void>
               #:name 'void
               #:c-type-name "void"
               #:ffspec 'void))
  
  (wrap-simple-type! wrapset
                     #:name 'bool
                     #:c-type-name "int"
                     #:ffspec 'sint)
  
  ;; FIXME: Scheme chars are 0-255, not [-128,127] like c chars *may* be
  (wrap-simple-type! wrapset
                     #:name 'char
                     #:c-type-name "char"
                     #:ffspec 'schar) ;; FIXME: see above

  (wrap-simple-type! wrapset
                     #:name 'unsigned-char
                     #:c-type-name "unsigned char"
                     #:ffspec 'uchar)

  (wrap-simple-type! wrapset
                     #:name 'float
                     #:c-type-name "float"
                     #:ffspec 'float)
  
  (wrap-simple-type! wrapset
                     #:name 'double
                     #:c-type-name "double"
                     #:ffspec 'double)

  (wrap-ranged-integer-type! wrapset
                             #:name 'short
                             #:c-type-name "short"
                             #:min "SHRT_MIN" #:max "SHRT_MAX"
                             #:ffspec 'sshort)

  (wrap-ranged-integer-type! wrapset 
                             #:name 'unsigned-short
                             #:c-type-name "unsigned short"
                             #:max "USHRT_MAX"
                             #:ffspec 'ushort)

  (wrap-ranged-integer-type! wrapset
                             #:name 'int
                             #:c-type-name "int"
                             #:min "INT_MIN" #:max "INT_MAX"
                             #:ffspec 'sint)
  
  (wrap-ranged-integer-type! wrapset
                             #:name 'unsigned-int
                             #:c-type-name "unsigned int"
                             #:max "UINT_MAX"
                             #:ffspec 'uint)

  (wrap-ranged-integer-type! wrapset
                             #:name  'long
                             #:c-type-name "long"
                             #:min "LONG_MIN" #:max "LONG_MAX"
                             #:ffspec 'slong)

  (wrap-ranged-integer-type! wrapset
                             #:name 'unsigned-long
                             #:c-type-name "unsigned long"
                             #:max "ULONG_MAX"
                             #:ffspec 'ulong)

  (wrap-ranged-integer-type! wrapset
                             #:name 'long-long
                             #:c-type-name "long long"
                             #:min "LLONG_MIN" #:max "LLONG_MAX"
                             #:ffspec 'slong_long)

  (wrap-ranged-integer-type! wrapset
                             #:name  'unsigned-long-long
                             #:c-type-name "unsigned long long"
                             #:max "ULLONG_MAX"
                             #:ffspec 'ulong_long)
  
  (add-type! wrapset
             (make <gw-ctype-mchars>
               #:name 'mchars
               #:c-type-name "char *"
               ;; We don't use const, since free() will be called
               #:c-const-type-name "char *" 
               #:ffspec 'pointer)))
