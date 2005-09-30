;;;; File: gw-glib-spec.scm
;;;; Copyright (C) 2004-2005 Andreas Rottmann
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

;;; Commentary:
;;
; This module is provided for compatibility with G-Wrap 1.3.4.  It is
; a stripped-down version of (gnome gw glib-spec) from guile-gnome,
; only providing support for the GList and GSList types.
;;
;;; Code:

(define-module (g-wrap gw-glib-spec)
  #:use-module (g-wrap compat)
  #:use-module (oop goops)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap util)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard))

(define-class <glib-wrapset> (<gw-guile-wrapset>)
  #:id 'gw-glib #:dependencies '(standard))

(define-method (client-global-declarations-cg (ws <glib-wrapset>))
  '("#include <glib.h>\n"))

(define-method (global-declarations-cg (ws <glib-wrapset>))
  (list
   (next-method)
   "#include <glib.h>\n"))

(define-method (initialize (ws <glib-wrapset>) initargs)
  (next-method ws (append '(#:module (g-wrap gw-glib)) initargs))

  (add-type! ws (make <glist-of-type> #:name 'glist-of
		      #:type-cname "GList*" #:func-prefix "g_list"))

  (add-type! ws (make <glist-of-type> #:name 'gslist-of
		      #:type-cname "GSList*" #:func-prefix "g_slist")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ((glist-of (<gtk-window> gw:const) gw:const) win-list)
;; shamelessly stolen from upstream g-wrap so that we can use glib 2.0

(define-class <glist-of-type> (<gw-type>)
  (type-cname #:getter type-cname #:init-keyword #:type-cname)
  (func-prefix #:getter func-prefix #:init-keyword #:func-prefix))

(define-class <gw-collection-typespec> (<gw-typespec>)
  (sub-typespec #:getter sub-typespec #:init-keyword #:sub-typespec))

(define-method (all-types (ts <gw-collection-typespec>))
  (cons (type (sub-typespec ts)) (next-method)))

(define-method (c-type-name (type <glist-of-type>))
  (type-cname type))

(define-method (c-type-name (type <glist-of-type>)
			    (typespec <gw-collection-typespec>))
  (if (memq 'const (options typespec))
      (list "const " (type-cname type))
      (type-cname type)))

;; if this succeeds, the glist-of typespec-options will be
;; (sub-typespec (caller-owned | callee-owned) [const])
(define-method (make-typespec (type <glist-of-type>) (options <list>))
  (if (null? options)
      (raise (condition
	      (&gw-bad-typespec
	       (type type) (options options)
	       (message "Missing glist-of options form.")))))
  (if (< (length options) 2)
      (raise
       (condition
	(&gw-bad-typespec
	 (type type) (options options)
	 (message "glist-of options form must have at least 2 options.")))))
  (let ((sub-typespec (car options))
	(glist-options (cdr options))
	(remainder (cdr options)))

    (if (not (is-a? sub-typespec <gw-typespec>))
	(raise (condition
		(&gw-bad-typespec
		 (type type) (options options)
		 (message "glist-of options form must have a sub-typespec as first option.")))))


    (set! remainder (delq 'const remainder))
    (if (and (memq 'caller-owned remainder)
	     (memq 'callee-owned remainder))
	(raise (condition
		(&gw-bad-typespec
		 (type type) (options options)
		 (message
		  "Bad glist-of options form (caller and callee owned!).")))))

    (if (not (or (memq 'caller-owned remainder)
		 (memq 'callee-owned remainder)))
	(raise
	 (condition
	  (&gw-bad-typespec
	   (type type) (options options)
	   (message
	    "Bad glist-of options form (must be caller or callee owned!).")))))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (if (null? remainder)
	(make <gw-collection-typespec>
	  #:type type
	  #:sub-typespec sub-typespec
	  #:options glist-options)
	(raise (condition
		(&gw-bad-typespec
		 (type type) (options options)
		 (message
		  (format #f "Bad glist-of options form - spurious options: ~S"
			  remainder))))))))

(define-method (unwrap-value-cg (glist-type <glist-of-type>)
				(value <gw-value>)
				status-var)

  (let* ((c-var (var value))
	 (scm-var (scm-var value))
	 (sub-typespec (sub-typespec (typespec value)))
	 (sub-type (type sub-typespec))
	 (tmp-rest-var (gen-c-tmp "scm_rest"))
	 (func-prefix (func-prefix glist-type))
	 (sub-item-c-type (c-type-name sub-type sub-typespec))
	 (tmp-sub-item-c-var (gen-c-tmp "c_item"))
	 (tmp-sub-item-scm-var (gen-c-tmp "scm_item"))
	 (tmp-sub-item (make <gw-value>
			 #:typespec sub-typespec
			 #:var tmp-sub-item-c-var
			 #:wrapped-var (string-append
					"&" tmp-sub-item-scm-var)))
	 (tmp-cursor (gen-c-tmp "cursor")))

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
       (unwrap-value-cg sub-type tmp-sub-item status-var)
       "\n"
       "    if(! " `(gw:error? ,status-var) " )\n"
       "    {\n"
       "       " c-var " = " func-prefix "_prepend (" c-var ", (gpointer)" tmp-sub-item-c-var");\n"
       "    }\n"
       "    " tmp-rest-var " = SCM_CDR (" tmp-rest-var ");\n"
       "  }\n"
       "  if(!" `(gw:error? ,status-var) ")\n"
       "  {\n"
       "    " c-var " = " func-prefix "_reverse(" c-var ");\n"
       "  }\n"
       "  else\n"
       "  {\n"
       "    " (c-type-name glist-type (typespec value)) tmp-cursor " = " c-var ";\n"
       "    while(" tmp-cursor ")\n"
       "    {\n"
       "      " sub-item-c-type " " tmp-sub-item-c-var ";\n"
       "      " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
       (string-append tmp-cursor "->data") ";\n"
       ;; FIMXE: had force #t here
       (destroy-value-cg sub-type tmp-sub-item status-var)
       tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
       "    }\n"
       "    " func-prefix "_free(" c-var ");\n"
       "    " c-var " = NULL;\n"
       "  }\n"
       "}\n")))

(define-method (wrap-value-cg (glist-type <glist-of-type>)
			      (value <gw-value>)
			      status-var)
  (let* ((c-var (var value))
	 (scm-var (scm-var value))
	 (sub-typespec (sub-typespec (typespec value)))
	 (sub-type (type sub-typespec))
	 (tmp-rest-var (gen-c-tmp "c_rest"))
	 (sub-item-c-type (c-type-name sub-type sub-typespec))
	 (tmp-sub-item-c-var (gen-c-tmp "c_item"))
	 (tmp-sub-item-scm-var (gen-c-tmp "scm_item"))
	 (tmp-sub-item (make <gw-value>
			 #:typespec sub-typespec
			 #:var tmp-sub-item-c-var
			 #:wrapped-var (string-append
					"&" tmp-sub-item-scm-var))))

    (list
     (c-type-name glist-type (typespec value)) tmp-rest-var " = " c-var ";\n"
     scm-var "= SCM_EOL;\n"
     "while(" tmp-rest-var " && (! " `(gw:error? ,status-var) "))\n"
     "{\n"
     "  " sub-item-c-type " " tmp-sub-item-c-var ";\n"
     "  SCM " tmp-sub-item-scm-var ";\n"
     "\n"
     "  " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
     (string-append tmp-rest-var "->data") ";\n"
     "\n"
     (wrap-value-cg sub-type tmp-sub-item status-var)
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

(define-method (destroy-value-cg (glist-type <glist-of-type>)
				  (value <gw-value>)
				  status-var)
  (let* ((c-var (var value))
	 (scm-var (scm-var value))
	 (options (options (typespec value)))
	 (sub-typespec (sub-typespec (typespec value)))
	 (sub-type (type sub-typespec))
	 (func-prefix (func-prefix glist-type))
	 (sub-item-c-type (c-type-name sub-type sub-typespec))
	 (tmp-sub-item-c-var (gen-c-tmp "c_item"))
	 (tmp-sub-item (make <gw-value>
		     #:typespec sub-typespec
		     #:var tmp-sub-item-c-var))
	 (tmp-cursor (gen-c-tmp "cursor")))
    (list
     "{\n"
     "  " (c-type-name glist-type (typespec value)) tmp-cursor " = " c-var ";\n"
     "  while(" tmp-cursor ")\n"
     "  {\n"
     "    " sub-item-c-type " " tmp-sub-item-c-var ";\n"
     "    " tmp-sub-item-c-var " = ( " sub-item-c-type ") "
     (string-append tmp-cursor "->data") ";\n"
     (destroy-value-cg sub-type tmp-sub-item status-var)
     tmp-cursor " = " (string-append tmp-cursor "->next") ";\n"
     "  }\n"
     (if (memq 'caller-owned options)
	 (list "  if(" c-var ")\n"
	       "  {\n"
	       "    " func-prefix "_free(" c-var ");\n"
	       "    " c-var " = NULL;\n"
	       "  }\n")
	 '())
     "}\n")))


(gw:register-wrapset "gw-glib" 'gw-glib)
