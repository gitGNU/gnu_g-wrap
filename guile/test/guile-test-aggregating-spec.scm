;;;; File: guile-test-aggregating-spec.scm
;;;; Copyright (C) 2005 Ludovic Court�s
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

(define-module (guile test guile-test-aggregating-spec)
  #:use-module (oop goops)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (test test-aggregating-spec))

(define-class <guile-test-aggregating-wrapset> (<test-aggregating-wrapset>
					     <gw-guile-wrapset>)
  #:id 'test-aggregating)

(define-method (initialize (ws <guile-test-aggregating-wrapset>) initargs)
  (next-method)

  (set! (module ws) '(test gw-test-aggregating)))
