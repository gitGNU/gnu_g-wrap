;;;; File: guile-test-child-spec.scm
;;;; Copyright (C) 2004 Andreas Rottmann
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

(define-module (guile test guile-test-child-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (guile test guile-test-parent-spec)
  #:use-module (test test-child-spec))

(define-class <guile-test-child-wrapset> (<test-child-wrapset>
                                          <gw-guile-wrapset>)
  #:id 'test-child)

(define-method (initialize (ws <guile-test-child-wrapset>) initargs)
  (next-method)

  (set! (module ws) '(test gw-test-child)))


