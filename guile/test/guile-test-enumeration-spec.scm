;;;; File: guile-test-enumeration-spec.scm
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

(define-module (guile test guile-test-enumeration-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (test test-enumeration-spec))

(define-class <guile-test-enumeration-wrapset> (<test-enumeration-wrapset>
                                                <gw-guile-wrapset>)
  #:id 'test-enumeration)

(define-method (initialize (ws <guile-test-enumeration-wrapset>) initargs)
  (next-method ws (append '(#:module (gw-test-enumeration)) initargs)))
