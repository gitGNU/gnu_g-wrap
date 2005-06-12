;;;; File: guile-test-standard-spec.scm
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

(define-module (scheme48 test s48-test-standard-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap scheme48)
  #:use-module (g-wrap scheme48 ws standard)
  #:use-module (test test-standard-spec))

(define-class <s48-test-standard-wrapset> (<test-standard-wrapset>
                                             <gw-s48-wrapset>)
  #:id 'test-standard
  #:structure 'test-standard
  #:shlib "libg-wrap-test-c-code")

