;;;; File: simple-type.scm
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
;;; Commentary:

;;
; This module is provided for compatibility with G-Wrap 1.3.4.
;;
;;; Code:

(define-module (g-wrap simple-type)
  #:use-module (oop goops)
  #:use-module (g-wrap util)

  #:export (gw:wrap-simple-type))

(define (gw:wrap-simple-type ws scm-name c-name typecheck unwrap wrap)
  (slot-push! ws 'simple-types (vector scm-name c-name typecheck unwrap wrap)))

