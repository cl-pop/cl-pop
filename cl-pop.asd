;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-POP, the Lisp POP Client

;;; Copyright (C) July 2006 Brian Sorg Liberating Insight LLC 

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-pop.asd
;;; Description: cl-pop ASDF system definition file

#+sbcl (require :sb-bsd-sockets)


(in-package :common-lisp)

(asdf:defsystem :cl-pop
        :version "0.2.0"
	:depends-on
	        (:usocket :cl-ppcre)
	:components 
		((:file "package")
                 (:file "cl-pop" :depends-on ("package"))))
