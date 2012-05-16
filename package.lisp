;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-POP, the Lisp POP Client

;;; Copyright (C) July 2006  Liberating Insight LLC 
;;; Author Brian Sorg 

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-pop.asd
;;; Description: cl-pop ASDF system definition file

(defpackage :cl-pop
   	(:use :cl :cl-ppcre)
	(:export :open-pop-connection
         :close-pop-connection
         :with-pop-connection
         :message-count
         :save-message
         :message-headers
         :retrieve-pop-message
         :delete-pop-message
         ;;;---  Raw pop commands -- Recommend using the higher level commands listed above  ----------
         :send-pop-stat
         :send-pop-list
         :send-pop-dele 
         :send-pop-rset
         :send-pop-uidl 
         :send-pop-noop
         :send-pop-top
         :send-pop-retr
         :send-pop-quit))
