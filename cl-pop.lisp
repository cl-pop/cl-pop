(in-package :cl-pop)
;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-POP, the Lisp POP Client

;;; Copyright (C) July 2006-07 Brian Sorg Liberating Insight LLC 

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-pop.lisp
;;; Description: cl-pop pop client 

(defparameter *pop-debug? nil)

(defmacro print-debug (str)
  "Debuging tool"
  `(when *pop-debug?
     (format *standard-output* ,str)))
  

(defclass pop-connection ()
  ((host :initarg :host
         :reader host)

   (port :initarg :port
         :initform 110
         :reader port)

   (username :initarg :username
             :initform ""
              :reader username)

   (password :initarg :password
             :initform ""
             :reader password)

   (socket :initform nil
           :initarg :socket
           :accessor socket)

   (socket-stream :initform nil
                  :initarg :socket-stream
                  :accessor socket-stream)

   (state :documentation "Current state of the connection options are :disconnected, :connected"
          :initform :disconneted
          :accessor state)))

;;--- Normally this would be .CRLF but since I use read-line to process the information
;;--- it already strips of the Line Feed
(defparameter *termination-string* (format nil ".~C" #\Return))

(defmacro with-pop-connection ((var &rest args) &body body)
  `(let ((,var (open-pop-connection ,@args)))
     (unwind-protect 
         (progn ,@body)
       (close-pop-connection ,var))))


(defun open-pop-connection (&key host (port 110) username password)

  (unless (and host (stringp host) port (numberp port))
    (error "Please provide a valid host name and port for the connection"))
  (let* ((socket (usocket:socket-connect host port))
         (conn (make-instance 'pop-connection
                             :host host
                             :port port
                             :username username
                             :password password
                             :socket socket
                             :socket-stream (usocket:socket-stream socket))))
    (multiple-value-bind (ans message)
        (read-single-pop-response conn)
      (if ans
          (authorize-pop-connection conn)
        (error "Sorry the connection failed with the following message: ~a" message)))))

(defun authorize-pop-connection (conn)

  (send-pop-command conn (format nil "USER ~a" (username conn)))
  (multiple-value-bind (ans message)
      (read-single-pop-response conn)
    (if (null ans)
        (progn 
          (close-pop-connection conn)
          (error "Pop Authorization Failled with the following message: ~a~% Closing connection" message))
      (progn
        (send-pop-command conn (format nil "PASS ~a" (password conn)))
        (multiple-value-bind (ans message)
            (read-single-pop-response conn)
          (if (null ans)
              (progn 
                (close-pop-connection conn)
                (error "Pop Authorization Failled with the following message: ~a~% Closing connection" message))
            (progn
              (setf (state conn) :connected)
              conn)))))))

(defun close-pop-connection (conn)

  (send-pop-quit conn)
  ;(usocket:socket-close conn)
  (usocket:socket-close (socket conn))
  (setf (state conn) :disconnected))

(defun message-count (conn)
  "Number of message currently in the mailbox"
  (first (send-pop-stat conn)))

(defun save-message (conn message-num pathname)
  "Saves message's entire contents including headers into a file with the given pathname"
    (send-pop-command conn (format nil "RETR ~a" message-num))
    (let ((sock (socket-stream conn)))
      (multiple-value-bind (ans message)
          (read-single-pop-response conn)
        (if ans
            (with-open-file (strm pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
              (loop
               (let ((str (read-line sock)))
                 (if (string= str *termination-string*)
                     (return)
                   (write-line (clean-pop-message str 0) strm)))))
          (format t "Failed to save message num ~a because ~a" message-num message)))))

(defun message-headers (conn message-num)
  "Returns an assoc list of the email headers, if duplicate or multiline headers are present, they will be appended togther to form one associated pair"
  (let ((headers (send-pop-top conn message-num 0)))
    (parse-raw-message-headers headers)))

(defun parse-raw-message-headers (headers)
  (let ((cur-header "")
        (cur-header-string "")
        (header-lst nil))
    (dolist (header headers)
      (let ((lst (cl-ppcre:split ": " header :limit 2)))
        (cond ((or (= (length lst) 1)
                   (and (stringp (first lst))
                        (string= cur-header (first lst))))
               (setq cur-header-string (concatenate 'string cur-header-string (second lst))))
              (t 
               (progn
                 (push (cons cur-header cur-header-string) header-lst)
                 (setq cur-header (first lst) cur-header-string (second lst)))))))
    (rest (nreverse header-lst))))    

(defun retrieve-pop-message (conn message-num &key (max-size nil))
  "Returns a plist of the message with :header containing the parsed header fields and body containing a list where each element in the list is a string representing 1 line of the message. If a max-size is providing in bytes the message size is checked first if it is greater then the max nil is returned"
  (when (or (null max-size)
            (<= (second (first (send-pop-list conn message-num)))
                max-size))
    (send-pop-command conn (format nil "RETR ~a" message-num))
    (let ((header? t)
          (header-lst nil)
          (body-lst nil)
          (sock (socket-stream conn)))
      (multiple-value-bind (ans message)
          (read-single-pop-response conn)
        (if ans
            (loop
             (let ((str (read-line sock)))
               (if (string= str *termination-string*)
                   (return)
                 (if (and header? 
                          (not (string= str (string #\Return)))) ;; empty line marks the end of the header section
                     (push (clean-pop-message str 0) header-lst)
                   (progn
                     (setq header? nil)
                     (push (clean-pop-message str 0 :mask-dot? t) body-lst))))))
          (format t "Failed to retrieve message with error: ~a" message)))
      (list :header (parse-raw-message-headers (nreverse header-lst))
            :body (nreverse body-lst)))))

(defun delete-pop-message (conn message-num)
  "Mark the given message number for deletion"
  (multiple-value-bind (ans message)
      (send-pop-dele conn message-num)
    (if ans
        t
      (progn
        (format t "Error occured while marking message for deletion: Message # ~a Error: ~a~%"
                message-num message)
        nil))))

    


;;-------------------------------------------------------------
;;----          Basic Pop Commands                         ----
;;-------------------------------------------------------------

(defun send-pop-stat (conn)
  "conn - pop-connection instance 
   Executes the pop stat command, returns a list if successful, with the number of messages in the mailbox and their combined size"
  (send-pop-command conn "STAT")
  (multiple-value-bind (ans message)
      (read-single-pop-response conn)
    (if ans
        (break-pop-response message :integers t)
      (format t "Stat command failed: ~a" message))))

(defun send-pop-list (conn &optional (message-num nil))
  "Conn -- pop-connection instance,
   Message-num -- integer, number of desired message optional
   Returns list of lists where the first atom is the message number and the second is the message size. If a message number is provided only the list for that particular message will be return."
  (send-pop-command conn (format nil "LIST~a"
                                 (if message-num (format nil " ~a" message-num) "")))
  (if message-num
      (multiple-value-bind (ans message)
          (read-single-pop-response conn)
        (if ans
            (list (break-pop-response message :integers t))
          (format t "List command failed: ~a" message)))
    (mapcar #'(lambda (str)
                (break-pop-response str :integers t))
            (read-multi-line-pop-response conn))))

(defun send-pop-dele (conn message-num)
  "conn - pop connection class
   message-num - pop message number to mark for deletion
   Mark the given message number for deletion. Deletion occurs when the Quit Command is given"
  (send-pop-command conn (format nil "DELE ~a" message-num))
  (read-single-pop-response conn))

(defun send-pop-rset (conn)
  "conn - pop connection class
   Reset all messages that have been marked for deletion"
  (send-pop-command conn "RSET")
  (read-single-pop-response conn))

(defun send-pop-uidl (conn &optional (message-num nil))
  "conn - pop-connection instance
   message-num (optional) if given only return list for this message
   Returns a list of lists, where the first element is the message num and the second is the unique id number for that message. If a message num is given only that individual messages information is returned"
  (send-pop-command conn (format nil "UIDL~a"
                                 (if message-num (format nil " ~a" message-num) "")))
  (if message-num
      (multiple-value-bind (ans message)
          (read-single-pop-response conn)
        (if ans
            (list (break-pop-response message))
          (format t "UIDL command failed: ~a" message)))
    (mapcar #'(lambda (str)
                (break-pop-response str))
            (read-multi-line-pop-response conn))))

(defun send-pop-noop (conn)
  "conn - pop-conneciton instance 
   Sends a no operation command"
  (send-pop-command conn "NOOP")
  (read-single-pop-response conn))


(defun send-pop-top (conn message-num line-count)
  "Retrieves the messages header for the given message num and the first # of lines of the body of the message as determined by the line-count input"
  (send-pop-command conn (format nil "TOP ~a ~a" message-num line-count))
  (read-multi-line-pop-response conn))

(defun send-pop-retr (conn message-num)
  "Retrieves the entire message and returns a list where each atom in the list is one line in the file. It is recommended that one uses the retrieve-pop-message function instead which offers a check for large mail messages."
  (send-pop-command conn (format nil "RETR ~a" message-num))
  (read-multi-line-pop-response conn :mask-dot? t))

(defun send-pop-quit (conn)
  "Ends the connection on the server"
  (send-pop-command conn "QUIT")
  (read-single-pop-response conn))
    
;;----------------------------------------------------------------
;;       Supporting Functions                                  ---
;;----------------------------------------------------------------

(defun send-pop-command (conn command)
  
  (print-debug (format nil "Send: ~a" command))
  (let ((sock (socket-stream conn)))
    (write-string command sock)
    (write-char #\Return sock)
    (write-char #\NewLine sock)
    (force-output sock)))

(defmethod read-single-pop-response (conn)
  "Reads a 1 line pop response, and returns two values, the first is whether the response came back as positive, the second is the information / message of the response."
  (let* ((resp (read-line (socket-stream conn)))
         (status-ok? (char= #\+ (elt resp 0)))
         (message (clean-pop-message resp (if status-ok? 4 5))))
    (print-debug (format nil "Response: Status ~a Message ~a"
                         status-ok? message))
    (values status-ok? message)))

(defmethod read-multi-line-pop-response (conn &key (mask-dot? nil))

  (let ((lst nil)
        (sock (socket-stream conn)))
    (when (read-single-pop-response conn)
      (loop
       (let ((str (read-line sock)))
         (if (string= str *termination-string*)
             (return)
           (push (clean-pop-message str 0 :mask-dot? mask-dot?) lst)))) 
      (nreverse lst))))

(defun clean-pop-message (str start &key (mask-dot? nil))
  "Removes status key and end of line characters from string"
  (string-right-trim (format nil "~{~a~}" (list #\Return #\Newline))
                                (subseq str start))
  (let ((lin (string-right-trim (format nil "~{~a~}" (list #\Return #\Newline))
                                (subseq str start))))
    (if (and mask-dot?
             (> (length lin) 1)
             (string= ".." (subseq lin 0 2)))
        (subseq lin 1)
      lin)))

(defun break-pop-response (str &key (integers nil))
  "Separate answers into their individual components. would be faster to use regular expressions, but this keeps to pop package lighter. The integers key can be set to t if the answer is known to be a list of integers"
  (let ((word-lst (cl-ppcre:split " " str)))
    (if integers
        (mapcar #'(lambda (str)
                    (parse-integer str))
                word-lst)
      word-lst)))

  #+NIL(let ((char-lst nil)
        (word-lst nil)
        (len (length str)))
    (dotimes (i len (push (format nil "~{~a~}" (nreverse char-lst)) word-lst)  )
      (let ((ch (elt str i)))
        (if (char= ch separator)
            (push (format nil "~{~a~}" (nreverse char-lst)) word-lst)
          (push ch char-lst))))
    (if integers
        (mapcar #'(lambda (str)
                    (parse-integer str))
                (nreverse word-lst))
      (nreverse word-lst)))
        
  
  
