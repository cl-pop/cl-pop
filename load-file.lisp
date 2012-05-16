(in-package :common-lisp)
;;;
;;; Example Load File 
;;;

(let ((base-dir #P"/home/bsorg/sandbox/opensource/"))
  (progn 
    (load (merge-pathnames base-dir "asdf.lisp"))
    
    (push (merge-pathnames "cl-base64-3.3.1/" base-dir) asdf:*central-registry*)
    (asdf:operate 'asdf:load-op 'cl-base64)
    
    (push (merge-pathnames "cl-smtp/" base-dir ) asdf:*central-registry*)
    (asdf:operate 'asdf:load-op 'cl-smtp)
    
    (push (merge-pathnames "cl-ppcre/" base-dir ) asdf:*central-registry*)
    (asdf:operate 'asdf:load-op 'cl-ppcre)
    
    (push (merge-pathnames "cl-pop/" base-dir ) asdf:*central-registry*)
    (asdf:operate 'asdf:load-op 'cl-pop)))
