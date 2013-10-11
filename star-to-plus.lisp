(defpackage :star-to-plus
  (:use :cl :series))

(in-package :star-to-plus)

(defun get-all-defconstant (dir &optional (suffix "lisp"))
  (let ((files (directory (make-pathname :directory dir :name :wild :type suffix))))
    (iter)))

(defun grep-defconstant (file pool)
  (iterate ((line (scan-file file #'read-line)))
    (let ((match (aref (or (nth-value 1 (ppcre:scan-to-strings ".*defconstant\\s+(.*)\\s+.*" line)) #(0)) 0)))
      (and match (push match pool)))))



(let (pool)
  (iterate ((i (scan (get-all-defconstant "/u/mc/lisp/Lisp/StarLisp/sbcl/"))))
    (grep-defconstant i pool))
  pool)

(let (res)
  (grep-defconstant "/u/mc/lisp/Lisp/StarLisp/sbcl/definitions.lisp" res)
  res)

  
