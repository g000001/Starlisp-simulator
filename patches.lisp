;;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10; Patch-File: T -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(import-then-export '*sim '*sim '(load-patches))

 
(defun load-patches ()
;  (load "C:/Lispcode/Starsim/F20/patches.lisp")
;  (load "C:/Lispcode/Starsim/F20/patches.lisp")
  (load "/u/mc/lisp/Lisp/StarLisp/sbcl/patches.lisp")
  )

;;; ===========================================================================

;;; *COLD-BOOT the Simulator to its default configuration
;;; to make sure things are working.


(eval-when (:load-toplevel :execute)
  (*cold-boot :initial-dimensions '(8 4))
  )

