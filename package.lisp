(defpackage "*SIM-I"
  (:use "COMMON-LISP")
  (:nicknames "*LISP-I")
  )


(make-package :*sim)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :*LISP-SIMULATOR *features*))
