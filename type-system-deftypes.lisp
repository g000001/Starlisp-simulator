;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I; MUSER: YES -*-

(in-package :*sim)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.

(deftype defined-float (&optional mantissa exponent)
  ;; #-CORMANLISP
  (declare (ignore mantissa exponent))
  'float)

(deftype string-char () 'character)

; orig ;(deftype pvar (&optional (element-type '*))
; orig ;  ;; I have to return a satisfies type with a closure so that typep can work.
; orig ;  ;; But, returning a closure will blow up both subtypep and the compiler on lucid.
; orig ;  (let ((closure (*lisp-i::pvar-type-predicate 
; orig ;                  (cadr (*lisp-i::canonical-pvar-type `(pvar ,element-type))))))
; orig ;    `(satisfies ,closure)))

;; type-system-deftypes.lisp
#|(deftype pvar (&optional (element-type '*))
  ;; I have to return a satisfies type with a closure so that typep can work.
  ;; But, returning a closure will blow up both subtypep and the compiler on lucid.
  (let ((g (gentemp))
        (closure (*lisp-i::pvar-type-predicate 
                  (cadr (*lisp-i::canonical-pvar-type `(pvar ,element-type))))))
;      (format t "////////// ~S //////////~%" closure)
      (setf (symbol-function g) 
            (lambda (&rest args) (apply closure args)))
      `(satisfies ,g)))|#

(defvar *pvar-satisfy-tem* '*)

(defun pvar-satisfy-func (&rest args)
  (apply (*lisp-i::pvar-type-predicate 
          (cadr (*lisp-i::canonical-pvar-type `(pvar ,*pvar-satisfy-tem*))))
         args))

(deftype pvar (&optional (element-type '*))
  (let ((*pvar-satisfy-tem* element-type))
    `(satisfies pvar-satisfy-func)))

;(cond ((symbolp closure) (coerce closure 'function))
;                  ('T ))

;(fboundp '(lambda (x) x))

#|(cond ((functionp closure) closure)
                ((fboundp closure) (coerce closure 'function))
                ('T closure))|#

;(defun *segment-set-p (&rest x)  x) ;; 謎
;(defun *SIM::*WORD-P  (&rest x)  x) ;; 謎

;(COERCE #'*SIM-I::*SEGMENT-SET-P 'FUNCTION)

;(coerce listp 'function)

;(LAMBDA (OBJECT) (UNSIGNED-PVARP OBJECT LENGTH))

;(SYMBOL-FUNCTION (LAMBDA (*SIM-I::OBJECT) (*SIM-I::UNSIGNED-PVARP *SIM-I::OBJECT LENGTH)))

#|(LAMBDA (*SIM-I::OBJECT)
    (*SIM-I::UNSIGNED-PVARP *SIM-I::OBJECT LENGTH))|#

;(*lisp-i::pvar-type-predicate 
; (cadr (*lisp-i::canonical-pvar-type `(pvar *))))

;(*lisp-i::canonical-pvar-type `(pvar *))

;(typep 'foo 'pvar)

#|(defun my-pvarp (&optional element-type '*)
  (funcall (*lisp-i::pvar-type-predicate 
            (cadr (*lisp-i::canonical-pvar-type `(pvar ,element-type))))))|#

#|(defun my-pvarp-* ()
  (funcall (*lisp-i::pvar-type-predicate 
            (cadr (*lisp-i::canonical-pvar-type `(pvar '*))))))|#


;(typep x '(satisfies p)) is equivalent to (if (p x) t nil).

#|(defun my-pvarp (&optional (element-type '*))
  ;; I have to return a satisfies type with a closure so that typep can work.
  ;; But, returning a closure will blow up both subtypep and the compiler on lucid.
  (let ((closure (*lisp-i::pvar-type-predicate 
                  (cadr (*lisp-i::canonical-pvar-type `(pvar ,element-type))))))
    (if (funcall closure element-type)
        t
        nil)))|#

#|(*lisp-i::pvar-type-predicate 
                  (cadr (*lisp-i::canonical-pvar-type `(pvar 'foo))))|#

#|(deftype pvar (&optional (element-type '*))
  ;; I have to return a satisfies type with a closure so that typep can work.
  ;; But, returning a closure will blow up both subtypep and the compiler on lucid.
  `(satisfies pvarp))|#

(deftype boolean-pvar ()
  `(pvar boolean))

(deftype signed-pvar (&optional width)
  `(pvar (signed-byte ,width)))

(deftype signed-byte-pvar (&optional width)
  `(pvar (signed-byte ,width)))

(deftype field-pvar (&optional width)
  `(pvar (unsigned-byte ,width)))

(deftype unsigned-pvar (&optional width)
  `(pvar (unsigned-byte ,width)))

(deftype unsigned-byte-pvar (&optional width)
  `(pvar (unsigned-byte ,width)))

(deftype float-pvar (&optional (mantissa '*) (exponent '*))
  `(pvar (defined-float ,mantissa ,exponent)))

(deftype short-float-pvar ()
  `(pvar short-float))

(deftype single-float-pvar ()
  `(pvar single-float))

(deftype double-float-pvar ()
  `(pvar double-float))

(deftype long-float-pvar ()
  `(pvar long-float))

(deftype extended-float ()
  `(defined-float ,extended-float-mantissa ,extended-float-exponent))

(deftype character-pvar ()
  `(pvar character))

(deftype string-char-pvar ()
  `(pvar string-char))

(deftype complex-pvar (&optional (mantissa '*) (exponent '*))
  `(pvar (complex (defined-float ,mantissa ,exponent))))

(deftype short-complex-pvar ()
  `(pvar (complex short-float)))

(deftype single-complex-pvar ()
  `(pvar (complex single-float)))

(deftype double-complex-pvar ()
  `(pvar (complex double-float)))

(deftype long-complex-pvar ()
  `(pvar (complex long-float)))

(deftype array-pvar (&optional (element-type '*) (dimensions '*))
  `(pvar (array ,element-type ,dimensions)))

(deftype vector-pvar (&optional (element-type '*) (length '*))
  `(pvar (array ,element-type (,length))))

(deftype string-pvar (&optional (length '*))
  `(pvar (array string-char (,length)))
  )

(deftype bit-vector-pvar (&optional (length '*))
  `(pvar (array (unsigned-byte 1) (,length)))
  )

(deftype general-pvar ()
  `(pvar t))

(deftype front-end ()
  't)

(deftype front-end-pvar ()
  `(pvar front-end)
  )


