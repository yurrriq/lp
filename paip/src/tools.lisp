(in-package #:paip)
(defpackage #:paip.tools
  (:use #:cl #:lisp-unit))
(in-package #:paip.tools)

(defun interactive-interpreter (prompt transformer)
  "(`prompt' for and) read an expression, `transform' it and print the result."
  (loop
    (handler-case
        (progn
          (if (stringp prompt)
              (print prompt)
              (funcall prompt))
          (print (funcall transformer (read))))
      (error (condition)
        (format t "~&;; Error ~a ignored. Back to top level."
                condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))
