(in-package #:paip)
(defpackage #:paip.intro
  (:use #:cl #:lisp-unit))
(in-package #:paip.intro)

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

;; Exercise 1.1
(defparameter *suffixes*
  '(MD Jr)
  "A list of suffixes that can appear at the end of a name.")

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (if (member (first (last name)) *suffixes*)
      (last-name (butlast name))
    (first (last name))))

(define-test test-last-name
  (assert-equal 'Morgan (last-name '(Rex Morgan MD)))
  (assert-equal 'Downey (last-name '(Morton Downey Jr))))

;; Exercise 1.2
(defun square (x) (expt x 2))

(defun power (x n)
  "Raise x to the power of n."
  (cond ((zerop n) 1)
        ((evenp n) (square (power x (/ n 2))))
        (t (* x (power x (- n 1))))))

(define-test test-power
  (assert-equal 9 (power 3 2)))

;; Exercise 1.3
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))
