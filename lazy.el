;;; lazy.el --- lazy lists from LOL -*- lexical-binding: t -*-

;; Author: Land of Lisp
;; Keywords: lisp

;;; Commentary:

;; THIS IS NOT MY CODE

;;; Code:
(require 'cl-lib)

;; this is really heavy, and probably needlessly so.
(defmacro lazy (&rest body)
  (let ((forced (cl-gensym))
	(value (cl-gensym)))
    `(let ((,forced nil)
	   (,value nil))
       (lambda ()
	 (unless ,forced
	   (setq ,value (progn ,@body))
	   (setq ,forced t))
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defvar lazy-integers
  (cl-labels ((f (n)
                 (lazy-cons n (f (1+ n)))))
    (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst)
                (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n)
              (lazy-null lst))
    (cons (lazy-car lst)
          (take (1- n)
                (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst)
          (take-all (lazy-cdr lst)))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (cl-labels ((f (lst-cur)
	      (if (lazy-null lst-cur)
                  (force (lazy-mapcan fun (lazy-cdr lst)))
                (cons (lazy-car lst-cur)
                      (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
	    (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
        (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
    (lazy-nth (1- n)
              (lazy-cdr lst))))

(defun lazy-remove-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          (lazy-remove-if fun (lazy-cdr lst))
        (lazy-cons x (lazy-remove-if fun (lazy-cdr lst)))))))

(defvar lazy-odd-integers (lazy-remove-if #'evenp lazy-integers))

(defvar lazy-fibinacci
  (cl-labels ((f (n n-1)
                 (lazy-cons n (f (+ n n-1) n))))
    (f 1 0)))

(provide 'lazy)
;;; lazy.el ends here
