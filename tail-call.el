;;; tail-call.el --- -*- lexical-binding:t -*-

;; Copyright (C) 2012  Evan Izaksonas-Smith

;; Author: Evan Izaksonas-Smith <tali713@rastafy>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'cl)

(defvar tail-call--recur-sym (cl-gensym))
(defvar real-defun (symbol-function 'defun))
(defalias 'defun~ real-defun)

;;; Original version is maintained for posterity:
;;
;; (defmacro defun-tail-call (name arglist &optional docstring &rest body)
;;   (declare (indent defun))
;;   (let ((real-body `(,docstring ,@body)))
;;     (setcar (last real-body)
;;             (tail-call-optimize name (car (last real-body))))
;;     (let ((args (gensym))
;;           (return (gensym))
;;           (recur (gensym)))
;;       `(defun ,name (&rest ,args)
;;          ,@(if (stringp docstring) `(,docstring))
;;          (declare (advertised-calling-convention ,arglist ""))
;;          (cl-flet ((,tail-call--recur-sym (&rest ,args)
;;                                          (throw ',recur ,args)))
;;            (catch ',return
;;              (while t
;;                (setq ,args
;;                      (catch ',recur
;;                        (throw ',return
;;                               (apply (lambda ,arglist ,@real-body) ,args)))))))))))


(defmacro defun-tail-call (name arglist &optional docstring &rest body)
  "Define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...).
See also the function `interactive'.
DECL is a declaration, optional, of the form (declare DECLS...) where
DECLS is a list of elements of the form (PROP . VALUES).  These are
interpreted according to `defun-declarations-alist'.
The return value is undefined.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  ;; We can't just have `decl' as an &optional argument, because we need
  ;; to distinguish
  ;;    (defun foo (arg) (toto) nil)
  ;; from
  ;;    (defun foo (arg) (toto)).
  (declare (doc-string 3))
  (if (not lexical-binding)
      `(defun~ ,name ,arglist ,docstring ,@body)
    (let* ((decls (cond
                  ((eq (car-safe docstring) 'declare)
                   (prog1 (cdr docstring) (setq docstring nil)))
                  ((eq (car-safe (car body)) 'declare)
                   (prog1 (cdr (car body)) (setq body (cdr body))))))
           (decls (if (cl-find-if (lambda (pair)
                                    (eq (car (pair))
                                        'advertised-calling-convention))
                                  decls)
                      decls
                    (cons `(advertised-calling-convention
                            ,arglist "")
                          decls))))
      (if docstring (setq body (cons docstring body))
        (if (null body) (setq body '(nil))))
      (let ((declarations
             (mapcar
              #'(lambda (x)
                  (let ((f (cdr (assq (car x) defun-declarations-alist))))
                    (cond
                     (f (apply (car f) name arglist (cdr x)))
                     ;; Yuck!!
                     ((and (featurep 'cl)
                           (memq (car x)  ;C.f. cl-do-proclaim.
                                 '(special inline notinline optimize warn)))
                      (if (null (stringp docstring))
                          (push (list 'declare x) body)
                        (setcdr body (cons (list 'declare x) (cdr body))))
                      nil)
                     (t (message "Warning: Unknown defun property %S in %S"
                                 (car x) name)))))
              decls))
            (def (list 'defalias
                       (list 'quote name)
                       (list 'function
                             (progn
                               ;; Capture the body and optimize its tail position.
                               (let ((old-body (copy-tree body)))
                                 (setcar (last body)
                                         (tail-call-optimize name (car (last body))))
                                 ;; if the body is unchanged use the original form.
                                 (if (tree-equal old-body body)
                                     (cons 'lambda (cons arglist body))
                                   ;; here begins the magic, some of this could be
                                   ;; cleaned up to simplify making the currect
                                   ;; function signature.  we create the symbols
                                   ;; recur and return for our catch and throw blocks
                                   ;; args is the actual argument list for the
                                   ;; function.
                                   (let ((args (cl-gensym))
                                         (return (cl-gensym))
                                         (recur (cl-gensym))) 
                                     `(lambda (&rest ,args)
                                        ;; capture the docstring if any.
                                        ,@(when (stringp (car body))
                                            (prog1 (list (car body))
                                              (setq body (cdr body))))
                                        ;; capture and place an interactive form in
                                        ;; the body, if one exitsts.
                                        ,@(let ((interactive
                                                 (cl-find-if (lambda (sexp)
                                                               (and (consp sexp)
                                                                    (eq (car sexp)
                                                                        'interactive)))
                                                             (cons docstring body))))
                                            (when interactive
                                              (setq body (remove interactive body))
                                              `(,interactive)))
                                        ;; and the true secret lives here, we bind a
                                        ;; function to throw the args back to a recur
                                        ;; form. as though we had explicitly recurred
                                        ;; in any location where optimization
                                        ;; occured.
                                        (cl-flet ((,tail-call--recur-sym
                                                   (&rest ,args)
                                                   (throw ',recur ,args)))
                                          ;; we establish an exit point
                                          (catch ',return
                                            ;; to extablish an iterative form
                                            (while t
                                              ;; we capture as args our for the
                                              ;; subsequent iteration that which is
                                              ;; thrown by a recur form
                                              (setq ,args
                                                    (catch ',recur
                                                      ;; but if the result doesn't
                                                      ;; trigger a recur form,
                                                      ;; instead the value is
                                                      ;; returned (even if the call
                                                      ;; is none the less recursive).
                                                      (throw ',return
                                                             (apply (lambda ,arglist ,@body)
                                                                    ,args))))))))))))))))
        (if declarations
            (cons 'prog1 (cons def declarations))
          def)))))

;;; This sets defun as our new function
(defalias 'defun 'defun-tail-call)
;;; This will revert
;;
;; (defalias 'defun 'defun~)



(defmacro let-recur (bindings &rest body)
  (declare (indent 2))
  (setcar (last body)
          (tail-call-optimize 'recur (car (last body))))
  (let ((args (cl-gensym))
        (return (cl-gensym))
        (recur (cl-gensym))
        (arglist (mapcar #'first bindings)))
    `(cl-labels
         ((,tail-call--recur-sym (&rest ,args)
                                (throw ',recur ,args))
          (recur (&rest ,args)
                 (catch ',return
                   (while t
                     (setq ,args
                           (catch ',recur
                             (throw ',return
                                    (apply (lambda ,arglist ,@body) ,args))))))))
       (apply #'recur (list ,@(mapcar #'second bindings))))))

(defun~ tail-call-optimize (name form)
  (if (consp form)
      (if (eq name (car form))
          `(,tail-call--recur-sym ,@(cdr form))
        (funcall (or (tail-optimize-function (car form))
                     (lambda (_ form) form))
                 name form))
    form))

(defmacro set-tail-optimize-function (symbol optimization-function)
  (declare (indent defun))
  `(plist-put (symbol-plist ,symbol) 'tail-optimize-fun
              ,optimization-function))

(defmacro get-tail-optimize-function (symbol)
  `(plist-get (symbol-plist ',symbol)
              'tail-optimize-fun))


(defun tail-call-optimize-progn (name form)
  (setcar (last form)
          (tail-call-optimize name (car (last form))))
  form)

;; Make this its own form
(defmacro add-tail-optimizations (&rest tail-optimizations)
  `(cl-macrolet ((gtco (symbol)
                      `(get-tail-optimize-function ,symbol)))
     ,@(mapcar (lambda (pair)
               `(set-tail-optimize-function
                  ',(car pair)
                  ,(cadr pair)))
             tail-optimizations)))

(add-tail-optimizations
     (progn #'tail-call-optimize-progn)
     (let (gtco progn))
     (let* (gtco let))
     (if (lambda (name form)
           (setf (third form)
                 (tail-call-optimize name (third form)))
           (funcall (gtco progn) name form)))
     (cond (lambda (name form)
             (setcdr form
                     (mapcar #'tail-call-optimize-progn (cdr form)))
             form)))

(provide 'tail-call)
;;; tail-call.el ends here

;;; Examples:

;; (pp (symbol-function
;;      (defun triangle (x &optional out)
;;        "foo"
;;        (let ((out (or out 0)))
;;          (if (< x 1) out
;;            (triangle (1- x) (+ x out)))))))

;; (let-recur ((count 5)
;;             (acc   1))
;;     (if (< count 1) acc
;;       (recur (1- count)
;;              (* count acc))))
 
;; (pp (symbol-function
;;      (defun foo-simple (&rest xs)
;;        "foo"
;;        (apply #'+ xs))))
