;;; tail-call.el --- -*- lexical-binding:t -*-

;; Copyright (C) 2012  Evan Izaksonas-Smith

;; Author: Evan Izaksonas-Smith
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

;; So this was made on a lark, it is not to be trusted or used in any
;; real code, it replaces DEFUN, so you know I mean business when I
;; say, you are fool if you decide to use this in production code.
;;
;; What's wrong with this code: Well, quite frankly any mechanisms for
;; handling tail recursion should live in eval not in defun, for
;; several reasons.  Primarilly eval is the place where evaluation
;; occurs, changing the evaluation model on a per defun basis is
;; wrong.  Further, eval sees expanded code greatly simplifying
;; tail-recursion.
;;
;; However, this way we can rely on lexical binding and test the
;; evaluation model without disturbance in the core.
;;
;; Another problem with this, is that while it promises a general
;; method for tail recursion, it currently only allows for self
;; recursion optimizations.
;;
;; What's right: well, though it would certainly be interesting to
;; create a prover that manages to discern whether or not a call can
;; be optimized, isn't it so much easier to simply store the function
;; which optimizes a form parallel to the definition?  Yeah, that's
;; what I thought you'd say, even with optimizing happening during
;; defun this allows a future implimentation to allow such a function
;; to simply be declared.

;;; Code:
(eval-when-compile
  (require 'cl))

(when (fboundp 'defun~)
  (or (eq (symbol-function 'defun~)
          (symbol-function 'defun))
      (defalias 'defun 'defun~)))
;; (eval-after-load "tail-call"
;;   '(defalias 'defun 'defun-tail-call))

(require 'cl-lib)
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

(defun tail-call--recur (&rest args)
  (throw :recur args))

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
                                    (eq (car pair)
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
                           (memq (car x) ;C.f. cl-do-proclaim.
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
                                         (tail-call-optimize :any (car (last body))))
                                 ;; if the body is unchanged use the original form.
                                 (if (tree-equal old-body body)
                                     (cons 'lambda (cons arglist body))
                                   ;; here begins the magic, some of this could be
                                   ;; cleaned up to simplify making the currect
                                   ;; function signature.  we create the symbols
                                   ;; recur and return for our catch and throw blocks
                                   ;; args is the actual argument list for the
                                   ;; function.
                                   (set-real-function name `(lambda ,arglist ,@body))
                                   (let ((args (cl-gensym))
                                         (return (cl-gensym))                                         
                                         (real-call (cl-gensym)))
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
                                        ;;
                                        ;; we establish an exit point and enter into
                                        ;; iteration each time the call frame will be
                                        ;; overwritten.  and bound as a lambda.

                                        ;; we capture as args for the subsequent
                                        ;; iteration that which is thrown by a recur
                                        ;; form but if the result doesn't trigger a
                                        ;; recur form, instead the value itself is
                                        ;; returned (even if the call is itself
                                        ;; recursive).
                                        ;;
                                        ;; This is going to allow for proper
                                        ;; tail-recursion
                                        (let ((,real-call (cons ',name ,args)))
                                          (catch ',return
                                            (while t
                                              (setq ,real-call
                                                    (catch :recur
                                                      (throw ',return
                                                             (apply (get-real-function (car ,real-call))
                                                                    (cdr ,real-call)))))))))))))))))
        (if declarations
            (cons 'prog1 (cons def declarations))
          def)))))

(defmacro let-recur (bindings &rest body)
  (declare (indent 2))
  (setcar (last body)
          (tail-call-optimize 'recur (car (last body))))
  (let ((args (cl-gensym))
        (return (cl-gensym))
        (recur (cl-gensym))
        (arglist (mapcar #'first bindings)))
    `(cl-labels
         ((tail-call--recur (&rest ,args)
                            (throw ',recur ,args))
          (recur (&rest ,args)
                 (catch ',return
                   (while t
                     (setq ,args
                           (catch ',recur
                             (throw ',return
                                    (apply (lambda ,arglist ,@body) ,args))))))))
       (apply #'recur (list ,@(mapcar #'second bindings))))))

(defun get-real-function (symbol)
  (or (plist-get (symbol-plist symbol)
                 'real-function)
      symbol))

(defun set-real-function (symbol function)
  (setplist symbol (plist-put (symbol-plist symbol)
                              'real-function
                              function)))

;;; Old version
;; (defun tail-call-optimize (name form)
;;   (if (consp form)
;;       (if (eq name (car form))
;;           `(,tail-call--recur-sym ,@(cdr form))
;;         (funcall (or (get-tail-optimize-function (car form))
;;                      (lambda (_ form) form))
;;                  name form))
;;     form))

(defun tail-call-optimize (name form)
  (if (consp form)
      (if (eq name :any)
          (if (functionp (car form))
              `(tail-call--recur (quote ,(car form))
                                 ,@(cdr form))
            (funcall (or (get-tail-optimize-function (car form))
                         (lambda (_ form) form))
                     name form))
        (if (eq name (car form))
            `(tail-call--recur ,@(cdr form))
          (funcall (or (get-tail-optimize-function (car form))
                       (lambda (_ form) form))
                   name form)))
    form))
 
(defmacro set-tail-optimize-function (symbol optimization-function)
  (declare (indent defun))
  `(plist-put (symbol-plist ,symbol) 'tail-optimize-fun
              ,optimization-function))

(defun get-tail-optimize-function (symbol)
  (plist-get (symbol-plist symbol)
             'tail-optimize-fun))

(defun tail-call-optimize-progn (name form)
  (setcar (last form)
          (tail-call-optimize name (car (last form))))
  form)

(defmacro add-tail-optimizations (&rest tail-optimizations)
  `(cl-macrolet ((gtco (symbol)
                       `(get-tail-optimize-function ',symbol))
                 (tco (name form)
                      `(tail-call-optimize ,name ,form)))
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
             (tco name (third form)))
       (funcall (gtco progn) name form)))
 (cond (lambda (name form)
         (setcdr form
                 (mapcar (apply-partially (gtco progn) name)
                         (cdr form)))
         form))
 (case (lambda (name form)
         (setcdr form
                 (funcall (gtco cond) name
                          (cdr form)))
         form))
 (pcase (gtco case)))

(provide 'tail-call)
;;; tail-call.el ends here

;;; Examples:
;;; This sets defun as our new function
;; (defalias 'defun 'defun-tail-call)
;;
;;; This will revert
;;
;; (defalias 'defun 'defun~)

;; example with:
;; (defalias 'defun 'defun-tail-call)
;; (defun triangle (x &optional out)
;;       "foo"
;;       (let ((out (or out 0)))
;;         (if (< x 1) out
;;           (triangle (1- x) (+ x out)))))

;; (pp (get-real-function 'triangle))
;; (lambda
;;   (x &optional out)
;;   "foo"
;;   (let
;;       ((out
;;         (or out 0)))
;;     (if
;;         (< x 1)
;;         out
;;       (tail-call--recur 'triangle
;;                         (1- x)
;;                         (+ x out)))))

;; (pp (symbol-function 'triangle))
;; (closure
;;  (t)
;;  (&rest G72149)
;;  "foo"
;;  (let
;;      ((G72151
;;        (cons 'triangle G72149)))
;;    (catch 'G72150
;;      (while t
;;        (setq G72151
;;              (catch :recur
;;                (throw 'G72150
;;                       (eval
;;                        `(apply ,(get-real-function
;;                                  (car G72151))
;;                                ',(cdr G72151))))))))))

;; (pp (tail-call-optimize-progn
;;      'recur
;;      '(let-recur ((count 5)
;;                     (acc   1))
;;             (if (< count 1) acc
;;               (recur (1- count)
;;                      (* count acc))))))

;; (pp (symbol-function
;;      (defun foo-simple (&rest xs)
;;        "foo"
;;         (apply #'+ xs))))

(defmacro pdefun (name arglist &rest body)
  "Like ordinary defun but uses pcases.  ARGLIST is strictly for
advertising the canonical signature."
  (declare (indent defun)
           (advertised-calling-convention (NAME ARGLIST [DOCSTRING] &rest PATTERNS) ""))
  (let ((args (gensym)))
    `(defun-tail-call ,name (&rest ,args)
       (declare (advertised-calling-convention ,arglist ""))
       ,@(pcase body
             (`(,(and docstring (pred stringp)) . ,body)
              `(,docstring
                (pcase ,args ,@body)))
             (body
              `((pcase ,args ,@body)))))))

(pdefun preverse (list | in out)
                    "A simple test of pdefun."
                    (`(,list)
                     (preverse list nil))

                    (`(nil ,reverse)
                     reverse)

                    (`((,head . ,tail) ,reverse)
                     (preverse tail `(,head . ,reverse))))

;;; Mutual recursion:
(fset '1-step-back (lambda))
(defun-tail-call 2-steps-forward (x y)
  (if (> x y) x
    (1-step-back (+ 2 x) y)))
(defun-tail-call 1-step-back (x y)
  (2-steps-forward (1- x) y))

(let-recur ((count 5000)
            (acc   1))
    (if (< count 1) acc
      (recur (1- count)
             (+ count acc))))
