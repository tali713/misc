;;; eai-tools.el --- Miscellanious tools extracted from .emacs -*- lexical-binding:t -*-

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
(require 'cl-lib)
(defadvice eval-last-sexp-1 (around double-arg-delete
                                    (eval-last-sexp-arg-internal)
                                    activate)
  "Delete preceding sexp when C-u C-u is used before eval-last-sexp."
  (let ((start-pos (point)))
    ad-do-it
    (when (and (consp eval-last-sexp-arg-internal)
               (= 16 (car eval-last-sexp-arg-internal)))
      (save-excursion (goto-char start-pos)
                      (backward-sexp)
                      (let ((bounds (bounds-of-thing-at-point 'sexp)))
                        (delete-region (car bounds)
                                       (cdr bounds)))))))

(defun recenter-defun-top-line ()
  "Recenter the window so that the current defun is placed on the top line."
  (interactive)
  (let ((current-point (point)))
    (beginning-of-defun)
    (recenter 0)
    (unless (> (- (line-number-at-pos current-point)
                  (line-number-at-pos))
               (window-height))
      (goto-char current-point))))

(defun reverse-string (str)
  "Reverses a string."
  (concat (reverse (coerce str 'list))))

(defun reverse-string-in-region (START END)
  "Reverses the text between START and END.
If called interactively, reverse the text between point and mark."
  (interactive `(,(point) ,(mark)))
  (let ((reversed-string (reverse-string (buffer-substring START END))))
    (delete-region START END)
    (insert reversed-string)))

(defun buffer-major-mode (buf)
  "Returns the of `major-mode' for buffer BUF."
  (with-current-buffer buf major-mode))

(defun buffers-with-mode (mode &optional frame)
  "Return a list of all existing live buffers whose mode is MODE.
If the optional arg FRAME is a frame, we return the buffer list in the
proper order for that frame: the buffers show in FRAME come first,
followed by the rest of the buffers."
  (remove-if-not (lambda (buf)
                   (eq mode (buffer-major-mode buf)))
                 (buffer-list frame)))

(defmacro buffers-where (expr &optional frame)
  "Get the list of buffers where EXPR is true.  EXPR will be
called in each buffer, using `with-current-buffer'."
  (let ((buf (gensym)))
    `(remove-if-not
      (lambda (,buf)
        (with-current-buffer ,buf
          ,expr))
      (buffer-list ,frame))))

(defun ido-switch-buffer-with-mode (&optional mode)
  "Switch to another buffer using ido filtered to mode.
When called interactively this function will treat the current
mode as MODE"
  (interactive `(,major-mode))
  (switch-to-buffer
   (get-buffer
    (ido-completing-read
     (replace-regexp-in-string (rx " mode" eos) " buffer: "
                               (easy-mmode-pretty-mode-name major-mode))
     (mapcar 'buffer-name
             (buffers-with-mode mode))))))

(defun transpose-matrix (m)
  (apply #'cl-mapcar 'list m))

(defun f-to-c (f-temp)
  (* (- f-temp 32.0)
     (/ 5.0 9)))
(defun c-to-f (c-temp)
  (+ 32.0 (/ (* c-temp 9.0)
             5)))
(defun mph-to-kph (v) (/ v 0.62137))
(defun kph-to-mph (v) (* v 0.62137))

(defun windchill (T-air V-wind &optional t-unit v-unit)
  "gives windchill takes in air temperature and wind velocity,
optional arguments to specify units, defaults to 'C and 'kph.
will accept 'F and 'mph."
  (let* ((t-unit (or t-unit 'C))
         (v-unit (or v-unit 'kmh))
         (T-air  (if (eq t-unit 'F) 
                     (f-to-c T-air)
                   T-air))
         (V-wind (if (eq v-unit 'mph) 
                     (mph-to-kph V-wind)
                   V-wind)))
    (format "%.1f %s"
            (funcall (if (eq t-unit 'F) 'c-to-f 'identity)
                     (+ 13.12 (* 0.6215 T-air)
                        (* -11.37 (expt V-wind 0.16))
                        (* 0.3965 T-air (expt V-wind 0.16))))
            t-unit))) 

(defun eval-from-string (str)
  (eval (car (read-from-string str))))


(defun blank-line-p () 
  (save-excursion (beginning-of-line)
                  (looking-at "[ \t]*$"))) 

(defun just-one-blank-line ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, do nothing.
On nonblank line, insert a blank line."
  (interactive "*")
  (if (blank-line-p)
      (progn
        (beginning-of-line)
        (delete-region (point)
                       (if (re-search-backward "[^ \t\n]" nil t)
                           (progn (forward-line 1) (point))
                         (point-min)))
        ;; Delete following blank lines, unless the current line is blank
        ;; and there are no following blank lines.
        (save-excursion
          (end-of-line)
          (forward-line 1)
          (delete-region (point)
                         (if (re-search-forward "[^ \t\n]" nil t)
                             (progn (beginning-of-line) (point))
                           (point-max)))))
    (newline)
    (just-one-blank-line))
  ;; Handle the special case where point is followed by newline and eob.
  ;; Delete the line, leaving point at eob.
  (when (looking-at "^[ \t]*\n\\'")
    (delete-region (point) (point-max))))

(defun just-one-dwim () 
  (interactive)
  (if (blank-line-p) 
      (just-one-blank-line)
    (just-one-space)))

(defun read-whole-buffer ()
  (first (read-from-string (buffer-substring-no-properties (point-min)
                                                           (point-max)))))

(defmacro compile-body (&rest body)
  `(funcall (byte-compile '(lambda ()
                             ,@body))))

(defun prefix->infix1 (sexp)
  (if (consp sexp) 
      (let ((token (car sexp)))
        (cons (prefix->infix1 (cadr sexp))
              (mapcan (lambda (el)
                        (list token (prefix->infix1 el)))
                      (cddr sexp))))
    sexp))

(defmacro prefix->infix (sexp)
  (prin1-to-string (prefix->infix1 sexp)))

(defun macroexpand-and-insert (&optional arg)
  (interactive "P")
  (let ((replacement (pp-to-string (macroexpand (preceding-sexp))))
        (opoint (point)))
    (when arg
      (forward-sexp -1)
      (delete-region opoint (point)))
    (insert replacement)))

(defun macroexpand-all-and-insert (&optional arg)
  (interactive "P")
  (let ((replacement (pp-to-string (macroexpand-all (preceding-sexp))))
        (opoint (point)))
    (when arg
      (forward-sexp -1)
      (delete-region opoint (point)))
    (insert replacement)))

(defun macroexpand-and-replace ()
  (interactive)
  (macroexpand-and-insert t))

(defmacro mapcarn (function sequence)
  (let ((el (cl-gensym))
        (n (cl-gensym)))
    `(mapcar (let ((,n 0))
               (lambda (,el)
                 (prog1 (funcall ,function ,n ,el)
                   (setq ,n (1+ ,n)))))
             ,sequence)))
;; (mapcarn 'cons (number-sequence 6 11))
;; ((0 . 6) (1 . 7) (2 . 8) (3 . 9) (4 . 10) (5 . 11))

(defun circular-list (&rest list)
  (nconc list list))

(defun make-circular (list)
  (apply #'circular-list list))

(defmacro keep-when (predicate list)
  (let ((drop (cl-gensym))
        (el (cl-gensym)))
    `(remove ',drop
             (mapcar (lambda (,el)
                       (if (funcall ,predicate ,el)
                           ,el
                         ',drop))
                     ,list))))

(defmacro keep-unless (predicate list)
  (let ((drop (cl-gensym))
        (el (cl-gensym)))
    `(remove ',drop
             (mapcar (lambda (,el)
                       (if (funcall ,predicate ,el)
                           ',drop
                         ,el))
                     ,list))))



(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

(defun my-isearch-forward ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun my-isearch-backward ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(defun lisp1.5--transform (sexp)
  (if (and sexp (listp sexp))
      (cond ((eq 'lambda (car sexp))
             `(lambda ,(cadr sexp) ,@(mapcar 'lisp1.5--transform (cddr sexp))))
            ((eq 'let (car sexp))
             `(let ,(mapcar (lambda (bind)
                              (if (consp bind)
                                  `(,(first bind)
                                    ,(lisp1.5--transform (second bind)))
                                bind))
                            (cadr sexp))
                ,@(mapcar 'lisp1.5--transform (cddr sexp))))
            ((eq 'define (car sexp))
             (let ((symbol (cadr sexp))
                   (body (cddr sexp)))
               (if (listp symbol)
                   `(setq ,(car symbol)
                          (lambda ,(cdr symbol)
                            ,@(mapcar 'lisp1.5--transform body)))
                 `(setq ,symbol ,(lisp1.5--transform (car body))))))
            ((eq 'quote (car sexp))
             sexp)
            (t
             (let ((func (car sexp)))
               (if (and (symbolp func)
                        (fboundp func))
                   `(,func ,@(mapcar 'lisp1.5--transform (cdr sexp)))
                 `(funcall ,@(mapcar 'lisp1.5--transform sexp))))))
    (if (and (symbolp sexp)
             (not (boundp sexp))
             (fboundp sexp))
        `(function ,sexp)
      sexp)))

(defmacro lisp1.5 (&rest body)
  `(progn ,@(mapcar 'lisp1.5--transform body)))

(defun cardinal->int (card)
  (do ((n 0 (1+ n))
       (card (coerce card 'list) (cdr card))
       (integer 0 (+ (* (car card)
                        (expt 2 (* 8 n)))
                     integer)))
      ((null card) integer)))

(defun int->cardinal (bits int)
  (let ((bytes (/ bits 8)))
    (do ((place 1 (1+ place))
         (int int (floor (/ int (expt 2 8))))
         (card () (cons (mod int (expt 2 8))
                        card)))
        ((> place bytes) (concat (nreverse card))))))
;;example use (cardinal->int (x-window-property "_NET_NUMBER_OF_DESKTOPS" nil "CARDINAL" 0))


(defun xor (&rest xs)
  "True if and only if an odd number of XS are true."
  (oddp (length (remove nil xs))))

(defmacro map-buffer (exp buffer-list)
  (declare (indent 1))
  `(mapcar (lambda (buf)
             (with-current-buffer buf
               ,exp))
           ,buffer-list)) (defadvice custom-save-variables (around make-custom-safe-themes-first activate)
  (cl-flet ((string< (S1 S2) (and (not (string= "custom-safe-themes" S2))
                                  (or (string= "custom-safe-themes" S1)
                                      (string< S1 S2)))))
    ad-do-it))

(defun fill-buffer ()
  (fill-region (point-min)
               (point-max)))

(defmacro run-on-file (file &rest exprs)
  (let ((already-visiting-p (cl-gensym))
        (output (cl-gensym)))
    `(let ((,already-visiting-p (find-buffer-visiting ,file)))
       (save-window-excursion
         (with-current-buffer (find-file ,file)
           (let ((,output (progn ,@exprs)))
             (unless ,already-visiting-p 
               (save-buffer)
               (kill-buffer))
             ,output))))))




(defmacro precur (fname args &rest pcases)
  "Intended for a common use case of pcase.  Establishes a
recursive pattern matching function with initial arglist ARGS.
the function is named FNAME.  PCASES is identical to the forms
used in `pcase' "
  (let ((pattern (cl-gensym)))
    `(cl-labels
         ((,fname (&rest ,pattern)
                  (pcase ,pattern
                    ,@pcases)))
       (,fname ,@args))))

(defun preverse (list)
  (precur rev (list nil)
          (`(nil ,reverse) reverse)
          (`((,head . ,tail) ,reverse) (rev tail `(,head . ,reverse)))))


(defmacro pdefun (name arglist &rest body)
  "Like ordinary defun but uses pcases.  ARGLIST is strictly for
advertising the canonical signature."
  (declare (indent defun)
           (advertised-calling-convention (NAME ARGLIST [DOCSTRING] &rest PATTERNS) ""))
  (let ((args (gensym)))
    `(defun ,name (&rest ,args)
       (declare (advertised-calling-convention ,arglist ""))
       ,@(pcase body
             (`(,(and docstring (pred stringp)) . ,body)
              `(,docstring
                (pcase ,args ,@body)))
             (body
              `((pcase ,args ,@body)))))))


(defmacro plambda (&rest pcases)
  (declare (indent defun))
  (let ((args (gensym)))
    `(lambda (,args)
       (pcase args ,@pcases))))


(pdefun preverse (list | in out)
  "A simple test of pdefun."
  (`(,list)
   (preverse list nil))
  
  (`(nil ,reverse)
   reverse)

  (`((,head . ,tail) ,reverse)
   (preverse tail `(,head . ,reverse))))

;; (defvar eai-real< (symbol-function '<))
;; (fset '<2 eai-real<)
;; (fset '< eai-real<)
;; (defmacro < (num1 num2 &rest nums)
;;      "Return t if numbers are in increasing order.  All must be numbers or markers."
;;      `(and (<2 ,num1 ,num2)
;;            (if ',nums
;;                (< ,num2
;;                   ,(car nums)
;;                   ,@(cdr nums))
;;              t)))


(provide 'eai-tools)
;;; eai-tools.el ends here
