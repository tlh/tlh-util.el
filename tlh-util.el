;;; tlh-util.el --- a bunch of elisp utility fns and macros

;; Commentary:
;;
;;  This is a collection of elisp utility functions and macros I've
;;  found useful at some point.  Most are written by me, but some are
;;  cannibalized from elsewhere.  I tried giving credit where due, but
;;  found that most of the time the person I got it from had
;;  themselves gotten it from someone else, and on backward into the
;;  mist.  So I've done away with credit, and therefore take credit
;;  for none of it.
;;
;;  None of these utilities are namespace-prefixed, so watch out for
;;  stomping.
;;

;; Installation:
;;
;;  - put `tlh-util.el' somewhere on your emacs load path
;;
;;  - add this line to your .emacs file:
;;    (require 'tlh-util)
;;

;;; Code:

(eval-when-compile
  (require 'cl))

;; macros misc

(defmacro with-gensyms (syms &rest body)
  (declare (indent defun))
  `(let ,(mapcar (lambda (s) (list s '(gensym))) syms)
     ,@body))

(defmacro abbrev (long short &optional indent-type)
  `(defmacro ,short (&rest body)
     (declare (indent ,indent-type))
     `(,',long ,@body)))

(abbrev destructuring-bind dbind defun)

(abbrev multiple-value-bind mvbind defun)

(defmacro until (test &rest body)
  (declare (indent defun))
  `(while (not ,test) ,@body))

(defmacro cif (&rest args)
  "Condish `if'"
  (cond ((null args) nil)
        ((null (cdr args)) `,(car args))
        (t `(if ,(car args)
                ,(cadr args)
              (cif ,@(cddr args))))))

(defmacro cmd (&rest body)
  `(lambda () (interactive) ,@body))

;; anaphoric macros

(defmacro aif (test then &rest else)
  `(let ((it ,test))
     (if it ,then ,@else)))

(defmacro acif (&rest forms)
  "Anaphoric condish `if'."
  (cif (null forms)   nil
       (oney forms) `,(car forms)
       `(aif ,(car forms)
             ,(cadr forms)
             (acif ,@(cddr forms)))))

(defmacro awhen (test &rest body)
  (declare (indent defun))
  `(aif ,test (progn ,@body)))

(defmacro aand (&rest args)
  (cif (null args)        t
       (null (cdr args))  (car args)
       `(aif ,(car args)  (aand ,@(cdr args)))))

(defmacro awhile (test &rest body)
  (declare (indent defun))
  (with-gensyms (res)
    `(do ((it ,test ,test) ,res)
         ((not it) ,res)
       (setf ,res (progn ,@body)))))

;; operations on numbers

(defun posp (num) (> num 0))

(defun negp (num) (< num 0))

(defun inverse (num) (/ 1 num))

(defun whole-number-p (num)
  (zerop (- num (truncate num))))

(defun avg (&rest nums)
  (/ (float (apply '+ nums)) (length nums)))

(defun rescale (value oldmin oldmax newmin newmax)
  (let ((diff (float (- oldmax oldmin))))
    (when (zerop diff)
      (error "rescale: min and max can not be equal"))
    (+ (* (/ (- newmax newmin) diff)
          (- value oldmin))
       newmin)))

(defun within (lo hi num)
  (and (<= lo num) (>= hi num)))

(defun confine-to (lo hi n)
  (max lo (min hi n)))

(defun astray (val min max)
  (cif (< val min) (- val min)
       (> val max) (- val max)
       0))

(defun keyed-comparison (key test objs)
  (let (res)
    (mapc (lambda (obj)
            (when (or (not res) (funcall test (funcall key obj) (funcall key res)))
              (setq res obj)))
          objs)
    res))

(defun keyed-min (key &rest objs)
  (keyed-comparison key '< objs))

(defun keyed-max (key &rest objs)
  (keyed-comparison key '> objs))

(defun hypotenuse (x1 y1 x2 y2)
  (sqrt (+ (expt (- y1 y2) 2)
           (expt (- x1 x2) 2))))

;; operations on lists

(defun add-all-to-list (list-var &rest args)
  (mapc (lambda (elt) (add-to-list list-var elt)) args))

(defun oney (lst)
  (and (consp lst)
       (not (cdr lst))))

(defun dotted (lst)
  (cdr (last lst)))

(defun access (key alist)
  (cadr (assoc key alist)))

(defun cnext (elt lst)
  (or (cadr (member elt lst))
      (car lst)))

(defun cprev (elt lst)
  (cnext elt (reverse lst)))

(defun remove-from-list (list elt)
  (set list (remove elt (symbol-value list))))

(defun select (pred lst)
  "Return the first element of LST that PRED returns t on."
  (catch 'result
    (map nil (lambda (elt)
               (and (funcall pred elt)
                    (throw 'result elt)))
         lst)))

(defun filter (pred seq)
  (let (acc)
    (mapc (lambda (elt) (and (funcall pred elt)
                        (setq acc (cons elt acc))))
          seq)
    (nreverse acc)))

(defun confined-nth (n lst)
  (nth (confine-to 0 (1- (length lst)) n) lst))

(defun firstn (lst n)
  (let ((i 0) acc)
    (catch 'result
      (dolist (elt lst)
        (when (>= i n)
          (throw 'result nil))
        (push elt acc)
        (incf i)))
    (nreverse acc)))

(defun lastn (lst n)
  (nreverse (firstn (reverse lst) n)))

(defun lastn (lst n)
  (let ((l (length lst)))
    (copy-list (nthcdr (- l n) lst))))

(defun group (lst n)
  "Iterative version that avoids stack overflow on long lists."
  (let ((lst lst) acc)
    (while lst
      (push (firstn lst n) acc)
      (setq lst (nthcdr n lst)))
    (nreverse acc)))

(defun pair (lst) (group lst 2))

(defun flatten-assoc-tree (tree pred)
  "Returns an alist of only (key . leaf) pairs in TREE. PRED
determines whether a value is a sub-alist or a leaf."
  (flet ((inner (lst)
                (mapcan (lambda (elt)
                          (cond ((atom elt) nil)
                                ((funcall pred elt) (inner elt))
                                (t (list elt))))
                        lst)))
    (inner tree)))

(defun last1 (lst) (car (last lst)))

(defun left-rotate (lst)
  (append (cdr lst) (list (car lst))))

(defun right-rotate (lst)
  (append (last lst) (butlast lst 1)))

(defun random-elt (lst)
  (nth (random (length lst)) lst))

(defun nths (n &rest lsts)
  (mapcar (lambda (lst) (nth n lst)) lsts))

(defmacro dsetq (varform valform)
  "Destructuring setq."
  (with-gensyms (inner var val value)
    `(labels ((,inner (,var ,val)
                      (cond ((null ,var)
                             nil)
                            ((atom ,var)
                             (set ,var ,val))
                            ((consp (car ,var))
                             (,inner (car ,var) (car ,val))
                             (,inner (cdr ,var) (cdr ,val)))
                            ((eq (car ,var) '&rest)
                             (set (cadr ,var) ,val))
                            ((eq (car ,var) '&optional)
                             (,inner (cdr ,var) ,val))
                            (t
                             (set (car ,var) (car ,val))
                             (,inner (cdr ,var) (cdr ,val))))))
       (let ((,value ,valform))
         (,inner ',varform ,value)
         ,value))))

;; operations on strings

(defun strcat (&rest objs)
  (mapconcat (lambda (obj) (format "%s" obj)) objs ""))

(defun symcat (&rest objs)
  (intern (apply 'strcat objs)))

;; movement

(defun pager-down (&optional arg)
  (interactive "P")
  (ignore-errors
    (scroll-up (or arg 1))
    (next-line (or arg 1))))

(defun pager-up (&optional arg)
  (interactive "P")
  (ignore-errors
    (scroll-down (or arg 1))
    (previous-line (or arg 1))))

(defun goto-buffer-percent (percent)
  (interactive "nPercent: ")
  (goto-line
   (truncate
    (* (/ percent 100.0)
       (count-lines (point-min) (point-max))))))

;; operations on buffer contents

(defun end-of-list-p ()
  (save-excursion
    (backward-char)
    (looking-at ")")))

(defun list-indent ()
  (interactive)
  (let (end)
    (when (end-of-list-p)
      (setq end t)
      (backward-sexp))
    (forward-char)
    (flet ((done () (= (point) (save-excursion (forward-sexp) (point)))))
      (condition-case nil
          (until (done)
            (forward-sexp)
            (and (not (done)) (newline-and-indent)))
        (error (forward-char)
               (when (not end)
                 (backward-list)))))))

(defun mark-list ()
  (interactive)
  (backward-up-list)
  (forward-list)
  (set-mark (point))
  (backward-list))

(defun empty-line-p (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    (looking-at "^$")))

(defun force-forward-line (n)
  (insert (make-string (setq n (forward-line n)) ?\n))
  (forward-line n))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun backward-kill-word-or-bol (arg)
  (interactive "p")
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (bw  (save-excursion (backward-word arg) (point)))
        (p   (point)))
    (kill-region p (if (= p bol) bw (max bol bw)))))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph)))

(defun word-count (&optional begin end)
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
        (e (if mark-active end   (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(defun close-all-parens ()
  (interactive)
  (let (c)
    (while (ignore-errors
             (setq c (save-excursion
                       (backward-up-list)
                       (string-to-char
                        (thing-at-point 'char)))))
      (insert (matching-paren c)))))

;; backward transposition

(defmacro def-backward-transpose (things)
  `(defun ,(symcat 'backward-transpose- things) ()
     (interactive)
     (,(symcat 'transpose- things) -1)))

(def-backward-transpose chars)
(def-backward-transpose words)
(def-backward-transpose lines)
(def-backward-transpose paragraphs)
(def-backward-transpose sexps)

;; region operations

(defun tlh-bounds-of-thing-at-point (thing)
  (if (eq thing 'defun)
      (save-excursion
        (cons (progn (beginning-of-defun) (point))
              (progn (end-of-defun) (point))))
    (bounds-of-thing-at-point thing)))

(defmacro with-bounds (type &rest body)
  (declare (indent defun))
  `(dbind (beg . end) (tlh-bounds-of-thing-at-point ,type)
     ,@body))

(defun duplicate-region (beg end)
  (interactive "r")
  (save-excursion
    (kill-ring-save beg end)
    (goto-char end)
    (unless (empty-line-p) (newline))
    (yank)))

(defun duplicate-and-comment-region (beg end)
  (interactive "r")
  (duplicate-region beg end)
  (comment-region beg end))

(defun kill-region-append-to-file (beg end filename)
  (interactive "r\nfFile: ")
  (append-to-file beg end (expand-file-name filename))
  (kill-region beg end))

;; line region

(defun mark-line ()
  (interactive)
  (with-bounds 'line
    (set-mark end)
    (goto-char beg)))

(defun comment-line ()
  (interactive)
  (with-bounds 'line
    (comment-region beg end)))

(defun uncomment-line ()
  (interactive)
  (with-bounds 'line
    (uncomment-region beg end)))

(defun comment-or-uncomment-line ()
  (interactive)
  (with-bounds 'line
    (comment-or-uncomment-region beg end)))

(defun kill-whole-line ()
  (interactive)
  (with-bounds 'line
    (kill-region beg end)))

(defun save-line ()
  (interactive)
  (kill-ring-save (point) (line-end-position)))

(defun kill-line-append-to-file (filename)
  (interactive "fFile: ")
  (with-bounds 'line
    (kill-region-append-to-file beg end filename)))

(defun duplicate-line ()
  (interactive)
  (with-bounds 'line
    (duplicate-region beg end)))

(defun duplicate-and-comment-line ()
  (interactive)
  (with-bounds 'line
    (duplicate-and-comment-region beg end)))

;; paragraph region

(defun comment-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (comment-region beg end)))

(defun uncomment-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (uncomment-region beg end)))

(defun comment-or-uncomment-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (comment-or-uncomment-region beg end)))

(defun kill-whole-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (kill-region beg end)))

(defun save-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (kill-ring-save beg end)))

(defun kill-paragraph-append-to-file (filename)
  (interactive "fFile: ")
  (with-bounds 'paragraph
    (kill-region-append-to-file beg end filename)))

(defun duplicate-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (duplicate-region beg end)))

(defun duplicate-and-comment-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (duplicate-and-comment-region beg end)))

(defun indent-paragraph ()
  (interactive)
  (with-bounds 'paragraph
    (indent-region beg end)))

(defun paragraph-align-regexp (regexp)
  (interactive "sRegexp: ")
  (with-bounds 'paragraph
    (align-regexp beg end (concat "\\(\\s-*\\)" regexp) 1 1 nil)
    (untabify beg end)))

;; defun region

(defun comment-defun ()
  (interactive)
  (with-bounds 'defun
    (comment-region beg end)))

(defun kill-defun ()
  (interactive)
  (with-bounds 'defun
    (kill-region beg end)))

(defun save-defun ()
  (interactive)
  (with-bounds 'defun
    (kill-ring-save beg end)))

(defun kill-defun-append-to-file (filename)
  (interactive "fFile: ")
  (with-bounds 'defun
    (kill-region-append-to-file beg end filename)))

(defun duplicate-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (call-interactively 'duplicate-region)))

(defun duplicate-and-comment-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (call-interactively 'duplicate-and-comment-region)))

(defun indent-defun ()
  (interactive)
  (with-bounds 'defun
    (indent-region beg end)))

;; buffer/file operations

(defun buffer-major-mode (buffer-or-name)
  (with-current-buffer buffer-or-name
    major-mode))

(defun get-buffers-by-mode (mode)
  (filter (lambda (buf) (eq (buffer-major-mode buf) mode))
          (buffer-list)))

(defun kill-buffers-by-mode (mode)
  (interactive "SMode: ")
  (mapc 'kill-buffer (get-buffers-by-mode mode)))

(defun delete-this-file ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (or file-name (error "Buffer is not visiting a file."))
    (when (yes-or-no-p "Really delete this file?")
      (delete-file file-name)
      (kill-this-buffer))))

;; windows operations

(defun inc-window-height (&optional inc)
  (interactive "P")
  (enlarge-window (or inc 1)))

(defun dec-window-height (&optional dec)
  (interactive "P")
  (enlarge-window (- (or dec 1))))

(defun inc-window-width (&optional inc)
  (interactive "P")
  (enlarge-window (or inc 1) t))

(defun dec-window-width (&optional dec)
  (interactive "P")
  (enlarge-window (- (or dec 1)) t))

(defun move-to-window-tenths (n)
  (interactive)
  (move-to-window-line (round (* (window-body-height) (/ n 10.0)))))

;; frame operations

(defun refresh-frame ()
  (interactive)
  (redraw-frame (selected-frame)))

;; misc

(defun sort-buffer-list (buffer-list &optional descending)
  "Alphabetically sorts BUFFER-LIST desctructively, in ascending
order unless DESCENDING is non-nil."
  (sort buffer-list
        (lambda (b1 b2) (let ((res (string< (buffer-name b2) (buffer-name b1))))
                     (if descending res (not res))))))

(defun remove-elc ()
  (let ((elc (concat (buffer-file-name) "c")))
    (and (file-exists-p elc) (delete-file elc))))

(defun inhibit-backup-of-buffer ()
  (interactive)
  (set (make-local-variable 'backup-inhibited) t)
  (message "Backup for this buffer is inhibited."))

(defun switch-to-help-buffer ()
  (interactive)
  (switch-to-buffer (help-buffer)))

(defun get-atoms (pred)
  (let (atoms)
    (mapatoms (lambda (sym)
                (and (funcall pred sym)
                     (push sym atoms))))
    atoms))

(defun totd ()
  (interactive)
  (random t)
  (let ((fn (random-elt (get-atoms 'commandp)))
        (help-window-select nil))
    (describe-function fn)
    fn))

(defun command-enable (command)
  (put command 'disabled nil))

(defun add-hooks (hook &rest fns)
  (mapc (lambda (fn) (add-hook hook fn)) fns))

(defun fill-keymap (keymap &rest binds)
  (dolist (bind (pair binds) keymap)
    (define-key keymap (read-kbd-macro (car bind)) (cadr bind))))

(defmacro defkeymap (name &rest binds)
  `(setq ,name (fill-keymap (make-sparse-keymap) ,@binds)))

(defun fill-registers (type &rest regs)
  (mapc (lambda (reg) (set-register (car reg) `(,type . ,(cadr reg)))) (pair regs)))

(defun view-url-source ()
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun message-point ()
  (interactive)
  (message (format "%s" (point))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun yow-comment ()
  (format ";; %s\n\n" (replace-regexp-in-string "\n" " " (yow))))

(defun get-scratch-buffer ()
  (interactive)
  (aif (get-buffer "*scratch*")
       (switch-to-buffer it)
       (progn (switch-to-buffer "*scratch*")
              (insert (yow-comment))
              (funcall initial-major-mode))))

(defun inc-transparency (num &optional frame)
  (interactive "P")
  (dbind (sym a1 a2) (assoc 'alpha (frame-parameters frame))
    (let ((new (max 0 (min 100 (+ a1 (or num 1))))))
      (set-frame-parameter frame 'alpha (list new a2)))))

(defun dec-transparency (num &optional frame)
  (interactive "P")
  (inc-transparency (- (or num 1)) frame))

(defun current-info ()
  (interactive)
  (message "%s  |  %s" (format-time-string "%H:%M:%S %A, %B %d %Y") (battery)))

(defun mbq-symbol-at-point (prompt)
  (list (read-from-minibuffer prompt (thing-at-point 'symbol))))

(defun slider (val &optional len knob-str)
  (let* ((len (or len 100))
         (pos (truncate (rescale val 0 100 0 len)))
         (knob-str (or knob-str "O")))
    (format "[%s%s%s]"
            (make-string (max 0 (1- pos)) ?=)
            knob-str
            (make-string (- len pos) ?-))))

(defmacro time-body (&rest body)
  (with-gensyms (old)
    `(let ((,old (current-time)))
       ,@body
       (subtract-time (current-time) ,old))))

;; buffer hook functions

(defun unicode-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun cleanup-buffer-on-save ()
  (add-hook (make-local-variable 'before-save-hook) 'cleanup-buffer))

(defun remove-elc-on-save ()
  (add-hook (make-local-variable 'after-save-hook) 'remove-elc))

;; provide

(provide 'tlh-util)

;;; tlh-util.el ends here
