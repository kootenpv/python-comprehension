(require 's)

(defun match-pattern-add-to-container (var-name line)
  (string-match (concat var-name ".\\(append\\|add\\)" "(\\([^)]+\\))") line)
  (match-string 2 line))

(defun python-find-symbol (start end)
  (let ((line (buffer-substring-no-properties start end)))
    (string-match "\\([^ \[(]+\\)[ ]*=" line)
    (match-string 1 line)))

(defun match-pattern-get-assign-dict (var-name line)
  ;; still needs key to be sovled
  (interactive)
  (string-match (concat var-name "\\[\\([^\\]+\\)][ ]*" "=[ ]*\\(.+\\)") line)
  (cons (match-string 1 line) (match-string 2 line)))

(defun match-pattern-get-assign-list (var-name line)
  ;; still needs key to be sovled
  (interactive)
  (cdr (match-pattern-get-assign-dict var-name line)))

(defun python-find-symbol-move (start)
  (save-excursion
    (goto-char start)
    (previous-line)
      ;; perhaps check if symbol occurs between start and end
    (python-find-symbol (line-beginning-position) (line-end-position))))

(defun python-get-indentation-count ()
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match "^\\([ ]+\\)" line)
    (length (match-string 1 line))))

(defun python-test-for-list (start)
  (goto-char start)
  (if (s-contains? "[]" (buffer-substring(line-beginning-position 0) (line-end-position 0)))
      "list"
    "dict/set"))

(defun python-comprehend ()
  (interactive)
  (search-backward "for" nil t)
  (beginning-of-line)
  (let ((start (point))
        (if? nil)
        (else?  nil)
        (indent-count (python-get-indentation-count)))
    (next-line)
    (while (and (looking-at " ") (not (looking-at "[ ]+if ")))
      (next-line))
    (when (looking-at "[ ]+if ")
      (setq if? (point))
      )
    (while (and (not (looking-at "[ ]+else"))
                (not (looking-at "[ ]+elif")) (looking-at " "))
      (next-line))
    (when (looking-at "[ ]+elif")
      (throw "looking-at-elif" "looking-at-elif"))
    (when (looking-at "[ ]+else")
      (setq else? (point)))
    (while (looking-at " ")
      (next-line))
    (let ((end (point))
          (var-name (python-find-symbol-move start))
          (comp-type (python-test-for-list start)))
      (goto-char start)
      (beginning-of-line 0) ;; line back
      (when (string-equal comp-type "list")
        (if (s-contains? ".append" (buffer-substring start end))
            (python-list-comprehension-append var-name start end if? else?)
          (python-list-comprehension-assign var-name start end if? else?)))
      (when (string-equal comp-type "dict/set")
        (if (s-contains? ".add" (buffer-substring start end))
            (python-set-comprehension-add var-name start end if? else?)
          (python-dict-comprehension-assign var-name start end if? else?))))
    (back-to-indentation)))



(defun python-list-comprehension-append (var-name start end if? else?)
  (python-single-comprehension-matcher var-name start end if? else? '("[" . "]") 'match-pattern-add-to-container))

(defun python-list-comprehension-assign (var-name start end if? else?)
  (python-single-comprehension-matcher var-name start end if? else? '("[" . "]") 'match-pattern-get-assign-list))

(defun python-set-comprehension-add (var-name start end if? else?)
  (python-single-comprehension-matcher var-name start end if? else? '("{" . "}") 'match-pattern-add-to-container))

(defun python-dict-comprehension-assign (var-name start end if? else?)
  (python-double-comprehension-matcher var-name start end if? else? '("{" . "}") 'match-pattern-get-assign-dict))



(defun python-single-comprehension-matcher (var-name start end if? else? container-pair matcher)
  (cond
       ((and var-name if? else?)
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = %s%s %s %s %s %s%s\n"
                                (s-repeat indent-count " ")
                                var-name
                                (car container-pair)
                                (get-python-symbol-value var-name if? else? matcher)
                                (get-python-expression if? else?)
                                (get-python-expression else? end)
                                (get-python-symbol-value var-name else? end matcher)
                                (get-python-expression start if?)
                                (cdr container-pair))
                        t (point) end))
       ((and var-name if?)
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = %s%s %s %s%s\n"
                                (s-repeat indent-count " ")
                                var-name
                                (car container-pair)
                                (get-python-symbol-value var-name if? end matcher)
                                (get-python-expression start if?)
                                (get-python-expression if? end)
                                (cdr container-pair))
                        t (point) end))
       (var-name
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = %s%s %s%s\n"
                                (s-repeat indent-count " ")
                                var-name
                                (car container-pair)
                                (get-python-symbol-value var-name start end matcher)
                                (get-python-expression start end)
                                (cdr container-pair))
                        t (point) end))))

(defun python-double-comprehension-matcher (var-name start end if? else? container-pair matcher)
  (cond
       ((and var-name if? else?)
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = %s%s: %s %s %s %s %s%s\n"
                                (s-repeat indent-count " ")
                                var-name
                                (car container-pair)
                                (s-trim (car (get-python-symbol-value var-name if? else? matcher)))
                                (s-trim (cdr (get-python-symbol-value var-name if? else? matcher)))
                                (get-python-expression if? else?)
                                (get-python-expression else? end)
                                (s-trim (cdr (get-python-symbol-value var-name else? end matcher)))
                                (get-python-expression start if?)
                                (cdr container-pair))
                        t (point) end))
       ((and var-name if?)
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = %s%s: %s %s %s%s\n"
                                (s-repeat indent-count " ")
                                var-name
                                (car container-pair)
                                (s-trim (car (get-python-symbol-value var-name if? end matcher)))
                                (s-trim (cdr (get-python-symbol-value var-name if? end matcher)))
                                (get-python-expression start if?)
                                (get-python-expression if? end)
                                (cdr container-pair))
                        t (point) end))
       (var-name
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = %s%s: %s %s%s\n"
                                (s-repeat indent-count " ")
                                var-name
                                (car container-pair)
                                (s-trim (car (get-python-symbol-value var-name start end matcher)))
                                (s-trim (cdr (get-python-symbol-value var-name start end matcher)))
                                (get-python-expression start end)
                                (cdr container-pair))
                        t (point) end))))

(defun get-python-expression (start end)
  (s-chop-suffix ":" (s-trim (car (split-string (buffer-substring start end) "\n")))))

(defun get-python-symbol-value (var-name start end matcher)
  (funcall matcher var-name (buffer-substring start end)))

(provide 'python-comprehension)
