(defun match-pattern-list (var-name line)
  (string-match (concat var-name ".append" "(\\([^)]+\\))") line)
  (match-string 1 line)
  )

(defun find-list-symbol (start end)
  (let ((line (buffer-substring-no-properties start end)))
    (string-match "\\([^ \[(]+\\)[ ]*=" line)
    (match-string 1 line)
    ))

(defun match-pattern-dict (var-name line)
  ;; still needs key to be sovled
  (interactive)
  (if (string-match (concat var-name ".append" "(\\([^)]+\\))") line)
      (match-string 1 line)
    (string-match (concat var-name "\\[[^=]+" "=[ ]*\\(.+\\)") line)
    (match-string 1 line)))

(defun getvarin (start)
  (save-excursion
    (goto-char start)
    (previous-line)
      ;; perhaps check if symbol occurs between start and end
    (find-list-symbol (line-beginning-position) (line-end-position))))

(defun python-get-indentation-count ()
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match "^\\([ ]+\\)" line)
    (length (match-string 1 line))))

(defun python-comprehend-list ()
  (interactive)
  (search-backward "for" nil t)
  (beginning-of-line)
  (let ((start (point))
        (if? nil)
        (else?  nil)
        (indent-count (python-get-indentation-count)))
    ;;(message (buffer-substring (point) (+ (point) 5)))
    (next-line)
    (while (and (looking-at " ") (not (looking-at "[ ]+if ")))
      (next-line))
    (when (looking-at "[ ]+if ")
      (setq if? (point))
      )
    ;; elif should also say no to if
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
          (dict? nil)
          (list? nil)
          (var-name (getvarin start)))
      (goto-char start)
      (beginning-of-line 0) ;; line back
      (cond
       ((and var-name if? else?)
        (message (concat "as " (s-repeat indent-count "z")))
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = [%s %s %s %s %s]\n"
                                (s-repeat indent-count " ")
                                var-name
                                (get-python-symbol-value var-name if? else?)
                                (get-python-expression if? else?)
                                (get-python-expression else? end)
                                (get-python-symbol-value var-name else? end)
                                (get-python-expression start if?))
                        t (point) end))
       ((and var-name if?)
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = [%s %s %s]\n"
                                (s-repeat indent-count " ")
                                var-name
                                (get-python-symbol-value var-name if? end)
                                (get-python-expression start if?)
                                (get-python-expression if? end))
                        t (point) end))
       (var-name
        (replace-string (buffer-substring-no-properties (point) end)
                        (format "%s%s = [%s %s]\n"
                                (s-repeat indent-count " ")
                                var-name
                                (get-python-symbol-value var-name start end)
                                (get-python-expression start end))
                        t (point) end))
       )))
  (back-to-indentation)
  )

(defun get-python-expression (start end)
  (s-chop-suffix ":" (s-trim (car (split-string (buffer-substring start end) "\n")))))

(defun get-python-symbol-value (var-name start end)
  (match-pattern-list var-name (buffer-substring start end)))

(provide 'python-comprehension)
