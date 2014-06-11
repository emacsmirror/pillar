(defconst p2l-editor-macros
  '("apl" "ab" "sd" "dc" "md" "on" "damien" "lr" "orla" "alex" "alx" "dr" "ja" "jr" "jb" "fp" "michael" "ew" "mb" "hw" "ben" "hjo" "ml"))

(defconst p2l-misc-macros-to-remove
  '("clsindexmain" "needlines"))

(defun p2l--setup-buffer ()
  (goto-char (point-min))
  (fundamental-mode)
  (font-lock-mode -1))

(defun p2l-remove-latex-comments ()
  (interactive)
  (p2l--setup-buffer)
  (delete-matching-lines "^%.*$"))

(defun p2l-remove-useless-macros ()
  (interactive)
  (p2l--setup-buffer)
  (while (re-search-forward
          (concat "\\\\"
                  (regexp-opt 
                   (append p2l-editor-macros
                           p2l-misc-macros-to-remove))
                  "{")
          nil t)
    (backward-char)
    (let ((before-curly (point))
          start)
      (setq start (match-beginning 0))
      (goto-char before-curly)
      (forward-sexp)
      (delete-region start (point)))))

(defun p2l-remove-header ()
  (interactive)
  (p2l--setup-buffer)
  (let ((start (point)))
    (when (search-forward "\\chapter{" nil t)
      (beginning-of-line)
      (delete-region start (point)))))

(defun p2l-remove-footer ()
  (interactive)
  (goto-char (point-max))
  (let ((end (point)))
    (when (search-backward "\\ifx\\wholebook\\relax" nil t)
     (beginning-of-line)
     (delete-region (point) end))))

(defun p2l--convert-command0-once (latex pillar &optional newline)
  (when (re-search-forward
         (concat "\\\\" latex) nil t)
    (let ((start (match-beginning 0))
          end)
      (replace-match pillar)
      (setq end (point))
      (goto-char start)
      (when (and newline (not (zerop (current-column))))
        (open-line 1)
        (setq end (1+ end)))
      (goto-char end)
      (when (looking-at "{}")
        (replace-match ""))
      t)))

(defun p2l--convert-command0 (latex pillar &optional newline)
  (p2l--setup-buffer)
  (while (p2l--convert-command0-once latex pillar newline)))

(defun p2l--convert-command1-once (latex pillar-begin &optional pillar-end newline)
  (when (p2l--convert-command0-once latex pillar-begin newline)
    (let ((start (point))
          end)
      (forward-sexp)
      (delete-backward-char 1)
      (insert pillar-end)
      (setq end (point))
      (goto-char start)
      (delete-forward-char 1)
      (goto-char (1- end))
      t)))

(defun p2l--convert-command1 (latex pillar-begin &optional pillar-end newline)
  (p2l--setup-buffer)
  (while (p2l--convert-command1-once latex pillar-begin pillar-end newline)))

(defun p2l--convert-command2-once (latex pillar-begin pillar-middle &optional pillar-end newline)
  (when (p2l--convert-command1-once latex pillar-begin pillar-middle newline)
    (let ((start (point)))
      (forward-sexp)
      (delete-backward-char 1)
      (insert pillar-end)
      (goto-char start)
      (delete-forward-char 1)
      t)))

(defun p2l--convert-command2 (latex pillar-begin pillar-middle &optional pillar-end newline)
  (p2l--setup-buffer)
  (while (p2l--convert-command2-once latex pillar-begin pillar-middle pillar-end newline)))

(defconst p2l--command0-conversion-table
  '(("ie" "''i.e.'',")))

(defconst p2l--command1-conversion-table
  '(("chapter" "!" "\n" t)
    ("section" "!!" "\n" t)
    ("subsection" "!!!" "\n" t)
    ("subsubsection" "!!!!" "\n" t)
    ("paragraph" "!!!!!" "\n" t)
    ("chalabel" "@cha:" "\n" t)
    ("seclabel" "@sec:" "\n" t)
    ("clsind" "==" "==")
    ("figref" "*fig:" "*")
    ("ct" "==" "==")
    ("emph" "''" "''")))

(defconst p2l--command2-conversion-table
  '(("mthind" "==" ">>" "==")))

(defun p2l--interpret-command0-conversion-table ()
  (dolist (conversion p2l--command0-conversion-table)
    (apply #'p2l--convert-command0 conversion)))

(defun p2l--interpret-command1-conversion-table ()
  (dolist (conversion p2l--command1-conversion-table)
    (apply #'p2l--convert-command1 conversion)))

(defun p2l--interpret-command2-conversion-table ()
  (dolist (conversion p2l--command2-conversion-table)
    (apply #'p2l--convert-command2 conversion)))

(defun p2l--delete-all-spaces ()
  (just-one-space 0))

(defun p2l-convert-itemize-list-once ()
  (let (before-begin after-end matched-env description-p item-begin)
    (when (re-search-forward "^\\\\begin{\\(itemize\\|enumerate\\|description\\)}" nil t)
      (setq before-begin (match-beginning 0))
      (setq matched-env (match-string 1))
      (re-search-forward (concat "^\\\\end{" matched-env "}"))
      (setq after-end (match-end 0))
      (setq description-p (string= matched-env "description"))
      (save-excursion
        (save-restriction
          (narrow-to-region before-begin after-end)
          (goto-char (point-min))
          (kill-line)
          (delete-blank-lines)
          (goto-char (point-max))
          (beginning-of-line)
          (kill-line)
          (delete-blank-lines)
          (goto-char (point-min))
          (while (re-search-forward "\\\\item" nil t)
            (replace-match (if description-p ";" "-"))
            (p2l--delete-all-spaces)
            (backward-char)
            (looking-back "[[:space:]\n\t]*" nil t)
            (delete-region (match-beginning 0) (match-end 0))
            (open-line 1)
            (when description-p
              (forward-char 2)
              (delete-forward-char 1) ;; [
              (p2l--delete-all-spaces)
              (re-search-forward "]")
              (delete-backward-char 1) ;; ]
              (p2l--delete-all-spaces)
              (newline 1)
              (insert ":")))
          (goto-char (point-min))
          (delete-blank-lines)))
      t)))

(defun p2l-convert-itemize-list ()
  (while (p2l-convert-itemize-list-once)))

(defun p2l-convert-buffer ()
  (interactive)
  (p2l--setup-buffer)  
  (p2l-remove-latex-comments)
  (p2l-remove-useless-macros)
  (p2l-remove-header)
  (p2l-remove-footer)
  (p2l--interpret-command0-conversion-table)
  (p2l--interpret-command1-conversion-table)
  (p2l--interpret-command2-conversion-table)
  (p2l-convert-itemize-list))

(provide 'latex2pillar)
