;;; pillar-latex2pillar.el --- Help converting LaTeX files to Pillar   -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Version: 0.1
;; Keywords: markup major-mode latex
;; URL: http://github.com/DamienCassou/pillar-mode
;;
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Utility functions to convert LaTeX files to Pillar ones

;;; Code:

(defconst p2l-editor-commands
  '("apl" "ab" "sd" "dc" "md" "on" "damien" "lr" "orla" "alex" "alx" "dr" "ja" "jr" "jb" "fp" "michael" "ew" "mb" "hw" "ben" "hjo" "ml"))

(defconst p2l-misc-commands-to-remove
  '("clsindexmain" "needlines"))

(defun p2l--setup-buffer ()
  "Prepare the buffer for conversion."
  (goto-char (point-min))
  (fundamental-mode)
  (font-lock-mode -1))

(defun p2l-remove-latex-comments ()
  "Remove all lines that start with %."
  (interactive)
  (p2l--setup-buffer)
  (delete-matching-lines "^%.*$"))

(defun p2l-remove-useless-commands ()
  "Delete some LaTeX command that do not affect output."
  (interactive)
  (p2l--setup-buffer)
  (while (re-search-forward
          (concat "\\\\"
                  (regexp-opt
                   (append p2l-editor-commands
                           p2l-misc-commands-to-remove))
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
  "Remove everything before \chapter{...}."
  (interactive)
  (p2l--setup-buffer)
  (let ((start (point)))
    (when (search-forward "\\chapter{" nil t)
      (beginning-of-line)
      (delete-region start (point)))))

(defun p2l-remove-footer ()
  "Remove the useless end of the file."
  (interactive)
  (goto-char (point-max))
  (let ((end (point)))
    (when (search-backward "\\ifx\\wholebook\\relax" nil t)
     (beginning-of-line)
     (delete-region (point) end))))

(defun p2l--convert-command0-once (latex pillar &optional newline)
  "Transform the next LATEX 0-arg command to PILLAR.
LATEX is the name of a 0-argument macro (e.g., \eg).  PILLAR is
the Pillar markup to replace the macro with.  NEWLINE, if t, will
make sure the replacement text will start on a newline."
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
  "Transform all LATEX 0-arg commands to PILLAR.
LATEX is the name of a 0-argument command (e.g., \eg).  PILLAR is
the Pillar markup to replace the command with.  NEWLINE, if t, will
make sure the replacement text will start on a newline."
  (p2l--setup-buffer)
  (while (p2l--convert-command0-once latex pillar newline)))

(defun p2l--convert-command1-once (latex pillar-begin &optional pillar-end newline)
  "Transform the next LATEX 1-arg command.
LATEX is the name of a 0-argument command (e.g., \eg).
PILLAR-BEGIN is the Pillar markup to replace the command name with.
PILLAR-END, if provided, is the Pillar markup that will be
written after the command's first argument.  NEWLINE, if t, will
make sure the replacement text will start on a newline."
  (when (p2l--convert-command0-once latex pillar-begin newline)
    (let ((start (point))
          end)
      (forward-sexp)
      (delete-char -1)
      (insert pillar-end)
      (setq end (point))
      (goto-char start)
      (delete-forward-char 1)
      (goto-char (1- end))
      t)))

(defun p2l--convert-command1 (latex pillar-begin &optional pillar-end newline)
  "Transform all LATEX 1-arg commands.
LATEX is the name of a 0-argument command (e.g., \eg).
PILLAR-BEGIN is the Pillar markup to replace the command name with.
PILLAR-END, if provided, is the Pillar markup that will be
written after the command's first argument.  NEWLINE, if t, will
make sure the replacement text will start on a newline."
  (p2l--setup-buffer)
  (while (p2l--convert-command1-once latex pillar-begin pillar-end newline)))

(defun p2l--convert-command2-once (latex pillar-begin pillar-middle &optional pillar-end newline)
  "Transform the next LATEX 2-arg command.
LATEX is the name of a 0-argument command (e.g., \eg).
PILLAR-BEGIN is the Pillar markup to replace the command name with.
PILLAR-MIDDLE, is the Pillar markup that will be written between
the command's first and second arguments.  PILLAR-END, if provided,
is the Pillar markup that will be written after the command's second
argument.  NEWLINE, if t, will make sure the replacement text
will start on a newline."
  (when (p2l--convert-command1-once latex pillar-begin pillar-middle newline)
    (let ((start (point)))
      (forward-sexp)
      (delete-char -1)
      (insert pillar-end)
      (goto-char start)
      (delete-forward-char 1)
      t)))

(defun p2l--convert-command2 (latex pillar-begin pillar-middle &optional pillar-end newline)
  "Transform all LATEX 2-arg commands.
LATEX is the name of a 0-argument command (e.g., \eg).
PILLAR-BEGIN is the Pillar markup to replace the command name with.
PILLAR-MIDDLE, is the Pillar markup that will be written between
the command's first and second arguments.  PILLAR-END, if provided,
is the Pillar markup that will be written after the command's second
argument.  NEWLINE, if t, will make sure the replacement text
will start on a newline."
  (p2l--setup-buffer)
  (while (p2l--convert-command2-once latex pillar-begin pillar-middle pillar-end newline)))

(defconst p2l--command0-conversion-table
  '(("ie" "''i.e.'',")
    ("eg" "''e.g.'',")
    ("etc" "etc.")))

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
    ("lct" "==" "==")
    ("emph" "''" "''")))

(defconst p2l--command2-conversion-table
  '(("mthind" "==" ">>" "==")))

(defun p2l--interpret-command0-conversion-table ()
  "Convert all LaTeX 0-arg commands."
  (dolist (conversion p2l--command0-conversion-table)
    (apply #'p2l--convert-command0 conversion)))

(defun p2l--interpret-command1-conversion-table ()
  "Convert all LaTeX 1-arg commands."
  (dolist (conversion p2l--command1-conversion-table)
    (apply #'p2l--convert-command1 conversion)))

(defun p2l--interpret-command2-conversion-table ()
  "Convert all LaTeX 2-arg commands."
  (dolist (conversion p2l--command2-conversion-table)
    (apply #'p2l--convert-command2 conversion)))

(defun p2l--delete-all-spaces ()
  "Remove all spaces around point.
Does *not* delete newline characters."
  (just-one-space 0))

(defun p2l-convert-list-once ()
  "Convert the next list (e.g, itemize or description)."
  (let (before-begin after-end matched-env description-p)
    (when (re-search-forward "^ *\\\\begin{\\(itemize\\|enumerate\\|description\\)}" nil t)
      (setq before-begin (match-beginning 0))
      (setq matched-env (match-string 1))
      (re-search-forward (concat "^ *\\\\end{" matched-env "}"))
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
          (let ((fill-paragraph-function nil)
                (fill-column 1000000000))
            (while (re-search-forward "^[ \t]*\n" nil t)
              (replace-match ""))
            (fill-paragraph))
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
              (delete-char -1) ;; ]
              (p2l--delete-all-spaces)
              (newline 1)
              (insert ":")))
          (goto-char (point-min))
          (delete-blank-lines)))
      t)))

(defun p2l-convert-list ()
  "Convert all lists (e.g, itemize or description)."
  (p2l--setup-buffer)
  (while (p2l-convert-list-once)))

(defun p2l-convert-figure-once ()
  "Convert the next figure."
  (let (before-begin after-end file caption label)
    (when (re-search-forward "^ *\\\\begin{figure}" nil t)
      (setq before-begin (match-beginning 0))
      (re-search-forward (concat "^ *\\\\end{figure}"))
      (setq after-end (match-end 0))
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
          (when
              (re-search-forward "\\\\includegraphics\\[[^]]*\\]{\\([^}]*\\)}" nil t)
            (setq file (match-string 1))
            (goto-char (point-min))
            (when (re-search-forward "\\\\caption{\\([^}]*\\)}" nil t)
              (setq caption (match-string 1))
              (when (re-search-forward "\\\\figlabel{\\([^}]*\\)}" nil t)
                (setq label (match-string 1))
                (delete-region (point-min) (point-max))
                (insert (format "+%s>file://figures/%s.png|label=fig:%s+"
                                caption file label))
                t))))))))

(defun p2l-convert-figure ()
  "Convert all figures."
  (p2l--setup-buffer)
  (while (p2l-convert-figure-once)))

(defun p2l-convert-code-once ()
  "Convert the next code block."
  (let (before-begin after-end file caption label)
    (when (re-search-forward "^ *\\\\begin{code}" nil t)
      (setq before-begin (match-beginning 0))
      (re-search-forward (concat "^ *\\\\end{code}"))
      (setq after-end (match-end 0))
      (save-excursion
        (save-restriction
          (narrow-to-region before-begin after-end)
          (goto-char (point-min))
          (kill-line)
          (insert "[[[")
          (goto-char (point-max))
          (beginning-of-line)
          (kill-line)
          (insert "]]]")
          t)))))

(defun p2l-convert-code ()
  "Convert all code blocks."
  (p2l--setup-buffer)
  (while (p2l-convert-code-once)))

(defun p2l-convert-double-quotes-once ()
  "Convert the next ``such LaTeX'' to use Pillar emphasis."
  (when (re-search-forward "``\\(.*?\\)''" nil t)
    (replace-match "''\\1''")
    t))

(defun p2l-convert-double-quotes ()
  "Convert all ``such LaTeX'' to use Pillar emphasis."
  (p2l--setup-buffer)
  (while (p2l-convert-double-quotes-once)))

(defun p2l-convert-buffer ()
  "Apply all LaTeX to Pillar conversions to the buffer."
  (interactive)
  (p2l--setup-buffer)
  (p2l-remove-latex-comments)
  (p2l-remove-useless-commands)
  (p2l-remove-header)
  (p2l-remove-footer)
  (p2l--interpret-command0-conversion-table)
  (p2l--interpret-command1-conversion-table)
  (p2l--interpret-command2-conversion-table)
  (p2l-convert-list)
  (p2l-convert-figure)
  (p2l-convert-code)
  (p2l-convert-double-quotes))

(provide 'pillar-latex2pillar)

;;; pillar-latex2pillar.el ends here

;;  LocalWords:  arg eg
