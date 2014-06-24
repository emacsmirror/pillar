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

(defun p2l--convert-command-once (latex num pillar &optional newline)
  "Convert next LATEX NUM argument command to PILLAR regex.
NUM is the arity of the LATEX command.  If NEWLINE is t, make
sure PILLAR starts its own line."
  (when
      (re-search-forward
       ;; make sure we match exactly the desired command, not one with
       ;; a correct prefix
       (concat "\\\\"
               latex
               (apply 'concat (make-list num "{\\([^}]*\\)}"))
               "\\([^[:alnum:]]\\|$\\)") nil t)
    (replace-match pillar)
    (when (and newline (not (zerop (current-column))))
      (open-line 1))
    (when (looking-at "{}")
      (replace-match ""))
    t))

(defun p2l--convert-command (latex num pillar &optional newline)
  "Convert LATEX NUM argument commands to PILLAR regex.
NUM is the arity of the LATEX command.  If NEWLINE is t, make
sure PILLAR starts its own line."
  (p2l--setup-buffer)
  (while (p2l--convert-command-once latex num pillar newline)))

(defconst p2l--command-conversion-table
  '(("ie" 0 "''i.e.'',")
    ("eg" 0 "''e.g.'',")
    ("etc" 0 "etc.")
    ("noindent" 0 "")
    ("pharo" 0 "Pharo")
    ("st" 0 "Smalltalk")
    ("dc" 1 "")
    ("clsindexmain" 1 "")
    ("chapter" 1 "!\\1\n" t)
    ("section" 1 "!!\\1\n" t)
    ("subsection" 1 "!!!\\1\n" t)
    ("subsubsection" 1 "!!!!\\1\n" t)
    ("paragraph" 1 "!!!!!\\1\n" t)
    ("chalabel" 1 "@cha:\\1\n" t)
    ("seclabel" 1 "@sec:\\1\n" t)
    ("ref" 1 "*\\1*")
    ("clsind" 1 "==\\1==")
    ("figref" 1 "Figure *fig:\\1*")
    ("charef" 1 "Chapter *cha:\\1*")
    ("secref" 1 "Section *sec:\\1*")
    ("figref" 1 "Figure *fig:\\1*")
    ("Figref" 1 "Figure *fig:\\1*")
    ("appref" 1 "Appendix *app:\\1*")
    ("tabref" 1 "Table *tab:\\1*")
    ("faqref" 1 "FAQ *faq:\\1*")
    ("ct" 1 "==\\1==")
    ("lct" 1 "==\\1==")
    ("emph" 1 "''\\1''")
    ("textbf" 1 "\"\"\\1\"\"")
    ("apl" 1 "")
    ("ab" 1 "")
    ("sd" 1 "")
    ("dc" 1 "")
    ("md" 1 "")
    ("on" 1 "")
    ("damien" 1 "")
    ("lr" 1 "")
    ("orla" 1 "")
    ("alex" 1 "")
    ("alx" 1 "")
    ("dr" 1 "")
    ("ja" 1 "")
    ("jr" 1 "")
    ("jb" 1 "")
    ("fp" 1 "")
    ("michael" 1 "")
    ("ew" 1 "")
    ("mb" 1 "")
    ("hw" 1 "")
    ("ben" 1 "")
    ("hjo" 1 "")
    ("ml" 1 "")
    ("clsindexmain" 1 "")
    ("index" 1 "")
    ("needlines" 1 "")
    ("ind" 1 "\\1")
    ("mthind" 2 "==\\1>>\\2==")
    ("cmindex" 2 "")
    ("emphsubind" 2 "''\\2''")))

(defun p2l--interpret-command-conversion-table ()
  "Convert all LaTeX commands."
  (dolist (conversion p2l--command-conversion-table)
    (apply #'p2l--convert-command conversion)))

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
  (let (before-begin after-end)
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
  (p2l-remove-header)
  (p2l-remove-footer)
  (p2l--interpret-command-conversion-table)
  (p2l-convert-list)
  (p2l-convert-figure)
  (p2l-convert-code)
  (p2l-convert-double-quotes))

(provide 'pillar-latex2pillar)

;;; pillar-latex2pillar.el ends here

;;  LocalWords:  arg eg
