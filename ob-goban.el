;;; ob-goban.el --- org-babel functions for goban evaluation

;; Copyright (C) Fungus Humungus

;; Author: Fungus Humungus
;; Keywords: Go
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'cl)

;; define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("goban" . "tmp"))

(defstruct goban-board
  "The details about the board to draw"
  board
  start-colour
  at-top
  at-bottom
  at-right
  at-left)

(defconst goban-margins 23)
(defconst goban-grid-size 23)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:goban '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:goban' function below.
(defun org-babel-expand-body:goban (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  ;;(require 'inf-goban)
  (print params)
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (if (consp pair)
            (format "%s=%S"
                    (car pair)
                    (org-babel-goban-var-to-goban (cdr pair)))
          (format "%s="
                  pair)))
      vars "\n") "\n" body "\n")))


(defun goban-parse-settings (line)
  (if (string-match line "B")
      'black
    'white))

(defun goban-parse-board (board)

  (let* ((lines (mapcar (lambda (row)
                          (replace-regexp-in-string "^\\$\\$ ?" "" row))
                        (remove-if-not (lambda (row) (string-match "^\\$\\$" row))
                                       (split-string board "\n"))))
         (settings (goban-parse-settings (first lines)))
         (rows (rest lines))
         (at-top (string-match "-" (first rows)))
         (at-bottom (string-match "-" (first (last rows))))
         (at-left (string-match "^|" (if at-top (second rows) (first rows))))
         (at-right (string-match "|$" (if at-top (second rows) (first rows))))
         (game (mapcar (lambda (row)
                         (replace-regexp-in-string " ?| ?" "" row))
                       (remove-if (lambda (row) (string-match "-" row)) rows))))
    (make-goban-board
     :board (mapcar (lambda (row) (split-string row " "))
                    game)
     :start-colour settings
     :at-top at-top
     :at-bottom at-bottom
     :at-left at-left
     :at-right at-right)))

(defun goban-num-rows (board)
  "The number of rows on the board"
  (length (goban-board-board board)))

(defun goban-num-cols (board)
  "The number of colums is taken as the length of the first row"
  (length (first (goban-board-board board))))

(defun goban-board-size (cols &optional include-margin)
  (+ (if include-margin
         goban-margins
       0)
     (* goban-grid-size cols)))

(defun goban-svg (cols rows board)
  (format "<svg version='1.1'
baseProfile='full'
width='%d'
height='%d'
xmlns='http://www.w3.org/2000/svg'>%s</svg>"
          (+ (* 2 goban-margins) (goban-board-size cols))
          (+ (* 2 goban-margins) (goban-board-size rows))
          board))

(defun goban-board (board)
  (let ((cols (goban-num-cols board))
        (rows (goban-num-rows board)))
    (concat
     (format "<rect x='%d' y='%d' width='%d' height='%d' fill='white' />"
             0 0 (goban-board-size cols t) (goban-board-size rows t))
     (mapconcat (lambda (col)
                  (format "<line x1='%d' x2='%d' y1='%d' y2='%d' stroke='black' />"
                          (+ (goban-board-size col) goban-margins)
                          (+ (goban-board-size col) goban-margins)
                          (if (goban-board-at-top board) goban-margins 0)
                          (goban-board-size rows (not (goban-board-at-bottom board)))))
                (number-sequence 0 (1- cols))
                "")
     (mapconcat (lambda (row)
                  (format "<line x1='%d' x2='%d' y1='%d' y2='%d' stroke='black' />"
                          (if (goban-board-at-left board) goban-margins 0)
                          (goban-board-size cols (not (goban-board-at-right board)))
                          (+ (goban-board-size row) goban-margins)
                          (+ (goban-board-size row) goban-margins)))
                (number-sequence 0 (1- rows))
                ""))))

(defun goban-stone (col row colour)
  (format "<circle cx='%d' cy='%d' r='%d' stroke='black' fill='%s' />"
          (+ (goban-board-size col) goban-margins)
          (+ (goban-board-size row) goban-margins)
          (/ goban-grid-size 2)
          colour))

(defun goban-clear-background (col row)
  "Clears the background of the point so the marker is clearer."
  (format "<circle cx='%d' cy='%d' r='%d' stroke='white' fill='white' />"
          (+ (goban-board-size col) goban-margins)
          (+ (goban-board-size row) goban-margins)
          (/ goban-grid-size 2)))

(defun goban-markers (col row letter)
  (concat
   (goban-clear-background col row)
   (format "<text x='%d' y='%d' font-family='verdana' font-size='24'>%s</text>"
           (+ (goban-board-size col) (- goban-margins 8))
           (+ (goban-board-size row) (+ goban-margins 8))
           letter)))

(defun goban-other-colour (number)
  (if (eq number 'black) 'white 'black))

(defun goban-colour-of-number (starting-colour number)
  (if (evenp number)
      (goban-other-colour starting-colour)
    starting-colour))

(defun goban-numbered-stone (col row starting-colour number)
  "Draw a numbered stone"
  (let ((colour (goban-colour-of-number starting-colour number)))
    (concat
     (goban-stone col row colour)
     (format "<text x='%d' y='%d' font-family='verdana' font-size='%d' stroke='%s' fill='%s'>%s</text>"
             (if (> number 9)
                 (+ (goban-board-size col) (- goban-margins 7))
                 (+ (goban-board-size col) (- goban-margins 5)))
             (+ (goban-board-size row) (+ goban-margins 6))
             (if (> number 9) 12 15)
             (goban-other-colour colour)
             (goban-other-colour colour)
             number))))

(defun goban-triangle (col row colour filled)
  (format "<polygon points='%d %d, %d %d, %d %d' stroke='%s' fill='%s' />"
          (+ (goban-board-size col) (- goban-margins 8))
          (+ (goban-board-size row) (+ goban-margins 6))
          (+ (goban-board-size col) (+ goban-margins 8))
          (+ (goban-board-size row) (+ goban-margins 6))
          (+ (goban-board-size col) (+ goban-margins 0))
          (+ (goban-board-size row) (- goban-margins 10))
          colour
          (if filled colour (goban-other-colour colour))))


(defun goban-star-point (col row)
  "Draw the star point"
  (format "<rect x='%d' y='%d' width='6' height='6' />"
          (+ (goban-board-size col) (- goban-margins 3))
          (+ (goban-board-size row) (- goban-margins 3))))

(defun goban-stones (board)
  "Draw the stones and other markers."
  (mapconcat
   (lambda (col)
     (mapconcat
      (lambda (row)
        (let ((at-pos (nth col (nth row (goban-board-board board)))))
          (setf case-fold-search nil)

          (concat
           (cond ((string-match "[XY]" at-pos)
                  (goban-stone col row 'black))
                 ((string-match "[OQ]" at-pos)
                  (goban-stone col row 'white))
                 ((string-match "[a-z]" at-pos)
                  (goban-markers col row (match-string 0 at-pos)))
                 ((string-match "[0-9]+" at-pos)
                  (goban-numbered-stone col row
                                        (goban-board-start-colour board)
                                        (string-to-number
                                         (match-string 0 at-pos))))
                 ((string-match "T" at-pos)
                  (concat (goban-clear-background col row)
                          (goban-triangle col row 'black t)))
                 ((string-match "," at-pos)
                  (goban-star-point col row)))

           (cond ((string-match "Q" at-pos)
                  (goban-triangle col row 'black nil))
                 ((string-match "Y" at-pos)
                  (goban-triangle col row 'white nil)))


           )))

      (number-sequence 0 (1- (goban-num-rows board))) ""))
   (number-sequence 0 (1- (goban-num-cols board)))
   ""))

(defun org-babel-execute:goban (body params)
  "Execute a block of Goban code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Goban source code block")
  (let ((out-file (cdr (or (assoc :file params)
                           (error "You need to specify a :file parameter"))))
        (board (goban-parse-board body)))
    (with-temp-file out-file
      (insert (goban-svg (goban-num-cols board)
                         (goban-num-rows board)
                         (concat
                          (goban-board board)
                          (goban-stones board))))))
  nil)

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:goban (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-goban-var-to-goban (var)
  "Convert an elisp var into a string of goban source code
specifying a var of the same value."
  (message "org-babel-goban-var-to-goban")
  (format "%S" var))

(defun org-babel-goban-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-goban-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-goban)
;;; ob-goban.el ends here
