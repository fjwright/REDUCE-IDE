;;; reduce-dired.el --- Run a file in REDUCE from a dired buffer -*- lexical-binding:t -*-

;; Copyright (C) 2025 Francis J. Wright

;; Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
;; Created: June 2025
;; Time-stamp: <2025-06-11 16:29:42 franc>
;; Homepage: https://reduce-algebra.sourceforge.io/reduce-ide/

;; This file is part of REDUCE IDE.

;; REDUCE IDE is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; REDUCE IDE is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with REDUCE IDE.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Load this file to provide commands in a Dired buffer to run the
;; current file as a REDUCE program in a new process buffer.  Query
;; the user whether to echo the file contents.  Start a new REDUCE
;; process named from the filename and input the file.

;; To automate loading this file, add

;;   (with-eval-after-load 'dired (require 'reduce-dired))

;; to your .emacs file.

;;; Code:

;; The key "r" is conveniently unbound in Dired mode, so...
(keymap-set dired-mode-map "r" #'dired-do-reduce-run-file)
;; Add the same binding as used in REDUCE mode:
(keymap-set dired-mode-map "C-c C-M-f" #'dired-do-reduce-run-file)

;; Add an item at the bottom of the Dired Immediate menu:
(keymap-set-after dired-mode-map
  "<menu-bar> <immediate> <run-in-reduce-sep>"
  '(menu-item "--"))
(keymap-set-after dired-mode-map
  "<menu-bar> <immediate> <run-in-reduce>"
  '(menu-item
    "Run This File in REDUCE" dired-do-reduce-run-file
    :help "Run REDUCE source file at cursor in a new REDUCE process"))

(defun dired-do-reduce-run-file (echo)
  "In Dired, run this file as a REDUCE program.
Echo the file contents if ECHO is non-nil.  Start a new REDUCE process
named from this filename and input this file."
  (interactive (list (y-or-n-p "Echo file input? "))
               dired-mode)
  ;; ‘reduce-run-file’ is autoloaded.
  (reduce-run-file (dired-get-file-for-visit) echo))

(provide 'reduce-dired)

;;; reduce-dired.el ends here
