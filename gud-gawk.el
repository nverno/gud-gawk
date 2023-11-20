;;; gud-gawk.el --- GUD support for gawk debugger -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/gud-gawk
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Created: 20 November 2023
;; Keywords: tools, languages, debugging, gud, awk

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; GUD support for gawk debugger (gawk -D).
;;
;;; Code:

(require 'gud)

(defcustom gud-gawk-command-name "gawk -D -f"
  "Command that executes the gawk debugger."
  :type 'string
  :group 'gud)

(defcustom gud-gawk-prompt-regexp "^gawk> *"
  "Regexp to match gawk debugger prompt."
  :type 'string
  :group 'gud)

(defvar gud-gawk-marker-regexp
  (concat
   "^\\(?:Breakpoint [^\n]+ at `\\([[:graph:]]+\\)':\\([0-9]+\\)\\|"
   "\\(?:Stopping in Rule \\.\\.\\.\\|nexti?\\|stepi?\\|[ns]\\)[ \t]*"
   "\n\\([0-9]+\\)[^\n]*"
   "\\)"))

(defvar gud-gawk-marker-regexp-start "^\\(?:Breakpoint [0-9]+,\\)")

(defvar-local gud-gawk-marker-acc "")
(defvar-local gud-gawk-current-file nil)

;; Notes from gud.el
;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-gawk-marker-filter (string)
  (setq gud-gawk-marker-acc (concat gud-gawk-marker-acc string))
  (let ((output ""))
    ;; Process all the complete markers in this chunk.
    (while (string-match gud-gawk-marker-regexp gud-gawk-marker-acc)
      (let (file line)
        (cond
         ((setq file (match-string 1 gud-gawk-marker-acc))
          (setq line (string-to-number (match-string 2 gud-gawk-marker-acc))))
         ((match-string 3 gud-gawk-marker-acc)
          (setq line (string-to-number (match-string 3 gud-gawk-marker-acc))
                file gud-gawk-current-file)))
        (setq gud-gawk-current-file file)
        ;; Extract the frame position from the marker.
        (setq gud-last-frame (cons file line)
              ;; Output everything instead of the below
              output (concat output (substring gud-gawk-marker-acc 0 (match-end 0)))
              ;; Set the accumulator to the remaining text.
              gud-gawk-marker-acc (substring gud-gawk-marker-acc (match-end 0)))))
    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-gawk-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match gud-gawk-marker-regexp-start gud-gawk-marker-acc)
        ;; Everything before the potential marker start can be output.
        (setq output (concat output (substring gud-gawk-marker-acc 0 (match-beginning 0)))
              ;; Everything after, we save, to combine with later input.
              gud-gawk-marker-acc (substring gud-gawk-marker-acc (match-beginning 0)))

      (setq output (concat output gud-gawk-marker-acc)
            gud-gawk-marker-acc ""))

    output))

;; Each entry must define the following at startup:
;;
;;<name>
;; comint-prompt-regexp
;; gud-<name>-massage-args
;; gud-<name>-marker-filter
;; gud-<name>-find-file
;;
;; The job of the massage-args method is to modify the given list of
;; debugger arguments before running the debugger.
;;
;; The job of the marker-filter method is to detect file/line markers in
;; strings and set the global gud-last-frame to indicate what display
;; action (if any) should be triggered by the marker.  Note that only
;; whatever the method *returns* is displayed in the buffer; thus, you
;; can filter the debugger's output, interpreting some and passing on
;; the rest.
;;
;; The job of the find-file method is to visit and return the buffer indicated
;; by the car of gud-tag-frame.  This may be a file name, a tag name, or
;; something else.

;; Byte-compiler complains about unknown functions
(eval-when-compile
  (defmacro gud-gawk--declare (&rest fns)
    (macroexp-progn
     `(,@(mapcar (lambda (fn)
                   `(declare-function
                     ,(intern (format "gud-%s" (symbol-name fn))) "gud" (arg)))
                 fns))))
  (gud-gawk--declare
   break tbreak remove step stepi next nexti cont finish up down
   print run statement until))

;;;###autoload
(defun gud-gawk (command-line)
  "Run gawk passing COMMAND-LINE as arguments.
The program is run with command line COMMAND-LINE in buffer named `*gud-FILE*'.

The directory containing the awk program becomes the initial working
directory and source-file directory for your debugger.

For general information about commands available to control the debugging
process from gud, see `gud-mode'."
  (interactive (list (gud-query-cmdline 'gawk)))

  (gud-common-init command-line nil 'gud-gawk-marker-filter)
  (setq-local gud-minor-mode 'gud-gawk)

  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line."           )
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t" "Set temporary breakpoint at current line." )
  (gud-def gud-remove "clear %f:%l"  "\C-d" "Remove breakpoint at current line"         )
  (gud-def gud-step   "step %p"      "\C-s" "Step source line with display."            )
  (gud-def gud-stepi  "stepi %p"     nil    "Step one instruction exactly."             )
  (gud-def gud-next   "next %p"      "\C-n" "Step program (skip subroutines)."          )
  (gud-def gud-nexti  "nexti %p"     nil    "Step one instruction (skip subroutines)."  )
  (gud-def gud-cont   "continue"     "\C-r" "Continue with display."                    )
  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function."        )
  (gud-def gud-up     "up %p"        "<"    "Up N stack frames (numeric arg)."          )
  (gud-def gud-down   "down %p"      ">"    "Down N stack frames (numeric arg)."        )
  (gud-def gud-print  "print %e"     "\C-p" "Print Awk expression at point."            )
  (gud-def gud-run    "run"          nil    "Run the program."                          )
  (gud-def gud-until  "until %l"     "\C-u" "Continue to current line."                 )
  (gud-def gud-statement "eval %e"  "\C-e" "Execute Awk statement at point."          )

  (gud-set-repeat-map-property 'gud-gdb-repeat-map)

  (setq comint-prompt-regexp gud-gawk-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'gud-gawk-mode-hook))

(provide 'gud-gawk)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; gud-gawk.el ends here
