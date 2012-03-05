;;; anything-ruby-mthds.el --- Quick fuzzy search of Ruby objects methods.

;; This file is not part of Emacs

;; Copyright (C) 2011 Jose Pablo Barrantes
;; Created: 2012-03-05
;; Version: 0.1.0

;;; Installation:

;; Put this file where you defined your `load-path` directory or just
;; add the following line to your emacs config file:

;; (load-file "/path/to/anything-ruby-mthds.el")

;; Finally require it:

;; (require 'anything-ruby-mthds)

;; Usage:
;; M-x anything-ruby-mthds

;; There is no need to setup load-path with add-to-list if you copy
;; `anything-ruby-mthds.el` to load-path directories.

;; Requirements:

;; http://www.emacswiki.org/emacs/Anything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything)

;;; --------------------------------------------------------------------
;;; - Customization
;;;
(defvar *anything-ruby-mthds-buffer-name*
  "*Anything ruby-mthds*")

(defcustom anything-ruby-mthds-object-cmd
  "mthdspool   \
   --object %s \
   --filter %s "
  "Ruby mthds script."
  :group 'anything-ruby-mthds
  :type 'string)

(defun anything-ruby-mthds-init ()
  "mthdspool process."
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (line-number-mode "%l") " "
          (:eval (propertize "(mthdspool pocess running) "
                             'face '((:foreground "red"))))))

  (setq obj (car (split-string anything-pattern "#")))
  (if (equal nil (cdr (split-string anything-pattern "#")))
      (setq mthd "")
    (setq mthd (car (cdr (split-string anything-pattern "#")))))

  (prog1
      (start-process-shell-command
       "anything-mthdspool-process" nil
       (format anything-ruby-mthds-object-cmd obj mthd))
    (set-process-sentinel
     (get-process "anything-mthdspool-process")
     #'(lambda (process event)
         (when (string= event "finished\n")
           (with-anything-window
             (kill-local-variable 'mode-line-format)
             (anything-update-move-first-line)
             (anything-ruby-mthds-fontify obj mthd)
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     (line-number-mode "%l") " "
                     (:eval (propertize
                             (format "[mthdspool Process Finished - (%s results)] "
                                     (let ((nlines (1- (count-lines
                                                        (point-min)
                                                        (point-max)))))
                                       (if (> nlines 0) nlines 0)))
                             'face 'compilation-info-face))))))))))

(defun anything-ruby-mthds-fontify (obj mthd)
  (goto-char 1)
  (while (re-search-forward (concat
                             "\\(^<\\)\\([SMI]\\{2\\}\\)") nil t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face compilation-column-face)
    (put-text-property (match-beginning 2) (match-end 2)
                       'face compilation-column-face)
    (forward-line 1))
  (goto-char 1)
  (while (re-search-forward (concat
                             "\\(^>\\)\\([SMI]\\{2\\}\\)") nil t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face compilation-info-face)
    (put-text-property (match-beginning 2) (match-end 2)
                       'face compilation-info-face)
    (forward-line 1))
  (goto-char 1)
  (while (re-search-forward (concat
                             "\\(^[<>]...\\)\\(.*\\)\\("
                             obj
                             "\\)\\(.*\\)\\(#\\)\\(.*\\)\\("
                             mthd
                             "\\)\\(.*\\)") nil t)
    (put-text-property (match-beginning 3) (match-end 3)
                       'face compilation-line-face)
    (put-text-property (match-beginning 7) (match-end 7)
                       'face compilation-warning-face)
    (forward-line 1)))

(defvar anything-c-source-ruby-mthds
  '((name . "Anything Ruby Mthds")
    (candidates . anything-ruby-mthds-init)
    (requires-pattern . 2)
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (kill-new (car (cdr (split-string candidate "#")))))))
  "Return a list of objects ruby methods.")

;;; --------------------------------------------------------------------
;;; - Interctive Functions
;;;
;;;###autoload
(defun anything-ruby-mthds ()
  "Return a list of objects ruby methods from Objectspace."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-ruby-mthds) *anything-ruby-mthds-buffer-name*))

(provide 'anything-ruby-mthds)
