;;; minibuffer-header.el --- Minibuffer header line -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/minibuffer-header
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package displays a customizable header line in the minibuffer
;; that is also able to display messages.
;;
;;; Usage:
;;
;; (require 'minibuffer-header)
;; (minibuffer-header-mode)
;;

;; NEWS:
;;
;; Version 0.1
;; - First version

;;; Code
(require 'text-property-search)

(defgroup minibuffer-header nil
  "Minibuffer header"
  :group 'minibuffer)

(defcustom minibuffer-header-show-message t
  "Whether to show messages in the header (on the right)."
  :type 'bool)

(defcustom minibuffer-header-hide-prompt nil
  "Whether to hide original minibuffer prompt."
  :type 'bool)

(defface minibuffer-header-face
  `((t :inherit highlight
       :extend t))
  "Face for the minibuffer header"
  :group 'minibuffer-header)

(defface minibuffer-header-message-face
  `((t :inherit (minibuffer-header-face italic)))
       
  "Face for the minibuffer header"
  :group 'minibuffer-header)

(defun minibuffer-header-format ()
  "Minibuffer header line"

  (concat 
   (propertize (format " #%d Minibuffer" (minibuffer-depth))
               'face '(bold minibuffer-header-face))
   (propertize (format " (%s)" this-command)
               'face 'minibuffer-header-face)
   ))


(defun minibuffer-header--setup ()
  "Install header line in the minibuffer"

  (set-window-margins nil 0 0)
  (set-window-fringes (minibuffer-window) 0 0)
  (cursor-intangible-mode t)

  ;; Install the header line
  (save-excursion
    (goto-char (point-min))
    (let* ((inhibit-read-only t)
           (left (minibuffer-header-format))
           (left (split-string left "\n"))
           (right " ")
	       (prompt-beg (point-min))
	       (prompt-end (or (next-property-change (+ 1 (point-min)))
		                   (max (point-min) (- (point-max) 0)))))

      (when minibuffer-header-hide-prompt
        (add-text-properties prompt-beg prompt-end '(invisible t)))

      (goto-char (point-min))
      (insert (propertize
               (concat (propertize (car left))
                       (propertize " "
                                   'message-beg t
                                   'face 'minibuffer-header-face
                                   'display `(space :align-to (- right ,(- (length right) -1))))
                       (propertize right
                                   'face 'minibuffer-header-message-face)
                       (propertize "\n"
                                   'face 'minibuffer-header-face
                                   'message-end t)
                       (mapconcat #'identity (cdr left) ""))
               'cursor-intangible t
               'read-only t
               'field t
               'rear-nonsticky t
               'front-sticky t))

  ;; Install our error function and message
  (when minibuffer-header-show-message
    (setq command-error-function #'minibuffer-header--command-error-function)
    (advice-add 'message :override #'minibuffer-header--message-override)))))

(defun minibuffer-header--exit ()
  "Remove our error function and message"

  (when minibuffer-header-show-message
    (when (eq 1 (minibuffer-depth))
      (setq command-error-function nil)
      (advice-remove 'message #'minibuffer-header--message-override))))

(defun minibuffer-header-message (&optional msg)
  "Display MSG at the right of the minibuffer header line"

  (when-let* ((msg (or msg " "))
              (window (active-minibuffer-window))
              (buffer (window-buffer window)))
      (with-current-buffer buffer
        (save-excursion

          ;; Search for message marker (in heade line)
          (goto-char (point-min))
          (let ((beg (when (text-property-search-forward 'message-beg) (- (point) 1)))
                (end (when (text-property-search-forward 'message-end) (point))))
            (when (and beg end)
              (let* ((inhibit-read-only t)
                     (message-log-max nil)
                     (width (- (window-width) beg 1))
                     (msg (if (> (length msg) width)
                              (format "%s…" (substring msg 0 (- width 1)))
                            msg))
                     (inhibit-message t))

                ;; Delete old message
                (delete-region beg end)

                ;; Insert new message
                (goto-char beg)
                (insert
                  (propertize
                   (concat
                    (propertize " " 'message-beg t
				                    'face 'minibuffer-header-face
                                    'display `(space :align-to (- right ,(- (length msg) -1))))
                    (propertize msg 'face 'minibuffer-header-message-face)
                    (propertize "\n" 'face 'minibuffer-header-face
				                     'message-end t))
                   'cursor-intangible t
                   'read-only t
                   'field t
                   'rear-nonsticky t
                   'front-sticky t)))))))))


(defun minibuffer-header--message-override (orig-fun &rest args)
  "This advice is used to override the original message function"

  ;; Debug buffer (since we cannot use message for log)
  ;; (with-current-buffer (get-buffer-create "*minibuffer-header-debug*")
  ;;  (goto-char (point-max))
  ;;  (insert (format "\n%s" args)))
  
  (let* ((msg (if (and (car args) (stringp (car args)))
                  (apply 'format-message args)
                " "))
         (msg (replace-regexp-in-string "%" "%%" msg)))
    (minibuffer-header-message msg)))

(defvar minibuffer-header--timer nil
  "Timer used to clear message automatically after a delay")
      
(defun minibuffer-header--command-error-function (data context caller)
  "This command-error function intercepts some message from the C API."

  (if (not (memq (car data) '(buffer-read-only
                              text-read-only
                              beginning-of-buffer
                              end-of-buffer
                              quit)))
      (command-error-default-function data context caller)
    (progn
      (minibuffer-header-message (format "%s" data))
      (when minibuffer-header--timer
        (cancel-timer minibuffer-header--timer))
      (setq minibuffer-header--timer
            (run-at-time minibuffer-message-timeout nil
                         #'minibuffer-header-message)))))

(define-minor-mode minibuffer-header-mode
  "Minor mode for installing a header line in the minibuffer"
  :group 'minibuffer-header
  (if minibuffer-header-mode
      (progn
      (add-hook 'minibuffer-setup-hook  #'minibuffer-header--setup)
      (add-hook 'minibuffer-exit-hook  #'minibuffer-header--exit))
    (progn
      (remove-hook 'minibuffer-setup-hook  #'minibuffer-header--setup)
      (remove-hook 'minibuffer-exit-hook  #'minibuffer-header--exit))))

(provide 'minibuffer-header)
;;; minibuffer-header.el ends here
