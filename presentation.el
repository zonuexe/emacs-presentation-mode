;;; presentation.el --- Scale text globally and adjust UI for presentations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: environment, faces, frames
;; Created: 7 Apr 2018
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.4") (compat "30"))
;; URL: https://github.com/zonuexe/emacs-presentation-mode
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Presentation mode is a global minor mode that provides a distraction-free
;; environment for live demos.  It scales text across all buffers and provides
;; hooks to automate UI changes, such as toggling line numbers or switching themes.
;;
;; Key features:
;; - Global text scaling for high visibility on projectors/screen-sharing.
;; - `presentation-on-hook' and `presentation-off-hook' for UI automation.
;; - Persistence of scale size between sessions.
;; - Optional integration with Emacs 29's `global-text-scale-adjust'.
;;
;; ## How to use
;;
;;  1. Execute `M-x presentation-mode' to start the presentation.
;;  2. Adjust scale size using `C-x C-+' or `C-x C--'.
;;     (See: https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Scale.html )
;;  3. Execute `M-x presentation-mode' again to end and restore your UI.
;;  4. Toggling the mode back on will automatically reproduce the last used scale.
;;  5. To set a permanent default scale, customize `presentation-default-text-scale'.
;;
;; ## Technical Notes
;;
;; ### Permanent font changes
;;
;; This mode is NOT intended for permanent font configuration.  If you wish to
;; change the default font size of Emacs frames permanently, please refer to:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parameter-Access.html
;;
;; ## Comparison
;;
;; - vs `default-text-scale': Use `default-text-scale' for permanent global
;;   scaling.  Use `presentation.el' for temporary demo environments.
;; - vs `org-tree-slide' / `org-present': These are specific to Org-mode slides.
;;   `presentation.el' is for general Emacs usage and live-coding.
;;

;;; Code:
(require 'cl-lib)
(require 'nadvice)
(require 'face-remap)
(eval-when-compile
  (require 'compat-29 nil t))

;; Customize variables:
(defgroup presentation nil
  "Display large charactor for presentation."
  :group 'covinience)

(defcustom presentation-on-hook nil
  "Functions to run whenever Presentation mode is turned on."
  :type 'hook)

(defcustom presentation-off-hook nil
  "Functions to run whenever Presentation mode is turned off."
  :type 'hook)

(defcustom presentation-default-text-scale 3
  "Text scale for presentation."
  :type 'integer)

(defcustom presentation-keep-last-text-scale t
  "If non-NIL, reproduce size the last time presentation mode was used.

Note that the size is not inherited when you quit Emacs.
Please set `presentation-default-text-scale' in the initialization processing of
your init.el."
  :type 'boolean)

(defcustom presentation-mode-lighter " Presentation"
  "String to display in mode line when Presentation Mode is enabled; nil for none."
  :type 'string)

(defcustom presentation-mode-ignore-major-modes '()
  "List of major modes unaffected by presentation mode."
  :type '(repeat (choice function symbol)))

(defcustom presentation-mode-ignore-minor-modes '(org-present-mode org-tree-slide-mode)
  "List of minor modes unaffected by presentation mode."
  :type '(repeat (choice variable symbol)))

(defcustom presentation-mode-text-scale-remap-header-line t
  "If non-nil, text scaling may change font size of header lines too."
  :type 'boolean)

(defcustom presentation-use-global-text-scale-adjust 'auto
  "Control use of `global-text-scale-adjust' when available.

- auto: use it when present and no ignored buffers require local scaling.
- always: use it whenever present; do not fall back for ignored buffers.
- never: never use it; rely on buffer-local `text-scale-mode' operations."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Always use global" always)
                 (const :tag "Never use global" never)))

;; Buffer local variables:
(defvar-local presentation-disable nil)

;; Variables:
(defvar presentation-last-text-scale nil)
(defvar presentation--current-global-level 0
  "Track current global text scale when using `global-text-scale-adjust'.")
(defvar presentation--fallback-used nil
  "Non-nil when buffer-local scaling fallback was used in this session.")
(defvar presentation--pending-off-hook nil)

;; Functions:

(defun presentation--reset-global-scale ()
  "Reset global text scale to default when previously adjusted."
  (when (and (eval-when-compile (fboundp 'global-text-scale-adjust))
             (/= presentation--current-global-level 0))
    (let ((last-command-event ?0))
      (global-text-scale-adjust 0))
    (setq presentation--current-global-level 0)))

(defun presentation--reset-all-scales ()
  "Reset text scaling using global adjust when suitable, else per-buffer."
  (let ((use-global
         (and (fboundp 'global-text-scale-adjust)
              (pcase presentation-use-global-text-scale-adjust
                ('always t)
                ('auto (not (presentation--buffers-require-local-scaling-p)))
                (_ nil)))))
    (when use-global
      (let ((last-command-event ?0))
        (global-text-scale-adjust 0))
      (setq presentation--current-global-level 0))
    (when (and (not use-global) presentation--fallback-used)
      (presentation--reset-global-scale)
      (save-selected-window
        (cl-loop for buf in (buffer-list)
                 do (with-current-buffer buf
                      (unless (presentation-ignore-current-buffer)
                        (text-scale-set 0))))))
    (setq presentation--fallback-used nil)))

(defun presentation--buffers-require-local-scaling-p ()
  "Return non-nil if any buffer should be excluded from global scaling.

We avoid `global-text-scale-adjust' when a buffer would be ignored, because
global face changes cannot be scoped per buffer."
  (cl-loop for buf in (buffer-list)
           thereis (with-current-buffer buf
                     (presentation-ignore-current-buffer))))

(defun presentation--run-delayed-off-hook ()
  "Helper to run `presentation-off-hook' once, ignoring errors."
  (when presentation--pending-off-hook
    (setq presentation--pending-off-hook nil)
    (ignore-errors
      (run-hooks 'presentation-off-hook)))
  (remove-hook 'post-command-hook #'presentation--run-delayed-off-hook))

(defun presentation--use-global-adjust-p ()
  "Return non-nil when `global-text-scale-adjust' can be used."
  (when (eval-when-compile (fboundp 'global-text-scale-adjust))
    (pcase presentation-use-global-text-scale-adjust
      ('always t)
      ('auto (not (presentation--buffers-require-local-scaling-p)))
      (_ nil))))

(defun presentation--global-text-scale-set (level)
  "Set global text scale LEVEL using `global-text-scale-adjust'."
  (let ((delta (- level presentation--current-global-level)))
    (cond
     ((= level 0)
      (let ((last-command-event ?0))
        (global-text-scale-adjust 0)))
     ((> delta 0)
      (let ((last-command-event ?+))
        (global-text-scale-adjust delta)))
     ((< delta 0)
      (let ((last-command-event ?-))
        (global-text-scale-adjust (- delta)))))
    (setq presentation--current-global-level level)))

(defun presentation--text-scale-set (&rest _args)
  "Set `text-scale-mode-amount' for each buffer or via global adjust."
  (setq presentation-last-text-scale text-scale-mode-amount)
  (if (presentation--use-global-adjust-p)
      (presentation--global-text-scale-set text-scale-mode-amount)
    (presentation-windows-text-scale-set text-scale-mode-amount)))

(defun presentation--text-scale-set-around (orig-fn level)
  "Around advice for ORIG-FN when delegating `text-scale-set' LEVEL.
Delegates to `global-text-scale-adjust' when enabled, else calls ORIG-FN."
  (if (presentation--use-global-adjust-p)
      (progn
        (setq presentation-last-text-scale level)
        (presentation--global-text-scale-set level))
    (presentation--reset-global-scale)
    (funcall orig-fn level)
    (presentation--text-scale-set)))

(defun presentation--text-scale-increase-around (orig-fn inc)
  "Around advice for ORIG-FN when delegating `text-scale-increase' INC.
Delegates to `global-text-scale-adjust' when enabled, else calls ORIG-FN."
  (if (presentation--use-global-adjust-p)
      (let* ((base (or presentation-last-text-scale text-scale-mode-amount 0))
             (level (+ base inc)))
        (setq presentation-last-text-scale level)
        (presentation--global-text-scale-set level))
    (presentation--reset-global-scale)
    (funcall orig-fn inc)
    (presentation--text-scale-set)))

(defun presentation--text-scale-apply ()
  "Set `presentation-last-text-scale' for each buffer."
  (if (presentation--use-global-adjust-p)
      (presentation--global-text-scale-set presentation-last-text-scale)
    (presentation-windows-text-scale-set presentation-last-text-scale)))

(defun presentation-ignore-current-buffer ()
  "Return non-NIL if `current-buffer' should be ignore for presentation."
  (or presentation-disable
      (memq major-mode presentation-mode-ignore-major-modes)
      (cl-some (lambda (m) (and (boundp m) (symbol-value m)))
               presentation-mode-ignore-minor-modes)))

(defun presentation-windows-text-scale-set (level)
  "Set LEVEL for each buffer."
  (if (presentation--use-global-adjust-p)
      (presentation--global-text-scale-set level)
    (setq presentation--fallback-used t)
    (presentation--reset-global-scale)
    (save-selected-window
      (walk-windows
       (lambda (win)
         (with-selected-window win
           (when (eval-when-compile (boundp 'text-scale-remap-header-line))
             (setq text-scale-remap-header-line presentation-mode-text-scale-remap-header-line))
           (unless (presentation-ignore-current-buffer)
             (setq text-scale-mode-amount level)
             (text-scale-mode (if (zerop text-scale-mode-amount) -1 1)))))
       t t))))

(defun presentation--add-scale-advice ()
  "Install text scale advices."
  (if (presentation--use-global-adjust-p)
      (progn
        (advice-add 'text-scale-set :around #'presentation--text-scale-set-around)
        (advice-add 'text-scale-increase :around #'presentation--text-scale-increase-around))
    (advice-add 'text-scale-set :after #'presentation--text-scale-set)
    (advice-add 'text-scale-increase :after #'presentation--text-scale-set)))

(defun presentation--remove-scale-advice ()
  "Remove text scale advices."
  (advice-remove 'text-scale-set #'presentation--text-scale-set-around)
  (advice-remove 'text-scale-increase #'presentation--text-scale-increase-around)
  (advice-remove 'text-scale-set #'presentation--text-scale-set)
  (advice-remove 'text-scale-increase #'presentation--text-scale-set))

;; Mode:
(defvar-keymap presentation-mode-map
  :doc "Keymap for `presentation-mode'.")

;;;###autoload
(define-minor-mode presentation-mode
  "Toggle Presentation mode ON or OFF."
  :group 'presentation
  :global t
  :lighter presentation-mode-lighter
  :keymap presentation-mode-map
  :require 'presentation
  (if presentation-mode
      (save-selected-window
        (setq presentation--fallback-used nil)
        (presentation--add-scale-advice)
        (add-hook 'window-configuration-change-hook  #'presentation--text-scale-apply)
        (let ((text-scale-mode-amount (or (when presentation-keep-last-text-scale presentation-last-text-scale)
                                          presentation-default-text-scale)))
          (presentation--text-scale-set))
        (run-hooks 'presentation-on-hook))
    (presentation--remove-scale-advice)
    (remove-hook 'window-configuration-change-hook  #'presentation--text-scale-apply)
    (presentation--reset-all-scales)
    (setq presentation--pending-off-hook t)
    (add-hook 'post-command-hook #'presentation--run-delayed-off-hook)))

(provide 'presentation)
;;; presentation.el ends here
