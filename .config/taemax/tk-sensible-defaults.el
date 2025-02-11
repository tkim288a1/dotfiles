;;; tk-sensible-defaults.el --- Reasonable settings for getting started.

;; Author: Tae Eun Kim <tae@tkim.xyz>

;; Heavily inspired by `sensible-defaults.el' by
;; Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 1.0.0
;; URL: https://github.com/hrs/sensible-defaults.el/sensible-defaults.el

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;; Code:

;;; Utility functions: ----------------------------------------------------------


(defun tk-sensible-defaults/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun tk-sensible-defaults/reset-text-size ()
  (interactive)
  (text-scale-set 0))


;;; Some more utility functions: ------------------------------------------------


(defun tk-sensible-defaults/vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun tk-sensible-defaults/hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(defun tk-sensible-defaults/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window (expand-file-name user-emacs-directory)))


;;; Some more utility functions stolen from crux: -------------------------------


(defun crux-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (crux-move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

(defun crux-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun crux-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (crux-move-to-mode-line-start))

(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (unless (use-region-p)
        (newline))
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'crux-rename-buffer-and-file #'crux-rename-file-and-buffer)


;;; Settings: -------------------------------------------------------------------


(defun tk-sensible-defaults/open-files-from-home-directory ()
  "When opening a file, start searching at the user's home
directory."
  (setq default-directory "~/"))

(defun tk-sensible-defaults/increase-gc-threshold ()
  "Allow 20MB of memory (instead of 0.76MB) before calling
garbage collection. This means GC runs less often, which speeds
up some operations."
  (setq gc-cons-threshold 20000000))

(defun tk-sensible-defaults/delete-trailing-whitespace ()
  "Call DELETE-TRAILING-WHITESPACE every time a buffer is saved."
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun tk-sensible-defaults/treat-camelcase-as-separate-words ()
  "Treat CamelCaseSubWords as separate words in every programming
mode."
  (add-hook 'prog-mode-hook 'subword-mode))

(defun tk-sensible-defaults/automatically-follow-symlinks ()
  "When opening a file, always follow symlinks."
  (setq vc-follow-symlinks t))

(defun tk-sensible-defaults/make-scripts-executable ()
  "When saving a file that starts with `#!', make it executable."
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(defun tk-sensible-defaults/single-space-after-periods ()
  "Don't assume that sentences should have two spaces after
periods. This ain't a typewriter."
  (setq sentence-end-double-space nil))

(defun tk-sensible-defaults/offer-to-create-parent-directories-on-save ()
  "When saving a file in a directory that doesn't exist, offer
to (recursively) create the file's parent directories."
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))

(defun tk-sensible-defaults/apply-changes-to-highlighted-region ()
  "Turn on transient-mark-mode."
  (transient-mark-mode t))

(defun tk-sensible-defaults/overwrite-selected-text ()
  "If some text is selected, and you type some text, delete the
selected text and start inserting your typed text."
  (delete-selection-mode t))

(defun tk-sensible-defaults/ensure-that-files-end-with-newline ()
  "If you save a file that doesn't end with a newline,
automatically append one."
  (setq require-final-newline t))

(defun tk-sensible-defaults/confirm-closing-emacs ()
  "Ask if you're sure that you want to close Emacs."
  (setq confirm-kill-emacs 'y-or-n-p))

(defun tk-sensible-defaults/quiet-startup ()
  "Don't present the usual startup message, and clear the scratch
buffer."
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun tk-sensible-defaults/make-dired-file-sizes-human-readable ()
  "Add file sizes in human-readable units (KB, MB, etc) to dired
buffers."
  (setq-default dired-listing-switches "-alh"))

(defun tk-sensible-defaults/shorten-yes-or-no ()
  "Don't ask `yes/no?', ask `y/n?'."
  (fset 'yes-or-no-p 'y-or-n-p))

(defun tk-sensible-defaults/always-highlight-code ()
  "Turn on syntax highlighting whenever possible."
  (global-font-lock-mode t))

(defun tk-sensible-defaults/refresh-buffers-when-files-change ()
  "When something changes a file, automatically refresh the
buffer containing that file so they can't get out of sync."
  (global-auto-revert-mode t))

(defun tk-sensible-defaults/show-matching-parens ()
  "Visually indicate matching pairs of parentheses."
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

(defun tk-sensible-defaults/flash-screen-instead-of-ringing-bell ()
  "When you perform a problematic operation, flash the screen
instead of ringing the terminal bell."
  (setq visible-bell t))

(defun tk-sensible-defaults/set-default-line-length-to (line-length)
  "Set the default line length to LINE-LENGTH."
  (setq-default fill-column line-length))

(defun tk-sensible-defaults/open-clicked-files-in-same-frame-on-mac ()
  "When you double-click on a file in the Mac Finder open it as a
buffer in the existing Emacs frame, rather than creating a new
frame just for that file."
  (setq ns-pop-up-frames nil))

(defun tk-sensible-defaults/yank-to-point-on-mouse-click ()
  "When middle-clicking the mouse to yank from the clipboard,
insert the text where point is, not where the mouse cursor is."
  (setq mouse-yank-at-point t))

(defun tk-sensible-defaults/set-modifiers-on-mac ()
  "When on a Mac, set Command key as meta and Option key as super."
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(defun tk-sensible-defaults/no-bell-no-flash ()
  "When you perform a problematic operation, do not ring the terminal bell
nor flash the screen."
  (setq ring-bell-function 'ignore))

(defun tk-sensible-defaults/nice-scrolling ()
  "No more choppy scrolling."
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1))

(defun tk-sensible-defaults/pretty-symbols ()
  "Use pretty symbols everywhere."
  (global-prettify-symbols-mode t))


;;; Settings selections: --------------------------------------------------------


(defun tk-sensible-defaults/use-all-settings ()
  "Use all of the sensible-defaults settings."
  (tk-sensible-defaults/open-files-from-home-directory)
  (tk-sensible-defaults/increase-gc-threshold)
  (tk-sensible-defaults/delete-trailing-whitespace)
  (tk-sensible-defaults/treat-camelcase-as-separate-words)
  (tk-sensible-defaults/automatically-follow-symlinks)
  (tk-sensible-defaults/make-scripts-executable)
  (tk-sensible-defaults/single-space-after-periods)
  (tk-sensible-defaults/offer-to-create-parent-directories-on-save)
  (tk-sensible-defaults/apply-changes-to-highlighted-region)
  (tk-sensible-defaults/overwrite-selected-text)
  (tk-sensible-defaults/ensure-that-files-end-with-newline)
  (tk-sensible-defaults/confirm-closing-emacs)
  (tk-sensible-defaults/quiet-startup)
  (tk-sensible-defaults/make-dired-file-sizes-human-readable)
  (tk-sensible-defaults/shorten-yes-or-no)
  (tk-sensible-defaults/always-highlight-code)
  (tk-sensible-defaults/refresh-buffers-when-files-change)
  (tk-sensible-defaults/show-matching-parens)
  (tk-sensible-defaults/flash-screen-instead-of-ringing-bell)
  (tk-sensible-defaults/set-default-line-length-to 80)
  (tk-sensible-defaults/open-clicked-files-in-same-frame-on-mac)
  (tk-sensible-defaults/yank-to-point-on-mouse-click))

(defun tk-sensible-defaults/tk-settings ()
  "TK's selection of tk-sensible-defaults settings."
  ;; (tk-sensible-defaults/open-files-from-home-directory)
  (tk-sensible-defaults/increase-gc-threshold)
  (tk-sensible-defaults/delete-trailing-whitespace)
  (tk-sensible-defaults/treat-camelcase-as-separate-words)
  (tk-sensible-defaults/automatically-follow-symlinks)
  (tk-sensible-defaults/make-scripts-executable)
  (tk-sensible-defaults/single-space-after-periods)
  (tk-sensible-defaults/offer-to-create-parent-directories-on-save)
  (tk-sensible-defaults/apply-changes-to-highlighted-region)
  (tk-sensible-defaults/overwrite-selected-text)
  (tk-sensible-defaults/ensure-that-files-end-with-newline)
  (tk-sensible-defaults/confirm-closing-emacs)
  (tk-sensible-defaults/quiet-startup)
  (tk-sensible-defaults/make-dired-file-sizes-human-readable)
  (tk-sensible-defaults/shorten-yes-or-no)
  (tk-sensible-defaults/always-highlight-code)
  (tk-sensible-defaults/refresh-buffers-when-files-change)
  (tk-sensible-defaults/show-matching-parens)
  ;; (tk-sensible-defaults/flash-screen-instead-of-ringing-bell)
  (tk-sensible-defaults/set-default-line-length-to 80)
  (tk-sensible-defaults/open-clicked-files-in-same-frame-on-mac)
  (tk-sensible-defaults/yank-to-point-on-mouse-click)
  (tk-sensible-defaults/set-modifiers-on-mac)
  (tk-sensible-defaults/no-bell-no-flash)
  (tk-sensible-defaults/nice-scrolling)
  (tk-sensible-defaults/pretty-symbols))


;;; Keybindings: ----------------------------------------------------------------


(defun tk-sensible-defaults/bind-home-and-end-keys ()
  "Make <home> and <end> move point to the beginning and end of
the line, respectively."
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line))

(defun tk-sensible-defaults/bind-keys-to-change-text-size ()
  "Bind C-+ and C-- to increase and decrease text size,
respectively."
  (define-key global-map (kbd "C-)") 'tk-sensible-defaults/reset-text-size)
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C-=") 'text-scale-increase)
  (define-key global-map (kbd "C-_") 'text-scale-decrease)
  (define-key global-map (kbd "C--") 'text-scale-decrease))


;;; TK additions: ---------------------------------------------------------------


(defun tk-sensible-defaults/bind-find-user-init-file ()
  "Bind C-c i to edit the `user-init-file', in another window."
  (global-set-key (kbd "C-c i")
		  'tk-sensible-defaults/find-user-init-file))

(defun tk-sensible-defaults/bind-other-window-related ()
  "Bind keys for movements to other window."
  (global-set-key (kbd "C-z") 'other-window)
  (global-set-key (kbd "C-x 2")
                  'tk-sensible-defaults/vsplit-other-window)
  (global-set-key (kbd "C-x 3")
                  'tk-sensible-defaults/hsplit-other-window))

(defun tk-sensible-defaults/bind-shrink-or-enlarge-window ()
  "Bind keys for shrinking or enlarging window."
  (global-set-key (kbd "s-C-<left>")  'shrink-window-horizontally)
  (global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "s-C-<down>")  'shrink-window)
  (global-set-key (kbd "s-C-<up>") 'enlarge-window))

(defun tk-sensible-defaults/bind-shells ()
  "Keybindings to open shells."
  (global-set-key (kbd "C-x m") 'eshell)
  (global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
  (global-set-key (kbd "C-x M-m") 'shell))

(defun tk-sensible-defaults/bind-ibuffer ()
  "Replace buffer-menu with ibuffer."
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(defun tk-sensible-defaults/bind-super-keybindings ()
  "Bind s-<key> for various convenient actions."
  (global-set-key (kbd "s-l") 'toggle-truncate-lines)
  (global-set-key (kbd "s-x") 'delete-frame)
  (global-set-key (kbd "s-+") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "s-0") 'tk-sensible-defaults/reset-text-size))

(defun tk-sensible-defaults/bind-crux-goodies ()
  "Bind keys to useful crux functions."
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
  (global-set-key (kbd "s-j") 'crux-top-join-line)
  (global-set-key (kbd "s-k") 'crux-kill-whole-line)
  (global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
  (global-set-key (kbd "s-o") 'crux-smart-open-line-above)
  (global-set-key (kbd "M-o") 'crux-smart-open-line))

(defun tk-sensible-defaults/bind-miscellaneous ()
  "Some miscellaneous keybindings."
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "C-s-j") (kbd "C-u 1 C-v"))
  (global-set-key (kbd "C-s-k") (kbd "C-u 1 M-v")))


;;; Keybinding selections: ------------------------------------------------------

(defun tk-sensible-defaults/use-all-keybindings ()
  "Use all of the tk-sensible-defaults keybindings."
  (tk-sensible-defaults/bind-commenting-and-uncommenting)
  (tk-sensible-defaults/bind-home-and-end-keys)
  (tk-sensible-defaults/bind-keys-to-change-text-size))

(defun tk-sensible-defaults/tk-keybindings ()
  "Use my personal custom keybindings."
  ;; (tk-sensible-defaults/bind-commenting-and-uncommenting)
  ;; (tk-sensible-defaults/bind-home-and-end-keys)
  ;; (tk-sensible-defaults/bind-keys-to-change-text-size)
  (tk-sensible-defaults/bind-other-window-related)
  (tk-sensible-defaults/bind-shrink-or-enlarge-window)
  (tk-sensible-defaults/bind-find-user-init-file)
  (tk-sensible-defaults/bind-other-window-related )
  (tk-sensible-defaults/bind-shrink-or-enlarge-window)
  (tk-sensible-defaults/bind-shells)
  (tk-sensible-defaults/bind-ibuffer)
  (tk-sensible-defaults/bind-super-keybindings)
  (tk-sensible-defaults/bind-crux-goodies)
  (tk-sensible-defaults/bind-miscellaneous))


;;; Non-default settings: -------------------------------------------------------

(defun tk-sensible-defaults/backup-to-temp-directory ()
  "Store backups and auto-saved files in
TEMPORARY-FILE-DIRECTORY (which defaults to /tmp on Unix),
instead of in the same directory as the file. This means we're
still making backups, but not where they'll get in the way.

WARNING: on most Unix-like systems /tmp is volatile, in-memory
storage, so your backups won't survive if your computer crashes!
If you're not willing to take this risk, you shouldn't enable
this setting."
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))


;;; Ultimate call: --------------------------------------------------------------

(defun tk-sensible-defaults/tk-set-it-all ()
  "Set all my default and non-default settings as well as keybindings."
  (tk-sensible-defaults/tk-settings)
  (tk-sensible-defaults/tk-keybindings)
  (tk-sensible-defaults/backup-to-temp-directory))


(provide 'tk-sensible-defaults)
;;; tk-sensible-defaults.el ends here
