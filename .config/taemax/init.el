;; https://iqss.github.io/IQSS.emacs/init.html
;; https://github.com/james-stoup/org-mode-better-defaults


;; add melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; use-package
(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-verbose nil)
(setq use-package-always-ensure t)

;; macos execution path
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; Load environment variables from shell
  (exec-path-from-shell-initialize)
  ;; Optionally, specify variables to import
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH")))

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no titlebar (emacs-plus on macOS)
;; https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#no-titlebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; set frame transparency
(add-to-list 'default-frame-alist '(alpha . (90 . 80)))
(set-frame-parameter (selected-frame) 'alpha '(90 . 80))

;; font size
(set-face-attribute 'default nil :height 170)

;; frame title
(setq frame-title-format
      ;; '("" invocation-name " Prelude: "
      '(" TK Emacs: "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable splash message, start *scratch* buffer by default
(setq initial-buffer-choice t)
(setq initial-scratch-message "")

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(global-prettify-symbols-mode t)

;; make ace-window leading character face more visible ----------------
;; evaluate after load?
;; (use-package ace-window
;;   :demand t
;;   :config
;;   (setq aw-scope 'frame)
;;   (set-face-attribute 'aw-leading-char-face nil :weight 'bold :height 3.0))

(use-package ace-window
  :init
  (setq aw-scope 'frame)
  :config
  (set-face-attribute 'aw-leading-char-face nil :weight 'bold :height 3.0)
  :bind (("C-z" . ace-window)
         ("C-S-z" . ace-swap-window)))

;; super-save: auto-save when switching focus
(use-package super-save
  ;; :hook
  ;; (after-init . super-save-mode)
  :defer 1
  :diminish
  :config
  (super-save-mode +1)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  ;; Enable deleting trailing white spaces before saving
  (setq super-save-delete-trailing-whitespace t)
  ;; ;; Enable deleting trailing white spaces before saving (except for the current line)
  ;; (setq super-save-delete-trailing-whitespace 'except-current-line)
  )

;; save minibuffer history
;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at
;; the top.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
(savehist-mode 1)

;; server
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start))

  ;; TODO: Automatically create a client frame on Emacs startup
  ;; (when (display-graphic-p)
  ;;   (start-process "emacsclient" nil "emacsclient"
  ;;                  "--alternate-editor='' --no-wait --create-frame"
  ;;                  "--frame-parameters='(quote (name . \"dropdown_emacsdd\"))'"))
  )

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)


;; ;; ------------------------------
;; ;; ivy-centric completion
;; ;; ------------------------------
;; ;; Ivy Configuration
;; (use-package ivy
;;   :ensure t
;;   :diminish
;;   :bind (("C-c C-r" . ivy-resume)
;;          ("<f6>" . ivy-resume))
;;   :init
;;   (ivy-mode 1)
;;   :config
;;   (setq ivy-use-virtual-buffers t
;;         enable-recursive-minibuffers t))

;; ;; Swiper Configuration
;; (use-package swiper
;;   :ensure t
;;   :bind (("C-s" . swiper)))

;; ;; Counsel Configuration
;; (use-package counsel
;;   :ensure t
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x C-f" . counsel-find-file)
;;          ("<f1> f" . counsel-describe-function)
;;          ("<f1> v" . counsel-describe-variable)
;;          ("<f1> l" . counsel-find-library)
;;          ("<f2> i" . counsel-info-lookup-symbol)
;;          ("<f2> u" . counsel-unicode-char)
;;          ("C-c g" . counsel-git) ; will override the keybinding for `magit-file-dispatch'
;;          ("C-c j" . counsel-git-grep)
;;          ("C-c a" . counsel-ag)
;;          ("C-x l" . counsel-locate)
;;          ("M-y" . counsel-yank-pop)
;;          :map minibuffer-local-map
;;          ("C-r" . counsel-minibuffer-history)))

;; ------------------------------
;; modern minibuffer packages (vertico, consult, etc)
;; ------------------------------
;; https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/

(use-package vertico
  :hook
  (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil))

(use-package marginalia
  :hook
  (after-init . marginalia-mode)
  ;; :config
  ;; (marginalia-mode 1)
  )

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :demand t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))


;; ------------------------------
;; theme
;; ------------------------------
;; set custom themes directory
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" user-emacs-directory))

;; stop asking if custom themes are safe
(setq custom-safe-themes t)

;; favorite themes
(use-package doom-themes
  :ensure t
  ;; :init (load-theme 'doom-one)
  )

(use-package ef-themes
  :ensure t
  :init (load-theme 'ef-cyprus)
  )

;; (use-package catppuccin-theme
;;   :ensure t
;;   :custom
;;   (catppuccin-flavor 'macchiato) ; 'latte, 'frappe, 'macchiato, or 'mocha
;;   ;; :init (load-theme 'catppuccin)
;;   )

;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(defvar tk-theme-selections
  '(tk-nord
    ;; catppuccin
    doom-one
    doom-nord
    doom-material-dark
    doom-tokyo-night
    ef-duo-dark
    ef-cyprus
    )
  "A list of selected themes.")

;; ------------------------------
;; system appearance change
;; ------------------------------
;; https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#system-appearance-change
(defun tk-apply-theme-system-appearance (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ef-cyprus t))
    ('dark (load-theme 'ef-duo-dark t))))
(add-hook 'ns-system-appearance-change-functions #'tk-apply-theme-system-appearance)


;; Shamelessly taken and modified from counsel.el
(defun tk-load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t)
        (when (fboundp 'powerline-reset)
          (powerline-reset)))
    (error "Problem loading theme %s" x)))

;; Below depends on "ivy". Re-written below so that it utilizes
;; built-in functions.

;; (defun tk-load-theme ()
;;   "Forward to `load-theme'.
;; Usable with `ivy-resume', `ivy-next-line-and-call' and
;; `ivy-previous-line-and-call'."
;;   (interactive)
;;   (ivy-read "Load custom theme: "
;;             (mapcar #'symbol-name
;;                     tk-theme-selections)
;;             :action #'tk-load-theme-action
;;             :caller 'tk-load-theme))

(defun tk-load-theme ()
  "Forward to `load-theme'.
Usable with `vertico' for theme selection."
  (interactive)
  (let ((theme (completing-read "Load custom theme: "
                                (mapcar #'symbol-name
                                        tk-theme-selections))))
    (tk-load-theme-action theme)))

(defun tk-reload-theme ()
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (load-theme current-theme t)
    (message "%s-theme reloaded." (symbol-name current-theme))))


;; ;;;###autoload
;; (defun crux-find-shell-init-file ()
;;   "Edit the shell init file in another window."
;;   (interactive)
;;   (let* ((shell (file-name-nondirectory (getenv "SHELL")))
;;          (shell-init-file (cond
;;                            ((string= "zsh" shell) crux-shell-zsh-init-files)
;;                            ((string= "bash" shell) crux-shell-bash-init-files)
;;                            ((string= "tcsh" shell) crux-shell-tcsh-init-files)
;;                            ((string= "fish" shell) crux-shell-fish-init-files)
;;                            ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
;;                            (t (error "Unknown shell"))))
;;          (candidates (cl-remove-if-not 'file-exists-p (mapcar 'substitute-in-file-name shell-init-file))))
;;     (if (> (length candidates) 1)
;;         (find-file-other-window (completing-read "Choose shell init file: " candidates))
;;       (find-file-other-window (car candidates)))))



;; ------------------------------
;; utility functions
;; ------------------------------
;; toggle transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 50) '(100 . 100)))))

;; Split window and switch to the children
(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

;; visit emacs init file
;; (defun tk-find-user-init-files ()
;;   "Edit the `user-init-file', in another window."
;;   (interactive)
;;   (find-file (expand-file-name user-emacs-directory)))
(defun tk-find-user-init-files ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (let ((default-directory (expand-file-name user-emacs-directory)))
    (find-file (consult--read (directory-files default-directory nil "^[^.]" t)))))

;; visit config files
;; (defun tk-find-user-conf-files ()
;;   "Edit the config files, in another window."
;;   (interactive)
;;   (find-file-other-window (expand-file-name "~/.config/")))
(defun tk-find-user-conf-files ()
  "Edit the config files, in another window."
  (interactive)
  (let ((default-directory (expand-file-name "~/.config/")))
    (find-file (consult--read (directory-files default-directory nil "^[^.]" t)))))

;; ------------------------------
;; Essential keybindings
;; ------------------------------
(global-set-key (kbd "C-c i") 'tk-find-user-init-files)
(global-set-key (kbd "C-c I") 'tk-find-user-conf-files)
(global-set-key (kbd "C-x 2") 'vsplit-other-window)
(global-set-key (kbd "C-x 3") 'hsplit-other-window)
(global-set-key (kbd "C-z") 'ace-window)
(global-set-key (kbd "s-l") 'toggle-truncate-lines)
(global-set-key (kbd "s-x") 'delete-frame)
(global-set-key (kbd "s-k") 'kill-whole-line)
(global-set-key (kbd "s-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>")  'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-s-j") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-s-k") (kbd "C-u 1 M-v"))
(global-set-key (kbd "C-M-S-t") 'toggle-transparency)
(global-set-key (kbd "s-T") 'tk-load-theme)
(global-set-key (kbd "s-t") 'tk-reload-theme)

;; OSX modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; clean ui
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; ;; padding
;; (use-package spacious-padding
;;   :ensure t
;;   :custom
;;   (spacious-padding-widths
;;    '( :internal-border-width 20
;;       :header-line-width 4
;;       :mode-line-width 6
;;       :tab-width 4
;;       :right-divider-width 20
;;       :scroll-bar-width 8
;;       :fringe-width 8))
;;   (spacious-padding-subtle-mode-line
;;    `( :mode-line-active 'default
;;       :mode-line-inactive vertical-border))
;;   (spacious-padding-subtle-mode-line nil)
;;   :config
;;   (spacious-padding-mode 0)
;;   (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

;; font
(use-package fontaine
  :ensure t
  :custom
  (fontaine-presets
   '((regular
      :default-family "JetBrains Mono"
      :default-weight normal
      :default-height 130
      :fixed-pitch-family "JetBrains Mono"
      :fixed-pitch-weight nil ; falls back to :default-weight
      :fixed-pitch-height 1.0
      :variable-pitch-family "Iosevka Comfy"
      :variable-pitch-weight normal
      :variable-pitch-height 1.5
      :line-spacing 1)
     (large
      :inherit regular
      :default-height 175
      :variable-pitch-height 1.3)))
  :config
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
  (fontaine-set-preset 'regular))

(when (>= emacs-major-version 26)
  (pixel-scroll-precision-mode 1))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-M-}" . mc/mark-next-like-this)
         ("C-M-{" . mc/mark-previous-like-this)
         ("C-M-|" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)))

;; selected region deleted upon key press
(delete-selection-mode t)

;; highlight the current line
(global-hl-line-mode +1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; ------------------------------
;; org mode
;; ------------------------------
(use-package org
  :ensure nil)				; what's the point?

;; I am not sure if I like org-mode in variable-pitch-mode.
;; (use-package variable-pitch-mode
;;   :ensure nil
;;   :hook
;;   (org-mode . variable-pitch-mode))
;; Remove the hook by (remove-hook 'org-mode-hook 'variable-pitch-mode)

(use-package visual-line-mode
  :ensure nil
  :hook
  (org-mode . visual-line-mode))

;; for line wrapping
;; (global-visual-line-mode 1) ; 1 for on, 0 for off.

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 110))

;; The above is equivalent to below
;; (defun tk/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))
;; (use-package visual-fill-column
;;   :hook (org-mode . tk/org-mode-visual-fill))

;; org bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))
    )))


;; dired sidebar
(defun tk-dired-jump-sidebar ()
  (interactive)
  (display-buffer-in-side-window
   (dired-noselect default-directory)
   '((side . left))))

;; (use-package vscode-icon
;;   :ensure t
;;   :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'nerd-icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Doom modeline
;;   Run `M-x nerd-icons-install-fonts' to install the necessary fonts.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 25))

;; Unfill
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))


;; ------------------------------
;; pdf tools
;; ------------------------------
;; First time it installs, it looks for `autoconf'. If emacs is
;; unaware of the system path, the installation/compilation of the
;; program may fail. In such a case, check if the path from inside
;; emacs. See also `exec-path-from-shell' package.
(use-package pdf-tools
  :demand t
  :config
  (pdf-tools-install))

;; midnite mode hook
;; automatically turns on midnight-mode for pdfs
(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))

;; set the amber profile as default (see below)
(setq pdf-view-midnight-colors '("#2E3440" . "#D8DEE9"))

;; change midnite mode colours functions
(defun tk/pdf-midnite-original ()
  "Set pdf-view-midnight-colors to original colours."
  (interactive)
  (setq pdf-view-midnight-colors '("#2E3440" . "#D8DEE9"))
  (pdf-view-midnight-minor-mode))

(defun tk/pdf-midnite-nord ()
  "Set pdf-view-midnight-colors to nord-inspired scheme."
  (interactive)
  (setq pdf-view-midnight-colors '("#D8DEE9" . "#2E3440" )) ; nord
  (pdf-view-midnight-minor-mode))

(defun tk/pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#FF9900" . "#0A0A12" )) ; amber
  (pdf-view-midnight-minor-mode))

(defun tk/pdf-midnite-green ()
  "Set pdf-view-midnight-colors to green on black."
  (interactive)
  (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
  (pdf-view-midnight-minor-mode))

(defun tk/pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1))

(defun tk/pdf-midnite-colour-schemes ()
  "Midnight mode colour schemes bound to keys"
  (local-set-key (kbd "!") (quote tk/pdf-midnite-original))
  (local-set-key (kbd "@") (quote tk/pdf-midnite-nord))
  (local-set-key (kbd "#") (quote tk/pdf-midnite-amber))
  (local-set-key (kbd "$") (quote tk/pdf-midnite-green))
  (local-set-key (kbd "%") (quote tk/pdf-no-filter)))

(add-hook 'pdf-view-mode-hook 'tk/pdf-midnite-colour-schemes)

(defun tk/pdf-view-keybinding ()
  "Midnight mode colour schemes bound to keys"
  (local-set-key (kbd "j") 'pdf-view-next-line-or-next-page)
  (local-set-key (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (local-set-key (kbd "J") 'pdf-view-next-page-command)
  (local-set-key (kbd "K") 'pdf-view-previous-page-command))

(add-hook 'pdf-view-mode-hook 'tk/pdf-view-keybinding)

;; Note: There are other modes
;;
;; C-c C-r m	pdf-view-midnight-minor-mode
;; C-c C-r p	pdf-view-printer-minor-mode
;; C-c C-r t	pdf-view-themed-minor-mode

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; ------------------------------
;; better writing environments
;; ------------------------------
;; Revert Dired and other buffers after changes to files in directories on disk.
;; Source: [[https://www.youtube.com/watch?v=51eSeqcaikM&list=PLEoMzSkcN8oNmd98m_6FoaJseUsa6QGm2&index=2][Dave Wilson]]
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed.
(global-auto-revert-mode 1)

;; Enable show-paren-mode (to visualize paranthesis)
;; (show-paren-mode 1) --> it seems to be on already.



;; ------------------------------
;; latex
;; ------------------------------




;; ------------------------------
;; recentf
;; ------------------------------
;; List recently opened files.
(recentf-mode 1)



;; ------------------------------
;; undo-tree
;; ------------------------------




;; ------------------------------
;; crux
;; ------------------------------
(use-package crux
  :ensure t)


;; ----------------------------
;; Editing
;; ----------------------------
(electric-pair-mode 1)

;; (global-set-key (kbd "M-SPC")
;; 		(defun my/cycle-spacing-impatient (&optional n preserve-nl-back)
;; 		  (interactive "*p")
;; 		  (cycle-spacing (if (= n 1) -1 n) preserve-nl-back 'fast)))

;; (global-set-key
;;  (kbd "C-w")
;;  (defun backward-kill-word-or-region (&optional arg)
;;    "Kill word backwards unless region is active,
;; kill region instead"
;;    (interactive)
;;    (if (region-active-p)
;;        (kill-region (region-beginning)
;; 				      (region-end))
;;      (backward-kill-word (or arg 1)))))

;; (advice-add 'kill-ring-save :around
;; 	    (defun kill-ring-save-advice (fun &rest args)
;; 	      "Save line to kill ring if region is inactive"
;; 	      (interactive)
;; 	      (if mark-active
;; 		  (funcall fun (region-beginning) (region-end))
;; 		(funcall fun (line-beginning-position)
;; 			 (line-beginning-position 2)))))


;; Some steals from crux. See prelude-mode.el
(global-set-key (kbd "s-o") (defun open-line-above (&optional arg)
			      (interactive)
			      (beginning-of-line)
			      (open-line (or arg 1))
			      (indent-according-to-mode)))

(global-set-key (kbd "M-o") (defun open-line-below (&optional arg)
			      (interactive)
			      (end-of-line)
			      (open-line (or arg 1))
			      (forward-line)
			      (indent-according-to-mode)
			      ))

(global-set-key (kbd "C-a")
		(defun back-to-indentation-or-beginning () (interactive)
		       (if (= (point) (progn (back-to-indentation) (point)))
			   (beginning-of-line))))


;; ----------------------------------
;; Features to add
;;  - [x] helm alternative --> ivy
;;  - [x] remove bells
;;  - [x] snippets
;;  - [x] pdf tools
;;  - [x] save when moving away from buffer --> super-save
;;  - [x] server-start
;;  - [x] crux goodies (join lines, etc) --> more work needed
;;  - [x] recentf
;;  - org mode
;;  - latex mode
;;  - whitespace-mode: see prelude-editor.el

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"                  ; Use lua-mode for files ending in .lua
  :interpreter "lua"                 ; Use lua-mode for Lua interpreter scripts
  :custom
  (lua-indent-level 4)               ; Set indentation level to 4 spaces
  :config
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq tab-width 4)     ; Set tab width to 4 spaces
              (setq indent-tabs-mode nil)))) ; Use spaces instead of tabs  
