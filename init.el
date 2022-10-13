;; _____________________________________________________________________________
;; Start loading config
;; _____________________________________________________________________________

(toggle-debug-on-error)

(switch-to-buffer "*Messages*")
(message "Started loading config at %s" (format-time-string "%T"))

;; _____________________________________________________________________________
;; Package management
;; _____________________________________________________________________________

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; _____________________________________________________________________________
;; Global configuration
;; _____________________________________________________________________________

(setq inhibit-startup-message t)

;; Make it easier to access emacs config
(set-register ?e (cons 'file "~/.emacs.d/README.org"))

;; Easy commenting
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;; Cycle through buffers easily
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)

;; Cycle through windows easily
(global-set-key (kbd "C-<tab>") 'other-window)
(defun switch-to-prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-window)

;; Simplify yes-or-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make the right option key not act as meta, to let me type characters that need option
(setq ns-right-alternate-modifier 'none)

;; Theme and display configuration
(load-theme 'tango-dark t)
(column-number-mode)
(global-display-line-numbers-mode)
(global-visual-line-mode)

;; Icons that can be used by several packages
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-minor-modes t))

;; Natural language formatting and spelling
(setq sentence-end-double-space nil)
(setq ispell-program-name "/opt/homebrew/bin/ispell")
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package nyan-mode
  :config (nyan-mode 1))

(use-package dad-joke)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package ivy
  :demand
  :bind (:map ivy-minibuffer-map
	      ("TAB" . ivy-alt-done))
  :config
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

;; Stop ./ and ../ from showing up in filepath completions
(setq ivy-extra-directories nil)

(use-package swiper
  :demand
  :bind ("C-s" . swiper))

(use-package counsel
  :config (counsel-mode 1)
  :bind ("C-c i" . counsel-imenu))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package company
  :hook ((prog-mode text-mode) . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; _____________________________________________________________________________
;; Dired
;; _____________________________________________________________________________

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom ((dired-listing-switches "-lah --group-directories-first"))
  :config
  (require 'dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq insert-directory-program "gls"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; _____________________________________________________________________________
;; IBuffer
;; _____________________________________________________________________________

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Side effects" (or
				(derived-mode . helpful-mode)
				(and
				 (name . "^\\*")
				 (size-lt . 1))))
	       ("Dired" (mode . dired-mode))
	       ("Org" (mode . org-mode))
	       ("Source code" (or
			       (derived-mode . prog-mode)
			       (derived-mode . protobuf-mode)))
	       ("Version control" (derived-mode . magit-section-mode))))))

(add-hook 'ibuffer-mode-hook
	  (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-default-sorting-mode 'filename/process)

(setq ibuffer-fontification-alist
      '((100 (eq major-mode 'java-mode) magit-process-ng)
	(10 buffer-read-only font-lock-constant-face)
	(15 (and buffer-file-name
		 (string-match ibuffer-compressed-file-name-regexp
			       buffer-file-name))
	    font-lock-doc-face)
	(20 (string-match "^\\*" (buffer-name)) font-lock-keyword-face)
	(25 (and (string-match "^ " (buffer-name))
		 (null buffer-file-name))
	    italic)
	(30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
	(35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
	(40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer)))

(defun nrm/ibuffer-toggle-current-group()
  (interactive)
  (ibuffer-forward-filter-group)
  (ibuffer-backward-filter-group)
  (ibuffer-toggle-filter-group))

(define-key ibuffer-mode-map (kbd "<tab>") 'nrm/ibuffer-toggle-current-group)

;; _____________________________________________________________________________
;; IBuffer
;; _____________________________________________________________________________

(use-package wgrep)

;; _____________________________________________________________________________
;; Magit
;; _____________________________________________________________________________

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind ("C-x g" . magit-status))

;; _____________________________________________________________________________
;; Org
;; _____________________________________________________________________________

(use-package org
  :bind
  ("\C-cl" . org-store-link)
  ("s-a" . org-agenda)
  ("s-c" . org-capture)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; _____________________________________________________________________________
;; Babel
;; _____________________________________________________________________________

;; Automatically tangle the README.org file on save
(defun nrm/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/README.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nrm/org-babel-tangle-config)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

;; Workflow states
(setq org-todo-keywords
      '((sequence "SCOPE(s)" "BACKLOG(b)" "TODO(t)" "WAIT(w)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; File structure
(setq org-directory "~/gtd")
(set-register ?g (cons 'file (concat org-directory "/actions.org")))

(setq org-agenda-files
      '("inbox.org"
	"meetings.org"
	"actions.org"))

;; Allow hadlines to be refiled to top level in a file, rather than under another headline
(setq org-refile-use-outline-path 'file)
;; Show file and headline paths in the refile completion buffer
(setq org-outline-path-complete-in-steps nil)
;; Refile targets to the top of files and headlines, rather than the end
(setq org-reverse-note-order t)

(setq org-refile-targets
      '(("actions.org" :maxlevel . 1)
	("rar.org" :maxlevel . 1)
	("media.org" :maxlevel . 1)
	("meetings.org" :maxlevel . 1)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-capture-templates
    `(("t" "Task" entry (file "inbox.org")
       "* SCOPE %?\n%U\n%a" :prepend t)
      ("m" "Meeting notes" entry (file "meetings.org")
       "* REVIEW %?\n%t" :prepend t)))

;; Agenda configuration
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-window-setup "current-window")
;; If an entry has a TODO label, don't check its children
(setq org-agenda-todo-list-sublevels nil)

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "REVIEW"
		((org-agenda-overriding-header "In Review")
		 (org-agenda-files org-agenda-files)))
	  (todo "TODO"
		((org-agenda-overriding-header "Ready for Work")
		 (org-agenda-files org-agenda-files)))
	  (todo "SCOPE"
		((org-agenda-overriding-header "In Scoping")
		 (org-agenda-files org-agenda-files)))))

	("w" "Workflow Status"
	 ((todo "WAIT"
		((org-agenda-overriding-header "Waiting")
		 (org-agenda-files org-agenda-files)))
	  (todo "REVIEW"
		((org-agenda-overriding-header "In Review")
		 (org-agenda-files org-agenda-files)))
	  (todo "TODO"
		((org-agenda-overriding-header "Ready for Work")
		 (org-agenda-files org-agenda-files)))
	  (todo "SCOPE"
		((org-agenda-overriding-header "In Scoping")
		 (org-agenda-files org-agenda-files)))
	  (todo "BACKLOG"
		((org-agenda-overriding-header "Project Backlog")
		 (org-agenda-files org-agenda-files)))))))

;; _____________________________________________________________________________
;; Shell
;; _____________________________________________________________________________

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

(global-set-key (kbd "s-t") 'vterm)

(use-package multi-vterm)

;; _____________________________________________________________________________
;; Rainbow delimiters
;; _____________________________________________________________________________

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-background 'rainbow-delimiters-base-error-face "#e6194b")
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#e6194b")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#f58231")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffe119")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#bfef45")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#aaffc3")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#42d4f4")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#4363d8")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#911eb4")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#f032e6"))

;; _____________________________________________________________________________
;; imenu-list
;; _____________________________________________________________________________

(use-package imenu-list
  :bind ("s-i" . imenu-list-smart-toggle)
  :custom (imenu-list-focus-after-activation t))

;; _____________________________________________________________________________
;; Compilation
;; _____________________________________________________________________________

(defun nrm/compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	  (switch-to-buffer "*compilation*")))))

(add-hook 'compilation-mode-hook 'nrm/compilation-hook)

(setq compilation-scroll-output t)

;; _____________________________________________________________________________
;; Flymake
;; _____________________________________________________________________________

(use-package flymake
  :bind
  (:map flymake-mode-map
	 ("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)))

;; _____________________________________________________________________________
;; Eglot
;; _____________________________________________________________________________

(use-package eglot
  :init
  (dolist (hook '(go-mode-hook python-mode-hook java-mode-hook))
    (add-hook hook 'eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil))

;; _____________________________________________________________________________
;; go-mode
;; _____________________________________________________________________________

(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Formating... && go fmt && echo Building... && go build -v && echo Testing... && go test -v")
  (setq compilation-read-command nil)
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Configure goimports
;; (setq gofmt-command "<path to goimports, eg ~/bin/goimports>")

;; ___________________________________________________________________________
;; Beancount
;; ___________________________________________________________________________

(use-package beancount-mode
  :ensure nil
  :mode ("\\.beancount\\'" . beancount-mode)
  :config
  (add-hook 'beancount-mode-hook 'outline-minor-mode)
  (add-hook 'save-buffer 'beancount-format-file)
  :bind
  (:map beancount-mode
	("C-c C-n" . outline-next-visible-heading)
	("C-c C-p" . outline-previous-visible-heading)))

;; Copied from beancount-mode/etc/emacsrc
(defun beancount-format-file ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (call-process-region (point-min) (point-max) "bean-format" t (current-buffer))
    (goto-line line-no)
    (recenter)))

(set-register ?b (cons 'file "~/beans"))

;; ___________________________________________________________________________
;; Local config
;; ___________________________________________________________________________

(let ((local-config "~/local-config.el"))
 (when (file-exists-p local-config)
   (load-file local-config)))

;; _____________________________________________________________________________
;; Finish loading config
;; _____________________________________________________________________________

(toggle-debug-on-error)

(message "Finished loading config at %s" (format-time-string "%T"))
(message "Emacs loaded in %s with %d garbage collections."
	 (format "%.2f seconds"
		 (float-time
		  (time-subtract after-init-time before-init-time)))
	 gcs-done)
