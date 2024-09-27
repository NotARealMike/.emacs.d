;; _____________________________________________________________________________
;; Start loading config
;; _____________________________________________________________________________

(toggle-debug-on-error)

(setq inhibit-startup-message t)

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

;; Make it easier to access emacs config
(set-register ?e (cons 'file "~/.emacs.d/README.org"))

;; Cycle through buffers easily
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)

;; Cycle through windows easily
(defun nrm/switch-to-prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'nrm/switch-to-prev-window)

;; Simplify yes-or-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; On MacOS, make the right option key not act as meta, to let me type characters that need option
(setq ns-right-alternate-modifier 'none)

;; _____________________________________________________________________________
;; Appearance
;; _____________________________________________________________________________

(setq ring-bell-function 'ignore)

(use-package ef-themes
  :config
  (load-theme 'ef-duo-dark t))

(use-package fontaine
  :custom
  (fontaine-presets
   '((regular
      :default-family "Menlo"
      :default-height 135)))
  :config
  (fontaine-set-preset 'regular))

(column-number-mode)
(global-display-line-numbers-mode)
(global-visual-line-mode)

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text nil)
  (visual-fill-column-enable-sensible-window-split t)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;; When wrapping, respect the paragraph indentation
(use-package adaptive-wrap
  :hook (visual-fill-column-mode . adaptive-wrap-prefix-mode))

;; Icons that can be used by several packages
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-minor-modes t))

(use-package golden-ratio)

;; _____________________________________________________________________________
;; Writing
;; _____________________________________________________________________________

;; Natural language formatting and spelling
(setq sentence-end-double-space nil)
(setq ispell-program-name "/opt/homebrew/bin/ispell")
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package nyan-mode
  :config (nyan-mode 1))

(use-package dad-joke)

;; _____________________________________________________________________________
;; Completion
;; _____________________________________________________________________________

;; VERTical Interactive COmpletion - update the minibuffer completions while typing
(use-package vertico
  :config
  (vertico-mode 1)
  :custom
  (vertico-count 20)
  (vertico-cycle t))

;; Order minibuffer completions by recency
(savehist-mode 1)

;; Rich annotations in the minibuffer
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Out of order pattern matching completion
;; Style dispatchers note: affix ! to invert match; affix & to match annotation instead of candidate
(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

;; Enhanced versions of builtin search and navigation commands
(use-package consult
  :custom
  (consult-line-start-from-top t)
  :bind (;; Prefix mnemonic: "alt search"
	 ;; Recursive grep
	 ("M-s M-g" . consult-grep)
	 ;; Search for file names recursively
	 ("M-s M-f" . consult-find)
	 ;; Search through the outline (headings) of the buffer
	 ("M-s M-o" . consult-outline)
	 ;; Search through the imenu items of the buffer
	 ("M-s M-i" . consult-imenu)
	 ;; Search the current buffer
	 ("C-s" . consult-line)
	 ;; Switch to another buffer, bookmark, or recently opened file
	 ;; Filters: b buffers; SPC hidden buffers; * modified buffers; f recent files; r registers; m bookmarks
	 ("C-x b" . consult-buffer)
	 ;; Interactively select item to yank from kill-ring
	 ("C-M-y" . consult-yank-from-kill-ring)
	 ;; Goto position at line:column
	 ("s-l" . consult-goto-line)))

;; Enable recent files as a virtual buffer source for consult-buffer
(recentf-mode 1)

;; Actions based on context
(use-package embark
  :bind
  ("C-." . embark-act)
  ("s-." . embark-dwim)
  (:map minibuffer-local-map
	;; Retains minibuffer behaviour
	("C-c C-c" . embark-collect)
	;; Exports contents to a buffer in an appropriate major mode
	("C-c C-e" . embark-export)))

(use-package embark-consult)

;; UI for completion at point; use M-SPC to insert a separator
(use-package corfu
  :config
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay (cons 1.0 0.1)))

;; Extra capfs
(use-package cape
  :config
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; _____________________________________________________________________________
;; Dired
;; _____________________________________________________________________________

(use-package dired
  ;; dired is not a -package.el package, so don't attempt to install it.
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (insert-directory-program "gls"))

(use-package dired-x
  ;; Also not a proper -package.el.
  :ensure nil
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package wdired
  :custom
  (wdired-use-dired-vertical-movement t))

;; _____________________________________________________________________________
;; IBuffer
;; _____________________________________________________________________________

(use-package ibuffer
  ;; ibuffer is not a -package.el package, so don't attempt to install it.
  :ensure nil
  :custom
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-saved-filter-groups
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
  :hook
  (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  (defun nrm/ibuffer-toggle-current-group()
    (interactive)
    (ibuffer-forward-filter-group)
    (ibuffer-backward-filter-group)
    (ibuffer-toggle-filter-group))
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("<tab>" . nrm/ibuffer-toggle-current-group)))

;; _____________________________________________________________________________
;; Ediff
;; _____________________________________________________________________________

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup t)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; _____________________________________________________________________________
;; Wgrep
;; _____________________________________________________________________________

(use-package wgrep)

;; _____________________________________________________________________________
;; Magit
;; _____________________________________________________________________________

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-diff-refine-hunk 'all)
  :bind ("C-x g" . magit-status))

;; _____________________________________________________________________________
;; Org
;; _____________________________________________________________________________

(use-package org
  :bind
  ("\C-cl" . org-store-link)
  ("s-a" . org-agenda)
  ("s-c" . org-capture)
  :custom
  (org-ellipsis " ▾")
  (org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-tag-alist
   '((:startgroup)
     ("@low" . ?l) ("@medium" . ?m) ("@high" . ?h)
     (:endgroup)
     (:startgroup)
     ("@planning" . ?p)
     ("@research" . ?r)
     ("@writing" . ?w)
     ("@coding" . ?c)
     ("@errand" . ?e)
     ("@buy" . ?b)
     (:endgroup)
     (:startgroup)
     ("@blocked" . ?z)
     ("@waiting" . ?x)
     (:endgroup)))
  ;; Record the date but not the time of day when a todo item is done
  (org-log-done 'time)
  (org-log-done-with-time nil)
  (org-log-into-drawer t)
  ;; Allow items to be refiled to the top level in a file, rather than under another headline
  (org-refile-use-outline-path 'file)
  ;; Show file and headline paths in the refile completion buffer
  (org-outline-path-complete-in-steps nil)
  ;; Refile targets to the top of files and headlines, rather than the end
  (org-reverse-note-order t)
  :config
  ;; Save all org buffers after refiling, to prevent entries being lost if Emacs crashes
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-agenda
  :ensure org
  :bind
  (:map org-agenda-mode-map
	("<return>" . nrm/org-agenda-switch-and-narrow))
  :custom
  (org-agenda-custom-commands
   '(("d" "Daily agenda"
      ((agenda "" ((org-agenda-span 'day)))
       (todo "PROG" ((org-agenda-overriding-header "In progress")))
       (tags-todo "-TODO=\"PROG\"+PRIORITY=\"A\"" ((org-agenda-overriding-header "Urgent tasks")))))
     ("n" "Non-blocked"
      ((tags-todo "-@blocked")))
     ("p" "Planning"
      ((tags-todo "+@planning" ((org-agenda-overriding-header "Planning tasks")))
       (tags-todo "-@low-@medium-@high" ((org-agenda-overriding-header "Untagged tasks")))))
     ("q" "Quick wins"
      ((tags-todo "+@low-@buy")))))
  ;; Display done items with their completion date
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-window-setup "current-window")
  ;; If an entry has a TODO label, don't check its children
  (org-agenda-todo-list-sublevels nil)
  :hook
  (org-agenda-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (defun nrm/org-agenda-switch-and-narrow ()
    (interactive)
    (org-agenda-switch-to)
    (org-narrow-to-subtree)))

;; _____________________________________________________________________________
;; Babel
;; _____________________________________________________________________________

;; Automatically tangle the README.org file on save
(defun nrm/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/README.org"))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nrm/org-babel-tangle-config)))

(setq org-confirm-babel-evaluate nil)

(use-package org-tempo
  :ensure nil
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell")))

;; _____________________________________________________________________________
;; Roam
;; _____________________________________________________________________________

(use-package org-roam
  :demand
  :custom
  (org-roam-directory "~/roam")
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-capture-templates
   (let ((default-file "roam-${slug}.org")
	 (default-header "#+title: ${title}\n#+category: ${title}\n#+date: %U\n#+filetags: "))
     `(("t" "Topic" plain
	"%?"
	:target (file+head ,default-file ,(concat default-header ":Topic:"))
	:unnarrowed t)
       ("a" "Project" plain
	"* Goals\n%?\n* Actions\n"
	:target (file+head ,default-file ,(concat default-header ":Project:AgendaSource:"))
	:unnarrowed t)
       ("p" "Person" plain
	"%?"
	:target (file+head ,default-file ,(concat default-header ":Person:"))
	:unnarrowed t)
       ("b" "Book" plain
	"- Author: %?\n- Notes: \n\n* Summary\n\n* Chapters\n\n* Comments\n"
	:target (file+head ,default-file ,(concat default-header ":Book:"))
	:unnarrowed t)
       ("c" "Company" plain
	"- Website: %?\n- Tech stack: \n\n* Notes\n\n* Applications\n"
	:target (file+head ,default-file ,(concat default-header ":Company:"))
	:unnarrowed t)
       ("j" "Trip" plain
	"* Flights/travel\n\n* Accommodation\n\n* Comments\n"
	:target (file+head ,default-file ,(concat default-header ":Trip:AgendaSource:"))
	:unnarrowed t)
       ("l" "Location" plain
	"* Comments\n%?\n* Visits\n\n* Ice cream\n\n* Restaurants\n\n* Points of interest\n"
	:target (file+head ,default-file ,(concat default-header ":Location:"))
	:unnarrowed t))))
  :bind (("s-r" . org-roam-node-find)
	 ;; Go to the file directly, skipping the capture prompt
	 ("s-g" . nil)
	 ("s-g t" . (lambda () (interactive) (org-roam-dailies-goto-today "d")))
	 ("s-g d" . (lambda () (interactive) (org-roam-dailies-goto-date nil "d")))
	 :map org-mode-map
	 ("C-c i" . org-roam-node-insert)
	 ("C-c b" . org-roam-buffer-toggle)
	 ("s-n" . org-roam-dailies-goto-next-note)
	 ("s-p" . org-roam-dailies-goto-previous-note))
  :config
  (org-roam-db-autosync-enable))

;; _____________________________________________________________________________
;; Org file structure
;; _____________________________________________________________________________

(defun nrm/generate-org-refile-targets ()
  (interactive)
  ;; Only this variable needs to be regenerated
  (setq roam-files (directory-files org-roam-directory t "org$"))
  (setq org-refile-targets
	'((roam-files :maxlevel . 3))))

;; Generate the refile target list when Emacs starts and also whenever a new Roam file is created (aprox)
(nrm/generate-org-refile-targets)
(add-hook 'org-capture-after-finalize-hook #'nrm/generate-org-refile-targets)

;; _____________________________________________________________________________
;; Org agenda
;; _____________________________________________________________________________

(defun nrm/roam-list-files-with-tag (tag-name)
  (mapcar #'org-roam-node-file
	  (seq-filter
	   (lambda (elt) (member tag-name (org-roam-node-tags elt)))
	   (org-roam-node-list))))

(setq nrm/org-agenda-generating-tag "AgendaSource")

(defun nrm/generate-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (nrm/roam-list-files-with-tag nrm/org-agenda-generating-tag)))

(defun nrm/toggle-org-agenda-files ()
  (interactive)
  (if (string= nrm/org-agenda-generating-tag "AgendaSource")
      (setq nrm/org-agenda-generating-tag "Work")
    (setq nrm/org-agenda-generating-tag "AgendaSource"))
  (message "Agenda files now generated by the tag: %s" nrm/org-agenda-generating-tag)
  (nrm/generate-org-agenda-files))

;; Generate the agenda file list when Emacs starts and also whenever a new Roam file is created (aprox)
(nrm/generate-org-agenda-files)
(add-hook 'org-capture-after-finalize-hook #'nrm/generate-org-agenda-files)

;; _____________________________________________________________________________
;; Org capture
;; _____________________________________________________________________________

(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/roam/Inbox.org") "* TODO %?\n%U")
	("c" "Context" entry (file "~/roam/Inbox.org") "* TODO %?\n%U\n%a")
	("w" "Work")
	("wi" "Inbox" entry (file "~/roam/Workbox.org") "* TODO %?\n%U")
	("wc" "Context" entry (file "~/roam/Workbox.org") "* TODO %?\n%U\n%a")))

;; _____________________________________________________________________________
;; Shell
;; _____________________________________________________________________________

(use-package vterm
  :commands vterm
  :hook (vterm-mode . goto-address-mode)
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

(global-set-key (kbd "s-t") 'vterm)

(use-package multi-vterm)

;; _____________________________________________________________________________
;; TRAMP
;; _____________________________________________________________________________

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  ;; Change this to get, for example, error messages only
  (tramp-verbose 3)
  ;; Load controlmaster options from ~/.ssh/config directly
  (tramp-use-ssh-controlmaster-options nil)
  :config
  (add-to-list 'tramp-connection-properties
	       (list "/ssh:" "direct-async-process" t)))

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
;; Compilation
;; _____________________________________________________________________________

(defun nrm/switch-to-compilation-buffer-hook (buffer status)
  (switch-to-buffer buffer)
  (delete-other-windows))

(add-hook 'compilation-finish-functions 'nrm/switch-to-compilation-buffer-hook)
(add-hook 'compilation-mode-hook 'goto-address-mode)

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
  (dolist (hook '(go-mode-hook java-mode-hook kotlin-mode-hook python-mode-hook rust-mode-hook))
    (add-hook hook 'eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil))

;; _____________________________________________________________________________
;; Go
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

;; _____________________________________________________________________________
;; Kotlin
;; _____________________________________________________________________________

(use-package kotlin-mode)

;; _____________________________________________________________________________
;; Rust
;; _____________________________________________________________________________

(use-package rustic
  :defer t
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-display-method 'ignore)
  (rustic-format-trigger 'on-compile))

(use-package rust-playground)

;; _____________________________________________________________________________
;; csv-mode
;; _____________________________________________________________________________

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :custom (csv-invisibility-default nil))

;; ___________________________________________________________________________
;; Beancount
;; ___________________________________________________________________________

(use-package beancount-mode
  :ensure nil
  :mode ("\\.beancount\\'" . beancount-mode)
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  :bind
  (:map beancount-mode-map
	("C-n" . outline-next-visible-heading)
	("C-p" . outline-previous-visible-heading)))

;; Copied from beancount-mode/etc/emacsrc
(defun beancount-format-file ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (call-process-region (point-min) (point-max) "bean-format" t (current-buffer))
    (goto-line line-no)
    (recenter)))

;; ___________________________________________________________________________
;; Local config
;; ___________________________________________________________________________

(let ((local-config "~/local-config.el"))
 (when (file-exists-p local-config)
   (load-file local-config)))

;; _____________________________________________________________________________
;; Finish loading config
;; _____________________________________________________________________________

;; (toggle-debug-on-error)

(message "Finished loading config at %s" (format-time-string "%T"))
(message "Emacs loaded in %s with %d garbage collections."
	 (format "%.2f seconds"
		 (float-time
		  (time-subtract after-init-time before-init-time)))
	 gcs-done)
