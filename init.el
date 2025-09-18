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
;; Shortcuts
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
;; Tweak recentering to be more comfortable
(setq recenter-positions '(middle 0.1 0.9))

(when (memq system-type '(darwin))
  ;; Make the right option key not act as meta, to let me type characters that need the option key
  (setq ns-right-alternate-modifier 'none)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; _____________________________________________________________________________
;; Appearance
;; _____________________________________________________________________________

(setq ring-bell-function 'ignore)

(setq frame-title-format nil)
(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)

(add-to-list 'default-frame-alist '(alpha . (95 . 80)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text nil)
  (visual-fill-column-enable-sensible-window-split t)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

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
  (doom-modeline-buffer-encoding nil))

(use-package nyan-mode
  :config (nyan-mode 1))

(use-package golden-ratio)

(use-package olivetti
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body width 80))

(use-package logos
  :hook (logos-focus-mode . nrm/present-funs)
  :custom
  (logos-outlines-are-pages t)
  :config
  (setq-default logos-hide-cursor t
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only t
                logos-olivetti t)
  (defun nrm/present-funs ()
    (visual-fill-column-mode 0)
    (display-line-numbers-mode 0))
  :bind
  ("C-x n n" . logos-narrow-dwim)
  ("C-x ]" . logos-forward-page-dwim)
  ("C-x [" . logos-backward-page-dwim)
  (:map logos-focus-mode-map
        ("<right>" . logos-forward-page-dwim)
        ("<left>" . logos-backward-page-dwim)))

;; _____________________________________________________________________________
;; Writing
;; _____________________________________________________________________________

(use-package text-mode
  :ensure nil
  :hook (text-mode . flyspell-mode)
  :custom
  ;; Recent versions of text-mode automatically add an ispell capf
  ;; This annoys me to no end so I disable it here
  (text-mode-ispell-word-completion nil)
  (sentence-end-double-space nil)
  (ispell-program-name "/opt/homebrew/bin/ispell"))

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
  :config
  (defun nrm/consult-grep-dwim ()
    (interactive)
    (if (or (vc-root-dir)
            (derived-mode-p 'magit-section-mode))
        (consult-git-grep)
      (consult-grep)))
  :bind (;; Prefix mnemonic: "alt search"
         ;; Recursive grep
         ("M-s M-g" . nrm/consult-grep-dwim)
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
  :bind
  (:map dired-mode-map
        ("b" . dired-up-directory))
  :custom
  (insert-directory-program "gls")
  (dired-listing-switches "-lah --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-x
  ;; Also not a proper -package.el.
  :ensure nil
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package wdired
  :custom
  (wdired-use-dired-vertical-movement t))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
	("<tab>" . dired-subtree-toggle)))

;; _____________________________________________________________________________
;; IBuffer
;; _____________________________________________________________________________

(use-package ibuffer
  ;; ibuffer is not a -package.el package, so don't attempt to install it.
  :ensure nil
  :custom
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   (list
    (cons "Projects" (nrm/ibuffer-project-filter-groups))
    '("Types"
      ("Dired" (mode . dired-mode))
      ("Org" (mode . org-mode))
      ("Version control" (derived-mode . magit-section-mode))
      ("Source code" (derived-mode . prog-mode)))))
  :hook
  (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Projects")))
  :config
  (defun nrm/ibuffer-project-filter-groups ()
    (project--read-project-list)
    (mapcar (lambda (project)
              (let ((root-dir (car project))
                    (name (file-name-nondirectory (directory-file-name (car project)))))
                ;; Some modes list their default-directory as relative paths, others as absolute paths
                `(,name (or
                         (directory . ,root-dir)
                         (directory . ,(expand-file-name root-dir))))))
            project--list))
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
  :hook (org-mode . org-indent-mode)
  :custom
  (org-ellipsis " ▾")
  (org-startup-folded 'content)
  ;; Always add blank lines before inserted headings, never before list items
  (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  ;; Allow items to be refiled to the top level in a file, rather than under another headline
  (org-refile-use-outline-path 'file)
  ;; Show full file and headline paths in the refile completion buffer
  (org-outline-path-complete-in-steps nil)
  ;; Refile targets to the top of files and headlines, rather than the end
  (org-reverse-note-order t)
  (org-confirm-babel-evaluate nil)
  :config
  ;; Load the backend required to transcode org to markdown
  (require 'ox-md)
  ;; Save all org buffers after refiling, to prevent entries being lost if Emacs crashes
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  ;; Automatically tangle the literate Emacs config file on save
  (defun nrm/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.emacs.d/README.org"))
      (org-babel-tangle)))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nrm/org-babel-tangle-config))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-tempo
  :ensure nil
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell")))

(use-package org
  :bind
  ("s-a" . org-agenda)
  ("s-c" . org-capture)
  :custom
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
  ;; By default, tasks have the lowest possible priority
  (org-priority-default org-priority-lowest))

(use-package org-agenda
  :ensure org
  :bind
  (:map org-agenda-mode-map
        ("<return>" . nrm/org-agenda-switch-and-narrow))
  :custom
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((todo "PROG" ((org-agenda-overriding-header "In progress")))
       (tags-todo "-TODO=\"PROG\"+PRIORITY=\"A\"" ((org-agenda-overriding-header "Urgent tasks")))
       (agenda "" ((org-agenda-span 'week)))))
     ("p" "Planning"
      ((tags-todo "-@low-@medium-@high" ((org-agenda-overriding-header "Inbox: Untagged tasks")))
       (tags-todo "+@waiting" ((org-agenda-overriding-header "Waiting for someone")))
       (tags-todo "+@blocked" ((org-agenda-overriding-header "Blocked on another task")))
       (tags-todo "+@planning" ((org-agenda-overriding-header "Planning tasks")))
       (todo "PROG" ((org-agenda-overriding-header "In progress")))
       (tags-todo "-TODO=\"PROG\"+PRIORITY=\"A\"" ((org-agenda-overriding-header "Current priorities")))
       (tags-todo "-TODO=\"PROG\"+PRIORITY=\"B\"" ((org-agenda-overriding-header "Next up")))
       (tags-todo "-TODO=\"PROG\"-PRIORITY=\"A\"-PRIORITY=\"B\"-@waiting-@blocked" ((org-agenda-overriding-header "Everything else")))))
     ("q" "Quick wins"
      ((tags-todo "+@low-@buy-@waiting-@blocked")))))
  ;; Display done items with their completion date
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-window-setup "current-window")
  ;; If an entry has a TODO label, don't check its children
  (org-agenda-todo-list-sublevels nil)
  :hook
  (org-agenda-mode . (lambda () (display-line-numbers-mode -1)))
  (org-agenda-mode . hl-line-mode)
  :config
  (defun nrm/org-agenda-switch-and-narrow ()
    (interactive)
    (org-agenda-switch-to)
    (org-narrow-to-subtree)))

;; _____________________________________________________________________________
;; Roam
;; _____________________________________________________________________________

(use-package org-roam
  :demand
  :custom
  (org-roam-directory "~/roam")
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?" :target
      (file+head "%<%Y-%m-%d>.org"
                 "#+title: %<%Y-%m-%d>\n#+STARTUP: showeverything\n\n* Tasks [/]\n\n- [ ]"))))
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
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
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
  (delete-dups (mapcar #'org-roam-node-file
                       (seq-filter
                        (lambda (elt) (member tag-name (org-roam-node-tags elt)))
                        (org-roam-node-list)))))

(setq nrm/org-agenda-generating-tag "AgendaSource")

(defun nrm/generate-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (nrm/roam-list-files-with-tag nrm/org-agenda-generating-tag))
  (message "Agenda files now generated by the tag: %s" nrm/org-agenda-generating-tag))

;; Generate the agenda file list when Emacs starts and also whenever a new Roam file is created (aprox)
(nrm/generate-org-agenda-files)
(add-hook 'org-capture-after-finalize-hook #'nrm/generate-org-agenda-files)

;; _____________________________________________________________________________
;; Org capture
;; _____________________________________________________________________________

(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/roam/Inbox.org") "* TODO %?\n%U" :empty-lines 1)
        ("c" "Context" entry (file "~/roam/Inbox.org") "* TODO %?\n%U\n%a" :empty-lines 1)))

;; _____________________________________________________________________________
;; Shell
;; _____________________________________________________________________________

(use-package vterm
  :hook (vterm-mode . goto-address-mode)
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm
  :bind
  ("s-T" . multi-vterm)
  ("s-t" . multi-vterm-next))

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
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-sync-connect nil))

;; _____________________________________________________________________________
;; Go
;; _____________________________________________________________________________

(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . (lambda ()
                     (setq-local compilation-read-command nil)
                     (setq-local compile-command
                                 (concat
                                  "echo Formating..."
                                  "&& go fmt "
                                  "&& echo Building... "
                                  "&& go build -v "
                                  "&& echo Testing... "
                                  "&& go test -v "))))
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))

;; _____________________________________________________________________________
;; Kotlin
;; _____________________________________________________________________________

(use-package kotlin-mode
  :hook (kotlin-mode . (lambda ()
                         (setq-local compile-command "./gradlew build")
                         (indent-tabs-mode -1))))

;; _____________________________________________________________________________
;; Rust
;; _____________________________________________________________________________

(use-package rustic
  :defer t
  :hook
  (rustic-mode . (lambda ()
                   (setq-local compilation-read-command nil)
                   (setq-local compile-command
                               (concat
                                "echo Formatting... "
                                "&& cargo fmt "
                                "&& echo Linting... "
                                "&& cargo clippy --benches --tests --all-features --all-targets -- -D warnings "
                                "&& echo Testing... "
                                "&& cargo test "))))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-display-method 'ignore)
  (rustic-format-trigger 'on-compile))

(use-package rust-playground)

;; _____________________________________________________________________________
;; File formats
;; _____________________________________________________________________________

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :custom (csv-invisibility-default nil))

(use-package json-mode
  :custom (js-indent-level 2))

;; ___________________________________________________________________________
;; Beancount
;; ___________________________________________________________________________

(use-package beancount
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
