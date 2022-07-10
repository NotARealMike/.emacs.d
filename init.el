;; _____________________________________________________________________________
;; Package management
;; _____________________________________________________________________________

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

;; Make sure use-package is used. This should only ever run once.
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
;; This avoids confusion as to what is loaded or not
(setq use-package-always-ensure t)

;; _____________________________________________________________________________
;; Global functionality
;; _____________________________________________________________________________

;; Make it easier to access emacs config
(set-register ?e (cons 'file "~/.emacs.d"))
(set-register ?i (cons 'file "~/.emacs.d/init.el"))

;; Easy commenting
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;; Cycle through windows easily
(global-set-key (kbd "C-<tab>") 'other-window)
(defun switch-to-prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-window)

;; Simplify yes-or-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically show a Lisp debugger when a command errors
(toggle-debug-on-error)

;; Make the right option key not act as meta, to let me type characters that need option
(setq ns-right-alternate-modifier 'none)

;; Theme and display configuration
(load-theme 'tango-dark t)
(column-number-mode)
(global-display-line-numbers-mode)
(global-visual-line-mode)

;; Natural language formatting and spelling
(setq sentence-end-double-space nil)
(setq ispell-program-name "/opt/homebrew/bin/ispell")
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

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

;; File structure
(setq org-directory "~/gtd")
(set-register ?g (cons 'file org-directory))

(setq org-agenda-files
      '("inbox.org"
	"tasks.org"
	"projects.org"))

(setq org-refile-targets
      '(("tasks.org" :maxlevel . 1)
	("projects.org" :maxlevel . 1)
	("rar.org" :maxlevel . 1)
	("media.org" :maxlevel . 1)))

;; Save all org buffers after refiling
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Workflow states
(setq org-todo-keywords
      '((sequence "BACKLOG(b)" "SCOPE(s)" "TODO(t)" "WAIT(w)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-capture-templates
    `(("t" "Task" entry (file "inbox.org")
       "* SCOPE %?\n%U\n%a" :prepend t)
      ("m" "Meeting notes" entry (file+datetree "meetings.org")
       "* %?\n%U" :tree-type week)))

(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Agenda configuration
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-log-mode-items '(closed clock state))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "TODO"
		((org-agenda-overriding-header "Next Tasks")))))

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
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "BACKLOG"
		((org-agenda-overriding-header "Project Backlog")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))))))

;; _____________________________________________________________________________
;; Babel
;; _____________________________________________________________________________

;; Automatically tangle the README.org file on save
(defun nrm/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/README.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nrm/org-babel-tangle-config)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

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
;; lsp-mode
;; _____________________________________________________________________________

(use-package lsp-mode
  :commands
  (lsp lsp-deferred lsp-register-custom-settings)
  :hook
  (go-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  (:map lsp-mode-map
	("M-." . xref-find-definitions))
  :config
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (setq lsp-eldoc-render-all t)

  ;; Performance hax from here: https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-idle-delay 0.200))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-ui-sideline-enable t
	lsp-ui-imenu-enable t
	lsp-ui-flycheck-enable t))

(use-package company
  :hook ((prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package yasnippet
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; _____________________________________________________________________________
;; go-mode
;; _____________________________________________________________________________

;; (setenv "GOPATH" "<~/>")
;; (setenv "GOROOT" "<output of "which go">")

(setenv "PATH"
	(concat
	 (getenv "GOPATH") "/bin:"
	 (getenv "GOROOT") "/bin:"
	 "/usr/local/bin:"
	 (getenv "PATH")))

;; (setq lsp-go-gopls-server-path "<output of "which gopls", eg ~/bin/gopls>")

(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Formating... && go fmt && echo Building... && go build -v && echo Testing... && go test -v")
  (setq compilation-read-command nil)
  ;; This needs to be here and not in a :hook statement because :hook
  ;; automatically sufixes '-hook' to 'gofmt-before-save'
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind
  (:map go-mode-map
	 ("M-," . compile)
	 ("s-l" . goto-line)))

(defun nrm/go-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	  (switch-to-buffer "*compilation*")))))

(add-hook 'compilation-mode-hook 'nrm/go-compilation-hook)

(setq compilation-scroll-output t)

;; Handle Go modules in large monorepos
(setq lsp-go-directory-filters ["-vendor" "-manifests"])
(lsp-register-custom-settings
 '(("gopls.memoryMode" "DegradeClosed")
   ("gopls.expandWorkspaceToModule" nil t)))

;; Configure goimports
;; (setq gofmt-command "<path to goimports, eg ~/bin/goimports>")
;; TODO: The following lines don't seem to cause the behaviour I expect. Fix them.
;; (setq lsp-go-goimports-local "<set of imports to separate, eg github.com/your-company>")
;; (setq gofmt-args '("-local" "<same as the variable above>"))

;; ___________________________________________________________________________
;; Beancount
;; ___________________________________________________________________________

;; (defun beancount-save () (interactive)
;;        (beancount-align-numbers (point-min) (point-max))
;;   (delete-trailing-whitespace)
;;   (save-buffer)
;;   )

;; (add-to-list 'load-path "~/.emacs.d/beancount-mode")
;; (require 'beancount)

;; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook #'outline-minor-mode)

;; ;;(define-key beancount-mode-map (kbd "s-s") 'beancount-save)
;; (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
;; (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)
