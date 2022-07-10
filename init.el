;; _____________________________________________________________________________
;; Global configuration
;; _____________________________________________________________________________
;; Stuff I want to apply everywhere

;; Assign registers to common files
(set-register ?e (cons 'file "~/.emacs.d"))
(set-register ?i (cons 'file "~/.emacs.d/init.el"))
(set-register ?g (cons 'file "~/org/gtd.org"))

;; Theme and display configuration
(load-theme 'tango-dark t)
(column-number-mode)
(global-display-line-numbers-mode)

;; Make the right option key not act as meta, to let me type characters that need option
(setq ns-right-alternate-modifier 'none)

;; Easy commenting
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Cycle through windows easily
(global-set-key (kbd "C-<tab>") 'other-window)
(defun switch-to-prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-window)

;; Simplify yes-or-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Natural language formatting and spelling
(setq sentence-end-double-space nil)
(setq ispell-program-name "/usr/local/bin/ispell")
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
;; Rainbow delimiters
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
;; Load extra configuration
;; _____________________________________________________________________________

(load-file "~/.emacs.d/local.el")

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
;; Magit
;; _____________________________________________________________________________

(use-package magit
  :bind ("C-x g" . magit-status))

;; _____________________________________________________________________________
;; Org-mode
;; _____________________________________________________________________________

(use-package org
  :hook (org-mode . auto-fill-mode)
  :init
  (setq org-agenda-files '("~/org/gtd.org"))
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-capture-templates
	'("t" "Task" entry (file+headline "~/org/gtd.org" "Tasks") "** NEXT %i%?"))
  (setq org-refile-targets
	'(("~/org/gtd.org" :maxlevel . 5)))
  (setq org-todo-keywords
	'((sequence "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CNLD(c)")))
  (setq org-log-done 'time)
  :bind
  ("\C-cl" . org-store-link)
  ("s-c" . org-capture)
  ("s-a" . org-agenda))

;; _____________________________________________________________________________
;; Go-mode
;; _____________________________________________________________________________

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-register-custom-settings)
  :hook (go-mode . lsp-deferred)
  :bind
  (:map lsp-mode-map
	("M-." . xref-find-definitions)))

(setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))

;; Performance hax from here: https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.200)
(setq lsp-log-io nil) ; if set to true can cause a performance hit


;;Optional - provides fancier overlays.

(use-package lsp-ui
  :commands lsp-ui-mode
  :init)

;; I'm not sure what these do. They come from here:
;; https://arenzana.org/posts/2019-12-03-emacs-go-mode-revisited/
;; Out of the three, I think only the second still exists.
;; (setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
;; (setq lsp-gopls-complete-unimported t)

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :hook ((emacs-lisp-mode go-mode) . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;;Optional - provides snippet support.

(use-package yasnippet
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;;lsp-ui-doc-enable is false because I don't like the popover that shows up on the right
;;I'll change it if I want it back
;; Also from here: https://arenzana.org/posts/2019-12-03-emacs-go-mode-revisited/
(setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

(defun custom-go-mode ()
  (display-line-numbers-mode 1))

(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Formating... && go fmt && echo Building... && go build -v && echo Testing... && go test -v")
  (setq compilation-read-command nil)
  (add-hook 'go-mode-hook 'custom-go-mode)
  ;; This needs to be here and not in a :hook statement because :hook
  ;; automatically sufixes '-hook' to 'gofmt-before-save'
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (
	 :map go-mode-map
	      ("M-," . compile)
	      ("s-l" . goto-line)))

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	  (switch-to-buffer "*compilation*")))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(setq compilation-scroll-output t)

;; Handle Go modules in large monorepos
(setq lsp-go-directory-filters ["-vendor" "-manifests"])
(lsp-register-custom-settings
 '(("gopls.memoryMode" "DegradeClosed")
   ("gopls.expandWorkspaceToModule" nil t)))

