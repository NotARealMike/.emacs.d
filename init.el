;; _____________________________________________________________________________
;; Global configuration
;; _____________________________________________________________________________
;; Stuff I want to apply everywhere

(set-register ?e (cons 'file "~/.emacs.d"))
(set-register ?i (cons 'file "~/.emacs.d/init.el"))
(set-register ?g (cons 'file "~/org/gtd.org"))

(column-number-mode)
(global-display-line-numbers-mode)
;(global-set-key ("s-l") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(setq sentence-end-double-space nil)

(setq ispell-program-name "/usr/local/bin/ispell")
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(defalias 'yes-or-no-p 'y-or-n-p)

;; _____________________________________________________________________________
;; Load extra configuration
;; _____________________________________________________________________________

(load-file "~/.emacs.d/local.el")

;; _____________________________________________________________________________
;; MELPA
;; _____________________________________________________________________________

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; _____________________________________________________________________________
;; Magit
;; _____________________________________________________________________________

(use-package magit
  :bind ("C-x g" . magit-status))

;; _____________________________________________________________________________
;; Org-mode
;; _____________________________________________________________________________

(use-package org
  :ensure t
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
  :ensure t
  :commands (lsp lsp-deferred)
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
  :ensure t
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
  :ensure t
  :hook ((emacs-lisp-mode go-mode) . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;;Optional - provides snippet support.

(use-package yasnippet
  :ensure t
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
  :ensure t
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes
   '("3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" default))
 '(ns-right-alternate-modifier 'none)
 '(package-selected-packages
   '(magit protobuf-mode ac-ispell abyss-theme go-mode lsp-mode gnu-elpa-keyring-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'magit-clean 'disabled nil)
