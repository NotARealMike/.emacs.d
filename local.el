;; Configuration of things that depend on the local machine, such as registers

;; _____________________________________________________________________________
;; Registers
;; _____________________________________________________________________________

;; _____________________________________________________________________________
;; Go environment
;; _____________________________________________________________________________

;; (setenv "GOPATH" "<~/>")
;; (setenv "GOROOT" "<output of "which go">")

(setenv "PATH"
	(concat
	 (getenv "GOPATH") "/bin:"
	 (getenv "GOROOT") "/bin:"
	 "/usr/local/bin:"
	 (getenv "PATH")
	 )
	)

;; (setq lsp-go-gopls-server-path "<output of "which gopls", eg ~/bin/gopls>")

;; Configure goimports
;; (setq gofmt-command "<path to goimports, eg ~/bin/goimports>")
;; TODO: The following lines don't seem to cause the behaviour I expect. Fix them.
;; (setq lsp-go-goimports-local "<set of imports to separate, eg github.com/your-company>")
;; (setq gofmt-args '("-local" "<same as the variable above>"))
