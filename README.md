For now, this is mostly a list of resources with some associated actions.

# [Emacs from scratch](https://github.com/daviwil/emacs-from-scratch)

A repo on GitHub with lots of interesting packages and configuration. It is an interesting source of inspiration. Some points to consider:

* structure of the repo
* packages and config to copy
* writing config as a literate org file

# Homebrew

Make sure that tools installed using Homebrew are accessible from Emacs.

Last time I installed brew, it told me this at the end. I should figure out how to add these to my emacs config.

```
==> Next steps:
- Run these two commands in your terminal to add Homebrew to your PATH:
    echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/mike/.zprofile
    eval "$(/opt/homebrew/bin/brew shellenv)"
- Run `brew help` to get started
- Further documentation:
    https://docs.brew.sh
```

# [libvterm](https://github.com/akermu/emacs-libvterm)

An emacs mode for running a terminal.

# LSP and gopls optimisations

Caveat: these may no longer be relevant after looking at the "Emacs from scratch" repo mentioned above.

Have a look through the [advertised main features](https://emacs-lsp.github.io/lsp-mode/page/main-features/) and see if there is anything else I want to add.

Have a look through the [options available in gopls](https://github.com/golang/tools/blob/master/gopls/doc/emacs.md).
