# emacs.d

This is my Emacs config. It's setup to work a number of different
languages, and use Vim-style bindings via the excellent [evil-mode][].

## What's in the box?

Out of the box there are some pretty major enhancements to Emacs, including:

- Great OS X support
- Org mode on top of Dropbox (`~/Dropbox/Org`)
- Projectile ready to go
- Replacement of the built-in <kbd>M-x</kbd> with `smex`
- Smartparens
- Use of `ido` everywhere

Support for a number of languages and file formats including but not
limited to:

- [Clojure][]
- [Erlang][]
- [Go][]
- [Haskell][]
- [HTML][]
- [Javascript][]
- [Lisp][]
- [Markdown][]
- [Python][]
- [Ruby][]
- [Ruby on Rails][]
- [Textile][]

## Requirements

* Emacs 23 or greater (note that Emacs 24 is required for some
  functionality, and will likely become the minimum required version
  some time soon.)

## Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

```
git clone https://github.com/jcf/emacs.d.git ~/.emacs.d
```

Upon starting up Emacs for the first time, further third-party
packages will be automatically downloaded and installed.

[evil-mode]: https://github.com/emacsmirror/evil

[Clojure]: https://github.com/jcf/emacs.d/blob/master/lisp/init-clojure.el
[Erlang]: https://github.com/jcf/emacs.d/blob/master/lisp/init-erlang.el
[Go]: https://github.com/jcf/emacs.d/blob/master/lisp/init-go.el
[HTML]: https://github.com/jcf/emacs.d/blob/master/lisp/init-html.el
[Haskell]: https://github.com/jcf/emacs.d/blob/master/lisp/init-haskell.el
[Javascript]: https://github.com/jcf/emacs.d/blob/master/lisp/init-javascript.el
[Lisp]: https://github.com/jcf/emacs.d/blob/master/lisp/init-lisp.el
[Markdown]: https://github.com/jcf/emacs.d/blob/master/lisp/init-markdown.el
[Python]: https://github.com/jcf/emacs.d/blob/master/lisp/init-python.el
[Ruby]: https://github.com/jcf/emacs.d/blob/master/lisp/init-ruby.el
[Ruby on Rails]: https://github.com/jcf/emacs.d/blob/master/lisp/init-rails.el
[Textile]: https://github.com/jcf/emacs.d/blob/master/lisp/init-textile.el
