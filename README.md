Emacs Anything Ruby Mthds
========================

Quick fuzzy search of Ruby objects methods.

## Functions:

```
M-x anything-ruby-mthds
```

## Installation:

In your emacs config:

```
(add-to-list 'load-path "~/.emacs.d/load/path/anything-ruby-mthds.el")
(require 'anything-ruby-mthds)
```

There is no need to setup load-path with add-to-list if you copy
`anything-ruby-mthds.el` to load-path directories.

## Requirements

### mthdspool

With RubyGems, simply open a terminal and type:

    $ {sudo} gem install mthdspool

Local installation:

[Download](http://github.com/jpablobr/mthdspool/download) the tarball package and:

    $ tar -xvzf mthdspool-{version}.tgz

or

    $ git clone https://github.com/jpablobr/mthdspool.git

then 

    $ cd mthdspool-{version}
    $ {sudo} rake install

__Important note:__ If using `rvm` you might want to specify the path
to the `mthdspool` script in your `~/.emacd` like so:

```lisp
(setq anything-ruby-mthds-object-cmd
      "~/.rvm/gems/ruby-1.9.3-p0@global/bin/mthdspool -object %s -filter %s")
```

### Anything.el
You will also need to install
[anything-mode](http://www.emacswiki.org/emacs/Anything). See
documentation for how to install.


## Copyright

Copyright 2012 Jose Pablo Barrantes. MIT Licence, so go for it.
