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

### [mthdspool Ruby gem](http://github.com/jpablobr/mthdspool)

Installation with RubyGems, simply open a terminal and type:

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
      "~/.rvm/gems/ruby-1.9.3-p0@global/bin/mthdspool --object %s --filter %s")
```

### [Anything.el](http://www.emacswiki.org/emacs/Anything)
You will also need to install
[anything-mode](http://www.emacswiki.org/emacs/Anything). See
documentation for how to install.


## Usage

When you run:

    M-x anything-ruby-mthds

you'll see the `anything` prompt where you'll be able to filter the
system Ruby objects methods (by using Ruby `Objectspace` class). For
example, if you type:

    File#res
    
It will do __two__ things: 

* First, find all the object that their name matches `File`.
* Second, filter each of those object methods by the ones that their name matches `res`.

This is done by appending a `#` character after the object you want to
see its methods. 

So, with the `File#res` query (in my system), will display the
following methods.

    >IM File::Stat#respond_to?
    >IM File::Stat#respond_to_missing?
    >IM File#respond_to?
    >IM File#respond_to_missing?
    >IM Gem::FilePermissionError#respond_to?
    >IM Gem::FilePermissionError#respond_to_missing?

Finally it will `kill/copy` the method you were looking for. :D

## Copyright

Copyright 2012 Jose Pablo Barrantes. MIT Licence, so go for it.
