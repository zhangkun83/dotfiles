This is my Emacs configuration.

I am using it with Emacs 26.1

Installation
============

Compiling Emacs
---------------

I prefer compiling Emacs from source, configured with
 `--with-x-toolkit=lucid` and `--without-xaw3d`. The lucid toolkit has
 a slick look-and-feel, but its 3D scroll bar is so ugly that I would
 disable it.

Also, make sure certain libraries' development packages are
installed. Emacs will still build without them, but will lack certain
features. Pay attention to the output of `./configure`, which will
show what features are enabled. On Ubuntu and Mint I have been hit by
missing following libraries:

 - libxft-dev : freetype fonts support
 - libxml2-dev : eww mode

Tags
----

Either **etags** or the **ctags** that comes with major Linux
distributions won't correctly parse Java generics. **exuberant etags**
does parse Java generics correctly, but is buggy. I find
[ctags.io](https://github.com/universal-ctags/ctags) works best, which
is what my code indexing tool (`zkindex`) uses.

Font
----

To configure the font of the GUI (menu etc), append `Xresources` to
`~/.Xresources`([ref](http://www.nongnu.org/emacsdoc-fr/manuel/lucid-resources.html)).
Run `xrdb ~/.Xresources` to load it.

Supporting scripts
------------------

Create symbolic links under any directory in your search path, for the
scripts under the `bin` directory. They provide better support for
emacs server/client, and are also needed by various custom
functionalities.

Environments
------------

Put those in `~/.profile` so that the supporting scripts are in
`$PATH` after you logged in:

```bash
export PATH=$HOME/.emacs.d/bin:$PATH
```

Put those in `~/.bashrc` so that the system-wide default editor will
be `emo`, which starts an Emacs server named `_daemon_` and opens
the file in that server.  It also assign an alias `e` to the editor.
In `init.el`, `$EDITOR` is set to `open-in-emacs-server` so that `e`
in shell mode would open a file in current Emacs session.

```bash
if [ -z "$EDITOR" ]; then
    export EDITOR="emo"
fi
alias e="$EDITOR"
```
