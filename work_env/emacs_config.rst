******************************
:program:`emacs` Configuration
******************************

If you use :program:`emacs` as your editor,
you may want to amend your configuration with some or all of the snippets below to add syntax highlighting for the various file types commonly used in the Salish Sea MEOPAR project,
and to improve your :program:`emacs` editing experience in other ways.

The snippets go in your :file:`$HOME/.emacs` file and some of them assume that you also have a :file:`$HOME/elisp/` directory to contain downloaded mode files,
etc.
If you don't already have one or both of those,
go ahead and create them with:

.. code-block:: bash

    touch $HOME/.emacs
    mkdir $HOME/elisp


:file:`.emacs` Snippets
=======================

You can use as many or as few of these as you wish by adding them to your :file:`$HOME/.emacs` file.
You will need to restart :program:`emacs` for the changes to take effect.

Enable syntax highlighting whenever possible:

.. code-block:: scheme

    ;; enable syntax highlighting
    (global-font-lock-mode 1)

Disable display of the splash screen when :program:`emacs` starts:

.. code-block:: scheme

    ;; don't display the splash screen
    (setq inhibit-startup-message t)

Display line and column numbers in the mode line:

.. code-block:: scheme

    ;; show line and column numbers in mode line
    (line-number-mode 1)
    (column-number-mode 1)

Force the use of spaces instead of tab characters:

.. code-block:: scheme

    ;; force emacs to always use spaces instead of tab characters
    (setq-default indent-tabs-mode nil)

Set the default tab width to 4 spaces.
(Many modes override this setting.):

.. code-block:: scheme

    ;; set default tab width to 4 spaces
    (setq default-tab-width 4)
    (setq tab-width 4)

Show trailing whitespace characters in red.
Trailing whitespace
(i.e. spaces,
tabs,
etc. at the ends of lines or empty lines at the ends of files)
is a terrible thing that should be eliminated with extreme prejudice.
This setting makes it stick out like a sore thumb.
The :kbd:`M-x delete-trailing-whitespace` command deletes all trailing whitespace characters from a buffer.
Please commit whitespace deletions separately from other file modifications to make reviewing and merging easier.

.. code-block:: scheme

    ;; default to showing trailing whitespace
    (setq-default show-trailing-whitespace t)

Automatically use :kbd:`f90-mode` for files whose names end with :kbd:`.F90`,
:kbd:`.h90`,
or that start with :kbd:`namelist`.
These patterns are applied in addition to the file name patterns that normally trigger :kbd:`f90-mode`.

.. code-block:: scheme

    ;; use f90-mode for .F90, .h90, and namelist files
    (add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))
    (add-to-list 'auto-mode-alist '("\\.h90\\'" . f90-mode))
    (add-to-list 'auto-mode-alist '("namelist*" . f90-mode))


Additional Editing Modes
========================

These instructions are for installing an configuring editing modes that are not part of the :program:`emacs` distribution.
Modes provide syntax highlighting,
command shortcuts,
and other features to help you edit files of various types.

To use 3rd party modes that are in your :file:`$HOME/elisp/` directory you need to add the following to your :file:`$HOME/.emacs` file:

.. code-block:: scheme

    ;; add my personal elisp repository to the load-path
    (add-to-list 'load-path "~/elisp")

The general procedure to install a 3rd party mode is:

* Download the EmacsLisp file
  (file extension :file:`.el`)
  into your :file:`$HOME/elisp/` directory
* Byte-compile the mode file in :program:`emacs` with the command :command:`M-x byte-compile-file`,
  giving the path and file name at the prompt that follow;
  e.g. :file:`~/elisp/yaml-mode.el`
* Add the appropriate configuration statements listed below to your :file:`$HOME/.emacs` file
* Restart :program:`emacs` for the changes to take effect

You can view the help for a mode when it is active with the :program:`emacs` command :kbd:`C-h m`.


YAML Mode
---------

This is useful for working on Salish Sea NEMO run description files.

Download :file:`yaml-mode.el` from https://raw.github.com/yoshiki/yaml-mode/master/yaml-mode.el into your :file:`$HOME/elisp/` directory:

.. code-block:: bash

    cd $HOME/elisp/
    wget https://raw.github.com/yoshiki/yaml-mode/master/yaml-mode.el

Byte-compile :file:`yaml-mode.el` in :program:`emacs` with :kbd:`M-x byte-compile-file`.

Configure :program:`emacs` to use :kbd:`yaml-mode` automatically whenever you visit a file with the extension :kbd:`.yaml` or :kbd:`.yml`:

.. code-block:: scheme

    ;; YAML mode
    ;; https://raw.github.com/yoshiki/yaml-mode/master/yaml-mode.el
    (require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


ReStructuredText Mode
---------------------

This is useful for working on the Salish Sea MEOPAR project documentation files.

Download :file:`rst.el` from http://docutils.sourceforge.net/tools/editors/emacs/rst.el into your :file:`$HOME/elisp/` directory:

.. code-block:: bash

    cd $HOME/elisp/
    wget http://docutils.sourceforge.net/tools/editors/emacs/rst.el

Byte-compile :file:`rst.el` in :program:`emacs` with :kbd:`M-x byte-compile-file`.

Configure :program:`emacs` to use :kbd:`rst-mode` automatically whenever you visit a file with the extension :kbd:`.rst`:

.. code-block:: scheme

    ;; add reStructuredText mode
    ;; http://docutils.sourceforge.net/tools/editors/emacs/rst.el
    (require 'rst)
    (setq auto-mode-alist
          (append '(("\\.rst$" . rst-mode)) auto-mode-alist))


Python Mode
-----------

:program:`emacs` includes a Python editing mode,
but these instructions are for installing a more comprehensive :kbd:`python-mode` that is maintained by the Python community.

Download :file:`python-mode.el` from http://bazaar.launchpad.net/~python-mode-devs/python-mode/python-mode/view/head:/python-mode.el and move it into your :file:`$HOME/elisp/` directory.

Byte-compile :file:`python-mode.el` in :program:`emacs` with :kbd:`M-x byte-compile-file`.

Configure :program:`emacs` to use :kbd:`python-mode` automatically whenever you visit a file with the extension :kbd:`.py`:

.. code-block:: scheme

    ;; add better Python mode
    ;; http://bazaar.launchpad.net/~python-mode-devs/python-mode/python-mode/view/head:/python-mode.el
    (setq auto-mode-alist
          (cons '("\\.py$" . python-mode)
           auto-mode-alist))
    (setq interpeter-mode-alist
          (cons '("python" . python-mode)
           interpreter-mode-alist))
    (autoload 'python-mode "python-mode" "Python editing mode." t)


Mercurial Mode
--------------

Allows you to work with Mercurial from within :program:`emacs`.

Download :file:`mercurial.el` from http://hg.intevation.org/mercurial/file/tip/contrib/mercurial.el into your :file:`$HOME/elisp/` directory:

.. code-block:: bash

    cd $HOME/elisp/
    wget http://hg.intevation.org/mercurial/file/tip/contrib/mercurial.el

Byte-compile :file:`mercurial.el` in :program:`emacs` with :kbd:`M-x byte-compile-file`.

Configure :program:`emacs` to always load Mercurial mode:

.. code-block:: scheme

    ;; always load mercurial support
    ;; http://hg.intevation.org/mercurial/file/tip/contrib/mercurial.el
    (load-file "~/elisp/mercurial.elc")
