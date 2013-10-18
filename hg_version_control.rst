.. _vc-with-hg:

Version Control with Mercurial
==============================

We use Mercurial_ (:command:`hg`) for version control of code,
documentation,
and pretty much any other important computer files in the Salish Sea MEOPAR project.

.. _Mercurial: http://mercurial.selenic.com/

The Mercurial site includes a `basic guide`_,
but `Mercurial - The Definitive Guide`_
(also known as "the redbean book") is the go-to reference.
If you are new to version control you should read at least Chapters 1_ and 2_.
Users experiences with other version control tools
(e.g :command:`svn` or :command:`git`)
can get up to speed with Mercurial by reading `Chapter 2`_.

.. _basic guide: http://mercurial.selenic.com/guide/
.. _Mercurial - The Definitive Guide: http://hgbook.red-bean.com/
.. _1: http://hgbook.red-bean.com/read/how-did-we-get-here.html
.. _2: http://hgbook.red-bean.com/read/a-tour-of-mercurial-the-basics.html
.. _Chapter 2: http://hgbook.red-bean.com/read/a-tour-of-mercurial-the-basics.html

The central storage of repositories is in the `SalishSea-MEOPAR`_ team account on Bitbucket.

.. _SalishSea-MEOPAR: https://bitbucket.org/salishsea/


Installing Mercurial
--------------------

Obviously,
you need to have Mercurial installed on your computer.
It is already installed on the Waterhole workstations,
:kbd:`sable`,
and :kbd:`salish` at UBC.
It is also installed on :kbd:`bugaboo` and :kbd:`jasper` on WestGrid and a request to install it on other systems will likely be addressed within an hour.
If you have administrator privileges on your workstation or laptop you can download and install Mercurial for your operating system from http://mercurial.selenic.com/downloads/,
otherwise,
contact your IT support to have it installed for you.

Windows users may want to use TortoiseHg_,
a GUI interface that integrates with Windows Explorer.
However,
this documentation focuses on command line use of Mercurial.
The workflows described below should be easily translatable into the TortoiseHg interface and it also includes a command line interface.

.. _TortoiseHg: http://tortoisehg.org/


Mercurial Configuration
-----------------------

Mercurial uses configuration settings in your :file:`$HOME/.hgrc` file as global settings for everything you do with it.
You should create or edit your :file:`$HOME/.hgrc` file to contain:

.. code-block:: ini

    [extensions]
    color =
    graphlog =
    pager =
    rebase =

    [pager]
    pager = LESS='FRX' less

    [ui]
    username = Your Name <your_email_address>
    ignore = $HOME/.hgignore
    ssh = ssh -C

The :kbd:`[extensions]` section enables several useful Mercurial extensions:

* :kbd:`color` shows log listing,
  diffs,
  etc. in colour

* :kbd:`graphlog` provides the :command:`hg glog` command and the synonymous :command:`hg log -G` command that formats the output as a graph representing the revision history using ASCII characters to the left of the log

* :kbd:`pager` sends output of Mercurial commands through the pager that you specify in the :kbd:`[pager]` section so that long output is displayed one page at a time

  .. note::

      The version of Mercurial on :kbd:`jasper` does not include the :kbd:`pager` extension and this extension should not be used there.

* :kbd:`rebase` enables rebasing which is particularly useful when working in repositories to which several contributors are pushing changes.
  As described below,
  :kbd:`rebase` allows changes that have been pushed by other contributors to be pulled into your cloned repo while you have committed changes that have not been pushed without having to do frivolous branch merges.

  .. todo::

      Add link to section about :command:`hg pull --rebase` when it exists

The :kbd:`[ui]` section configures the Mercurial user interface:

* :kbd:`username` defines the name and email address that will be used in your commits.
  You should use the same email address as the one you have registered on Bitbucket.

* :kbd:`ignore` is the path and name of an ignore file to be applied to all repositories
  (see :ref:`global-ignore-file`)

* :kbd:`ssh` specifies the :command:`ssh` command to use when communicating with remote Mercurial instances like the one on Bitbucket.
  Setting it to :command:`ssh -C` enables data compression.

See the `Mercurial configuration file docs`_ for more information about configuration options.

.. _Mercurial configuration file docs: http://www.selenic.com/mercurial/hgrc.5.html


.. _global-ignore-file:

Global Ignore File
------------------

Mercurial uses the file specified by :kbd:`ignore` in the :kbd:`[ui]` configuration section to define a set of ignore patterns that will be applied to all repos.
The recommended path and name for that file is :file:`$HOME/.hgignore`.

You should create or edit your :file:`$HOME/.hgignore` file to contain::

  syntax: glob
  *~
  *.pyc
  *.egg-info
  .DS_Store
  .coverage
  .cache

  syntax: regexp
  (.*/)?\#[^/]*\#$
  ^docs/(.*)build/

The :kbd:`syntax: glob` section uses shell wildcard expansion to define file patterns to be ignored.

The :kbd:`syntax: regexp` section uses regular expressions to define ignore patterns.
The :kbd:`^docs/(.*)build/` pattern ignores the products of Sphinx documentation builds in :file:`docs/` directories.

Most repos have their own :file:`.hgignore` file that defines patterns to ignore for that repo in addition to those specified globally.

See the `ignore file syntax docs`_ for more information.

.. _ignore file syntax docs: http://www.selenic.com/mercurial/hgignore.5.html
