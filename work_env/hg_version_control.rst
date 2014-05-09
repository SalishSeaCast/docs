.. _vc-with-hg:

******************************
Version Control with Mercurial
******************************

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

The central storage of repositories is in the `SalishSea-MEOPAR`_ team account on `Bitbucket.org`_.
If you haven't done so already,
you should:

* Create a `Bitbucket.org`_ account.
  If you use an academic domain email address (like :kbd:`@eos.ubc.ca`) you will get perks like unlimited private repo collaboration.
* Send your Bitbucket user id to dlatornell@eos.ubc.ca so that you can be added to the `SalishSea-MEOPAR`_ team account.
* Follow the `Bitbucket ssh Set-up`_ instructions to enable key authentication.
  You probably won't need to do steps 7 and 8 at the end.

  .. note::

      You only need to do the :command:`ssh-keygen` part described in step 3 once.
      Once you have an ssh key-pair you can use it from all of your working environments.

.. _Bitbucket.org: https://bitbucket.org/
.. _SalishSea-MEOPAR: https://bitbucket.org/salishsea/
.. _Bitbucket ssh Set-up: https://confluence.atlassian.com/pages/viewpage.action?pageId=270827678


Installing Mercurial
====================

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


.. _MercurialConfiguration:

Mercurial Configuration
=======================

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

* :kbd:`graphlog` provides the :kbd:`hg glog` command and the synonymous :kbd:`hg log -G` command that formats the output as a graph representing the revision history using ASCII characters to the left of the log

* :kbd:`pager` sends output of Mercurial commands through the pager that you specify in the :kbd:`[pager]` section so that long output is displayed one page at a time

  .. note::

      The version of Mercurial on :kbd:`jasper` does not include the :kbd:`pager` extension and this extension should not be used there.

* :kbd:`rebase` enables rebasing which is particularly useful when working in repositories to which several contributors are pushing changes.
  As described below,
  :kbd:`rebase` allows changes that have been pushed by other contributors to be pulled into your cloned repo while you have committed changes that have not been pushed without having to do frivolous branch merges.
  See :ref:`PullingAndRebaseingChangesFromUpstream` for more details.

The :kbd:`[ui]` section configures the Mercurial user interface:

* :kbd:`username` defines the name and email address that will be used in your commits.
  You should use the same email address as the one you have registered on Bitbucket.

* :kbd:`ignore` is the path and name of an ignore file to be applied to all repositories
  (see :ref:`global-ignore-file`)

* :kbd:`ssh` specifies the :command:`ssh` command to use when communicating with remote Mercurial instances like the one on Bitbucket.
  Setting it to :kbd:`ssh -C` enables data compression.

See the `Mercurial configuration file docs`_ for more information about configuration options.

.. _Mercurial configuration file docs: http://www.selenic.com/mercurial/hgrc.5.html


.. _global-ignore-file:

Global Ignore File
==================

Mercurial uses the file specified by :kbd:`ignore` in the :kbd:`[ui]` configuration section to define a set of ignore patterns that will be applied to all repos.
The recommended path and name for that file is :file:`$HOME/.hgignore`.

You should create or edit your :file:`$HOME/.hgignore` file to contain::

  syntax: glob
  *~
  *.pyc
  *.egg-info
  .ipynb_checkpoints
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


Mercurial Workflows
===================

.. note::

    Mercurial commands may be shortened to the fewest number of letters that uniquely identifies them.
    For example,
    :kbd:`hg status` can be spelled :kbd:`hg stat` or even :kbd:`hg st`.
    If you don't provide enough letters Mercurial will show the the possible command completions.


.. _PullingAndRebaseingChangesFromUpstream:

Pulling and Rebasing Changes from Upstream
------------------------------------------

The upstream Bitbucket repos from which you cloned your local working repos are the central repos to which everyone working on the project push their changes.
This section describes workflows for pulling those changes into your repos,
how to do so without having to do frivolous branch merges,
and how to recover from the common mistakes.

Use :kbd:`hg incoming` to see changes that are present in the upstream repo that have not yet been pulled into your local repo.
Similarly,
:kbd:`hg outgoing` will show you the changes that are present in your local repo that have not been pushed upstream.

Ensure that you have committed all of your changes before you pull new changes from upstream;
i.e.
:kbd:`hg status` should show nothing or a list of untracked files marked with the :kbd:`!` character.

:kbd:`hg pull --rebase` will pull the changes from upstream and merge your locally committed changes on top of them.
Using :kbd:`rebase` avoids the creation of a new head
(aka a branch)
in your local repo and an unnecessary merge commit that results from the use of :kbd:`hg pull --update`.
That reserves branching and merging for the relatively rare occasions when temporarily divergent lines of development are actually required.

The `rebase extension docs`_ have more information and diagrams of what's going on in this `common rebase use case`_.

.. _rebase extension docs: http://mercurial.selenic.com/wiki/RebaseExtension
.. _common rebase use case: http://mercurial.selenic.com/wiki/RebaseExtension#A_common_case


Rebasing an Accidental Branch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sooner or later you will accidentally create a branch in your local repo.
Using :kbd:`hg pull --rebase` with uncommitted changes and then commiting those changes is one way that an accidental branch can happen.
:kbd:`hg glog` is a variant of the :kbd:`hg log` command that shows an ASCII-art graph of the commit tree to the left of the commit log,
providing a way of visualizing branches.

:kbd:`hg rebase` can be used to move the changes on an accidental branch to the tip of the repo.
See the `scenarios section`_ of the `rebase extension docs`_ for diagrams and rebase command options for moving branches around in various ways.

.. _scenarios section: http://mercurial.selenic.com/wiki/RebaseExtension#Scenarios


Aborting a Merge
----------------

You may find yourself having followed Mercurial's workflow suggestions have having merged changes from upstream but then realizing that you really should have rebased.
At that point if you try to do almost anything other than commit the merge Mercurial will stop you with a message like::

  abort: outstanding uncommitted merges

You can use :kbd:`hg update --clean` to discard the uncommitted changes,
effectively aborting the merge
(and any other uncommitted changes you might have).
After that you should use :kbd:`hg glog` or :kbd:`hg heads` to examine your repo structure because you may well have an accidental branch that you will want to rebase.

Incidentally,
:kbd:`hg update --clean` can be used any time that you want to discard all uncommitted changes,
but be warned,
it does so without keeping a backup.
See :kbd:`hg revert` for a less destructive way of discarding changes on a file by file basis
(but note that :kbd:`hg revert` cannot be used to undo a merge).


Amending the Last Commit
------------------------

:kbd:`hg commit --amend` can be used to alter the last commit,
provided that it has not yet been pushed up stream.
This allows for correction or elaboration of the commit message,
inclusion of additional changes in the commit,
or addition of new files to the commit,
etc.


Commit Message Style
--------------------

Commit messages can be written on the command line with the :kbd:`hg commit -m` option with the message enclosed in double-quotes
(:kbd:`"`);
e.g.

.. code-block:: bash

    hg commit -m"Add Salish Sea NEMO model quick-start section."

Assuming that you have the :envvar:`EDITOR` environment variable set :kbd:`hg commit` without the :kbd:`-m` option will open your editor for you to write your commit message and the files to be committed will be shown in the editor.
Using your editor for commit message also makes it easy to write multi-line commit messages.

Here are recommendations for commit message style::

  Short (70 chars or less) summary sentence.

  More detailed explanatory text, if necessary.  Wrap it to about 72
  characters or so. The blank line separating the summary from the body
  is critical (unless you omit the body entirely).

  Write your commit message in the imperative: "Fix bug" and not "Fixed bug"
  or "Fixes bug."

  Further paragraphs come after blank lines.

  - Bullet points are okay, too

  - Typically a hyphen or asterisk is used for the bullet, followed by a
    single space, with blank lines in between

  - Use a hanging indent
