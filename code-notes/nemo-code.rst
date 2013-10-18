********************
NEMO-code Repository
********************

These notes describe the Salish Sea MEOPAR project `NEMO-code`_ repository and its maintenance.
They are a narrative guide to the Mercurial log and diffs that can be obtained from the repository itself or via the Bitbucket interface.

.. _NEMO-code: https://bitbucket.org/salishsea/nemo-code

The `NEMO-code`_ repo is a Mercurial repository in which is maintained the merger of the trunk of the main NEMO :command:`svn` repository and the changes made by the Salish Sea MEOPAR project team.

Team members using SSH key authentication on Bitbucket may clone the `NEMO-code`_ repo with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code

For password authentication use:

.. code-block:: bash

    hg clone https://douglatornell@bitbucket.org/salishsea/nemo-code NEMO-code

.. note::

    The `NEMO-code`_ repository is a private repository for members of the Salish Sea MEOPAR project team.
    That is because it contains parts of the NEMO_ codebase.
    Although that codebase is openly licensed it's developers require registration_ to access the code.

    If you have completed that registration and would like access to the `NEMO-code`_,
    please contact `Susan Allen`_,
    the Salish Sea MEOPAR project leader.

    .. _NEMO: http://www.nemo-ocean.eu/
    .. _registration: http://www.nemo-ocean.eu/user/register
    .. _Susan Allen: mailto://sallen@eos.ubc.ca


Mirroring the NEMO :command:`svn` Repo
======================================

The :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repository is an :command:`svn` checkout of http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012 and also a read-only Mercurial repository.
It was intialized with:

.. code-block:: bash

    cd /ocean/sallen/hg_repos
    svn --username "dlatornell" co -r 3819 http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012
    hg init NEMO-hg-mirror
    cd NEMO-hg-mirror
    cat > .hgignore
    .svn
    DOC/NEMO_book.pdf
    ctrl-d
    hg add
    hg ci -m"Initialize NEMO svn mirror at r3819 of ^/branches/2012/dev_v3_4_STABLE_2012."

:command:`svn` v1.7.5 was used on :kbd:`salish` for the :command:`svn` part of the initialization.


.. _PullChangesFromNEMOsvn:

Workflow to Pull Changes from NEMO :command:`svn` Repo
------------------------------------------------------

.. todo::

    Write Workflow to Pull Changes from NEMO :command:`svn` Repo


Workflow to Merge :kbd:`trunk` and Salish Sea Revisions
-------------------------------------------------------

Merging changes from NEMO :kbd:`trunk` and the Salish Sea central `NEMO-code` repo on Bitbucket is done in a repo that is used for only that purpose.
Doug does the merges on his laptop.
The repo in which the merging is done was created by cloning the :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` repo:

.. code-block:: bash

    hg clone ssh://sable.eos.ubc.ca//ocean/sallen/hg_repos/NEMO-hg-mirror NEMO-mirror-merge

and setting the paths in its :file:`.hg/hgrc` to:

.. code-block:: ini

    [paths]
    bb = ssh://hg@bitbucket.org/salishsea/nemo-code
    default-push = ssh://hg@bitbucket.org/salishsea/nemo-code
    mirror = ssh://sable.eos.ubc.ca//ocean/sallen/hg_repos/NEMO-hg-mirror

Those paths mean that the repo for :command:`hg pull` and :command:`hg incoming` commands must be specified explicitly.
The :kbd:`bb` and :kbd:`mirror` paths are provided to facilitate pulling from `NEMO-code`_ on Bitbucket and :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror`,
respectively.
:command:`hg push` and :command:`hg outgoing` commands will act on the `NEMO-code`_ repo,
unless otherwise specified.

After the :ref:`PullChangesFromNEMOsvn` has been completed those changes from `NEMO-code`_ are pulled and updated into :kbd:`NEMO-mirror-merge`,

.. code-block:: bash

    cd NEMO-mirror-merge
    hg pull --update bb

The changes from :file:`/ocean/sallen/hg_repos/NEMO-hg-mirror` are also pulled and updated into :kbd:`NEMO-mirror-merge`,
resolving any merge conflicts as necessary:

.. code-block:: bash

    hg pull --update mirror

Finally,
the result of the updates and merges is pushed to `NEMO-code`_:

.. code-block:: bash

    hg push bb

If other users have pushed changes to `NEMO-code`_ while merge conflicts were being handled :command:`hg pull --rebase` can be used to bring in those changes and deal with any additional merge conflicts.
