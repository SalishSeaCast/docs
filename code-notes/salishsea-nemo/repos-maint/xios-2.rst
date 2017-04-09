.. _XIOS-2CodeRepoMaintenance:

****************************
XIOS-2 Code Repo Maintenance
****************************

.. _XIOS-2MaintSetup:

Set-up
======

The :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror` repository is an :command:`svn` checkout of http://forge.ipsl.jussieu.fr/nemo/svn/branches/2012/dev_v3_4_STABLE_2012 and also a read-only Mercurial repository.
It was initialized with:

.. code-block:: bash

    $ cd /ocean/sallen/hg_repos
    $ svn co http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/trunk XIOS-2-hg-mirror
    $ hg init XIOS-2-hg-mirror
    $ cd XIOS-2-hg-mirror
    $ cat > .hgignore
    syntax: glob
    .svn
    ctrl-d
    $ hg add
    $ hg ci -m"Initialize XIOS-2 svn mirror at r1078 of ^/trunk."

:command:`svn` v1.8.8 was used on :kbd:`skookum` for the :command:`svn` part of the initialization.

Doug maintains an :file:`XIOS-2-mirror-merge` repo on his laptop.
That repo is used to merge changes from the upstream Subversion repository that are brought in via the :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror` repo,
and from the `Salish Sea team XIOS-2 repo`_ on Bitbucket.
The :file:`XIOS-2-mirror-merge` repo was created by cloning the :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror` repo:

.. _Salish Sea team XIOS-2 repo: https://bitbucket.org/salishsea/xios-2

.. code-block:: bash

    hg clone ssh://skookum.eos.ubc.ca//ocean/sallen/hg_repos/XIOS-2-hg-mirror XIOS-2-mirror-merge

and setting the paths in its :file:`.hg/hgrc` to:

.. code-block:: ini

    [paths]
    bb = ssh://hg@bitbucket.org/salishsea/xios-2
    default-push = ssh://hg@bitbucket.org/salishsea/xios-2
    mirror = ssh://skookum.eos.ubc.ca//ocean/sallen/hg_repos/XIOS-2-hg-mirror

Those paths mean that the repo for :command:`hg pull` and :command:`hg incoming` commands must be specified explicitly.
The :kbd:`bb` and :kbd:`mirror` paths are provided to facilitate pulling from :kbd:`xios-2` on Bitbucket and :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror`,
respectively.
:command:`hg push` and :command:`hg outgoing` commands will act on the :kbd:`xios-2` repo on Bitbucket,
unless otherwise specified.

The `Salish Sea team XIOS-2 repo`_ on Bitbucket was created via the Bitbucket web interface and populated there by an :command:`hg push` from Doug's :file:`XIOS-2-mirror-merge` repo.

A working copy was then created by cloning the `Salish Sea team XIOS-2 repo`_ on Bitbucket as :file:`XIOS-2`.
The workflow sections below explain how these 4 repo clones are used to pull changes from upstream and merge them with changes that Salish Sea team members push to Bitbucket.


.. figure:: XIOS-2CodeRepoMaint.svg

   XIOS-2 code repositories and workflow to update and merge SVN and local changes


.. _PullChangesFromXIOS-2svn:

Workflow to Pull Changes from XIOS-2 :command:`svn` Repo
--------------------------------------------------------

The workflow to pull changes from the master XIOS :command:`svn` repo and commit them to our :file:`XIOS-2-hg-mirror` repo is somewhat automated by the :ref:`Marlin`.

#. Review the upstream changes in the source browser at http://forge.ipsl.jussieu.fr/ioserver/log/ to select a range of changes to be pulled into our :file:`XIOS-2-hg-mirror` repo.

#. Working on :kbd:`salish` in the :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror` repo with an activated virtualenv in which :command:`marlin` is installed:

   .. code-block:: bash

       $ ssh salish
       $ workon marlin
       (marlin)$ cd /ocean/sallen/hg_repos/XIOS-2-hg-mirror

#. Use :kbd:`marlin incoming` information about the next SVN revision that will be pulled from upstream and confirm that it is the expected revision:

   .. code-block:: bash

       (marlin)$ marlin incoming

   **TODO** Add sample output

   The :kbd:`--limit` option can be used to see more incoming revisions;
   see :command:`marlin help incoming` for details.

#. Use :kbd:`marlin update` to update the working copy to the next upstream commit and commit the SVN update as a Mercurial changeset with the SVN commit message as the body of the Mercurial commit message and echo that message:

   .. code-block:: bash

       (marlin)$ marlin update

   **TODO** Add sample output

   The :kbd:`--to-rev` option can be used to apply a series of upstream updates,
   committing them to Mercurial one at a time;
   see :command:`marlin help update` for details.


Workflow to Merge XIOS-2 :command:`svn` Repo and Salish Sea Revisions
---------------------------------------------------------------------

Merging changes from NEMO :command:`svn` and the `Salish Sea team XIOS-2 repo`_ on Bitbucket is done in a repo that is used for only that purpose.
Doug does the merges on his laptop.
The repo in which the merging is done was created by cloning the :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror` repo as described in the :ref:`XIOS-2MaintSetup` section.

After the :ref:`PullChangesFromNEMOsvn` has been completed the workflow to merge those changes with Salish Sea MEOPAR project revisions is:

#. Pull and update recent changes from the `Salish Sea team XIOS-2 repo`_ on Bitbucket into :kbd:`XIOS-2-mirror-merge`:

   .. code-block:: bash

       cd XIOS-2-mirror-merge
       hg pull --update bb

#. Pull and update the changes from :file:`/ocean/sallen/hg_repos/XIOS-2-hg-mirror` into :kbd:`XIOS-2-mirror-merge`:

   .. code-block:: bash

       hg pull mirror

#. Because the changesets pulled from the `Salish Sea team XIOS-2 repo`_ on Bitbucket are public a branch merge is necessary:

   .. code-block:: bash

       hg merge
       hg commit -m"Merge svn updates."

#. Push the result of the updates and merges to the `Salish Sea team XIOS-2 repo`_ on Bitbucket:

   .. code-block:: bash

       hg push bb

   If other users have pushed changes to the `Salish Sea team XIOS-2 repo`_ on Bitbucket while merge conflicts were being handled :command:`hg pull --rebase` can be used to bring in those changes and deal with any additional merge conflicts.

#. Notify team members of the upstream merge.