******************************
NEMO-code Repository Evolution
******************************

These notes described the evolution of the Salish Sea MEOPAR project `NEMO-code`_ repository.
They are a narrative guide to the Mercurial log and diffs that can be obtained from the repository itself or via the Bitbucket interface.

.. _NEMO-code: https://bitbucket.org/salishsea/nemo-code

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


Initialization to NEMO-3.1
==========================

The following steps took the `NEMO-code`_ repo from initialization to NEMO-3.1 that could be built on :kbd:`jasper.westgrid.ca` and used to successfully run the :kbd:`GYRE` configuration:

* An :command:`svn` checkout of the trunk of the :kbd:`modipsl` framework was done from http://forge.ipsl.jussieu.fr/igcmg/svn/modipsl/trunk.
  That yielded revision 2163.
  The repo state was tagged as `modipsl-r2163`_.

  .. _modipsl-r2163: https://bitbucket.org/salishsea/nemo-code/commits/tag/modipsl-r2163

  .. note::

      At this point the only directories in the :kbd:`modipsl` tree that are populated at :file:`doc/` and :file:`util/`.
      The :file:`bin/`,
      :file:`config/`,
      :file:`lib/`,
      :file:`modeles/`,
      and :file:`tmp/` directories are empty and therefore not included in the Mercurial repo.
