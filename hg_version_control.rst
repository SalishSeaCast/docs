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
