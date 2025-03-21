#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Salish Sea MEOPAR project documentation build configuration file
#
# This file is execfile()d with the current directory set to
# its containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

import os


# -- General configuration ----------------------------------------------------


# Add any Sphinx extension module names here, as strings.
# They can be extensions coming with Sphinx
# (named 'sphinx.ext.*')
# or your custom ones.
extensions = [
    'nbsphinx',
    "notfound.extension",
    'IPython.sphinxext.ipython_console_highlighting',
    'sphinx.ext.intersphinx',
    'sphinx.ext.todo',
    'sphinx.ext.mathjax',
]

intersphinx_mapping = {
    'moaddocs': ('https://ubc-moad-docs.readthedocs.io/en/latest/', None),
    'nemocmd': ('https://nemo-cmd.readthedocs.io/en/latest/', None),
    'salishseatools': ('https://salishsea-meopar-tools.readthedocs.io/en/latest/', None),
    'salishseacmd': ('https://salishseacmd.readthedocs.io/en/latest/', None),
    'salishseanowcast': ('https://salishsea-nowcast.readthedocs.io/en/latest/', None),
}

# URLs linkcheck will ignore
linkcheck_ignore = [
    # Private GitHub repositories
    'https://github.com/SalishSeaCast/barotropic-tides',
    'https://github.com/SalishSeaCast/CONCEPTS-110',
    'https://github.com/SalishSeaCast/internal-tides',
    'https://github.com/SalishSeaCast/jies-plume-paper',
    'https://github.com/SalishSeaCast/mixing-paper',
    'https://github.com/SalishSeaCast/NEMO-Forcing',
    'https://github.com/SalishSeaCast/NEMO-3.1',
    'https://github.com/SalishSeaCast/NEMO-3.4-Code',
    'https://github.com/SalishSeaCast/NEMO-3.6-code',
    'https://github.com/SalishSeaCast/private-tools',
    'https://github.com/SalishSeaCast/SoG-Obs',
    'https://github.com/SalishSeaCast/XIOS-1.0',
    'https://github.com/SalishSeaCast/XIOS-2',
    # wiley.com and tandfonline.com throw 403: Forbidden errors for DOI links
    'https://onlinelibrary.wiley.com/doi/',
    'https://www.tandfonline.com/doi/',
    # intel.com throws 403: Forbidden errors for fortran compiler docs link
    'https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/overview.html',
]

todo_include_todos = True

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = 'Salish Sea MEOPAR'
copyright = (
    '2013 – present, '
    'Salish Sea MEOPAR Project Contributors '
    'and The University of British Columbia'
    )

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
#
# The short X.Y version.
version = ''
# The full version, including alpha/beta/rc tags.
release = ''

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'


# -- Options for HTML output --------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = "sphinx_rtd_theme"

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
html_favicon = '_static/MEOPAR_favicon.ico'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
html_last_updated_fmt = '%b %d, %Y'

# If false, no module index is generated.
html_domain_indices = False

# If false, no index is generated.
html_use_index = False

# Output file base name for HTML help builder.
htmlhelp_basename = 'SalishSea-MEOPARdoc'


# -- Options for LaTeX output -------------------------------------------------

# Grouping the document tree into LaTeX files.
# List of tuples
# (source start file, target name, title,
#  author, documentclass [howto/manual]).
latex_documents = [(
    'index',
    'SalishSea-MEOPAR-docs.tex',
    'Salish Sea MEOPAR Documentation',
    'Salish Sea MEOPAR Project Contributors',
    'manual',
)]
