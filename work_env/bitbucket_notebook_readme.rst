.. _JupyterNotebookREADMEsOnBitbucekt:

*************************************
Jupyter Notebook READMEs on Bitbucket
*************************************

Bitbucket.org has a feature whereby a :file:`README.md` file containing text and `Markdown`_ markup present in any directory is rendered to HTML below the list of files in that directory.
See https://bitbucket.org/salishsea/analysis-ben/src/tip/notebooks/ as an example.

.. _Markdown: http://commonmark.org/

We can use that feature in directories that contain `Jupyter Notebook`_ files to provide links to our notebooks rendered to HTML by the `Jupyter Notebook Viewer`_ service.
Doing so makes the notebooks easily visible to anyone without having to run Jupyter Notebook.
It is also an easy way to generate notebook viewer links to paste into the Google Drive "whiteboard" documents for weekly group meetings.

.. _Jupyter Notebook: http://jupyter.org/
.. _Jupyter Notebook Viewer: https://nbviewer.jupyter.org/

You could hand edit the :file:`README.md` file,
but that's tedious and error prone,
so it is an obvious candidate for code automation.
Here is a prototype :file:`make_readme.py` module that provides that automation:

.. code-block:: python
    :linenos:

    # Copyright 2013-2016 The Salish Sea MEOPAR contributors
    # and The University of British Columbia

    # Licensed under the Apache License, Version 2.0 (the "License");
    # you may not use this file except in compliance with the License.
    # You may obtain a copy of the License at

    #    http://www.apache.org/licenses/LICENSE-2.0

    # Unless required by applicable law or agreed to in writing, software
    # distributed under the License is distributed on an "AS IS" BASIS,
    # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    # See the License for the specific language governing permissions and
    # limitations under the License.

    """Salish Sea NEMO Jupyter Notebook collection README generator
    """
    import datetime
    import glob
    import json
    import os
    import re


    NBVIEWER = 'http://nbviewer.jupyter.org/urls'
    REPO = 'bitbucket.org/salishsea/tools/raw/tip'
    REPO_DIR = 'SalishSeaNowcast/notebooks/figures/publish'
    TITLE_PATTERN = re.compile('#{1,6} ?')


    def main():
        url = os.path.join(NBVIEWER, REPO, REPO_DIR)
        readme = """\
    The Jupyter Notebooks in this directory are for development and testing of
    the results figures generation modules of the Salish Sea model nowcast system.

    The links below are to static renderings of the notebooks via
    [nbviewer.jupyter.org](http://nbviewer.jupyter.org/).
    Descriptions under the links below are from the first cell of the notebooks
    (if that cell contains Markdown or raw text).

    """
        for fn in glob.glob('*.ipynb'):
            readme += '* ##[{fn}]({url}/{fn})  \n    \n'.format(fn=fn, url=url)
            readme += notebook_description(fn)
        license = """
    ##License

    These notebooks and files are copyright 2013-{this_year}
    by the Salish Sea MEOPAR Project Contributors
    and The University of British Columbia.

    They are licensed under the Apache License, Version 2.0.
    http://www.apache.org/licenses/LICENSE-2.0
    Please see the LICENSE file for details of the license.
    """.format(this_year=datetime.date.today().year)
        with open('README.md', 'wt') as f:
            f.writelines(readme)
            f.writelines(license)


    def notebook_description(fn):
        description = ''
        with open(fn, 'rt') as notebook:
            contents = json.load(notebook)
        try:
            first_cell = contents['worksheets'][0]['cells'][0]
        except KeyError:
            first_cell = contents['cells'][0]
        first_cell_type = first_cell['cell_type']
        if first_cell_type not in 'markdown raw'.split():
            return description
        desc_lines = first_cell['source']
        for line in desc_lines:
            suffix = ''
            if TITLE_PATTERN.match(line):
                line = TITLE_PATTERN.sub('**', line)
                suffix = '**'
            if line.endswith('\n'):
                description += (
                    '    {line}{suffix}  \n'
                    .format(line=line[:-1], suffix=suffix))
            else:
                description += (
                    '    {line}{suffix}  '.format(line=line, suffix=suffix))
        description += '\n' * 2
        return description


    if __name__ == '__main__':
        main()

Here's how to set up and use this script:

#. Put the code above into a file called :file:`make_readme.py` in a directory that contains Jupyter Notebook files.

#. Edit line 26 to point to the repo that your directory is in.
   If you are setting this up for a directory in your local clone of the :file:`analysis-fred/` repository you should change line 26 from:

   .. code-block:: python

       REPO = 'bitbucket.org/salishsea/tools/raw/tip'

   to:

   .. code-block:: python

       REPO = 'bitbucket.org/salishsea/analysis-fred/raw/tip'

#. Edit line 27 to point to the directory containing this :file:`make_readme.py` script and the notebooks that it will create links to.
   If the directory is :file:`analysis-fred/notebooks/` you should change line 27 from:

   .. code-block:: python

       REPO_DIR = 'SalishSeaNowcast/notebooks/figures/publish'

   to:

   .. code-block:: python

       REPO_DIR = 'notebooks'

#. Edit lines 34-35 to describe what your notebooks are about.
   You can put as much text as you want there.
   It is the beginning of the text that will appear between the list of files on the Bitbucket page and the list of links to the Notebook Viewer renderings of your notebooks.

#. Save the :file:`make_readme.py` file.
   You won't need to edit it again unless you want to change the preamble text starting at line 34.

#. Run the :file:`make_readme.py` script to create your :file:`README.md` file:

   .. code-block:: bash

       $ python make_readme.py

#. Use Mercurial to add,
   commit,
   and push to Bitbucket your new notebook(s),
   the :file:`make_readme.py` script,
   and the :file:`README.md` file:

   .. code-block:: bash

       $ hg add make_readme.py README.md MyNotebook.ipynb
       $ hg ci -m"Add new notebook, make_readme script and README file."
       $ hg push

#. Use your browser to navigate to the repo and directory on Bitbucket.org and you should see the rendered :file:`README.md` and a link to the Notebook Viewer for your notebook(s).

#. Each time you create a new notebook in the directory,
   run :command:`python make_readme.py` to update the :file:`README.md` file and commit it along with your new notebook.

The :file:`make_readme.py` script reads the first cell of each notebook in the directory and,
if that cell contains text,
adds it to the :file:`README.md` file.
That lets you include a title and brief description of your notebooks along with the links on the Bitbucket page.
If you change the contents of that 1st cell in an existing notebook you need to run :command:`python make_readme.py`,
commit the :file:`README.md` changes,
and push them to Bitbucket in order to update the page there.
