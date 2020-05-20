# CCPP Framework Developers Guide

The PDF files in this directory are generated in the following manner:

When the ``ccpp_prebuild.py`` script is run for a host model, such as a single
column model (SCM) or the UFS Weather Model, a file named ``CCPP_VARIABLES_SCM.tex`` or 
``CCPP_VARIABLES_FV3.tex`` is created in this directory.  

To create the PDF files, the latex to pdf converter is necessary:

``pdflatex CCPP_VARIABLES_SCM.tex``
``pdflatex CCPP_VARIABLES_FV3.tex``
