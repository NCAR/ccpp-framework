# CCPP Framework Developers Guide

The PDF files in this directory are generated in the following manner:

When the ``ccpp_prebuild.py`` script is run for the single column model (SCM) or
a host model (e.g. FV3), a file named ``CCPP_VARIABLES_SCM.tex`` or 
``CCPP_VARIABLES_FV3.tex`` is created in this directory.  

To create the PDF files, the pdf to latex converter is necessary:

``pdflatex CCPP_VARIABLES_SCM.tex``
``pdflatex CCPP_VARIABLES_FV3.tex``
