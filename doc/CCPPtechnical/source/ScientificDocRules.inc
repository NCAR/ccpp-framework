.. _SciDoc:

Scientific Documentation Rules
==============================

Technically, scientific documentation is not needed for a parameterization
to work with the CCPP. However, scientific and technical documents are
important for code maintenance and for fostering understanding among stakeholders.
As such, it is required of physics schemes in order to be included in the CCPP. This
section describes the process used for documenting parameterizations in the CCPP.
Doxygen was chosen as a tool for generating human-readable output due to its
built-in functionality with Fortran, its high level of configurability, and its
ability to parse inline comments within the source code. Keeping documentation
with the source itself increases the likelihood that the documentation will be
updated along with the underlying code. Additionally, inline documentation is
amenable to version control.

The purpose of this section is to provide an understanding of how to properly
document a physics scheme using doxygen inline comments in the Fortran code
and metadata information contained in the ``.meta`` files. It covers what kind of
information should be in the documentation, how to mark up the inline comments
so that doxygen will parse them correctly, where to put various comments within
the code, how to include information from the ``.meta`` files, 
and how to configure and run doxygen to generate HTML
output. For an example of the HTML rendering of the CCPP Scientific Documentation, see 
https://dtcenter.org/GMTB/v3.0/sci_doc.
Part of this documentation, namely metadata about subroutine arguments, has
functional significance as part of the CCPP infrastructure. The metadata must be
in a particular format to be parsed by Python scripts that “automatically” generate
a software cap for a given physics scheme. Although the procedure outlined herein
is not unique, following it will provide a level of continuity with previous
documented schemes.

Reviewing the documentation for CCPP parameterizations is a good way of getting
started in writing documentation for a new scheme. 

Doxygen Comments and Commands
-----------------------------

All doxygen commands start with a backslash (``“\”``) or an at-sign (``“@”``). The
doxygen inline comment blocks begin with “!>”, and subsequent lines begin with ``“!!”``,
which means that regular Fortran comments using ``“!”`` are not parsed by doxygen.

In the first line of each Fortran file, a brief one-sentence overview of the file purpose
is present using the doxygen command ``“\\file”``:

.. code-block:: console

   !> \file gwdps.f
   !! This file is the  parameterization of orographic gravity wave
   !! drag and mountain blocking.

A parameter definition begins with ``“!<”``, where the sign ``‘<’`` just tells
Doxygen that documentation follows. Example:

.. code-block:: console

   integer, parameter, public :: NF_VGAS = 10   !< number of gas species
   integer, parameter         :: IMXCO2  = 24   !< input CO2 data longitude points
   integer, parameter         :: JMXCO2  = 12   !< input CO2 data latitude points
   integer, parameter         :: MINYEAR = 1957 !< earlist year 2D CO2 data available


Doxygen Documentation Style
---------------------------

To document a physics suite, a broad array of information should be included
in order to serve both software engineering and scientific purposes. The
documentation style could be divided into four categories:

* Doxygen Files
* Doxygen Pages (overview page and scheme pages)
* Doxygen Modules
* Bibliography

Doxygen files
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Doxygen provides the ``“\\file”`` tag as a way to provide documentation on the
Fortran source code file level. That is, in the generated documentation,
one may navigate by source code filenames (if desired) rather than through
a “functional” navigation. The most important documentation organization is
through the “module” concept mentioned below, because the division of a scheme
into multiple source files is often functionally irrelevant. Nevertheless,
using a ``“\\file”`` tag provides an alternate path to navigate the documentation
and it should be included in every source file. Therefore, it is prudent to
include a small documentation block to describe what code is in each file
using the ``“\\file”`` tag, e.g.:

.. code-block:: fortran

   !> \file gwdps.f
   !! This file is the  parameterization of orographic gravity wave
   !! drag and mountain blocking.

The brief description for each file is displayed next to the source filename
on the doxygen-generated “File List” page:

.. figure:: _static/DoxygenFileList.png
   :align: center

Doxygen Overview Page
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Pages in Doxygen can be used for documentation that is not directly attached
to a source code entity such as file or module. They are external text files
that generate pages with a high-level scientific overview and 
typically contain a longer description of a project or suite. You can refer to
any source code entity from within the page.

The GMTB maintains a main page, created by the Doxygen command
``“\\mainpage”``, containing an overall description and background of the CCPP.  
Physics developers do not have to edit the file with the mainpage, which has a
user-visible title, but not label:

.. code-block:: console

    /**
    \mainpage Introduction
    ...
    */
 
All other pages listed under the main page are created using the Doxygen
tag ``“\\page”`` described in the next section. In any Doxygen page,
you can refer to any entity of source code by using Doxygen tag ``“\\ref”``
or ``“@ref”``. Example in ``GFSv15_suite.txt``:
 
The GFS v15 physics suite uses the parameterizations in the following order,
as defined in

.. code-block:: console

  \c FV3_GFS_v15 :
   - \ref fast_sat_adj
   - \ref GFS_RRTMG
   - \ref GFS_SFCLYR
   - \ref GFS_NSST
   - \ref GFS_NOAH
   - \ref GFS_SFCSICE
   - \ref GFS_HEDMF
   - \ref GFS_GWDPS
   - \ref GFS_RAYLEIGH
   - \ref GFS_OZPHYS
   - \ref GFS_H2OPHYS
   - \ref GFS_SAMFdeep
   - \ref GFS_GWDC
   - \ref GFS_SAMFshal
   - \ref GFDL_cloud
   - \ref GFS_CALPRECIPTYPE
   - \ref STOCHY_PHYS

The HTML result is `here <https://dtcenter.org/GMTB/v3.0/sci_doc/GFS_v15_page.html>`_.
You can see that the ``“-”`` signs before ``“@ref”`` generate a list with bullets.
Doxygen command ``“\\c”`` displays its argument using a typewriter font.

Physics Scheme Pages
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each major scheme in CCPP should have its own scheme page containing an
overview of the parameterization.  These pages are not tied to the Fortran
code directly; instead, they are created with a separate text file that starts
with the command ``“\\page”``.  Scheme pages are stored in the ``ccpp-physics/physics/docs/pdftxt``
directory. Each page has a label (e.g., “GFS_SAMFdeep” in the following example) and a
user-visible title (“GFS Scale-Aware Simplified Arakawa-Schubert (sa-SAS) Deep Convection
Scheme” in the following example).  It is noted that labels must be unique
across the entire doxygen project so that the ``“\\ref”`` command can be used
to create an unambiguous link to the structuring element. It therefore makes
sense to choose label names that refer to their context.

.. code-block:: console

   /**
   \page GFS_SAMFdeep GFS Scale-Aware Simplified Arakawa-Schubert (sa-SAS) Deep Convection Scheme
   \section des_deep Description
    The scale-aware mass-flux (SAMF) deep convection scheme is an
    updated version of the previous Simplified Arakawa-Schubert (SAS) scheme
    with scale and aerosol awareness and parameterizes the effect of deep
    convection on the environment (represented by the model state variables)
    in the following way …

   \section intra_deep  Intraphysics Communication
   \ref arg_table_samfdeepcnv_run

   \section gen_al_deep General Algorithm
   \ref general_samfdeep

   */


The physics scheme page will often describe the following:

1. Description section (``“\\section”``), which usually includes:

      * Scientific origin and scheme history (``“\\cite”``)
      * Key features and differentiating points
      * A picture is worth a thousand words (``“\\image”``)

        To insert images into doxygen documentation, you’ll need to have your
        images ready in a graphical format, such as Portable Network Graphic (png), depending
        on which type of doxygen output you are planning to generate. For example, for LaTeX
        output, the images must be provided in Encapsulated PostScript (.eps), while for
        HTML output the images can be provided in the png format. Images are stored in
        ``ccpp-physics/physics/docs/img`` directory.  Example of including an image for
        HTML output:

.. code-block:: console

   \image  html  gfdl_cloud_mp_diagram.png "Figure 1: GFDL MP at a glance (Courtesy of S.J. Lin at GFDL)" width=10cm

2. Intraphysics Communication Section (``“\\section”``)

    The argument table for CCPP entry point subroutine ``{scheme}_run`` will be in this section.
    It is created by inserting a reference link (``“\\ref”``) to the table in the Fortran code
    for the scheme.

3. General Algorithm Section (``“\\section”``)

   The general description of the algorithn will be in this section.  It is created by inserting
   a reference link (``“\\ref”``) in the Fortran code for the scheme.

The symbols ``“/\*\*”`` and ``“*/”`` need to be the first and last entries of the page.
See an example of GFS Scale-Aware Simplified Arakawa-Schubert (sa-SAS) Deep Convection Scheme
page in the previous page.


Note that separate pages can also be created to document something that is not a scheme.
For example, a page could be created to describe a suite, or how a set of schemes work
together.  Doxygen automatically generates an index of all pages that is visible at the
top-level of the documentation, thus allowing the user to quickly find, and navigate
between, the available pages.

.. _DoxygenModules:

Doxygen Modules
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The CCPP documentation is based on doxygen modules (note this is not the same as
Fortran modules). Each doxygen module pertains to a particular parameterization and
is used to aggregate all code related to that scheme, even when it is in separate
files. Since doxygen cannot know which files or subroutines belong to each physics
scheme, each relevant subroutine must be tagged with the module name. This allows
doxygen to understand your modularized design and generate the documentation accordingly.
`Here <https://dtcenter.org/GMTB/v3.0/sci_doc/modules.html>`_
is a list of module list defined in CCPP.

A module is defined using:

.. code-block:: console

   !>\defgroup group_name group_title

Where ``group_name`` is the identifier and the ``group_title`` is what the
group is referred to in the output. In the example below, we’re defining a parent
module “GFS radsw Main”:

.. code-block:: fortran

   !> \defgroup module_radsw_main GFS radsw Main
   !! This module includes NCEP's modifications of the RRTMG-SW radiation
   !! code from AER.
   !! ...
   !!\author   Eli J. Mlawer, emlawer@aer.com
   !!\author   Jennifer S. Delamere, jdelamer@aer.com
   !!\author   Michael J. Iacono, miacono@aer.com
   !!\author   Shepard A. Clough
   !!\version NCEP SW v5.1  Nov 2012 -RRTMG-SW v3.8
   !!

One or more contact persons should be listed with \author. If you make
significant modifications or additions to a file, consider adding an
\author and a \version line for yourself. The above example generates
the Author, Version sections on the page. All email addresses are converted
to mailto hypertext links automatically:

**Author**
    Eli J. Mlawer, emlawer@aer.com

    Jennifer S. Delamere, jdelamer@aer.com

    Michael J. Iacono, miacono@aer.com

    Shepard A. Clough
**Version**
    NCEP SW v5.1  Nov 2012 -RRTMG-SW v3.8

In order to include other pieces of code in the same module, the following
tag must be used at the beginning of a comment block:

.. code-block:: console

   \ingroup group_name

For example:

.. code-block:: fortran

   !>\ingroup module_radsw_main
   !> The subroutine computes the optical depth in band 16:  2600-3250
   !! cm-1 (low - h2o,ch4; high - ch4)
   !-----------------------------------
         subroutine taumol16
   !...................................

In the same comment block where a group is defined for a physics scheme,
there should be some additional documentation. First, using the ``“\\brief”``
command, a brief one or two sentence description of the scheme should be
included. After a blank doxygen comment line, begin the scheme origin
and history using ``“\\version”``, ``“\\author”`` and ``“\\date”``.

Each subroutine that is a CCPP entry point to a parameterization, should
be further documented with a documentation block immediately preceding
its definition in the source. The documentation block should include at
least the following components:

* A brief one- or two-sentence description with the ``"\\brief"`` tag

* A more detailed one or two paragraph description of the function of the subroutine

* A comment indicating that metadata information about the subroutine arguments follows
  (in this example, the subroutine is called ``SUBROUTINE_NAME``.  Note that this line is 
  also functional documentation used during the CCPP *prebuild* step.

.. code-block:: fortran

  !! \section arg_table_SUBROUTINE_NAME Argument Table
 
* For subroutines that are non-empty, a second comment indicating that a table
  of metadata to describe the subroutine arguments will be included from a separate file
  in HTML format (in this case, file ``SUBROUTINE_NAME.html``). Note that empty
  subroutines, as is sometimes the case for ``init`` and ``finalize`` subroutines, 
  do not require the inclusion of a file with metadata information. 
  Please refer to the section below for information on how to generate the HTML files
  with metadata information from the ``.meta`` files.

  The argument table should be immediately followed by a blank doxygen line “!!”.

.. code-block:: fortran

   !! \htmlinclude SUBROUTINE_NAME.html
   !!

* A section called “General Algorithm” with a bullet or numbered list of
  the tasks completed in the subroutine algorithm

* At the end of initial subroutine documentation block, a “Detailed algorithm”
  section is started and the entirety of the code  is encompassed with the
  ``“!> @{”`` and ``“!> @}”`` delimiters. This way, any comments explaining detailed
  aspects of the code are automatically included in the “Detailed Algorithm” section.

For subroutines that are not a CCPP entry point to a scheme, no inclusion of
metadata information is required.  
But it is suggested that following ``“\\ingroup”`` and ``“\\brief”``, use
``“\\param”`` to define each argument with local name, a short description and unit, i.e.,

.. code-block:: console

   !>  \ingroup HEDMF
   !!  \brief This subroutine is used for calculating the mass flux and updraft properties.
   !!  ...
   !! 
   !!  \param[in] im      integer, number of used points
   !!  \param[in] ix      integer, horizontal dimension
   !!  \param[in] km      integer, vertical layer dimension
   !!  \param[in] ntrac   integer, number of tracers
   !!  \param[in] delt    real, physics time step
   !!  ...
   !!  \section general_mfpbl mfpbl General Algorithm
   !!  -# Determine an updraft parcel's entrainment rate, buoyancy, and vertical velocity.
   !!  -# Recalculate the PBL height ...
   !!  -# Calculate the mass flux profile and updraft properties.
   !!  \section detailed_mfpbl mfpbl Detailed Algorithm
   !>  @{
          subroutine mfpbl(im,ix,km,ntrac,delt,cnvflg,                       &
          &   zl,zm,thvx,q1,t1,u1,v1,hpbl,kpbl,                              &
          &   sflx,ustar,wstar,xmf,tcko,qcko,ucko,vcko)
            …
          end subroutine mfpbl
   !>  @}

Bibliography
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Doxygen can handle in-line paper citations and link to an automatically created
bibliography page. The bibliographic data for any papers that are cited need to
be put in BibTeX format and saved in a .bib file. The bib file for CCPP is
included in the repository, and the doxygen configuration option 
``cite_bib_files`` points to the included file. 

Citations are invoked with the following tag:

.. code-block:: console

   \cite bibtex_key_to_paper

Equations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See `link <http://www.doxygen.nl/manual/formulas.html>`_ for information
about including equations. For the best rendering, the following option
should be set in the Doxygen configuration file:

.. code-block:: console

   USE_MATHJAX            = YES
   MATHJAX_RELPATH        =  https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2

There are many great online resources to use the LaTeX math typesetting used in doxygen.

Doxygen Configuration
-----------------------------

Configuration File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The CCPP is distributed with a doxygen configuration file
``./ccpp/physics/physics/docs/ccpp_doxyfile``, such that you don’t need to
create an additional one.

If starting from scratch, you can generate a default configuration file using the command:

.. code-block:: console

   doxygen -g <config_file>

Then you can edit the default configuration file to serve your needs. The default
file includes plenty of comments to explain all the options. Some of the important
things you need to pay attention to are:

 * The name of your project:

.. code-block:: console

   PROJECT_NAME = ‘your project name’

* The input files (relative to the directory where you run doxygen):

.. code-block:: console

   INPUT = 

The following lines should be listed here: the doxygen mainpage text file, the
scheme pages, and the source codes to be contained in the output. The order in
which schemes are listed determines the order in the HTML result.


* The directory where to put the documentation (if you leave it empty, then the
  documentation will be created in the directory where you run doxygen):

.. code-block:: console

   OUTPUT_DIRECTORY = doc

* The type of documentation you want to generate (HTML, LaTeX and/or something else):

.. code-block:: console

   GENERATE_HTML = YES

If HTML is chosen, the following tells doxygen where to put the documentation
relative to the OUTPUT_DIRECTORY:

.. code-block:: console

   HTML_OUTPUT = html

and

.. code-block:: console

   HTML_FILE_EXTENSION = .html

determines the extension of the files.

* Other important settings for a Fortran code project are:

.. code-block:: console

   OPTIMIZE_FOR_FORTRAN        =    YES
   EXTENSION_MAPPING           = .f=FortranFree        \
                                 .F90=FortranFree      \
                                 .f90=FortranFree
   LAYOUT_FILE                 = ccpp_dox_layout.xml
   CITE_BIB_FILES              = library.bib
   FILE_PATTERN                = *.f     \
                                 *.F90   \
                                 *.f90   \
                                 *.txt
   GENERATE_TREEVIEW           = yes

Doxygen files for layout (``ccpp_dox_layout.xml``), a HTML style (``ccpp_dox_extra_style.css``),
and bibliography (``library.bib``) are provided with the CCPP. Additionally, a 
configuration file is supplied, with the following variables modified from the default:

Diagrams
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On its own, Doxygen is capable of creating simple text-based class diagrams.
With the help of the additional software GraphViz, Doxygen can generate
additional graphics-based diagrams, optionally in Unified Modeling Language (UML) style. To enable
GraphViz support, the configure file parameter ``“HAVE_DOT”`` must be set to ``“YES”``.

You can use doxygen to create call graphs of all the physics schemes in CCPP.
In order to create the call graphs you will need to set the following options in
your doxygen config file:

.. code-block:: console

   HAVE_DOT           	= YES
   EXTRACT_ALL        	= YES
   EXTRACT_PRIVATE    	= YES
   EXTRACT_STATIC     	= YES
   CALL_GRAPH         	= YES

Note that will need the DOT (graph description language) utility to be installed
when starting doxygen. Doxygen will call it to generate the graphs. On most
distributions the DOT utility can be found in the GraphViz package. Here is
the call graph for subroutine *mpdrv* in GFDL cloud microphysics generated by doxygen:

.. figure:: _static/DoxygenCallGraph.png
   :align: center

Including metadata information
-----------------------------------------------

As described above, a table of metadata information should be included in the documentation
for every CCPP entrypoint scheme. Before doxygen is run, the table for each scheme must be manually 
created in separate
files in HTML format, with one file per non-empty scheme. The HTML files are included in the
Fortran files using the doygen markup below. 

.. code-block:: fortran

   !! \htmlinclude SUBROUTINE_NAME.html
   !!

The tables should be created using a Python script distrbuted
with the CCPP Framework, ``ccpp/framework/scripts/metadata2html.py``. The syntax for running this 
script from the directory above where the CCPP is installed is:

.. code-block:: fortran

   ccpp/framework/scripts/metadata2html.py -m ccpp/physics/physics/file.meta -o ccpp/physics/physics/docs

where ``-m`` is used to specify a file with metadata information and ``-o`` is used to specify
the directory for output. Note that a single input file (``.meta``) may have more than one CCPP entrypoint
scheme, and therefore can be used to generate more than one HTML file.

Note that the ``.meta`` files are supplied with the CCPP Physics, and that there is a ``.meta`` file for
each Fortran file that contains one or more CCPP entrypoint schemes. The ``.meta`` files are located in the same
directory as the scheme Fortran files (``ccpp/physics/physics``).

To generate a complete Scientific Documentation, documentation, script ``/ccpp/framework/scripts/metadata2html.py`` 
must be run separately for each ``.meta`` file available in ccpp/physics/physics.

Using Doxygen
-------------------------------

In order to generate the doxygen-based documentation, one needs to follow five steps:

#. Have the doxygen executable installed on your computer. For example, for the NOAA
   machine Theia, add the following line into ``.cshrc`` file in your home directory:

   ``alias doxygen /scratch4/BMC/gmtb/doxygen-1.8.10/bin/doxygen``

    Source your ``.cshrc`` file. 

#. Document your code, including doxygen main page, scheme pages and inline
   comments within source code as described above.

#. Run ``metadata2html.py`` to create files in HTML format containing metadata
   information for each CCPP entrypoint scheme.

#. Prepare a Bibliography file in BibTex format for papers referred to in the physics suites. 

#. Create or edit a doxygen configuration file to control what doxygen pages, source
   files and bibliography file get parsed, how the source files get parsed, and to
   customize the output.

#. Run doxygen from directory ``ccpp/physics/physics/docs`` using the command line 
   to specify the doxygen configuration file as an argument:

  ``$doxygen $PATH_TO_CONFIG_FILE/<config_file>``

   Running this command may generate warnings or errors that need to be fixed
   in order to produce proper output. The location and type of output 
   (HTML, LaTeX, etc.) are specified in the configuration file.
   The generated HTML documentation can be viewed by pointing an HTML
   browser to the ``index.html`` file in the ``./docs/doc/html/`` directory.

For precise instructions on creating the scientific documentation, contact the GMTB
helpdesk at gmtb-help@ucar.edu.
