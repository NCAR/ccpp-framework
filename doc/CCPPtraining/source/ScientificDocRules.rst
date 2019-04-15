.. _ScientificDocRules:

****************************************
Scientific Documentation Rules
****************************************

Technically, scientific documentation is not needed for a parameterization
to work with the CCPP. However, scientific and technical documentation is
important for code maintenance and for fostering understanding among stakeholders
and is required of physics schemes in order to be included in the CCPP. This
section describes the process used for documenting parameterizations in the CCPP.
Doxygen was chosen as a platform for generating human-readable output due to its
built-in functionality with Fortran, its high level of configurability, and its
ability to parse in-line comments within the source code. Keeping documentation
with the source itself increases the likelihood that the documentation will be
updated along with the underlying code. Additionally, inline documentation is
amenable to version control.

The purpose of this section is to provide an understanding of how to properly
document a physics scheme using doxygen in-line comments. It covers what kind of
information should be in the documentation, how to mark up the in-line comments
so that doxygen will parse it correctly, where to put various comments within
the code, and how to configure and run doxygen to generate HTML (or other format)
output. Part of this documentation, namely subroutine argument tables, have
functional significance as part of the CCPP infrastructure. These tables must be
in a particular format to be parsed by Python scripts that “automatically” generate
a software cap for a given physics scheme. Although the procedure outlined herein
is not unique, following it will provide a level of continuity with previous
documented schemes.  Reviewing the documentation for CCPP v2.0 parameterizations
is a good way of getting started in writing documentation for a new scheme. The
CCPP Scientific Documentation can be converted to html format 
(see https://dtcenter.org/gmtb/users/ccpp/docs/sci_doc_v2/).

=============================
Doxygen comments and commands
=============================

Firstly, be aware that regular Fortran comments using ``“!”`` are not parsed
by Doxygen. The Doxygen in-line comment block begins with ``“!>”``, and
subsequent lines begin with ``“!!”``. All Doxygen commands start with a backslash
(``“\”``) or an at-sign (``“@”``). Example in the first line of each Fortran file,
brief one sentence overview of the file purpose using Doxygen command **“\\file”**:

.. code-block:: fortran

   !> \file gwdps.f
   !! This file is the  parameterization of orographic gravity wave
   !! drag and mountain blocking.

The parameter definition begins with ``“!<”``, where the sign ``‘<’`` just tells
Doxygen that documentation follows after the member. Example:

.. code-block:: fortran

   integer, parameter, public :: NF_VGAS = 10   !< number of gas species
   integer, parameter         :: IMXCO2  = 24   !< input CO2 data longitude points
   integer, parameter         :: JMXCO2  = 12   !< input CO2 data latitude points
   integer, parameter         :: MINYEAR = 1957 !< earlist year 2D CO2 data available

If you simply want Doxygen to use the original comments, you must modify the comment 
line to use ``“!>”`` at the first line and ``“!!”`` at the subsequent lines.

=============================
Doxygen Documentation Style
=============================

To document a physics suite, a broad array of information should be included
in order to serve both software engineering and scientific purposes. The
documentation style could be divided into four categories:

* Doxygen Files
* Doxygen Pages (Overview page and scheme pages)
* Doxygen Modules
* Bibliography

-----------------------------
Doxygen files
-----------------------------

Doxygen provides the “\\file” tag as a way to provide documentation on the
Fortran source code file level. That is, in the generated documentation,
one may navigate by source code filenames (if desired) rather than through
a “functional” navigation. The most important documentation organization is
through the “module” concept mentioned below, because the division of a scheme
into multiple source files is often functionally irrelevant. Nevertheless,
using a “\file” tag provides an alternate path to navigate the documentation
and it should be included in every source file. Therefore, it is prudent to
include a small documentation block to describe what code is in each file
using the “\\file” tag, e.g.:

.. code-block:: fortran

   !> \file gwdps.f
   !! This file is the  parameterization of orographic gravity wave
   !! drag and mountain blocking.

The brief description for each file is displayed next to the source filename
on the Doxygen-generated “File List” page (see :numref:`Figure %s <doxygen_file_list>`).

.. _doxygen_file_list:

.. figure:: _static/DoxygenFileList.png
   :align: center

-----------------------------
Doxygen Overview Page
-----------------------------

Pages in Doxygen can be used for documentation that is not directly attached
to a source code entity such as file or module. They are external text files
that generate pages with a high-level scientific overview and 
typically contain a longer description of a project or suite. You can refer to
any source code entity from within the page.

The GMTB maintains a main page, created by the Doxygen command
**“\\mainpage”**, containing an overall description and background of the CCPP.  
Physics developers do not have to edit the file with the mainpage, which has a
user-visible title, but not label:

.. code-block:: console

    /**
    \mainpage Introduction
    ...
    */
 
All other pages listed under the main page are created using the Doxygen
tag **“\\page”** described in the next section. In any Doxygen page,
you can refer to any entity of source code by using Doxygen tag **“\\ref”**
or **“@ref”**. Example in ``code_overview.txt``:
 
.. code-block:: console

   The FV3GFS physics suite uses the parameterizations in the following order,
   as defined in  \c suite_SCM_GFS_2017_updated.xml and \c suite_SCM_GFS_2018_updated.xml:
    + @ref GFS_RRTMG
    + @ref GFS_SFCLYR
    + @ref GFS_NSST
    + @ref GFS_NOAH
    + @ref GFS_SFCSICE
    + @ref GFS_HEDMF
    + @ref GFS_GWDPS
    + @ref GFS_RAYLEIGH
    + @ref GFS_OZPHYS
    + @ref GFS_H2OPHYS
    + @ref GFS_SAMFdeep
    + @ref GFS_GWDC
    + @ref GFS_SAMFshal
    + GFS Microphysics (MP) scheme option:
      + @ref GFDL_cloud 
         + @ref gfdlmp
         + @ref fast_sat_adj
      + @ref GFS_ZHAOC
         + @ref condense
         + @ref precip
    + @ref GFS_CALPRECIPTYPE

The HTML result is `here <https://dtcenter.org/gmtb/users/ccpp/docs/sci_doc_v2/subpage_overview.html>`_.
You can see that the **“+”** signs before **“@ref”** generate a list with bullets.
Doxygen command **“\\c”** displays its argument using a typewriter font.

-----------------------------
Physics Scheme Pages
-----------------------------

Each major scheme in CCPP should have its own scheme page containing an
overview of the parameterization.  This page is not tied to the Fortran
code directly; instead, it is created with a separate text file that starts
with the command **“\\page”**.  Each page has a label (e.g., “GFS_ZHAOC” in
the following example) and a user-visible title (“GFS Zhao-Carr Microphysics
Scheme” in the following example). It is noted that labels must be unique
across the entire Doxygen project so that the **“\\ref”** command can be used
to create an unambiguous link to the structuring element. It therefore makes
sense to choose label names that refer to their context.

.. code-block:: console

   /**
   \page GFS_ZHAOC GFS Zhao-Carr Microphysics Scheme

   \section des_zhao Description
   This is the GFS scheme for grid-scale condensation and precipitation which is
   based on Zhao and Carr (1997) \cite zhao_and_carr_1997 and Sundqvist et al.
   (1989) \cite sundqvist_et_al_1989 .
   ...
   Figure 1 shows a schematic illustration of this scheme.
   ...
   \image  html  GFS_zhaocarr_schematic.png "Figure 1: Schematic illustration of the precipitation scheme" width=10cm
   …

   \section intro_zhao Intraphysics Communication
    + For grid-scale condensation and evaporation of cloud process 
   (\ref arg_table_zhaocarr_gscond_run)
   + For precipitation (snow or rain) production 
   (\ref arg_table_zhaocarr_precpd_run)

   \section Gen_zhao  General Algorithm
    + \ref general_gscond
    + \ref general_precpd

   */

The HTML result is `here <https://dtcenter.org/gmtb/users/ccpp/docs/sci_doc_v2/GFS_ZHAOC.html>`__.
The physics scheme page will often describe the following:

a. Description section (**“\\section”**), which usually includes:
      * Scientific origin and scheme history (**“\\cite”**)
      * Key features and differentiating points
      * A picture is sometimes worth a thousand words (**“\\image”**)

        To insert images into Doxygen documentation, you’ll need to have your
        images ready in a graphical format (i.e., .png) depending
        on which Doxygen output you’re willing to generate. For example, for LaTeX
        output the images must be provided in Encapsulated PostScript (.eps); For
        HTML output the images could be provided in the Portable Network Graphic
        (.png) format. Example of including the same image in multiple formats for
        HTML and LaTeX outputs:

.. code-block:: console

   \image  html  gfdl_cloud_mp_diagram.png "Figure 1: GFDL MP at a glance (Courtesy of S.J. Lin at GFDL)" width=10cm
   \image  latex  gfdl_cloud_mp_diagram.eps "Figure 1: GFDL MP at a glance (Courtesy of S.J. Lin at GFDL)" width=10cm

In the CCPP, images are put under ``./docs/img/`` directory.

b. Intraphysics Communication Section (**“\\section”**)

   * The argument table for CCPP entry point subroutine **{scheme}_run** will be in this section.
     It is created by inserting a reference link (**“\\ref”**) to the table in the Fortran code
     for the scheme.

c. General Algorithm Section (**“\\section”**)

   * The general description of the algorithn will be in this section.  It is created by inserting
     a reference link (**“\\ref”**) in the Fortran code for the scheme.

The symbols **“/\*\*”** and **“*/”** need to be the first and last entries of the page.
Here is an example of GFS Zhao-Carr microphysics scheme page:

Note that separate pages can also be created to document something that is not a scheme.
For example, a page could be created to describe a suite, or how a set of schemes work
together.  Doxygen automatically generates an index of all pages that is visible at the
top-level of the documentation, thus allowing the user to quickly find, and navigate
between, the available pages.

-----------------------------
Doxygen Modules
-----------------------------

The CCPP documentation is based on Doxygen modules (note this is not the same as
Fortran modules). Each Doxygen module pertains to a particular parameterization and
is used to aggregate all code related to that scheme, even when it is in separate
files. Since Doxygen cannot know which files or subroutines belong to each physics
scheme, each relevant subroutine must be tagged with the module name. This allows
Doxygen to understand your modularized design and generate the documentation accordingly.
`Here <https://dtcenter.org/gmtb/users/ccpp/docs/sci_doc_v2/modules.html>`__ 
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
significant modifications or additions to a file , consider adding an
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
there should be some additional documentation. First, using the “\brief”
command, a brief one or two sentence description of the scheme should be
included. After a blank doxygen comment line, begin the scheme origin
and history using “\version”, “\author” and “\date”.

Each subroutine that is a CCPP entry point to a parameterization, should
be further documented with a documentation block immediately preceding
its definition in the source. The documentation block should include at
least the following components:

* A brief one- or two-sentence description with the \brief tag
* A more detailed one or two paragraph description of the function of the subroutine
* An argument table that includes entries for each subroutine argument
   * The argument table content should be immediately preceded by the following line:


.. code-block:: fortran

   !!\section arg_table_SUBROUTINE_NAME

This line is also functional documentation used during the CCPP prebuild step.  The
first line of the table should contain the following “header” names

a. **local_name**: contains the local subroutine variable name
b. **standard_name**: CF-compliant standard name
c. **long_name**: a short description
d. **units**: format follows “unit exponent”, i.e. m2 s-2 for m2/s2 
e. **rank**: 0 for scalar, 1 for 1-D array, 2 for 2-D array, etc.
f. **type**: integer, real, logical, etc.
g. **kind**: the specified floating point precision kind (at present, to be
   extended to different integer kinds in the future)
h. **intent**: in, out, inout
i. **optional**: T/F

The argument table should be immediately followed by a blank doxygen line “!!”,
which is needed to denote the end of an argument table. Here is an example :

.. code-block:: fortran

   !! \section arg_table_scheme_X__run Argument Table
   !! | local_name | standard_name                            | long_name                                   | units   | rank | type    |    kind   | intent | optional |
   !! |------------|------------------------------------------|---------------------------------------------|---------|------|---------|-----------|--------|----------|
   !! | im         | horizontal_loop_extent                   | horizontal loop extent                      | count   |    0 | integer |           | in     | F        |
   !! | levs       | vertical_dimension                       | vertical layer dimension                    | count   |    0 | integer |           | in     | F        |
   !! | vdftra     | vertically_diffused_tracer_concentration | tracer concentration diffused by PBL scheme | kg kg-1 |    3 | real    | kind_phys | inout  | F        |

The order of arguments in the table does not have to match the order of actual
arguments in the subroutine, but it is preferred.

* A section called “General Algorithm” with a bullet or numbered list of
  the tasks completed in the subroutine algorithm

* At the end of initial subroutine documentation block, a “Detailed algorithm”
  section is started and the entirety of the code  is encompassed with the
  “!> @{” and “!> @}” delimiters. This way, any comments explaining detailed
  aspects of the code are automatically included in the “Detailed Algorithm” section.

For subroutines that are not a CCPP entry point to a scheme, no argument table
is required. But it is suggested that following “\ingroup” and “\brief”, use
“\param” to define each argument with local name, a short description and unit, i.e.,

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

-----------------------------
Bibliography
-----------------------------

Doxygen can handle in-line paper citations and link to an automatically created
bibliography page. The bibliographic data for any papers that are cited need to
be put in BibTeX format and Saved in a .bib file. The bib file for CCPP is
included in the repository, and the Doxygen configuration option 
``cite_bib_files`` points to the included file. 

Citations are invoked with the following tag:

.. code-block:: console

   \cite bibtex_key_to_paper

-----------------------------
Equations
-----------------------------

See `link <http://www.doxygen.nl/manual/formulas.html>`_ for information
about including equations. For the best rendering, the following option
should be set in the Doxygen configuration file:

.. code-block:: console

   USE_MATHJAX            = YES
   MATHJAX_RELPATH        =  https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2

There are many great online resources to use the LaTeX math typesetting used in Doxygen.

============================
Doxygen Configuration
============================

-----------------------------
Configuration file
-----------------------------
The CCPP contains a Doxygen configuration file
``./ccpp/physics/physics/docs/ccpplatex_dox``, such that you don’t need to
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

* The input files (relative to the directory where you run Doxygen):

.. code-block:: console

   INPUT = 

The following lines should be listed here: the Doxygen mainpage text file, the
scheme pages, and the source codes to be contained in the output. The order in
which schemes are listed determines the order in the html result.


* The directory where to put the documentation (if you leave it empty, then the
  documentation will be created in the directory where you run Doxygen):

.. code-block:: console

   OUTPUT_DIRECTORY = doc

* The type of documentation you want to generate (HTML, LaTeX and/or something else):

.. code-block:: console

   GENERATE_HTML = YES

If HTML is chosen, the following tells doxygen where to put the html documentation
relative OUTPUT_DIRECTORY:

.. code-block:: console

   HTML_OUTPUT = html
   HTML_FILE_EXTENSION = .html

where ``HTML_FILE_EXTENSION`` tells what the extension of the html files should be.

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

Doxygen files for layout (ccpp_dox_layout.xml), a html style (ccpp_dox_extra_style.css),
and bibliography (library.bib) are provided with the CCPP. Additionally, a 
configuration file is supplied, with the following variables modified from the default:

-------------------------------
Diagrams
-------------------------------

On its own, Doxygen is capable of creating simple text-based class diagrams.
With the help of the additional software GraphViz, Doxygen can generate
additional graphics-based diagrams, optionally in UML style. To enable
GraphViz support, the configure file parameter “HAVE_DOT” must be set to “YES”.

You can use Doxygen to create call graphs of all the physics schemes in CCPP.
In order to create the call graphs you will need to set the following options in
your Doxygen config file:

.. code-block:: console

   HAVE_DOT           	= YES
   EXTRACT_ALL        	= YES
   EXTRACT_PRIVATE    	= YES
   EXTRACT_STATIC     	= YES
   CALL_GRAPH         	= YES

Note that will need the DOT (graph description language) utility to be installed
when starting Doxygen. Doxygen will call it to generate the graphs. On most
distributions the DOT utility can be found in the GraphViz package. Here is
the call graph for subroutine mpdrv in GFDL cloud microphysics generated by Doxygen:

.. figure:: _static/DoxygenCallGraph.png
   :align: center

===============================
Using Doxygen
===============================
In order to generate Doxygen-based documentation, you need to follow four steps:

1. Have the Doxygen executable installed on your computer. This is already done on
   theia machine. Add the following line into .cshrc file under your home directory:

   ``alias doxygen/scratch4/BMC/gmtb/doxygen-1.8.10/bin/doxygen``

Source your .cshrc file. 

2. Document your code, including Doxygen main page, scheme pages and in-line
   comments within source code as described above.

3. Prepare a Bibliography file in BibTex format for paper referred in the physics suite. 

4. create or edit a Doxygen configuration file to control what Doxygen pages, source
   files and bibliography file get parsed, how the source files get parsed, and to
   customize the output.

5. Run the Doxygen command from the command line with the doxygen configuration file
   given as an argument:

  ``$doxygen $PATH_TO_CONFIG_FILE/<config_file>``

Running this command may output compiler-like warning or errors that need to be fixed
in order to produce proper output. Output is generated depending on the type specified
(HTML, LaTeX, etc.) in the configuration file and is put in a location specified in the
configuration file. The generated HTML documentation can be viewed by pointing a HTML
browser to the ``index.html`` file in the ``./docs/doc/html/`` directory.

For precise instructions on creating the scientific documentation, contact the GMTB
helpdesk at gmtb-help@ucar.edu.
