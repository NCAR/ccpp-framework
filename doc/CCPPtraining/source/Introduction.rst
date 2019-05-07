.. _introduction:
.. include:: prolog.inc 

------------
Introduction
------------

This document contains training material for the Common Community Physics Package (CCPP). It decsribes the:

* CCPP overview
* physics schemes and interstitials
* suite definition files
* CCPP-compliant parameterizations
* adding a new scheme/suite
* host-side coding
* fundamentals of obtaining, pre-building, building and running the CCPP with NEMSfv3gfs
* CCPP code management and governance

For the latest version of the released code, please visit the `GMTB Website <http://www.dtcenter.org/gmtb/users/ccpp>`_

Please send questions and comments to the help desk: `gmtb-help@ucar.edu`

How To Use This Document
------------------------

This table describes the type changes and symbols used in this guide.

+------------------------+------------------------------+---------------------------------------+
| **Typeface or Symbol** |  **Meaning**                 |  **Example**                          |
+========================+==============================+=======================================+
| ``AaBbCc123``          | The names of commands,       | Edit your ``.bashrc`` |br|            |
|                        | files, and directories; |br| | Use ``ls -a`` to list all files. |br| |
|                        | on-screen computer output    | ``host$ You have mail!``              |
+------------------------+------------------------------+---------------------------------------+
| :mod:`AaBbCc123`       | What you type contrasted     | ``host$`` :mod:`su`                   |
|                        | with on-screen computer      |                                       |
|                        | output                       |                                       |
+------------------------+------------------------------+---------------------------------------+
| ``%``                  | Command-line prompt          | ``% cd $TOP_DIR``                     |
+------------------------+------------------------------+---------------------------------------+

Following these typefaces and conventions, shell commands, code examples, namelist variables, etc.
will be presented in this style:

.. code-block:: console

   % mkdir ${TOP_DIR}
