.. _Overview:

*************************
CCPP Overview
*************************

Ideas for this project originated within the Earth System Prediction Capability (ESPC)
Physics Interoperability group, which has representatives from NCAR, the Navy, NOAA
Research, NOAA National Weather Service, and other groups. Physics Interoperability,
or the ability to run a given physics suite in various host models, has been a goal
of this multi-agency group for several years. An initial mechanism to run the physics
of NOAA’s Global Forecast System (GFS) model in other host models was developed by
the NOAA Environmental Modeling Center (EMC) and later augmented by NOAA GFDL.  The
CCPP expanded on that work by meeting additional requirements put forth by
`NOAA <https://dtcenter.org/gmtb/users/ccpp/developers/requirements/CCPP_REQUIREMENTS.pdf>`_,
and brought new functionalities to the physics-dynamics interface. Those include
the ability to choose the order of parameterizations, to subcycle individual 
parameterizations by running them more frequently than other parameterizations, 
and to group arbitrary sets of parameterizations allowing other computations in
between them (e.g., dynamics and coupling computations).

The architecture of the CCPP and its connection to a host model is shown in
:numref:`Figure %s <ccpp_arch_host>`.
There are two distinct parts to the CCPP: a library of physical parameterizations
that conforms to selected standards (CCPP-Physics) and an infrastructure (CCPP-Framework)
that enables connecting the physics to host models. Chapter 2 discusses the requirements
for making parameterizations CCPP-compliant and Chapter 5 discusses how to create suites
from parameterizations.

.. _ccpp_arch_host:

.. figure:: _static/ccpp_arch_host.png
   :align: center

   *Architecture of the CCPP and its connection with a host model,
   here represented as the driver for an atmospheric model (yellow box). The dynamical
   core (dycore), the physics, and other aspects of the model (such as coupling) are
   connected to the driving host through the pool of physics caps. The CCPP-Physics is
   denoted by the gray box at the bottom of the physics, and encompasses the
   parameterizations, which are accompanied by parameterization caps.*

The host model also needs to have functional documentation for any variable that will be
passed to, or received from, the physics. The CCPP-Framework is used to compare the variables
requested by each physical parameterization against those provided by the host model, and
check whether they are available (otherwise an error is issued).  This process has greatly
served to expose the variables passed between physics and dynamics, and also to clarify
how suites exchange information among parameterizations. During runtime, the CCPP-Framework
is responsible for communicating the necessary variables between the host model and the
parameterizations, as described in Chapter 6.

Building the CCPP involves a prebuild step, described in Chapter 3, and an actual build
step (described in Chapter 8). As of this writing, the CCPP has been connected with the
GMTB Single Column Model (SCM) and with various configurations of NOAA’s Unified
Forecast System (UFS). As described in detail later in this document, there are
multiple modes of building the CCPP along with the UFS, and the user should choose the
mode that best matches his needs. With the CCPP dynamic build, all CCPP-compliant
parameterizations are compiled into a library which is linked to the host model at
runtime. Conversely, with the CCPP static build, a single suite is compiled into a
library and linked to the host model when it is compiled. The dynamic build favors
flexibility, as users can choose the parameterizations and their order at runtime,
while the static build favors performance, as it provides superior execution time and
a smaller memory footprint. The type of build defines several differences in the
creation and use of the auto-generated code, many of which are not exposed to the user.
The differences pertain to  the interfaces between CCPP-Framework and the physics
(physics caps - described in Chapter 4) and the host model (host model cap), as well
as in the procedures for calling the physics. When the CCPP is used with the SCM, the
dynamic build is used.

The CCPP Physics is envisioned to contain parameterizations and suites that are used
operationally in the UFS, as well as parameterizations that are under development 
for possible transition to operations in the future. By distributing and supporting
the CCPP to the scientific community, a large number of scientists can experiment
with and innovate physics.  :numref:`Figure %s <ccpp_ecosystem>` shows the CCPP ecosystem,
a significant effort in transition of research to operations that can benefit physics
developers, users, and NOAA operations.  The CCPP-Physics and its associated CCPP-Framework
are developed as open source codes, follow industry-standard code management practices,
and are freely distributed through GitHub (https://github.com/NCAR/ccpp-physics and
https://github.com/NCAR/ccpp-framework). The code management for CCPP is described in Chapter 7.

.. _ccpp_ecosystem:

.. figure:: _static/ccpp_ecosystem.png
   :align: center

   *CCPP ecosystem*

The first public release of CCPP took place in April 2018 and included all parameterizations
of the operational GFS v14, along with the ability to connect to the GMTB Single Column Model.
The second public release of the CCPP took place in August of 2018 and additionally included
the GFS v15 physics suite.  Since then, additional parameterizations have been made CCPP-compliant,
in order to encompass the suites that are under consideration for GFS v16.

A summary of the parameterization in the CCPP can be found in :numref:`Table %s <scheme_suite_table>`.
The first row lists the suites currently available in the CCPP. The first column
denotes the types of parameterizations, where cu stands for convective parameterization and H2O for
photolysis. The operational GFS suite (column 1) used Zhao-Carr microphysics, Eddy-Diffusivity Mass
Flux (EDMF) PBL, scale-aware Simplified Arakawa Schubert (saSAS) convection, Rapid Radiation Transfer
Model for General Circulation Models (RRTMG) radiation, GFS surface layer, Noah LSM, Navy Research
Laboratory (NRL) ozone, and no photolysis scheme. Variations used in the other suites are denoted in
bold. The FV3GFS v1 suite, targeted for March 2019 operational implementation, uses the GFDL 
microphysics and the NRL 2015 ozone and photolysis schemes. The other three suites are candidates for
future operational implementations. The FV3GFS v1.1 suite uses Turbulent Kinetic Energy (TKE)-based
EDMF, the EMC and Climate Process Team (CPT) suite uses Morrison-Gettelman 3 (MG3) microphysics and
Chikira-Sugiyama convection with Arakawa-Wu extension (CSAW) deep convection, and  the NOAA Global
Systems Division (GSD) suite uses Thompson microphysics,  Mellor-Yamada-Nakashini-Niino (MYNN) PBL,
shallow convection and surface layer scheme, and Rapid Update Cycle (RUC) LSM.  In addition
to the schemes listed, it is expected that, as time goes on, other parameterizations will be
considered for inclusion in the CCPP. 

.. _scheme_suite_table:

.. table:: *Schemes and suites available in the CCPP*

   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | **Schemes/Suites** | **GFS v14** | **GFS V15** | **FV3GFS v1.1** | **EMC/CPT** | **GSD**      |
   +====================+=============+=============+=================+=============+==============+
   | Microphysics       | Zhao-Carr   | GFDL        | GFDL            | MG3         | Thompson     |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | PBL                | EDMF        | EDMF        | TKE EDMF        | EDMF        | MYNN         |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | Deep cu            | saSAS       | saSAS       | saSAS           | CSAW        | GF           |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | Shallow cu         | saSAS       | saSAS       | saSAS           | saSAS       | MYNN         |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | Radiation          | RRTMG       | RRTMG       | RRTMG           | RRTMG       | RRTMG        |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | Surface Layer      | GFS         | GFS         | GFS             | GFS         | MYNN         |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | Land surface       | Noah        | Noah        | Noah            | Noah        | RUC          |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | Ozone              | NRL 2006    | NRL 2015    | NRL 2015        | NRL 2015    | NRL 2015     |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+
   | H2O                | None        | NRL 2015    | NRL 2015        | NRL 2015    | NRL 2015     |
   +--------------------+-------------+-------------+-----------------+-------------+--------------+

The CCPP is governed by the groups that contribute to its development.  Governance for the 
CCPP-Physics is currently led by NOAA, and the GMTB works with EMC and the NGGPS Program Office
to determine which schemes and suites should be included and supported. Governance for the
CCPP-Framework is done jointly by NOAA and NCAR, and more information can be found at
https://github.com/NCAR/ccpp-framework/wiki. Additional information can also be found on
the GMTB website at https://dtcenter.org/gmtb/users/ccpp. Please direct all inquiries to gmtb-help@ucar.edu. 
