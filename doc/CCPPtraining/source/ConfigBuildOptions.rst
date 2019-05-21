.. _ConfigBuildOptions:
  
*****************************************
CCPP Configuration and Build Options
*****************************************
Overview
While the CCPP-Framework code can be compiled independently, the CCPP-Physics code can only be used within a host modeling system that provides the variables and the kind, type, and DDT definitions. As such, it is advisable to integrate the CCPP configuration and build process with the host model’s. Part of the build process, known as the prebuild step since it precedes compilation, involves running a Python script that performs multiple functions. These functions include configuring the CCPP-Physics for use with the host model and autogenerating FORTRAN code to communicate variables between the physics and the dynamical core. The prebuild step will be discussed in detail in Chapter 8.
There are some differences between building and running the SCM and the UFS Atmosphere. In the case of the UFS Atmosphere as the host model, there are several build options (Fig. 3.1). The choice can be specified through command-line options supplied to the compile.sh script for manual compilation or through a regression test (RT) configuration file. Detailed instructions for building the code are discussed in Chapter 9.
