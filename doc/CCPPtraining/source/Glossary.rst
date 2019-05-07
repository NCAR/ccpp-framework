Glossary
*************************

.. glossary::

   CCPP
      the Common Community Physics Package designed to facilitate the implementation of
      physics innovations in state-of-the-art atmospheric models

   CCPP framework
      the driver that connects the physics schemes with a host model

   CCPP physics
      the pool of CCPP-compliant physics schemes

   dynamic CCPP build
      CCPP framework and physics libraries are dynamically linked to the executable

   entry/exit point subroutine
      a subroutine that is exposed to the CCPP framework and serves as a public interface for the 
      underlying code; for example, scheme A uses several source files and subroutines to parameterize
      a physical process, but its interaction with a host model (exchange of arguments) is entirely
      through one subroutine, which is defined as the entry/exit point subroutine

   group
      Set of physics schemes within a suite definition file (SDF) that are called together, for example ‘radiation’

   host cap
      manually-generated interface (that includes some auto-generated code) between the host
      model/application and the CCPP framework and physics

   host model/application
      an atmospheric driver or other model that allocates memory, provides metadata for the 
      variables passed into and out of the physics, and controls time-stepping; the framework
      that the physics schemes are connected with

   hybrid CCPP
      enables use of non-CCPP physics and CCPP-compliant physics in the same run

   interstitial scheme
      a scheme used to calculate the missing variables based on existing variables in the model
      and run before/after the scheme and cannot be part of the scheme itself. At present, 
      adding interstitial schemes should be done in cooperation with the GMTB Help Desk

   parameterization
      the representation, in a dynamic model, of physical effects in terms of admittedly 
      oversimplified parameters, rather than realistically requiring such effects to be 
      consequences of the dynamics of the system (AMS Glossary)

   physics cap
      auto-generated interface between an individual  physics scheme and the CCPP framework

   set
      Collection of physics schemes that do not share memory (e.g. fast and slow physics)

   standalone CCPP
      non-Hybrid CCPP. Only CCPP-compliant parameterizations can be used. Physics scheme
      selection and order is determined by an external suite definition file (SDF)

   standard_name
      variable names based on CF conventions (http://cfconventions.org) that are uniquely
      identified by CCPP physics schemes and provided by a host model

   static CCPP build
      CCPP framework and physics libraries are statistically linked to the executable

   subcycling
      executing a physics scheme in a loop with a shorter timestep than the rest of physics (or the dynamics)

   suite
      a collection of physics schemes and interstitial schemes that is known to work well together

   suite definition file (SDF)
      an external file containing information about a physics suite’s construction; its contents
      describe the schemes that are called, what order they are called, whether they are subcycled,
      and whether they are grouped into units to be called together with intervening non-physics code

   .xsd file extension
      XML schema definition
