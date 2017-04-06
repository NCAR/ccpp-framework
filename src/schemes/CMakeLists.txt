#------------------------------------------------------------------------------
# Add all the physics schemes in their own sub-directory (a.k.a repos)
add_subdirectory(noop)
add_subdirectory(check)

#------------------------------------------------------------------------------
# Set the sources
add_sources(
    dummy.f90
    dummy_scm.f90
    dummy_scm2.f90
)
