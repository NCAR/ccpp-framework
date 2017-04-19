#------------------------------------------------------------------------------
# Add all the physics schemes in their own sub-directory (a.k.a repos)
add_subdirectory(noop)
add_subdirectory(check)
add_subdirectory(scm)

#------------------------------------------------------------------------------
# Push all the sources upwards
add_sources()
