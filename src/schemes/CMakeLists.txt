#------------------------------------------------------------------------------
# Add all the physics schemes in their own sub-directory (a.k.a repos)
add_subdirectory(mynn)
add_subdirectory(rrtmg)

#------------------------------------------------------------------------------
# Set the sources
add_sources(
    bc.f90
    dummy.f90
    gf.f90
    mp.f90
    pbl.f90
    ruclsm.f90
    sfc.f90
    thompsonaero.f90
)
