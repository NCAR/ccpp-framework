# Capgen Test

Contains tests for overall capgen capabilities such as:
- Multiple suites
- Multiple groups
- General DDT usage
- DDT with undocumented DDT member variable
- Dimensions with `ccpp_constant_one:N` and just `N`
- Non-standard dimensions (not just horizontal and vertical) (including integer dimensions)
- Variables that should be promoted to suite level
- Dimensions that are set in the register phase and used to allocate module-level
  interstitial variables
- Threading

