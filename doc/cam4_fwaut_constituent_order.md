# cam4 (QPC4) bit-for-bit difference: root cause is constituent registration order

**Status:** root cause found and **proven**. Decision requested from CAM-SIMA.
**Date:** 2026-06-11   **Author:** D. Heinzeller

## Executive summary

The CAM-SIMA test
`SMS_D_Ln9.mpasa120_mpasa120.QPC4.derecho_intel.cam-outfrq_analy_ic_cam4`
(full cam4 physics on the MPAS dynamical core) fails its bit-for-bit (b4b)
comparison against the capgen baseline. The difference is **machine-epsilon
roundoff** — state and flux fields agree to 14–17 significant digits; the
comparison is loud only in RK-microphysics *ratio* diagnostics (e.g. `FWAUT`,
RMS ≈ 4.24e-2), which are ratios of two near-zero autoconversion rates and so
amplify any roundoff. The behavior is identical under GNU and Intel.

The physics source, `suite_cam4.xml`, and `src/data/registry.xml` are
**byte-identical** between the two builds. The difference is purely in the
generated CCPP caps. We have traced it to a single cause and **proven** it:

> **capgen registers the advected constituents in a different order than the
> original capgen.** Specifically, `cloud_liquid` and `cloud_ice`
> are swapped. This changes the floating-point summation order in the energy/water
> thermodynamic diagnostics, which the energy fixer then spreads across all columns
> as a tiny, pervasive heating — the source of the b4b difference.

A one-off patch that forces capgen's advected water species into the
original-capgen order makes **QPC4 bit-for-bit identical** to the baseline.

## The difference (runtime constituent list, `debug_output = 2`)

| index | original capgen (baseline) | capgen |
|------:|----------------------------|-----------|
| 1 | **cloud_liquid** (advected) | **cloud_ice** (advected) |
| 2 | **cloud_ice** (advected) | **cloud_liquid** (advected) |
| 3 | water_vapor (advected) | water_vapor (advected) |
| 4–10 | CFC12, O3, CH4, O2, N2O, CFC11, CO2 | CFC12, O2, CH4, CO2, O3, N2O, CFC11 |

Indices 1–3 are the advected water species; 4–10 are non-advected trace gases.
The advected block is what matters (see mechanism). `water_vapor` is index 3 in
both — the only advected difference is the **cloud_liquid ↔ cloud_ice swap**.

## Mechanism

1. `air_composition` builds `thermodynamic_active_species_idx` by walking the
   advected constituents in **constituent-index order**.
2. `get_hydrostatic_energy` (`cam_thermo`) sums the water species in that order.
   Baseline sums `cloud_liquid + cloud_ice + water_vapor`; capgen sums
   `cloud_ice + cloud_liquid + water_vapor`. Same values, **different FP order**.
3. The resulting machine-eps difference in total energy/water is picked up by the
   global energy fixer (`check_energy_fix`), which redistributes it as a uniform
   heating across all columns. From that point the two runs differ at roundoff
   level everywhere, surfacing loudly only in ratio diagnostics like `FWAUT`.

`air_composition.F90` and `cam_constituents.F90` are byte-identical between the
two builds, so the entire difference originates in the registration order the
generated cap produces. In the CCPP framework, registration order is the
hash-table iteration order in `ccpp_model_constituents_t%lock_table` (advected
packed first) — i.e. an arbitrary, generator-dependent order, not a deliberate
physical ordering.

## Proof

Forcing capgen's advected water species into the baseline order
`[cloud_liquid = 1, cloud_ice = 2, water_vapor = 3]` (a flag-guarded one-off
patch in the framework's `ccp_model_const_table_lock`) makes QPC4 reproduce the
ccpp-prebuild baseline **bit-for-bit** (cprnc: all fields identical). This
isolates constituent ordering as the *sole* cause. See section "Artifacts"
below for the full patch.

## Assessment — neither order is "wrong"

Both builds register the same constituents with identical properties; the
ordering is not physically meaningful, and the resulting solutions are
roundoff-equivalent and both physically correct. The b4b failure reflects only
that capgen's (arbitrary) order differs from the (equally arbitrary) order
the capgen baseline happened to produce.

## Decision requested

To resolve QPC4 (and any other case sensitive to constituent order), we propose:

1. Give capgen a **deterministic, documented** constituent-registration order
   (e.g. water vapor first, with a clear rule for how constituents land in the
   array) — replacing today's hash-bucket order.
2. Adopt the new documented order and **re-baseline** the affected CAM-SIMA cases once.

The temporary proof patch will be removed once the path is agreed.

## Artifacts

- **Patch:** Stored as `ccpp_constituent_prop_mod.F90.patch` in the top-level
directory of the `feature/capgen` ccpp-framework branch):
```
--- capgen/src/ccpp_constituent_prop_mod.F90
+++ capgen/src/ccpp_constituent_prop_mod.F90
@@ -1392,6 +1392,17 @@
     type(ccpp_constituent_properties_t), pointer :: cprop
     character(len=dimname_len) :: dimname
     character(len=*), parameter :: subname = 'ccp_model_const_table_lock'
+    ! === ONE-OFF cam4 constituent-reorder experiment ===
+    ! When .true., force the cam4 advected water species into original-capgen
+    ! order [cloud_liquid=1, cloud_ice=2, water_vapor=3] instead of hash-table
+    ! order, to prove the FWAUT b4b diff is driven purely by constituent order.
+    ! Only the 3 cam4 water-species std-names are remapped; everything else keeps
+    ! its normal hash-order index, so other suites are unaffected unless they
+    ! advect exactly these names.  Flip to .false. (or delete) to restore.
+    logical, parameter :: l_const_reorder = .true.
+    integer            :: const_pos
+    character(len=512) :: sname_reorder
+    ! === end experiment ===
 
     astat = 0
     errcode_local = 0
@@ -1460,9 +1471,24 @@
                   errcode_local = errcode_local + 1
                   exit
                 end if
-                call cprop%set_const_index(index_advect, &
+                ! === ONE-OFF cam4 constituent-reorder experiment ===
+                const_pos = index_advect
+                if (l_const_reorder) then
+                  call cprop%standard_name(sname_reorder, &
+                      errcode=errcode, errmsg=errmsg)
+                  select case (trim(sname_reorder))
+                  case ('cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water')
+                    const_pos = 1
+                  case ('cloud_ice_mixing_ratio_wrt_moist_air_and_condensed_water')
+                    const_pos = 2
+                  case ('water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water')
+                    const_pos = 3
+                  end select
+                end if
+                call cprop%set_const_index(const_pos, &
                     errcode=errcode, errmsg=errmsg)
-                call this%const_metadata(index_advect)%set(cprop)
+                call this%const_metadata(const_pos)%set(cprop)
+                ! === end experiment ===
               else
                 index_const = index_const + 1
                 if (index_const > num_vars) then
```

- **Run directories (Derecho) Intel:** Because the SIMA baselines change continuously, 
  - Baseline (original capgen, https://github.com/climbfuji/CAM-SIMA/tree/feature/capgen-reference):
    - `/glade/derecho/scratch/heinzell/aux_sima_intel_20260614203021/`
    - capgen differences to be evaluated against this baseline, because the official baseline changes frequently
    - Both the capgen baseline and the capgen test fail for this test:
```
  SMS_Ln9.ne3pg3_ne3pg3_mg37.FKESSLER.derecho_intel.cam-outfrq_se_cslam_multitape (Overall: NLFAIL) details:
    FAIL SMS_Ln9.ne3pg3_ne3pg3_mg37.FKESSLER.derecho_intel.cam-outfrq_se_cslam_multitape NLCOMP
```
  - capgen (https://github.com/climbfuji/CAM-SIMA/tree/feature/capgen), unpatched (shows the FWAUT diff):
    - `/glade/derecho/scratch/heinzell/aux_sima_intel_20260614202951/` with the following `mpasa120_mpasa120.QPC4` test dirs:
      - `SMS_Ln9.mpasa120_mpasa120.QPC4.derecho_intel.cam-outfrq_analy_ic_cam4.GC.aux_sima_intel_20260614202951.ORIGINAL_NO_PATCH`
      - `SMS_D_Ln9.mpasa120_mpasa120.QPC4.derecho_intel.cam-outfrq_analy_ic_cam4.GC.aux_sima_intel_20260614202951.ORIGINAL_NO_PATCH`
  - capgen (https://github.com/climbfuji/CAM-SIMA/tree/feature/capgen) + reorder patch (**b4b**):
    - `/glade/derecho/scratch/heinzell/aux_sima_intel_20260614202951/` with the following `mpasa120_mpasa120.QPC4` test dirs:
      - `SMS_Ln9.mpasa120_mpasa120.QPC4.derecho_intel.cam-outfrq_analy_ic_cam4.GC.aux_sima_intel_20260614202951`
      - `SMS_D_Ln9.mpasa120_mpasa120.QPC4.derecho_intel.cam-outfrq_analy_ic_cam4.GC.aux_sima_intel_20260614202951`

- **Run directories (Derecho) GNU:** Because the SIMA baselines change continuously, 
  - Baseline (original capgen, https://github.com/climbfuji/CAM-SIMA/tree/feature/capgen-reference):
    - `/glade/derecho/scratch/heinzell/aux_sima_gnu_20260611123848/`
    - capgen differences to be evaluated against this baseline, because the official baseline changes frequently
    - Both the capgen baseline and the capgen test fail for this test:
```
  SMS_Ln2.ne3pg3_ne3pg3_mg37.FPHYStest.derecho_gnu.cam-outfrq_hb_vdiff_derecho (Overall: FAIL) details:
    FAIL SMS_Ln2.ne3pg3_ne3pg3_mg37.FPHYStest.derecho_gnu.cam-outfrq_hb_vdiff_derecho RUN time=13
  SMS_Ln9.ne3pg3_ne3pg3_mg37.FADIAB.derecho_gnu.cam-outfrq_se_cslam (Overall: FAIL) details:
    FAIL SMS_Ln9.ne3pg3_ne3pg3_mg37.FADIAB.derecho_gnu.cam-outfrq_se_cslam RUN time=13
```
  - capgen (https://github.com/climbfuji/CAM-SIMA/tree/feature/capgen), unpatched (shows the FWAUT diff):
    - `/glade/derecho/scratch/heinzell/aux_sima_gnu_20260611123837/` with the following `mpasa120_mpasa120.QPC4` test dirs:
      - `SMS_Ln9.mpasa120_mpasa120.QPC4.derecho_gnu.cam-outfrq_analy_ic_cam4.GC.aux_sima_gnu_20260611123837.ORIGINAL_NO_PATCH`
      - `SMS_D_Ln9.mpasa120_mpasa120.QPC4.derecho_gnu.cam-outfrq_analy_ic_cam4.GC.aux_sima_gnu_20260611123837.ORIGINAL_NO_PATCH`
  - capgen (https://github.com/climbfuji/CAM-SIMA/tree/feature/capgen) + reorder patch (**b4b**):
    - `/glade/derecho/scratch/heinzell/aux_sima_gnu_20260611123837/` with the following `mpasa120_mpasa120.QPC4` test dirs:
      - `SMS_Ln9.mpasa120_mpasa120.QPC4.derecho_gnu.cam-outfrq_analy_ic_cam4.GC.aux_sima_gnu_20260611123837`
      - `SMS_D_Ln9.mpasa120_mpasa120.QPC4.derecho_gnu.cam-outfrq_analy_ic_cam4.GC.aux_sima_gnu_20260611123837`
