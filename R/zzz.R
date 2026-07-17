# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Package-level imports + global-variable declarations.
#
# This file silences R CMD check's "no visible binding for global
# variable" warnings for two valid patterns morie uses:
#
# 1. `.data` from rlang -- used by ggplot2 NSE inside aes(). morie
#    doesn't formally depend on rlang (ggplot2 is in Suggests), so we
#    declare `.data` as a global to keep the rlang dependency optional
#    while still satisfying the check.
#
# 2. Python-port placeholder lookups in tps_statphysics.R + laniyonu_*
#    where code does `if (exists("morie_tps_load_tps_dataset")) ...`
#    and then calls the function in the conditional branch. R's static
#    analyzer flags the call site as an undefined global because the
#    function is only defined in the Python sibling, not in R. These
#    are intentional NotYetPorted placeholders.

utils::globalVariables(c(
  # ggplot2/rlang NSE
  ".data",
  # Python-port placeholder (intentional `exists()`-guarded lookup;
  # the tps loaders graduated to real R functions in tps_statphysics.R
  # and must NOT be declared here -- pkgload reports a declared global
  # that shadows a real export as a mask conflict)
  "morie_spatial_spillover_decomposition",
  # geepack::geeglm NSE: cluster id column added at runtime then passed
  # by bare name to the formula-style `id` arg (see 3MMM.48 fix).
  ".gee_cluster_id_int_"
))

#' Internal helper: OnLoad
#' @noRd
.onLoad <- function(libname, pkgname) {
  # The fast-stat kernels in src/morie_fast.cpp resolve rmbl_* routines
  # that rmoriebricklayer registers via R_RegisterCCallable (LinkingTo).
  # R_GetCCallable only finds them once the provider's DLL is loaded, and
  # a DESCRIPTION Imports: alone does not load it -- so load its namespace
  # (which triggers its useDynLib + registration) before any C call.
  requireNamespace("rmoriebricklayer", quietly = TRUE)
  # Seed the stat-command registry at load time (single source of the
  # seeds; see R/stat_commands.R). try() so a downstream optional-dep
  # failure never aborts package load.
  try(.morie_seed_stat_commands(), silent = TRUE)
}
