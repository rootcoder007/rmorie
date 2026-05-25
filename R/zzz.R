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
  # Python-port placeholders (intentional `exists()`-guarded lookups)
  "morie_tps_load_tps_dataset",
  "morie_tps_load_tps",
  "morie_spatial_spillover_decomposition",
  # geepack::geeglm NSE: cluster id column added at runtime then passed
  # by bare name to the formula-style `id` arg (see 3MMM.48 fix).
  ".gee_cluster_id_int_"
))
