# SPDX-License-Identifier: AGPL-3.0-or-later
# basicspace's Fortran (aldmck, blackbox, blackbox_transpose) writes past its arrays on a matrix
# with a single row or a single column and only errors afterwards (valgrind, 2026-09-22). rmorie
# never calls basicspace; the tests that compare against it check the shape first with this.
.sv_basicspace_shape_ok <- function(M, min_rows = 2L, min_cols = 2L) {
  is.matrix(M) && is.double(M) && nrow(M) >= min_rows && ncol(M) >= min_cols
}
