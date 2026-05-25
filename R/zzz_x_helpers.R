# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Internal helper: ensure x is a numeric matrix with >= 2 columns, as
# required by caret / glmnet / xgboost / randomForest. Vector inputs
# (e.g. rnorm(80)) get expanded to a (n x 2) design matrix by adding
# a squared-feature column; 1-column matrices get the same treatment.
# Multi-column matrices and data frames pass through unchanged.
#
# This is invoked at the top of every morie ML-callable function so
# users can pass a vector predictor without hitting glmnet's
# "x should be a matrix with 2 or more columns" error.
.morie_ensure_design_matrix <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.vector(x) && !is.list(x)) {
    x <- as.numeric(x)
    return(cbind(x = x, x_sq = x * x))
  }
  if (is.matrix(x) && ncol(x) < 2L) {
    v <- as.numeric(x[, 1L])
    return(cbind(x = v, x_sq = v * v))
  }
  x
}
