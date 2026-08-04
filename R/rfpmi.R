# SPDX-License-Identifier: AGPL-3.0-or-later
#' Permutation-based RF variable importance (out-of-bag permutation)
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 633-681], Chapter 15, Section 15.4, pp. 642-643, read as
#' rendered page images.
#'
#' The book gives this measure in PROSE ONLY.  It states no equation and
#' attaches no equation number to it anywhere in the chapter, and the chapter's
#' numbered equations (15.5)-(15.8) are the splitting rules, not the
#' importance.  What it says is that the prediction error is computed on the
#' out-of-bag observations, then "the values of the jth variable are randomly
#' permuted in the OOB observations and j new PE is computed.  The differences
#' between the two are then averaged over all the trees, and normalized by the
#' standard deviation of the differences.  The variable showing the largest
#' decrease in prediction accuracy is the most important variable."  Page 640
#' defines the out-of-bag set: "Each tree makes use of around two-thirds
#' (63.2%) of the observations to build the tree.  The remaining observations
#' are referred to as Out-Of-Bag (OOB)."
#'
#' Implemented exactly as that prose reads:
#' Imp(X_j) = mean_b [ PE_b(X_j permuted) - PE_b ] / sd_b [ same ], with PE_b
#' the mean squared error over tree b's own out-of-bag rows.  The normalisation
#' by the standard deviation across trees is the book's, and is what makes the
#' result a z-like score rather than a raw error difference; pass
#' normalise = FALSE for the raw mean difference.
#'
#' DETERMINISM.  The book says "randomly permuted".  The permutation used is
#' the reversal of the out-of-bag row order, which is deterministic, is a
#' genuine permutation, and is identical in both arms.  The out-of-bag sets
#' come from the shared deterministic bootstrap in aaa_mvsml_rf_shared.R, whose
#' out-of-bag fraction reproduces the book's own 36.8%.
#'
#' @param forest the number of trees to grow; NULL means 100.  A forest object
#'   is not accepted: this package's forests are grown deterministically from
#'   the data, so the number of trees is the only thing that needs carrying.
#' @param X n-by-p matrix of independent variables.
#' @param y length-n response, or an n-by-q matrix for the multivariate case.
#' @param mtry,nodesize as in Rfmlt; the p. 643 regression defaults.
#' @param normalise divide by the standard deviation of the per-tree
#'   differences, as the book's own sentence prescribes.
#' @return list: estimate, importance, ranking, oob_size, mtry, n, method.
#' @keywords internal
#' @examples
#' X <- cbind(sin((0:19) * 0.7), (0:19) / 20)
#' Rfpmi(5L, X, 2 * X[, 1])$ranking
#' @export
Rfpmi <- function(forest, X, y, mtry = NULL, nodesize = 5L, normalise = TRUE) {
  XX <- .s03mat(X)
  YY <- if (is.matrix(y) || is.data.frame(y)) .s03mat(y) else matrix(as.numeric(y), ncol = 1L)
  d <- .rfcheck(XX, YY); n <- d[1]; p <- d[2]; q <- d[3]
  B <- if (is.null(forest)) 100L else as.integer(forest)
  if (B < 2L) stop("rf_permutation_importance: need at least two trees to normalise")
  ns <- as.integer(nodesize)
  if (ns < 1L) stop("rf_permutation_importance: nodesize must be at least 1")
  m <- if (is.null(mtry)) .rfmtry(p) else as.integer(mtry)
  if (m < 1L || m > p) {
    stop("rf_permutation_importance: mtry must lie between 1 and the number of columns of X")
  }
  Ys <- .rfstd(YY, n, q)
  f <- .rfforest(XX, Ys, B, ns, m, q)
  imp <- .rfperm(f$trees, f$oob, XX, Ys, q, normalise)
  order <- order(-imp, seq_len(p)) - 1L
  list(estimate = imp[order[1L] + 1L], importance = imp, ranking = order,
       oob_size = vapply(f$oob, length, 0L), mtry = m, n = n,
       method = paste0("OOB permutation VIM of Chapter 15 pp. 642-643, prose ",
                       "only -- the book states no equation for it"))
}
