# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random forest for multivariate/multi-output response
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 633-681\], Chapter 15 "Random Forest for Genomic Prediction",
#' read as rendered page images.  Section 15.4, pp. 639-640, gives the forest
#' algorithm itself (quoted in aaa_mvsml_rf_shared.R); Section 15.7, p. 656,
#' gives the multivariate splitting rule, which is what makes this the
#' multi-output and not the single-output problem.
#'
#' Page 656, attributing the rule to Tang and Ishwaran (2017): "Assuming that
#' there are measures of q traits in each observation, that is,
#' y_i = (y_i,1, ..., y_i,q)^T, the goal is to minimize the multivariate sums
#' of squares (MSS), MSS = sum_\{j=1\}^\{q\} ( sum_\{i=1\}^\{L\} (y_ij - ybar_Lj)^2 +
#' sum_\{i=1\}^\{R\} (y_ij - ybar_Rj)^2 )  (15.5), where ybar_Lj and ybar_Rj are
#' the sample means of the jth response variable in the left and right daughter
#' nodes."  The page then requires standardisation -- "such a splitting rule
#' (15.5) can only be effective if each of the response variables is measured
#' on the same scale ... We therefore calibrate MSS by assuming that each
#' response variable has been standardized (with mean zero and variance equal
#' to one).  The standardization is applied prior to splitting a node" -- and
#' gives the working form MSS = sum_\{j=1\}^\{q\} ( (1/n_L)(sum_\{i=1\}^\{L\} y*_ij)^2
#' + (1/n_R)(sum_\{i=1\}^\{R\} y*_ij)^2 )  (15.6).
#'
#' BOOK ERRATUM, recorded, and it inverts the optimisation.  The page says
#' "minimizing MSS is equivalent to minimizing" (15.6).  It is equivalent to
#' MAXIMISING it.  Expanding (15.5), MSS = sum_j sum_all y^2 -
#' \[ (sum_L y)^2/n_L + (sum_R y)^2/n_R \], so the bracket -- which is exactly
#' (15.6) -- enters with a minus sign and the first term does not depend on the
#' split.  Three further things on the same page agree: (15.7), the
#' classification analogue with the identical functional form, is introduced
#' with "the best split s for X is obtained by maximizing"; the page closes by
#' saying "(15.6) and (15.7) are equivalent optimization problems"; and reusing
#' the label MSS for a quantity that is not the sum of squares is itself a slip.
#' This implementation maximises (15.6), and the anchors verify against (15.5)
#' computed directly.
#'
#' DETERMINISM is described in aaa_mvsml_rf_shared.R: the bootstrap of step 1
#' is drawn by a fixed LCG and the mtry candidates of step 2(a) by van der
#' Corput offsets, so both arms grow the identical forest.
#'
#' @param X n-by-p matrix of independent variables.
#' @param Y_matrix n-by-q matrix of responses; q = 1 is the ordinary regression
#'   forest.
#' @param n_trees B, the number of trees.
#' @param mtry number of candidate variables per split; p/3 rounded up by
#'   default, the p. 643 regression default.
#' @param nodesize minimum terminal node size; 5 by default, the p. 643
#'   regression default.
#' @param standardize apply the p. 656 standardisation before splitting.
#' @return list: estimate, y_hat, importance, mss, oob_size, mtry, n, method.
#' @keywords internal
#' @examples
#' X <- cbind(sin((0:19) * 0.7), (0:19) / 20)
#' Rfmlt(X, cbind(2 * X[, 1], X[, 2]), 5L)$mtry
#' @export
Rfmlt <- function(X, Y_matrix, n_trees = 100L, mtry = NULL, nodesize = 5L,
                  standardize = TRUE) {
  XX <- .s03mat(X)
  YY <- .s03mat(Y_matrix)
  d <- .rfcheck(XX, YY)
  n <- d[1]
  p <- d[2]
  q <- d[3]
  B <- as.integer(n_trees)
  if (B < 1L) stop("rf_multivariate: n_trees must be at least 1")
  ns <- as.integer(nodesize)
  if (ns < 1L) stop("rf_multivariate: nodesize must be at least 1")
  m <- if (is.null(mtry)) .rfmtry(p) else as.integer(mtry)
  if (m < 1L || m > p) {
    stop("rf_multivariate: mtry must lie between 1 and the number of columns of X")
  }
  Ys <- if (standardize) .rfstd(YY, n, q) else YY
  f <- .rfforest(XX, Ys, B, ns, m, q)
  yhat <- .rfpredict(f$trees, XX, q)
  imp <- .rfperm(f$trees, f$oob, XX, Ys, q)
  mss <- 0
  for (j in seq_len(q)) mss <- mss + sum((Ys[, j] - sum(Ys[, j]) / n)^2)
  list(estimate = sum(yhat) / (n * q), y_hat = yhat, importance = imp,
       mss = mss, oob_size = vapply(f$oob, length, 0L), mtry = m, n = n,
       method = paste0("Chapter 15 Sect. 15.4 forest with the eq. (15.6) ",
                       "multivariate split, maximised (the page's ",
                       "'minimizing' is an erratum)"))
}
