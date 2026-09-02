# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random forest mean decrease in impurity (MDI) variable importance
#'
#' NOT IN THE BOOK.  Montesinos Lopez, Montesinos Lopez and Crossa (2022),
#' Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#' Springer, was searched in full -- all seventeen page-range volumes and the
#' index, \[Pages 683-691\].  Chapter 15, volume \[Pages 633-681\], is the random
#' forest chapter, and the only variable importance it defines is the
#' out-of-bag PERMUTATION measure of pp. 642-643, implemented separately as
#' Rfpmi.  The words "mean decrease in impurity" do not occur; impurity appears
#' only as the splitting criterion, where p. 643 names Gini for classification
#' and the "weighted mean squared error splitting criterion" for regression,
#' both attributed to Breiman, Friedman, Olshen and Stone (1984), Chapter 8.4
#' and Chapter 4.3.
#'
#' CITATION CARE, because the obvious attribution is wrong.  Mean decrease in
#' impurity is NOT defined in Breiman, L. (2001), Random forests, Machine
#' Learning 45(1), 5-32, doi:10.1023/A:1010933404324.  That paper defines only
#' the permutation measure; its Section 10 computes importance by permuting a
#' variable in the out-of-bag cases, and it never sums impurity decreases over
#' the nodes that split on a variable.
#'
#' The formula implemented here is the one stated in Louppe, G., Wehenkel, L.,
#' Sutera, A. and Geurts, P. (2013), Understanding variable importances in
#' forests of randomized trees, Advances in Neural Information Processing
#' Systems 26, which writes it for a forest of M trees as
#' Imp(X_j) = (1/M) sum_T sum_{t in T : v(s_t) = X_j} p(t) Delta i(s_t, t),
#' with p(t) = N_t/N the fraction of samples reaching node t and
#' Delta i(s_t, t) = i(t) - (N_tL/N_t) i(t_L) - (N_tR/N_t) i(t_R).  The
#' impurity function i itself, and the practice of accumulating its decrease,
#' are from Breiman, Friedman, Olshen and Stone (1984), Classification and
#' Regression Trees, Wadsworth, the source the book's own p. 643 cites for the
#' splitting rules.  The impurity used here is the within-node sum of squares,
#' matching the regression rule the book names.
#'
#' DETERMINISM is described in aaa_mvsml_rf_shared.R.
#'
#' @param forest the number of trees to grow; NULL means 100.
#' @param X n-by-p matrix of independent variables.
#' @param y length-n response, or an n-by-q matrix for the multivariate case.
#' @param mtry,nodesize as in Rfmlt; the p. 643 regression defaults.
#' @return list: estimate, importance, relative, ranking, total, mtry, n,
#'   method.
#' @keywords internal
#' @examples
#' X <- cbind(sin((0:19) * 0.7), (0:19) / 20)
#' Rfmdi(5L, X, 2 * X[, 1])$ranking
#' @export
Rfmdi <- function(forest, X, y, mtry = NULL, nodesize = 5L) {
  XX <- .s03mat(X)
  YY <- if (is.matrix(y) || is.data.frame(y)) .s03mat(y) else matrix(as.numeric(y), ncol = 1L)
  d <- .rfcheck(XX, YY); n <- d[1]; p <- d[2]; q <- d[3]
  B <- if (is.null(forest)) 100L else as.integer(forest)
  if (B < 1L) stop("rf_mdi_importance: n_trees must be at least 1")
  ns <- as.integer(nodesize)
  if (ns < 1L) stop("rf_mdi_importance: nodesize must be at least 1")
  m <- if (is.null(mtry)) .rfmtry(p) else as.integer(mtry)
  if (m < 1L || m > p) {
    stop("rf_mdi_importance: mtry must lie between 1 and the number of columns of X")
  }
  Ys <- .rfstd(YY, n, q)
  f <- .rfforest(XX, Ys, B, ns, m, q)
  imp <- .rfmdi_imp(f$trees, p)
  tot <- sum(imp)
  rel <- if (tot > 0) imp / tot else rep(0, p)
  order <- order(-imp, seq_len(p)) - 1L
  list(estimate = imp[order[1L] + 1L], importance = imp, relative = rel,
       ranking = order, total = tot, mtry = m, n = n,
       method = paste0("MDI of Louppe et al. (2013) with the CART (1984) ",
                       "sum-of-squares impurity; not in the book, and not in ",
                       "Breiman (2001) either"))
}
