# SPDX-License-Identifier: AGPL-3.0-or-later
#' Goodness of fit of a loglinear model: Pearson chi-squared and G^2
#'
#' Source READ FROM THE CORPUS PDF, page rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.5, printed page 851, equations (8.95) and (8.96):
#' \code{chi2 = sum (y_ij - n p_ij)^2 / (n p_ij)} and
#' \code{G2 = 2 sum y_ij log(y_ij / (n p_ij))}, with \code{sum y_ij = n}
#' and p_ij the cell probabilities estimated under the model.  The book
#' calls (8.95) the Pearson-residual statistic and (8.96) the
#' likelihood-quotient statistic, notes that G2 is usually preferred
#' because its minimum comes from the maximum-likelihood estimate, and
#' states that both are asymptotically chi-squared.
#'
#' The default model is independence (8.97),
#' \code{log n pi_ij = mu + lam_i^X + lam_j^Y}, whose fitted counts are
#' the row-times-column product over the grand total and whose degrees
#' of freedom are \code{(k1 - 1)(k2 - 1)}.  Fitted counts for any other
#' model may be passed in \code{expected} with the matching \code{df}.
#'
#' \code{0 log 0} is taken as 0.
#'
#' @param observed Contingency table (matrix) of non-negative counts.
#' @param expected Optional fitted cell counts under the model.
#' @param df Degrees of freedom; required when expected is supplied.
#' @param alpha Significance level for the reject decision.
#' @return list: statistic, g2, chisq, expected, df, pvalue,
#'   pvalue_chisq, reject, n.
#' @examples
#' Llgsq(matrix(c(10, 20, 30, 40), 2, 2))$g2
#' @export
Llgsq <- function(observed, expected = NULL, df = NULL, alpha = 0.05) {
  o <- as.matrix(observed)
  storage.mode(o) <- "double"
  k1 <- nrow(o)
  k2 <- ncol(o)
  if (k1 == 0L || k2 == 0L) stop("the table must not be empty")
  if (any(!is.finite(o)) || any(o < 0)) {
    stop("every observed count must be finite and non-negative")
  }
  total <- sum(o)
  if (total <= 0) stop("the table must contain at least one positive count")
  if (is.null(expected)) {
    if (k1 < 2L || k2 < 2L) stop("the independence model needs a table of at least 2 by 2")
    e <- outer(rowSums(o), colSums(o)) / total
    if (is.null(df)) df <- (k1 - 1L) * (k2 - 1L)
  } else {
    e <- as.matrix(expected)
    storage.mode(e) <- "double"
    if (nrow(e) != k1 || ncol(e) != k2) stop("observed and expected must have the same shape")
    if (is.null(df)) stop("df is required when expected is supplied")
  }
  df <- as.integer(df)[1]
  if (is.na(df) || df < 1L) stop("df must be at least 1")
  if (!(alpha > 0 && alpha < 1)) stop("alpha must be strictly between 0 and 1")
  if (any(!is.finite(e)) || any(e <= 0)) {
    stop("every expected count must be finite and positive")
  }
  chi <- sum((o - e)^2 / e)
  g2 <- 2 * sum(ifelse(o > 0, o * log(o / e), 0))
  pg <- stats::pchisq(g2, df, lower.tail = FALSE)
  pc <- stats::pchisq(chi, df, lower.tail = FALSE)
  list(
    statistic = g2, g2 = g2, chisq = chi, expected = e, df = df,
    pvalue = pg, pvalue_chisq = pc, reject = pg < alpha, n = total
  )
}
