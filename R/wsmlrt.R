# SPDX-License-Identifier: AGPL-3.0-or-later
#' Likelihood ratio test from two maximised log-likelihoods.
#'
#' The degrees of freedom are the DIFFERENCE IN DIMENSION, not the number
#' of parameters in either model. A negative statistic is raised rather
#' than clamped to zero.
#'
#' Formula: lambda = 2 (l_full - l_null); p = P(chi^2_df > lambda)
#'
#' @param loglik_full Maximised log-likelihood over the whole space.
#' @param loglik_null Maximised log-likelihood over the null subspace.
#' @param df dim(Theta) - dim(Theta_0), at least 1.
#' @return List with \code{statistic}, \code{p_value}, \code{df},
#'   \code{loglik_full}, \code{loglik_null}.
#' @references Wasserman (2004), All of Statistics, Definition 10.21 and
#'   Theorem 10.22, under which lambda converges to chi^2 with r - q
#'   degrees of freedom, "the dimension of Theta minus the dimension of
#'   Theta_0". Fetched as the full text of the book.
#' @export
Lrtest <- function(loglik_full, loglik_null, df) {
  lf <- as.numeric(loglik_full); ln <- as.numeric(loglik_null)
  df <- as.integer(df)
  if (df < 1L) stop("df must be at least 1")
  lam <- 2 * (lf - ln)
  if (lam < 0)
    stop("the unrestricted log-likelihood is below the restricted one; the models are not nested or one did not converge")
  .t1_result(statistic = lam, p_value = stats::pchisq(lam, df, lower.tail = FALSE),
             df = as.numeric(df), loglik_full = lf, loglik_null = ln,
             method = "Likelihood ratio test, Wasserman Theorem 10.22")
}
