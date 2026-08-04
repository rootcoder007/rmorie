# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayes factor for H0: p = p* against a nonparametric alternative.
#'
#' Everything is done in logs. \code{lam} is the prior weight of the
#' ALTERNATIVE, so the null carries 1 - lam.
#'
#' Formula: B_n = prod_i p*(X_i) / int prod_i p(X_i) dPi_1(p);
#'   Pi_n(p = p* | X) = (1-lam) L* / [ (1-lam) L* + lam int L dPi_1 ]
#'
#' @param loglik_null log prod_i p*(X_i).
#' @param log_marginal_alt log int prod_i p(X_i) dPi_1(p).
#' @param lam Prior weight of the ALTERNATIVE, in (0, 1).
#' @return List with \code{log_bayes_factor}, \code{bayes_factor},
#'   \code{posterior_null}, \code{posterior_alt}, \code{prior_null},
#'   \code{lam}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Section 10.5.1 and Theorem 10.24.
#'   The book's prose calls lambda the weight of the null while its own
#'   formula gives the null the weight 1 - lambda; the FORMULA is
#'   followed here. Read from the copy of the book held in the corpus.
#' @export
Ptnulltst <- function(loglik_null, log_marginal_alt, lam = 0.5) {
  ln <- as.numeric(loglik_null); la <- as.numeric(log_marginal_alt)
  lam <- as.numeric(lam)
  if (lam <= 0 || lam >= 1)
    stop("lam must lie strictly between 0 and 1")
  lbf <- ln - la
  a <- log(1 - lam) + ln
  b <- log(lam) + la
  mx <- max(a, b)
  denom <- mx + log(exp(a - mx) + exp(b - mx))
  post0 <- exp(a - denom)
  .t1_result(log_bayes_factor = lbf, bayes_factor = exp(lbf),
             posterior_null = post0, posterior_alt = 1 - post0,
             prior_null = 1 - lam, lam = lam,
             method = "Point-null Bayes factor, Ghosal Section 10.5.1")
}
