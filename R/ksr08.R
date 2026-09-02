# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multiplier bootstrap with exponential weights (Dirichlet weights).
#'
#' The weights are divided by their own mean so the total weight stays n.
#' With standard exponential multipliers mu = tau = 1, so the scaling
#' factor is 1 -- written out rather than dropped, because it is not 1 for
#' any other weight distribution.
#'
#' Formula: Ptilde_n f = n^-1 sum_i (xi_i / xibar_n) f(X_i);
#'   Gtilde_n = sqrt(n) (mu/tau) (Ptilde_n - P_n), xi ~ Exp(1)
#'
#' @param x The sample.
#' @param B Number of replicates (fixed budget).
#' @param seed Seed for the pinned generator.
#' @return List with \code{estimate}, \code{boot_mean}, \code{boot_sd},
#'   \code{process_sd}, \code{ci_lower}, \code{ci_upper}, \code{mu},
#'   \code{tau}, \code{B}, \code{n}.
#' @references Kosorok (2008), Introduction to Empirical Processes and
#'   Semiparametric Inference, Section 2.2.3. Fetched as the full text of
#'   the book.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Multboot(V)
Multboot <- function(x, B = 200, seed = 1) {
  x <- .t1_vec(x); n <- length(x); B <- as.integer(B)
  if (n < 2L) stop("the sample must have at least two observations")
  if (B < 2L) stop("B must be at least 2")
  Pn <- mean(x)
  g <- .t1_lcg(seed)
  stat <- numeric(B)
  for (b in seq_len(B)) {
    w <- numeric(n)
    for (i in seq_len(n)) {
      u <- g$unif()
      if (u <= 0) u <- 1e-300
      w[i] <- -log(u)
    }
    wb <- mean(w)
    if (wb == 0) stop("the multiplier weights summed to zero")
    stat[b] <- sum(w / wb * x) / n
  }
  bm <- mean(stat); bsd <- stats::sd(stat)
  q <- sort(stat)
  lo <- q[max(1L, floor(0.025 * (B - 1)) + 1L)]
  hi <- q[min(B, ceiling(0.975 * (B - 1)) + 1L)]
  .t1_result(estimate = Pn, boot_mean = bm, boot_sd = bsd,
             process_sd = sqrt(n) * 1 * bsd, ci_lower = lo, ci_upper = hi,
             mu = 1, tau = 1, B = as.numeric(B), n = as.numeric(n),
             method = "Multiplier bootstrap, Kosorok Section 2.2.3")
}

# NAMESPACE exported both of these names, but only the short function above
# was ever defined, so loading the namespace could not resolve them. Same
# alias pattern as ksr02.R.

#' @rdname Multboot
#' @keywords internal
#' @export
morie_ksr08_kosorok_multiplier_bootstrap <- Multboot

#' @rdname Multboot
#' @keywords internal
#' @export
morie_kosorok_multiplier_bootstrap <- Multboot
