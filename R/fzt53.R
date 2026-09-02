# SPDX-License-Identifier: AGPL-3.0-or-later

#' Asymptotic normality of the boundary-free KDFE (Theorem 5.3)
#'
#' Theorem 5.3: under D1-D5,
#' \deqn{\frac{\tilde F_X(x) - F_X(x)}{\sqrt{\mathrm{Var}\[\tilde F_X(x)\]}} \to_D N(0,1).}{(Ftilde(x) - F(x)) / sqrt(Var\[Ftilde(x)\]) ->_D N(0,1).}
#'
#' The proof is a Lyapunov argument, easy for a reason worth keeping: because
#' `0 <= W(v) <= 1` for every `v`, the `(2+delta)` moment of each summand is
#' bounded by `2^(2+delta) < Inf` with NO assumption on `F_X`. The estimator
#' averages bounded variables, so Lyapunov's condition is automatic.
#'
#' Returns the standardised statistic and a Wald interval for `F_X(x)`, clipped
#' to `\[0,1\]`: the estimand is a probability, and an unclipped Wald interval
#' routinely leaves the unit interval near the boundary -- precisely the region
#' this construction exists to handle.
#'
#' The CENTRING is `F_X(x)`, not `E\[Ftilde(x)\]`. The theorem standardises by
#' the variance alone, so the interval inherits the `O(h^2)` bias of Theorem
#' 5.2 and is not bias-corrected; supply `bias` to subtract it.
#'
#' @param estimate `Ftilde(x)`.
#' @param variance `Var\[Ftilde(x)\]`, e.g. from `Bfkdfbv`.
#' @param null The value of `F_X(x)` to test against.
#' @param bias Bias to subtract before standardising.
#' @param level Confidence level for the interval.
#' @return Named list with ``statistic``, ``p_value``, ``lower``, ``upper``, ``se``, ``level``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.3.
#' @examples
#' Bfkdfnorm(estimate = 0.5, variance = 0.0025, null = 0.5)
#' @export
Bfkdfnorm <- function(estimate, variance, null = NULL, bias = 0, level = 0.95) {
  if (variance <= 0) stop("the variance must be positive.")
  if (!(level > 0 && level < 1)) stop("level must lie strictly in (0, 1).")
  se <- sqrt(variance)
  centre <- estimate - bias
  z <- stats::qnorm(0.5 + level / 2)
  if (is.null(null)) {
    stat <- NA_real_; pval <- NA_real_
  } else {
    stat <- (centre - null) / se
    pval <- 2 * (1 - stats::pnorm(abs(stat)))
  }
  list(statistic = stat, p_value = pval,
       lower = max(0, centre - z * se), upper = min(1, centre + z * se),
       se = se, level = level,
       method = "asymptotic normality of the boundary-free KDFE (Theorem 5.3)")
}

# CANONICAL TEST
# r <- Bfkdfnorm(estimate = 0.5, variance = 0.0025, null = 0.5)
# stopifnot(abs(r$statistic) < 1e-15)

#' @rdname Bfkdfnorm
#' @keywords internal
#' @export
morie_fauzi_thm5_3_bdfree_normality <- Bfkdfnorm
