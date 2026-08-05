# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nonparametric control-function estimator
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 5.5.2, pages 186-187.  The model is
#'
#'   Y = g(X) + U,  E(U | V = v, W = w) = E(U | V = v),
#'   V = X - E(X | W)                                           (5.92)
#'
#' which is NON-NESTED with the NPIV model (5.4): it does not require
#' E(U | W = w) = 0, and that condition does not imply (5.92) either.
#' Conditioning on (X, W) is equivalent to conditioning on (V, W), so
#' with h(v) = E(U | V = v)
#'
#'   E(Y | X, V) = g(X) + h(V)                                  (5.93)
#'
#' which is a nonparametric ADDITIVE model, estimated here by
#' backfitting with local-linear smoothers (Chapter 3, as the text
#' directs).  V is not observed, so it is replaced by
#' Vhat = X - Ehat(X | W) with a nonparametric first stage, following
#' Newey, Powell and Vella (1999).
#'
#' h is called a control function because it "controls" for the
#' influence of X on U; it is not a nuisance term to be discarded, and
#' it is returned.
#'
#' This is NOT the parametric Rivers-Vuong control function, in which
#' both stages are linear and the second stage is a single added
#' regressor.  Here both stages are nonparametric and nothing is
#' assumed linear; the two coincide only when g and E(X | W) happen to
#' be affine.
#'
#' Additive models identify g and h only up to a constant that can be
#' shifted between them, so both are returned mean-zero with the level
#' carried by intercept.
#'
#' @param x Numeric vector, the endogenous regressor.
#' @param y Numeric vector, the response.
#' @param w Numeric vector, the instrument.
#' @param bandwidth Numeric or NULL; common bandwidth.  Default is
#'   Silverman's rule per variable.
#' @param iters Integer, backfitting sweeps.  A FIXED count with no
#'   tolerance-based exit, so both language arms take the same path.
#' @return Named list with g_hat, h_hat, v_hat, intercept, fitted,
#'   resid_sd, bandwidth_x, bandwidth_v, bandwidth_w, n, method.
#' @keywords internal
#' @examples
#' n <- 40
#' w <- sin(0.7 * seq_len(n))
#' x <- 2 * w + 0.5 * cos(1.9 * seq_len(n))
#' Hrzctrl(x, x + w, w)$resid_sd
#' @export
Hrzctrl <- function(x, y, w, bandwidth = NULL, iters = 30L) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  w <- as.numeric(w)
  n <- length(x)
  if (length(y) != n || length(w) != n) {
    stop(sprintf("x, y, w must have the same length; got %d, %d, %d.",
                 n, length(y), length(w)))
  }
  if (n < 4L) stop(sprintf("need at least 4 observations, got %d.", n))
  iters <- as.integer(iters)
  if (iters < 1L) stop(sprintf("iters must be at least 1, got %d.", iters))

  hw <- if (is.null(bandwidth)) .hrz_silverman(w) else as.numeric(bandwidth)
  hx <- if (is.null(bandwidth)) .hrz_silverman(x) else as.numeric(bandwidth)

  # First stage: Vhat = X - Ehat(X | W).
  ex_w <- .hrz3_ll_smooth(w, x, w, hw)
  v_hat <- x - ex_w

  hv <- if (is.null(bandwidth)) {
    .hrz_silverman(v_hat)
  } else {
    as.numeric(bandwidth)
  }

  # Second stage: backfit the additive model (5.93).
  ybar <- sum(y) / n
  g <- rep(0, n)
  hh <- rep(0, n)
  for (it in seq_len(iters)) {
    rg <- y - ybar - hh
    g <- .hrz3_ll_smooth(x, rg, x, hx)
    g <- g - sum(g) / n
    rh <- y - ybar - g
    hh <- .hrz3_ll_smooth(v_hat, rh, v_hat, hv)
    hh <- hh - sum(hh) / n
  }

  fitted <- ybar + g + hh
  resid_sd <- sqrt(sum((y - fitted)^2) / n)

  list(g_hat = g, h_hat = hh, v_hat = v_hat, intercept = ybar,
       fitted = fitted, resid_sd = resid_sd, bandwidth_x = hx,
       bandwidth_v = hv, bandwidth_w = hw, n = n,
       method = paste("Horowitz (2009) eqs. (5.92)-(5.93),",
                      "nonparametric control function"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzctrl
#' @keywords internal
#' @export
morie_horowitz_control_function <- Hrzctrl
