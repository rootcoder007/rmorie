# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P5: separation in a logistic fit (research/lean/P5Separation.lean;
# Albert & Anderson 1984; Weisburd & Britt ch. 1 on the Baldus proportionality-review logit).
#
#   Research.P5.ll1_strictMono / ll1_neg     each term -log(1 + exp(-u)) is strictly increasing and < 0
#   Research.P5.loglik_lt_shift              s_i x_i.d > 0 for all i  =>  loglik(b + t d) > loglik(b) for t > 0
#   Research.P5.loglik_lt_shift_quasi        s_i x_i.d >= 0 for all i and > 0 for one  =>  the same
#   Research.P5.no_mle / no_mle_quasi        hence no maximum-likelihood estimate exists
#   Research.P5.loglik_neg                   loglik(b) < 0 for every b
#   Research.P5.loglik_tendsto_zero          loglik(t d) -> 0 as t -> infinity along a separating direction

#' Detect separation in a logistic regression and show why the fit cannot converge
#'
#' With labels coded as signs \eqn{s_i = 2y_i - 1}, a direction \eqn{d} with
#' \eqn{s_i x_i \cdot d > 0} for every observation (complete separation), or
#' \eqn{\ge 0} for all and \eqn{> 0} for one (quasi-complete), makes the
#' log-likelihood strictly increase along \eqn{d} from every starting point
#' (\code{Research.P5.loglik_lt_shift}, \code{loglik_lt_shift_quasi}), so no
#' maximum-likelihood estimate exists (\code{no_mle}, \code{no_mle_quasi}) and
#' the likelihood climbs towards its supremum 0 without reaching it
#' (\code{loglik_neg}, \code{loglik_tendsto_zero}). A fitting routine that
#' stops with huge coefficients and a warning is reporting this geometry, not
#' a misspecified model. The exact check is a linear programme (Konis 2007):
#' maximise \eqn{\sum_i s_i x_i \cdot d} subject to \eqn{0 \le s_i x_i \cdot d \le 1};
#' a positive optimum is a separating direction. The programme is solved by
#' rmorie's own dense tableau simplex (Dantzig pricing with Bland's rule
#' after a run of degenerate pivots, so it cannot cycle); it is the same
#' programme the earlier \pkg{lpSolve} path solved and reaches the same
#' optimum (cross-validated in the test suite). \code{method = "glm"}
#' instead inspects a fit with many iterations for fitted probabilities at
#' 0 or 1, which is reported as the heuristic it is.
#' @param y Binary outcome (0/1).
#' @param x Design matrix (one row per observation); an intercept column is
#'   added unless \code{intercept = FALSE}.
#' @param intercept Add a column of ones.
#' @param method \code{"auto"} or \code{"lp"} (the exact linear programme,
#'   native) or \code{"glm"} (the fit heuristic).
#' @return A list with \code{separation} (\code{"none"}, \code{"quasi-complete"}
#'   or \code{"complete"}), \code{direction} (a separating \eqn{d}, or
#'   \code{NULL}), \code{margins} (\eqn{s_i x_i \cdot d}), \code{n_zero_margin},
#'   \code{loglik_along} (the log-likelihood at \eqn{t d} for
#'   \eqn{t = 0, 1, 10, 100}: increasing towards 0 when separated),
#'   \code{method} and \code{theorems}.
#' @examples
#' # one perfectly predictive dummy: every y = 1 has z = 1
#' x <- cbind(z = c(1, 1, 1, 0, 0, 0, 0), w = c(0.2, -1, 0.5, 0.1, -0.4, 1.2, 0.3))
#' y <- c(1, 1, 1, 0, 0, 0, 0)
#' morie_logit_separation(y, x)[c("separation", "loglik_along")]
#' @references Konis, K. (2007). \emph{Linear programming algorithms for
#'   detecting separated data in binary logistic regression models}. DPhil
#'   thesis, University of Oxford.
#' @export
morie_logit_separation <- function(y, x, intercept = TRUE, method = c("auto", "lp", "glm")) {
  method <- match.arg(method)
  x <- as.matrix(x)
  n <- nrow(x)
  if (length(y) != n) stop("y must have one entry per row of x", call. = FALSE)
  if (!all(y %in% c(0, 1))) stop("y must be 0/1", call. = FALSE)
  if (isTRUE(intercept)) x <- cbind("(Intercept)" = 1, x)
  s <- 2 * y - 1
  sx <- s * x                        # rows s_i x_i
  loglik <- function(b) sum(-log1p(exp(-as.numeric(sx %*% b))))
  if (method == "auto") method <- "lp"
  direction <- NULL
  if (method == "lp") {
    # max sum_i s_i x_i.d  s.t.  0 <= s_i x_i.d <= 1, d free
    p <- ncol(x)
    # free variables: d = dp - dm with dp, dm >= 0; the rows are
    # -sx d <= 0 and sx d <= 1 (all right-hand sides >= 0)
    obj <- c(colSums(sx), -colSums(sx))
    con <- rbind(cbind(-sx, sx), cbind(sx, -sx))
    rhs <- c(rep(0, n), rep(1, n))
    sol <- .lsep_simplex_max(obj, con, rhs)
    if (sol$status == 0L && sol$objval > 1e-8) direction <- sol$solution[seq_len(p)] - sol$solution[p + seq_len(p)]
  } else {
    fit <- suppressWarnings(stats::glm.fit(x, y, family = stats::binomial(), control = list(maxit = 200)))
    fitted <- fit$fitted.values
    if (any(fitted < 1e-8 | fitted > 1 - 1e-8)) direction <- as.numeric(fit$coefficients)
  }
  margins <- if (is.null(direction)) NULL else as.numeric(sx %*% direction)
  sep <- "none"
  if (!is.null(margins)) {
    tol <- 1e-10 * max(1, max(abs(margins)))
    if (all(margins > tol)) sep <- "complete"
    else if (all(margins > -tol) && any(margins > tol)) sep <- "quasi-complete"
    else if (method == "glm") sep <- "none"
  }
  if (sep == "none") direction <- NULL
  along <- if (is.null(direction)) NULL else {
    ts <- c(0, 1, 10, 100)
    stats::setNames(vapply(ts, function(t) loglik(t * direction), numeric(1)), paste0("t=", ts))
  }
  list(separation = sep, direction = direction, margins = margins,
       n_zero_margin = if (is.null(margins)) NA_integer_ else sum(abs(margins) <= 1e-10 * max(1, max(abs(margins)))),
       loglik_along = along, method = method,
       theorems = c("Research.P5.loglik_lt_shift", "Research.P5.loglik_lt_shift_quasi", "Research.P5.no_mle",
                    "Research.P5.no_mle_quasi", "Research.P5.loglik_neg", "Research.P5.loglik_tendsto_zero"))
}

# Dense tableau simplex for  max c'z  s.t.  A z <= b, z >= 0, with b >= 0, so
# the slack basis is feasible and no phase one is needed. Dantzig pricing;
# after 50 consecutive degenerate pivots it switches to Bland's rule (lowest
# eligible index for both the entering and the leaving variable), which
# cannot cycle. Returns status 0 (optimal), 3 (unbounded) or 4 (iteration
# cap), the solution and the objective value.
.lsep_simplex_max <- function(cvec, A, b, tol = 1e-9, max_iter = 50000L) {
  A <- as.matrix(A)
  m <- nrow(A)
  nv <- ncol(A)
  if (any(b < 0)) stop(".lsep_simplex_max needs b >= 0", call. = FALSE)
  N <- nv + m
  Tab <- unname(cbind(A, diag(m), as.numeric(b)))
  z <- c(-as.numeric(cvec), numeric(m), 0)
  basis <- nv + seq_len(m)
  degenerate_run <- 0L
  for (it in seq_len(max_iter)) {
    red <- z[seq_len(N)]
    elig <- which(red < -tol)
    if (!length(elig)) {
      sol <- numeric(N)
      sol[basis] <- Tab[, N + 1L]
      return(list(status = 0L, solution = sol[seq_len(nv)], objval = z[N + 1L],
                   iterations = it - 1L))
    }
    bland <- degenerate_run >= 50L
    j <- if (bland) elig[1L] else elig[which.min(red[elig])]
    col <- Tab[, j]
    pos <- which(col > tol)
    if (!length(pos)) return(list(status = 3L, solution = NULL, objval = Inf))
    ratio <- Tab[pos, N + 1L] / col[pos]
    cand <- pos[ratio <= min(ratio) + tol]
    i <- cand[which.min(basis[cand])]
    degenerate_run <- if (Tab[i, N + 1L] <= tol) degenerate_run + 1L else 0L
    Tab[i, ] <- Tab[i, ] / Tab[i, j]
    others <- which(Tab[, j] != 0)
    others <- others[others != i]
    if (length(others)) {
      Tab[others, ] <- Tab[others, , drop = FALSE] -
        outer(Tab[others, j], Tab[i, ])
    }
    z <- z - z[j] * Tab[i, ]
    basis[i] <- j
  }
  list(status = 4L, solution = NULL, objval = NA_real_)
}
