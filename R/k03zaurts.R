# SPDX-License-Identifier: AGPL-3.0-or-later

## Zivot-Andrews (1992) asymptotic critical values at 1%, 5% and 10%,
## in the encoding used by urca::ur.za.
.k03_za_cval <- list(
  intercept = c(-5.34, -4.80, -4.58),
  trend     = c(-4.93, -4.42, -4.11),
  both      = c(-5.57, -5.08, -4.82)
)

#' Zivot--Andrews unit root test with an endogenous break
#'
#' The null is a unit root with no break. The alternative is a
#' trend-stationary process whose intercept, trend, or both shift at a
#' single break date \eqn{T_B} that is \emph{not} known in advance but
#' chosen by the data. The regression run at each candidate break is, in
#' levels,
#' \deqn{y_t = \mu + \alpha y_{t-1} + \beta t + \theta DU_t(T_B)
#'   + \gamma DT_t(T_B) + \sum_{j=1}^{k} c_j \Delta y_{t-j} + e_t,}
#' with break dummies \eqn{DU_t(T_B) = 1} for \eqn{t > T_B} and 0
#' otherwise (intercept shift), and \eqn{DT_t(T_B) = t - T_B} for
#' \eqn{t > T_B} and 0 otherwise (trend shift).
#'
#' \code{model = "intercept"} includes \eqn{DU} only (Zivot & Andrews
#' model A), \code{"trend"} includes \eqn{DT} only (model B), and
#' \code{"both"} includes both (model C). The statistic is the minimum
#' over candidate break dates of the t-ratio for \eqn{\alpha = 1},
#' \eqn{t_\alpha(T_B) = (\hat\alpha(T_B) - 1)/se(\hat\alpha(T_B))}, and
#' the reported break point attains that minimum. Because \eqn{T_B} is
#' chosen to minimise the statistic the null distribution is not the
#' Dickey--Fuller one, hence the tabulated critical values.
#'
#' Candidate break dates run over \eqn{1, \ldots, n-1} without trimming,
#' matching \code{urca::ur.za}. Candidates whose design is rank deficient
#' (which happens near the ends of the sample, where a dummy is almost
#' constant) are skipped rather than allowed to produce a spurious
#' minimum.
#'
#' The regression layout, the untrimmed break search and the critical
#' values follow \code{urca::ur.za} (urca 1.3-4, \code{R/ur-za.R}), the
#' reference implementation of this test in R. Both arms were checked
#' against it and agree to twelve decimal places.
#'
#' Mirrors \code{morie.fn.zaurts} on the Python side.
#'
#' @param x Numeric vector, the series to test.
#' @param model One of \code{"intercept"}, \code{"trend"}, \code{"both"}.
#' @param lags Non-negative integer number of lagged differences.
#' @return Named list with \code{statistic}, \code{break_point},
#'   \code{model}, \code{lags}, \code{cval_1pct}, \code{cval_5pct},
#'   \code{cval_10pct}, \code{tstats}, \code{n}, \code{method}.
#' @references Zivot E & Andrews D W K (1992). Further evidence on the
#'   great crash, the oil-price shock, and the unit-root hypothesis.
#'   \emph{Journal of Business & Economic Statistics} 10(3), 251--270.
#' @examples
#' set.seed(11)
#' y <- cumsum(rnorm(60)) + c(rep(0, 30), rep(2.5, 30))
#' Zaurts(y, "intercept", 0)$statistic
#' @export
Zaurts <- function(x, model = "intercept", lags = 0) {
  xv <- as.numeric(x)
  xv <- xv[!is.na(xv)]
  n <- length(xv)
  model <- match.arg(model, c("intercept", "trend", "both"))
  lags <- as.integer(lags)
  if (length(lags) != 1L || is.na(lags) || lags < 0L) {
    stop("lags must be a non-negative integer", call. = FALSE)
  }
  ncol_base <- lags + 3L
  if (n < ncol_base + 2L) {
    stop("insufficient number of observations", call. = FALSE)
  }

  dy <- diff(xv)
  start <- lags + 1L                 # 0-based index of first usable row
  rows <- seq.int(start, n - 1L)     # 0-based
  yvec <- xv[rows + 1L]
  base <- matrix(0, nrow = length(rows), ncol = ncol_base)
  base[, 1L] <- 1                                  # intercept
  base[, 2L] <- xv[rows]                           # y_{t-1}
  base[, 3L] <- rows + 1                           # trend
  if (lags > 0L) {
    for (j in seq_len(lags)) {
      base[, 3L + j] <- dy[rows - j]               # dy_{t-j}
    }
  }

  tstats <- rep(NA_real_, n - 1L)
  for (z in seq_len(n - 1L)) {
    post <- (rows + 1L) > z
    extra <- NULL
    if (model %in% c("intercept", "both")) {
      extra <- cbind(extra, as.numeric(post))
    }
    if (model %in% c("trend", "both")) {
      extra <- cbind(extra, ifelse(post, (rows + 1L) - z, 0))
    }
    xmat <- cbind(base, extra)
    qrx <- qr(xmat)
    if (qrx$rank < ncol(xmat)) next
    fit <- qr.coef(qrx, yvec)
    resid <- yvec - as.vector(xmat %*% fit)
    dfree <- length(yvec) - ncol(xmat)
    if (dfree <= 0L) next
    sigma2 <- sum(resid^2) / dfree
    xtxi <- chol2inv(qr.R(qrx))
    se <- sqrt(sigma2 * xtxi[2L, 2L])
    if (!is.finite(se) || se <= 0) next
    tstats[z] <- (fit[2L] - 1) / se
  }

  if (all(is.na(tstats))) {
    stop("no candidate break date gave an estimable regression",
         call. = FALSE)
  }
  bpoint <- which.min(tstats)
  cv <- .k03_za_cval[[model]]
  list(statistic = unname(tstats[bpoint]),
       break_point = as.integer(bpoint),
       model = model,
       lags = as.integer(lags),
       cval_1pct = cv[1L],
       cval_5pct = cv[2L],
       cval_10pct = cv[3L],
       tstats = unname(tstats),
       n = n,
       method = "Zivot-Andrews unit root test with endogenous break")
}
