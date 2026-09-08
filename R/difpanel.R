# SPDX-License-Identifier: AGPL-3.0-or-later

#' SIBTEST for differential item functioning
#'
#' Stratifies respondents on a matching score and compares the studied
#' item's mean between reference and focal groups within each stratum,
#' weighting strata by how many respondents they hold:
#' \eqn{\hat\beta = \sum_k \pi_k (\bar Y_{Rk} - \bar Y_{Fk})}, with
#' \eqn{B = \hat\beta / \hat\sigma(\hat\beta)} referred to the normal.
#'
#' The matching score defaults to the rest score, the total of the other
#' items. Including the studied item would let it help decide its own
#' stratum, biasing the comparison towards finding no DIF.
#'
#' The regression correction is on by default and is not optional in
#' practice. Matching on an observed score leaves a residual ability
#' difference inside each stratum whenever the groups differ in ability,
#' and the uncorrected statistic reads that as bias. Measured on data
#' with an ability gap and no DIF at all, the uncorrected form rejects
#' 0.81 of items on a 6-item test and still 0.18 on 40 items against a
#' nominal 0.05; correcting brings that to 0.28, 0.10 and 0.075 at 6, 12
#' and 20 items with power intact. Short tests remain the hard case.
#'
#' Mirrors \code{morie.fn.difsbs} on the Python side.
#'
#' @param x Numeric matrix of item responses (n x p). Every column is
#'   tested against the rest score of the others.
#' @param group Binary group indicator; the level sorting first is the
#'   reference group.
#' @param matching Optional matching score. Defaults to the per-item
#'   rest score.
#' @param min_per_cell Strata with fewer than this many respondents in
#'   either group are dropped. Default 2.
#' @param correct Apply the true-score regression correction.
#' @return Named list with \code{beta}, \code{statistic}, \code{p_value},
#'   \code{se}, \code{n_strata}, \code{n_reference}, \code{n_focal},
#'   \code{correct}, \code{method}.
#' @references Shealy R & Stout W (1993). A model-based standardization
#'   approach that separates true bias/DIF from group ability
#'   differences and detects test bias/DTF as well as item bias/DIF.
#'   \emph{Psychometrika}, 58(2), 159-194.
#' @examples
#' set.seed(1)
#' x <- matrix(rbinom(1200, 1, 0.5), 200, 6)
#' morie_sibtest(x, rbinom(200, 1, 0.5))$statistic
#' @export
morie_sibtest <- function(x, group, matching = NULL, min_per_cell = 2L,
                          correct = TRUE) {
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  n <- nrow(X)
  p <- ncol(X)
  if (length(group) != n) {
    stop("group must have one entry per row of x; got ", length(group),
      " and ", n, ".",
      call. = FALSE
    )
  }
  lev <- sort(unique(group))
  if (length(lev) != 2L) {
    stop("group must be binary; got ", length(lev), " distinct values.", call. = FALSE)
  }
  if (!all(is.finite(X))) stop("x must be finite.", call. = FALSE)
  if (p < 2L && is.null(matching)) {
    stop("With one item there is no rest score to match on; supply `matching`.",
      call. = FALSE
    )
  }
  is_ref <- group == lev[1L]
  if (!is.null(matching) && length(matching) != n) {
    stop("matching must have one entry per row of x; got ", length(matching),
      " and ", n, ".",
      call. = FALSE
    )
  }

  beta <- se <- numeric(p)
  nstrata <- integer(p)

  for (j in seq_len(p)) {
    rest <- X[, -j, drop = FALSE]
    score <- if (is.null(matching)) rowSums(rest) else as.numeric(matching)
    y <- X[, j]

    adj <- list()
    if (correct) {
      for (tag in c("ref", "foc")) {
        m <- if (tag == "ref") is_ref else !is_ref
        s <- score[m]
        vbar <- mean(s)
        var_s <- stats::var(s)
        rho <- 1
        if (is.null(matching) && ncol(rest) > 1L && is.finite(var_s) && var_s > 0) {
          q <- rest[m, , drop = FALSE]
          ki <- ncol(q)
          rho <- (ki / (ki - 1)) * (1 - sum(apply(q, 2L, stats::var)) / var_s)
        }
        rho <- min(max(rho, 0), 1)
        sl <- if (is.finite(var_s) && var_s > 0) stats::cov(s, y[m]) / var_s else 0
        adj[[tag]] <- c(vbar = vbar, rho = rho, slope = sl)
      }
    }

    b <- 0
    v <- 0
    used <- 0L
    for (k in sort(unique(score))) {
      sel <- score == k
      yr <- y[sel & is_ref]
      yf <- y[sel & !is_ref]
      if (length(yr) < min_per_cell || length(yf) < min_per_cell) next
      pi_k <- (length(yr) + length(yf)) / n
      mr <- mean(yr)
      mf <- mean(yf)
      if (correct) {
        a <- adj[["ref"]]
        f <- adj[["foc"]]
        tr <- a[["vbar"]] + a[["rho"]] * (k - a[["vbar"]])
        tf <- f[["vbar"]] + f[["rho"]] * (k - f[["vbar"]])
        target <- (tr + tf) / 2
        mr <- mr + a[["slope"]] * (target - tr)
        mf <- mf + f[["slope"]] * (target - tf)
      }
      b <- b + pi_k * (mr - mf)
      v <- v + pi_k^2 * (stats::var(yr) / length(yr) + stats::var(yf) / length(yf))
      used <- used + 1L
    }
    beta[j] <- b
    se[j] <- sqrt(v)
    nstrata[j] <- used
  }

  B <- ifelse(se > 0, beta / se, NA_real_)
  list(
    beta = beta,
    statistic = B,
    p_value = 2 * stats::pnorm(-abs(B)),
    se = se,
    n_strata = nstrata,
    n_reference = sum(is_ref),
    n_focal = sum(!is_ref),
    correct = correct,
    method = paste0(
      "SIBTEST (Shealy & Stout 1993)",
      if (correct) ", true-score regression correction" else ", uncorrected"
    )
  )
}

# Pedroni (1999) Table 2, "Adjustment Terms for Panel Cointegration
# Tests", transcribed from the author's working paper at
# https://web.williams.edu/Economics/wp/pedronicriticalvalues.pdf
# Rows are m = 2..7 regressors, excluding constants and trends; there is
# no m = 1 row, so a bivariate regression cannot be standardised from it.
# Columns per statistic are (mean, variance).
.PEDRONI_T2 <- list(
  standard = list(
    "2" = list(panel_v = c(6.982, 81.145), panel_rho = c(-6.388, 64.288), panel_t = c(-1.662, 1.559), group_rho = c(-9.889, 41.943), group_t = c(-1.992, 0.649)),
    "3" = list(panel_v = c(10.402, 140.804), panel_rho = c(-10.191, 89.962), panel_t = c(-2.156, 1.286), group_rho = c(-13.865, 57.801), group_t = c(-2.440, 0.600)),
    "4" = list(panel_v = c(14.254, 182.450), panel_rho = c(-14.136, 103.176), panel_t = c(-2.571, 1.028), group_rho = c(-17.834, 72.097), group_t = c(-2.819, 0.567)),
    "5" = list(panel_v = c(18.198, 217.784), panel_rho = c(-18.042, 120.787), panel_t = c(-2.926, 0.928), group_rho = c(-21.805, 88.611), group_t = c(-3.151, 0.559)),
    "6" = list(panel_v = c(22.169, 256.530), panel_rho = c(-21.985, 132.499), panel_t = c(-3.244, 0.820), group_rho = c(-25.750, 103.371), group_t = c(-3.450, 0.544)),
    "7" = list(panel_v = c(26.120, 277.429), panel_rho = c(-25.889, 143.561), panel_t = c(-3.533, 0.750), group_rho = c(-29.627, 117.059), group_t = c(-3.723, 0.530))
  ),
  intercept = list(
    "2" = list(panel_v = c(11.754, 104.546), panel_rho = c(-9.495, 57.610), panel_t = c(-2.177, 0.964), group_rho = c(-12.938, 51.490), group_t = c(-2.453, 0.618)),
    "3" = list(panel_v = c(15.197, 151.094), panel_rho = c(-13.256, 81.772), panel_t = c(-2.576, 0.923), group_rho = c(-16.888, 67.123), group_t = c(-2.827, 0.585)),
    "4" = list(panel_v = c(18.910, 190.661), panel_rho = c(-17.163, 99.331), panel_t = c(-2.930, 0.843), group_rho = c(-20.841, 81.835), group_t = c(-3.157, 0.560)),
    "5" = list(panel_v = c(22.715, 231.864), panel_rho = c(-21.013, 119.546), panel_t = c(-3.241, 0.800), group_rho = c(-24.775, 98.278), group_t = c(-3.452, 0.553)),
    "6" = list(panel_v = c(26.603, 270.451), panel_rho = c(-24.944, 134.341), panel_t = c(-3.531, 0.750), group_rho = c(-28.720, 113.131), group_t = c(-3.726, 0.542)),
    "7" = list(panel_v = c(30.457, 293.431), panel_rho = c(-28.795, 144.615), panel_t = c(-3.795, 0.685), group_rho = c(-32.538, 126.059), group_t = c(-3.976, 0.525))
  ),
  trend = list(
    "2" = list(panel_v = c(21.162, 160.249), panel_rho = c(-14.011, 64.219), panel_t = c(-2.648, 0.690), group_rho = c(-17.359, 66.387), group_t = c(-2.872, 0.555)),
    "3" = list(panel_v = c(24.556, 198.167), panel_rho = c(-17.600, 83.815), panel_t = c(-2.967, 0.686), group_rho = c(-21.116, 81.832), group_t = c(-3.179, 0.548)),
    "4" = list(panel_v = c(28.046, 239.425), panel_rho = c(-21.287, 103.905), panel_t = c(-3.262, 0.688), group_rho = c(-24.930, 97.362), group_t = c(-3.464, 0.543)),
    "5" = list(panel_v = c(31.738, 276.997), panel_rho = c(-25.130, 124.613), panel_t = c(-3.545, 0.686), group_rho = c(-28.849, 113.145), group_t = c(-3.737, 0.538)),
    "6" = list(panel_v = c(35.537, 310.982), panel_rho = c(-28.981, 138.227), panel_t = c(-3.806, 0.654), group_rho = c(-32.716, 127.989), group_t = c(-3.986, 0.530)),
    "7" = list(panel_v = c(39.231, 348.217), panel_rho = c(-32.756, 154.378), panel_t = c(-4.047, 0.638), group_rho = c(-36.494, 140.756), group_t = c(-4.217, 0.518))
  )
)

# Internal: Newey-West (1987) long-run variance, default bandwidth
# 4 (T/100)^(2/9).
#' Internal: Newey-West (1987) long-run variance, default bandwidth
#'
#' 4 (T/100)^(2/9).
#'
#' @param u A vector; its length is taken.
#' @param bandwidth Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .nw_lrv(u = x)
#' res
.nw_lrv <- function(u, bandwidth = NULL) {
  Tn <- length(u)
  if (Tn < 2L) {
    return(if (Tn) stats::var(u) else 0)
  }
  K <- if (is.null(bandwidth)) as.integer(4 * (Tn / 100)^(2 / 9)) else as.integer(bandwidth)
  K <- max(0L, min(K, Tn - 1L))
  uc <- u - mean(u)
  s <- sum(uc * uc) / Tn
  for (lag in seq_len(K)) {
    gl <- sum(uc[(lag + 1L):Tn] * uc[1L:(Tn - lag)]) / Tn
    s <- s + 2 * (1 - lag / (K + 1)) * gl
  }
  max(s, 1e-12)
}

# Internal: Pedroni's steps 2-4 nuisance terms for one panel unit.
# Returns L11^2, lambda_i and sigma^2_i.
#' Internal: Pedroni\'s steps 2-4 nuisance terms for one panel unit
#'
#' Returns L11^2, lambda_i and sigma^2_i.
#'
#' @param y Passed to \code{diff}.
#' @param Zx A matrix; passed to \code{ncol}.
#' @param e A vector; its length is taken and its elements indexed.
#' @param bandwidth Passed to \code{.nw_lrv}.
#' @return A vector, from \code{c}.
#' @export
.pdcoin_nuisance <- function(y, Zx, e, bandwidth) {
  dy <- diff(y)
  dX <- apply(Zx, 2L, diff)
  if (!is.matrix(dX)) dX <- matrix(dX, ncol = ncol(Zx))
  eta <- if (ncol(dX) > 0L) stats::lm.fit(dX, dy)$residuals else dy
  L11_sq <- .nw_lrv(eta, bandwidth)

  lag_e <- e[-length(e)]
  de <- diff(e)
  denom <- sum(lag_e^2)
  gamma <- if (denom > 0) sum(lag_e * de) / denom else 0
  u <- de - gamma * lag_e
  sigma2 <- .nw_lrv(u, bandwidth)
  s2 <- mean((u - mean(u))^2)
  c(L11_sq = L11_sq, lambda = (sigma2 - s2) / 2, sigma2 = sigma2)
}

#' Pedroni panel cointegration statistics
#'
#' Computes all five of Pedroni's tabulated panel cointegration
#' statistics and standardises each one, following his own five-step
#' recipe: fit the cointegrating regression and keep the residuals;
#' regress the differenced dependent on the differenced regressors and
#' take the long-run variance of \emph{those} residuals as
#' \eqn{\hat L_{11i}^2}; fit
#' \eqn{\hat e_{i,t} = \hat\gamma_i \hat e_{i,t-1} + \hat u_{i,t}} and form
#' \eqn{\hat\lambda_i = (\hat\sigma_i^2 - \hat s_i^2)/2}. Long-run
#' variances use the Newey-West estimator.
#'
#' Each statistic is read against a normal only after his equation (2),
#' \eqn{Z = (\chi_{N,T} - \mu\sqrt{N})/\sqrt{v}}, with \eqn{\mu} and
#' \eqn{v} from Table 2, transcribed here for all three deterministic
#' cases and m = 2..7 regressors.
#'
#' The panel variance statistic diverges to \eqn{+\infty} under the
#' alternative, so it alone is read from the right tail; the other four
#' go negative under cointegration and are read from the left.
#'
#' Table 2 has no m = 1 row, so a bivariate cointegrating regression
#' cannot be standardised from it and that case is refused rather than
#' extrapolated.
#'
#' Mirrors \code{morie.fn.pdcoin} on the Python side.
#'
#' @param x Numeric matrix; first column the dependent variable, the
#'   rest regressors.
#' @param groups Unit label per row. At least two units.
#' @param lags Augmentation lags in the residual ADF regressions.
#' @param case One of "intercept" (default), "standard", "trend".
#' @param bandwidth Newey-West truncation lag; default
#'   \code{4 (T/100)^(2/9)}.
#' @return Named list with \code{statistics} and \code{z} and
#'   \code{p_values} (each a named list over panel_v, panel_rho,
#'   panel_t, group_rho, group_t), plus \code{n_units},
#'   \code{n_regressors}, \code{case}, \code{p_value}, \code{method}
#'   and \code{warnings}.
#' @references Pedroni P (1999). Critical values for cointegration tests
#'   in heterogeneous panels with multiple regressors. \emph{Oxford
#'   Bulletin of Economics and Statistics}, 61(S1), 653-670.
#' @examples
#' set.seed(1)
#' Tn <- 80
#' N <- 6
#' x1 <- unlist(lapply(seq_len(N), function(i) cumsum(rnorm(Tn))))
#' x2 <- unlist(lapply(seq_len(N), function(i) cumsum(rnorm(Tn))))
#' y <- x1 + 0.5 * x2 + rnorm(N * Tn)
#' morie_panel_cointegration(cbind(y, x1, x2), rep(seq_len(N), each = Tn))$p_value
#' @export
morie_panel_cointegration <- function(x, groups, lags = 1L,
                                      case = "intercept", bandwidth = NULL) {
  X <- as.matrix(x)
  if (ncol(X) < 2L) {
    stop("x needs a dependent column and at least one regressor; got ",
      ncol(X), " column(s).",
      call. = FALSE
    )
  }
  if (length(groups) != nrow(X)) {
    stop("groups must have one entry per row of x; got ", length(groups),
      " and ", nrow(X), ".",
      call. = FALSE
    )
  }
  if (!all(is.finite(X))) stop("x must be finite.", call. = FALSE)
  units <- unique(groups)
  if (length(units) < 2L) {
    stop("Need at least 2 panel units, got ", length(units), ".", call. = FALSE)
  }
  lags <- as.integer(lags)
  if (lags < 0L) stop("lags must not be negative, got ", lags, ".", call. = FALSE)
  if (!case %in% names(.PEDRONI_T2)) {
    stop("case must be one of ", paste(names(.PEDRONI_T2), collapse = ", "),
      "; got ", case, ".",
      call. = FALSE
    )
  }

  A_num <- A_den <- 0
  g_rho_sum <- g_t_sum <- 0
  sig_over_L <- T_used <- numeric(0)
  skipped <- c()
  for (u in units) {
    sel <- groups == u
    yv <- X[sel, 1L]
    Zx <- X[sel, -1L, drop = FALSE]
    e <- stats::lm.fit(cbind(1, Zx), yv)$residuals
    if (length(e) < 4L * (lags + 1L)) {
      skipped <- c(skipped, u)
      next
    }
    nz <- .pdcoin_nuisance(yv, Zx, e, bandwidth)
    lag_e <- e[-length(e)]
    de <- diff(e)
    ss <- sum(lag_e^2)
    if (ss <= 0 || nz[["L11_sq"]] <= 0) {
      skipped <- c(skipped, u)
      next
    }
    cross <- sum(lag_e * de) - length(lag_e) * nz[["lambda"]]

    A_den <- A_den + ss / nz[["L11_sq"]]
    A_num <- A_num + cross / nz[["L11_sq"]]
    sig_over_L <- c(sig_over_L, nz[["sigma2"]] / nz[["L11_sq"]])
    g_rho_sum <- g_rho_sum + cross / ss
    g_t_sum <- g_t_sum + cross / sqrt(nz[["sigma2"]] * ss)
    T_used <- c(T_used, length(e))
  }

  if (A_den <= 0 || length(T_used) == 0L) {
    stop("No panel unit had enough usable observations; check group sizes and lags.",
      call. = FALSE
    )
  }

  N <- length(T_used)
  T_bar <- mean(T_used)
  sigma_tilde2 <- mean(sig_over_L)

  stats_raw <- list(
    panel_v = T_bar^2 * N^1.5 / A_den,
    panel_rho = T_bar * sqrt(N) * A_num / A_den,
    panel_t = A_num / sqrt(sigma_tilde2 * A_den),
    group_rho = T_bar * N^-0.5 * g_rho_sum,
    group_t = N^-0.5 * g_t_sum
  )

  warn <- character(0)
  if (length(skipped)) {
    warn <- c(warn, paste0(
      length(skipped), " unit(s) skipped for too few observations: ",
      paste(skipped, collapse = ", ")
    ))
  }

  m <- ncol(X) - 1L
  key <- as.character(m)
  z <- list()
  pv <- list()
  if (!is.null(.PEDRONI_T2[[case]][[key]])) {
    for (nm in names(stats_raw)) {
      mv <- .PEDRONI_T2[[case]][[key]][[nm]]
      zz <- (stats_raw[[nm]] - mv[1L] * sqrt(N)) / sqrt(mv[2L])
      z[[nm]] <- zz
      pv[[nm]] <- if (nm == "panel_v") stats::pnorm(zz, lower.tail = FALSE) else stats::pnorm(zz)
    }
  } else {
    warn <- c(warn, paste0(
      "Pedroni Table 2 covers m = 2..7 regressors; this panel has m = ",
      m, ", so no standardised p-values are available."
    ))
  }

  list(
    statistics = stats_raw,
    z = z,
    p_values = pv,
    n_units = N,
    n_regressors = m,
    case = case,
    p_value = if (length(pv)) pv[["group_t"]] else NULL,
    method = "Pedroni panel cointegration; all five statistics standardised by Table 2",
    warnings = warn
  )
}
