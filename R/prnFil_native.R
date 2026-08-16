# morie.fn -- function file (rootcoder007/morie)
# Prophet's automatic changepoint selection.
#
# Where does a trend change? Specifying the dates by hand needs the
# answer in advance. Prophet instead puts a large number of candidate
# changepoints down -- one per month over several years is typical -- and
# lets a sparse prior decide which are real:
#
# .. math:: \delta_j \sim \mathrm{Laplace}(0, \tau).
#
# **Sparsity is the whole mechanism, and :math:`\tau` is the only dial.**
# The Laplace prior is the Bayesian form of an L1 penalty, so most
# :math:`\delta_j` are driven to zero and a handful survive. Small
# :math:`\tau` means heavy shrinkage and a nearly straight trend; large
# :math:`\tau` lets the trend bend wherever the data suggests. The paper
# is explicit about the consequence: as :math:`\tau` grows the training
# error falls and the *forecast* uncertainty widens, because the
# flexibility that fits the history is projected forward. That trade-off
# is measured here rather than described -- the anchor sweeps
# :math:`\tau` and checks training error falls monotonically while the
# number of active changepoints rises.
#
# **Changepoints are placed only in the first 80% of the history.** A
# changepoint near the end has almost no data after it to estimate its
# rate, so it fits the last few points and then dominates every forecast.
# The default ``changepoint_range=0.8`` is that guard, and it is a
# default worth understanding rather than tuning away.
#
# **Forecast uncertainty comes from projecting the same rate of change
# forward.** Future changepoints are simulated at the historical
# frequency :math:`S/T` with magnitudes drawn from the inferred
# :math:`\mathrm{Laplace}(0,\lambda)`, so the interval widens with
# horizon. The paper says plainly that this will not have exact coverage;
# it is an indication, and above all an overfitting detector.
#
# References
# ----------
# Taylor, S. J. & Letham, B. (2018) "Forecasting at Scale", *The American
# Statistician* 72(1), 37-45, doi:10.1080/00031305.2017.1380080;
# preprint *PeerJ Preprints* 5:e3190v2,
# doi:10.7287/peerj.preprints.3190v2. Sec. 3.1.3 (automatic changepoint
# selection) and Sec. 3.1.4 (trend forecast uncertainty).
#
# Tibshirani, R. (1996) "Regression Shrinkage and Selection via the
# Lasso", *Journal of the Royal Statistical Society Series B* 58(1),
# 267-288, doi:10.1111/j.2517-6161.1996.tb02080.x. The L1 penalty the
# Laplace prior corresponds to.
#
# Park, T. & Casella, G. (2008) "The Bayesian Lasso", *Journal of the
# American Statistical Association* 103(482), 681-686,
# doi:10.1198/016214508000000337. The Laplace-prior formulation.

.prnFil_eps <- 1e-12

#' .prnFil_changepoint_path
#'
#' A step of the prnFil_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Passed to \code{morie_prphet_fit}.
#' @param y Passed to \code{morie_prphet_fit}.
#' @param taus Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_changepoints Passed to \code{morie_prphet_fit}. Defaults to \code{15}.
#' @param changepoint_range Passed to \code{morie_prphet_fit}. Defaults to \code{0.8}.
#' @param seasonalities Passed to \code{morie_prphet_fit}.
#' @param ... Passed through.
#' @return The value of \code{rows}, as built in the body.
#' @export
.prnFil_changepoint_path <- function(t, y, taus = NULL, n_changepoints = 15,
                                      changepoint_range = 0.8,
                                      seasonalities = NULL, ...) {
  grid <- if (is.null(taus)) c(0.001, 0.01, 0.05, 0.1, 0.5, 1.0)
          else as.numeric(taus)
  if (length(grid) < 2L) {
    stop(sprintf("prnFil: need at least 2 tau values, got %d", length(grid)))
  }
  rows <- list()
  for (tau in grid) {
    f <- morie_prphet_fit(t, y, n_changepoints = n_changepoints,
                     changepoint_range = changepoint_range,
                     changepoint_prior = tau,
                     seasonalities = seasonalities, ...)
    d <- f[["deltas"]]
    # exactly zero, not "small": the L1 solution really does zero them,
    # which is the whole point of the Laplace prior
    active <- sum(d != 0.0)
    rows[[length(rows) + 1L]] <- list(
      tau = tau, active = active,
      rmse = sqrt(mean(f[["residual"]]^2)),
      l1 = sum(abs(d)),
      deltas = d
    )
  }
  rows
}

#' .prnFil_select_changepoints
#'
#' A step of the prnFil_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Passed to \code{morie_prphet_fit}.
#' @param y Passed to \code{morie_prphet_fit}.
#' @param tau Passed to \code{morie_prphet_fit}. Defaults to \code{0.05}.
#' @param n_changepoints Passed to \code{morie_prphet_fit}. Defaults to \code{15}.
#' @param changepoint_range Passed to \code{morie_prphet_fit}. Defaults to \code{0.8}.
#' @param seasonalities Passed to \code{morie_prphet_fit}.
#' @param ... Passed through.
#' @return A list with \code{estimate}, \code{selected}, \code{selected_index}, \code{deltas}, \code{candidates}, \code{n_selected}, \code{n_candidates}, \code{tau}, \code{fit}, \code{last_candidate_fraction}, \code{changepoint_range}, \code{rmse}, \code{method}.
#' @export
.prnFil_select_changepoints <- function(t, y, tau = 0.05,
                                        n_changepoints = 15,
                                        changepoint_range = 0.8,
                                        seasonalities = NULL, ...) {
  f <- morie_prphet_fit(t, y, n_changepoints = n_changepoints,
                   changepoint_range = changepoint_range,
                   changepoint_prior = tau, seasonalities = seasonalities,
                   ...)
  d <- f[["deltas"]]
  cps <- f[["changepoints"]]
  keep <- which(d != 0.0)
  tv <- f[["t"]]
  span <- tv[length(tv)] - tv[1L]
  last_candidate_fraction <- if (length(cps) > 0L && span > 0.0)
    (cps[length(cps)] - tv[1L]) / span
  else 0.0
  list(
    estimate = cps[keep],
    selected = cps[keep],
    selected_index = keep, "deltas" = d, "candidates" = cps,
    n_selected = length(keep), n_candidates = length(cps),
    tau = as.numeric(tau), fit = f,
    last_candidate_fraction = last_candidate_fraction,
    changepoint_range = as.numeric(changepoint_range),
    rmse = sqrt(mean(f[["residual"]]^2)),
    method = "automatic changepoint selection under a Laplace prior, Taylor & Letham (2018) Sec. 3.1.3"
  )
}

#' .prnFil_simulate_future_trend
#'
#' A step of the prnFil_native implementation. Called by \code{.prnFil_trend_intervals}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$changepoints}, \code{$deltas}, \code{$k}, \code{$m}, \code{$t} from it.
#' @param t_future Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_sims Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return The value of \code{sims}, as built in the body.
#' @export
.prnFil_simulate_future_trend <- function(fit, t_future, n_sims = 200,
                                           seed = 0) {
  tv <- fit[["t"]]
  cps <- fit[["changepoints"]]
  d <- fit[["deltas"]]
  Tspan <- tv[length(tv)] - tv[1L]
  S <- length(cps)
  if (Tspan <= 0.0) {
    stop("prnFil: the history has no span")
  }
  lam <- if (S > 0L) sum(abs(d)) / S else 0.0
  rate <- S / Tspan
  state <- .ghc_rng(seed)
  tf <- as.numeric(t_future)
  dt <- if (length(tf) > 1L) tf[2L] - tf[1L] else 1.0
  sims <- list()
  for (sim in seq_len(as.integer(n_sims))) {
    nd <- d
    ncps <- cps
    for (j in seq_along(tf)) {
      tv2 <- tf[j]
      # one Bernoulli per future time step, at the historical
      # changepoint frequency
      u1 <- .ghc_unif(state, 1L)
      if (u1[1L] < rate * dt) {
        u2 <- .ghc_unif(state, 1L)
        u <- u2[1L] - 0.5
        mag <- if (lam > 0.0) {
          sgn <- if (u < 0.0) -1.0 else 1.0
          -lam * sgn * log(1.0 - 2.0 * abs(u))
        } else 0.0
        ncps <- c(ncps, tv2)
        nd <- c(nd, mag)
      }
    }
    sims[[sim]] <- morie_prphet_piecewise_trend(tf, fit[["k"]], fit[["m"]], nd, ncps)
  }
  sims
}

#' .prnFil_trend_intervals
#'
#' A step of the prnFil_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit Passed to \code{.prnFil_simulate_future_trend}.
#' @param t_future A vector; its length is taken.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.8}.
#' @param n_sims Passed to \code{.prnFil_simulate_future_trend}. Defaults to \code{200}.
#' @param seed Passed to \code{.prnFil_simulate_future_trend}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{median}, \code{lower}, \code{upper}, \code{width}, \code{level}, \code{n_sims}, \code{note}, \code{method}.
#' @export
.prnFil_trend_intervals <- function(fit, t_future, level = 0.8,
                                     n_sims = 200, seed = 0) {
  sims <- .prnFil_simulate_future_trend(fit, t_future, n_sims = n_sims,
                                         seed = seed)
  H <- length(t_future)
  lo_q <- 0.5 - as.numeric(level) / 2.0
  hi_q <- 0.5 + as.numeric(level) / 2.0
  lo <- hi <- med <- numeric(H)
  for (h in seq_len(H)) {
    col <- sort(vapply(sims, function(s) s[h], numeric(1)))
    lo[h] <- .s03quantile7(col, lo_q)
    hi[h] <- .s03quantile7(col, hi_q)
    med[h] <- .s03quantile7(col, 0.5)
  }
  list(
    estimate = med, median = med, lower = lo, upper = hi,
    width = hi - lo,
    level = as.numeric(level), n_sims = as.integer(n_sims),
    note = "the paper does not claim exact coverage for these; they indicate uncertainty and detect overfitting",
    method = "trend forecast uncertainty by simulating future changepoints, Taylor & Letham (2018) Sec. 3.1.4"
  )
}

#' .prnFil_cheatsheet
#'
#' A step of the prnFil_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.prnFil_cheatsheet <- function() {
  paste0("prnFil: lay down many candidate changepoints, let ",
         "delta_j ~ Laplace(0, tau) decide. Small tau = straight ",
         "trend, large tau = bends everywhere; training error falls ",
         "and forecast intervals WIDEN as tau grows, which is the ",
         "overfitting signal. Candidates only in the first 80 per ",
         "cent: a changepoint near the end has no data after it and ",
         "dominates every forecast.")
}

# compact alias per ledger/NAMING.md
.prnFil_selectchangepoints <- .prnFil_select_changepoints

# public names resolved by fn/_lazy_map.json
.prnFil_prophet_changepoint <- .prnFil_select_changepoints

# morie_prnFil entry point
morie_prnFil <- list(
  changepoint_path = .prnFil_changepoint_path,
  select_changepoints = .prnFil_select_changepoints,
  selectchangepoints = .prnFil_selectchangepoints,
  prophet_changepoint = .prnFil_prophet_changepoint,
  simulate_future_trend = .prnFil_simulate_future_trend,
  trend_intervals = .prnFil_trend_intervals,
  cheatsheet = .prnFil_cheatsheet
)
