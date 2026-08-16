# morie.fn.bats -- base R port of the Python BATS/TBATS arm.
# Sources: De Livera, A. M., Hyndman, R. J., & Snyder, R. D. (2010).
# Forecasting time series with complex seasonal patterns using
# exponential smoothing. Monash University Department of Econometrics
# and Business Statistics Working Paper 15/10 (the JASA 106(496),
# 1513-1527, 2011 version). Hyndman et al. (2007) for the
# forecastability region. Mirrors bats_python_reference.py exactly in
# maths, argument names, return keys, and error conditions. No random
# draws in the Python arm, so the shared RNG helpers are not needed.

#' BATS and TBATS exponential smoothing
#'
#' Equations 3a-3f (BATS) or with the trigonometric seasonal of 4a-4c
#' (TBATS) of De Livera, Hyndman & Snyder (2010). The seed state is
#' concentrated out by least squares rather than searched, the
#' concentrated log-likelihood carries the Box-Cox Jacobian, and the
#' optimiser is constrained to the forecastability region of Hyndman
#' et al. (2007).
#'
#' @param y Numeric series; strictly positive when \code{use_box_cox}
#'   is TRUE.
#' @param seasonal_periods Numeric vector of seasonal periods, each
#'   exceeding 1.
#' @param harmonics NULL for the BATS index seasonal of eq. 3e, or a
#'   vector of harmonic counts for TBATS (eq. 4a-4c). A length-zero
#'   vector fills in the index-equivalent defaults for each period;
#'   NA entries fall back to the default for that period.
#' @param use_box_cox NULL tries both TRUE and FALSE and keeps the
#'   lower AIC; TRUE or FALSE fixes the choice.
#' @param use_trend NULL tries both; TRUE or FALSE fixes the choice.
#' @param damped NULL tries both when \code{use_trend} is TRUE;
#'   TRUE or FALSE fixes the choice.
#' @param p,q ARMA orders, non-negative integers.
#' @param long_run_b Long-run trend value; the short-run trend
#'   converges to this rather than to zero.
#' @param h Forecast horizon.
#' @param maxiter Maximum Nelder-Mead iterations per starting point.
#' @return A named list matching the Python payload keys:
#'   \code{model, omega, phi, alpha, beta, gamma, ar, ma, seed_state,
#'   fitted, fitted_transformed, residuals, loglik, aic, n_par,
#'   sigma2, spectral_radius, forecastable, forecast,
#'   forecast_transformed, candidates, spec, method, note}.
#' @references De Livera, A. M., Hyndman, R. J., & Snyder, R. D.
#'   (2010). Forecasting time series with complex seasonal patterns
#'   using exponential smoothing. Monash University Department of
#'   Econometrics and Business Statistics Working Paper 15/10.
#' @export
morie_bats <- function(y, seasonal_periods = numeric(0),
                       harmonics = NULL,
                       use_box_cox = NULL, use_trend = NULL,
                       damped = NULL, p = 0L, q = 0L,
                       long_run_b = 0, h = 0L, maxiter = 2000L) {
  y <- as.numeric(y)
  prefix <- if (is.null(harmonics)) "bats" else "tbats"
  if (length(y) < 4L)
    stop(paste0(prefix, ": the series is too short"))
  periods <- as.numeric(seasonal_periods)
  # the two-cycles check lives only on the BATS path in the Python arm
  if (is.null(harmonics) && length(periods) > 0L &&
      any(length(y) <= 2 * periods))
    stop("bats: the series is shorter than two full cycles of a seasonal period")

  # Resolve harmonics: NULL -> BATS index seasonal; vector -> TBATS.
  # Mirrors Python tbats: harmonics=None fills in index-equivalent k_i;
  # user-supplied vectors are accepted as-is.
  if (!is.null(harmonics)) {
    hv <- as.numeric(harmonics)
    if (length(hv) == 0L) {
      harm_list <- lapply(periods, function(m)
        length(seasonal_harmonics(m)))
    } else {
      harm_list <- lapply(seq_along(periods), function(i) {
        if (is.na(hv[i]))
          length(seasonal_harmonics(periods[i])) else hv[i]
      })
    }
  } else {
    harm_list <- NULL
  }

  bc <- if (is.null(use_box_cox)) c(TRUE, FALSE) else as.logical(use_box_cox)
  tr <- if (is.null(use_trend)) c(TRUE, FALSE) else as.logical(use_trend)

  best <- NULL
  tried <- list()
  for (b in bc) for (t in tr) {
    dm <- if (is.null(damped) && t) c(TRUE, FALSE)
          else (as.logical(damped) && t)
    for (d in dm) {
      spec <- BatsSpec(periods, harm_list, b, t, d, p, q)
      fit <- .fit_spec(y, spec, long_run_b, maxiter)
      tried[[length(tried) + 1L]] <- list(label = label(spec),
                                          aic = fit$aic)
      if (is.null(best) || fit$aic < best$fit$aic)
        best <- list(spec = spec, fit = fit)
    }
  }

  spec <- best$spec
  fit <- best$fit
  z <- box_cox(y, fit$omega)
  fc <- if (h > 0L) .forecast(spec, fit$theta, fit$x0, z,
                              as.integer(h), long_run_b) else numeric(0)
  u <- .unpack(spec, fit$theta)
  list(model = label(spec), omega = u$omega, phi = u$phi,
       alpha = u$alpha, beta = u$beta, gamma = u$gam, ar = u$ar,
       ma = u$ma, seed_state = fit$x0,
       fitted = inv_box_cox(fit$fitted, u$omega),
       fitted_transformed = fit$fitted,
       residuals = fit$resid, loglik = fit$loglik, aic = fit$aic,
       n_par = fit$n_par,
       sigma2 = sum(fit$resid^2) / length(fit$resid),
       spectral_radius = spectral_radius(spec, fit$theta),
       forecastable = is_forecastable(spec, fit$theta),
       forecast = if (length(fc)) inv_box_cox(fc, u$omega) else numeric(0),
       forecast_transformed = fc, candidates = tried, spec = spec,
       method = paste0("De Livera, Hyndman & Snyder (2010) eq. 3a-3f",
                       if (!is.null(spec$harmonics))
                         " with the trigonometric seasonal of eq. 4a-4c"
                       else ""),
       note = paste0("the seed state is concentrated out by least ",
                     "squares, not searched; the likelihood carries ",
                     "the Box-Cox Jacobian (omega-1) sum log y_t so ",
                     "that fits at different omega are comparable"))
}

#' box_cox
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}, \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; passed to \code{log}.
#' @param omega Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
box_cox <- function(y, omega) {
  y <- as.numeric(y)
  if (any(y <= 0))
    stop("bats: the Box-Cox transform needs a strictly positive series")
  if (omega == 0) return(log(y))
  (y^omega - 1) / omega
}

#' inv_box_cox
#'
#' A step of the bats_native implementation. Called by \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @param omega Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
inv_box_cox <- function(z, omega) {
  z <- as.numeric(z)
  if (omega == 0) return(exp(z))
  base <- omega * z + 1
  if (any(base <= 0))
    stop("bats: the inverse Box-Cox transform is undefined here (omega*z + 1 <= 0)")
  base^(1 / omega)
}

#' seasonal_harmonics
#'
#' A step of the bats_native implementation. Called by \code{bats_filter}, \code{BatsSpec}, \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Numeric; combined arithmetically in the body.
#' @param k Optional; may be \code{NULL}. A count; the body uses it as \code{seq_len(...)}.
#' @return A vector, from \code{vapply}.
#' @export
seasonal_harmonics <- function(m, k = NULL) {
  m <- as.numeric(m)
  if (m <= 1) stop("bats: a seasonal period must exceed 1")
  if (is.null(k)) {
    mi <- as.integer(round(m))
    if (abs(m - mi) < 1e-9)
      k <- if (mi %% 2L == 0L) mi %/% 2L else (mi - 1L) %/% 2L
    else
      k <- as.integer(floor(m / 2))
  }
  k <- as.integer(k)
  if (k < 1L)
    stop("bats: a seasonal component needs at least one harmonic")
  if (k > m / 2 + 1e-9)
    stop(paste0("bats: k = ", k, " exceeds m/2 = ", m / 2,
                "; the harmonics above m/2 are aliases of those below"))
  vapply(seq_len(k), function(j) 2 * pi * j / m, numeric(1))
}

#' BatsSpec
#'
#' A step of the bats_native implementation. Called by \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param periods A vector; its length is taken and its elements indexed. Defaults to \code{numeric(0)}.
#' @param harmonics Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param use_box_cox Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.
#' @param use_trend Coerced to logical by the body, with \code{as.logical}. Defaults to \code{TRUE}.
#' @param damped Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.
#' @param p Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @param q Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @return The value of \code{structure}.
#' @export
BatsSpec <- function(periods = numeric(0), harmonics = NULL,
                     use_box_cox = FALSE, use_trend = TRUE,
                     damped = FALSE, p = 0L, q = 0L) {
  periods <- as.numeric(periods)
  for (m in periods) {
    if (m <= 1) stop("bats: a seasonal period must exceed 1")
  }
  if (is.null(harmonics)) {
    for (m in periods) {
      if (abs(m - round(m)) > 1e-9)
        stop(paste0("bats: the index seasonal of eq. 3e needs integer ",
                    "periods; m = ", m, " is not one. Use the ",
                    "trigonometric seasonal (harmonics=...) for a ",
                    "fractional period."))
    }
    harm <- NULL
  } else {
    if (length(harmonics) != length(periods))
      stop(paste0("bats: ", length(harmonics),
                  " harmonic counts for ", length(periods), " periods"))
    harm <- vapply(seq_along(periods), function(i)
      length(seasonal_harmonics(periods[i], harmonics[[i]])),
      integer(1))
  }
  use_bc <- as.logical(use_box_cox)
  use_tr <- as.logical(use_trend)
  dmp <- as.logical(damped) && use_tr
  pi <- as.integer(p)
  qi <- as.integer(q)
  if (pi < 0L || qi < 0L)
    stop("bats: p and q must be non-negative")
  structure(list(periods = periods, harmonics = harm,
                 use_box_cox = use_bc, use_trend = use_tr,
                 damped = dmp, p = pi, q = qi),
            class = "BatsSpec")
}

#' n_states
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}, \code{fit_seed_state}, \code{state_matrices}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec A list; the body reads \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_trend} from it.
#' @return A numeric value.
#' @export
n_states <- function(spec) {
  n <- 1L + if (spec$use_trend) 1L else 0L
  if (!is.null(spec$harmonics))
    n <- n + 2L * sum(spec$harmonics)
  else
    n <- n + as.integer(sum(round(spec$periods)))
  n + spec$p + spec$q
}

#' n_free
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.
#' @return The value of \code{n}, as built in the body.
#' @export
n_free <- function(spec) {
  n <- 1L
  if (spec$use_trend) {
    n <- n + 1L
    if (spec$damped) n <- n + 1L
  }
  if (!is.null(spec$harmonics)) n <- n + 2L * length(spec$periods)
  else n <- n + length(spec$periods)
  n <- n + spec$p + spec$q
  if (spec$use_box_cox) n <- n + 1L
  n
}

#' label
#'
#' A step of the bats_native implementation. Called by \code{morie_bats}, \code{morie_rmrl}, \code{morie_rmrl_qlearn_flat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox} from it.
#' @return A character value.
#' @export
label <- function(spec) {
  head <- if (!is.null(spec$harmonics)) "TBATS" else "BATS"
  om <- if (spec$use_box_cox) "omega" else "1"
  ph <- if (spec$damped) "phi" else "1"
  if (!is.null(spec$harmonics))
    seas <- paste(sprintf("{%g, %d}", spec$periods, spec$harmonics),
                  collapse = ", ")
  else
    seas <- paste(sprintf("%g", spec$periods), collapse = ", ")
  parts <- paste(c(om, ph, spec$p, spec$q), collapse = ", ")
  paste0(head, "(", parts,
         if (nzchar(seas)) paste0(", ", seas) else "", ")")
}

#' .unpack
#'
#' A step of the bats_native implementation. Called by \code{.forecast}, \code{.sarima_fit}, \code{bats_filter} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.
#' @param theta A vector; indexed elementwise.
#' @return A list with \code{alpha}, \code{beta}, \code{phi}, \code{gam}, \code{ar}, \code{ma}, \code{omega}.
#' @export
.unpack <- function(spec, theta) {
  i <- 1L
  alpha <- theta[i]; i <- i + 1L
  beta <- 0
  phi <- 1
  if (spec$use_trend) {
    beta <- theta[i]; i <- i + 1L
    if (spec$damped) { phi <- theta[i]; i <- i + 1L }
  }
  if (!is.null(spec$harmonics)) {
    g1 <- numeric(length(spec$periods))
    g2 <- numeric(length(spec$periods))
    for (a in seq_along(spec$periods)) {
      g1[a] <- theta[i]
      g2[a] <- theta[i + 1L]
      i <- i + 2L
    }
    gam <- list(g1, g2)
  } else {
    gam <- theta[seq(i, length.out = length(spec$periods))]
    i <- i + length(spec$periods)
  }
  ar <- theta[seq(i, length.out = spec$p)]
  i <- i + spec$p
  ma <- theta[seq(i, length.out = spec$q)]
  i <- i + spec$q
  omega <- if (spec$use_box_cox) theta[i] else 1
  list(alpha = alpha, beta = beta, phi = phi, gam = gam, ar = ar,
       ma = ma, omega = omega)
}

#' bats_filter
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}, \code{.forecast}, \code{fit_seed_state} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z A vector; its length is taken and its elements indexed.
#' @param spec A list; the body reads \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_trend} from it.
#' @param theta Passed to \code{.unpack}.
#' @param x0 A vector; indexed elementwise.
#' @param long_run_b Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A list with \code{resid}, \code{fitted}, \code{carry}.
#' @export
bats_filter <- function(z, spec, theta, x0, long_run_b = 0) {
  u <- .unpack(spec, theta)
  alpha <- u$alpha; beta <- u$beta; phi <- u$phi
  gam <- u$gam; ar <- u$ar; ma <- u$ma
  n <- length(z)
  lev <- as.numeric(x0[1])
  i <- 2L
  trend <- 0
  if (spec$use_trend) {
    trend <- as.numeric(x0[i])
    i <- i + 1L
  }
  trig <- !is.null(spec$harmonics)
  if (trig) {
    lam <- lapply(seq_along(spec$periods), function(a)
      seasonal_harmonics(spec$periods[a], spec$harmonics[a]))
    s <- list(); sstar <- list()
    for (a in seq_along(spec$periods)) {
      ki <- spec$harmonics[a]
      s[[a]] <- as.numeric(x0[seq(i, length.out = ki)])
      i <- i + ki
      sstar[[a]] <- as.numeric(x0[seq(i, length.out = ki)])
      i <- i + ki
    }
  } else {
    lam <- NULL
    buf <- list()
    for (a in seq_along(spec$periods)) {
      mi <- as.integer(round(spec$periods[a]))
      buf[[a]] <- as.numeric(x0[seq(i, length.out = mi)])
      i <- i + mi
    }
  }
  dlag <- as.numeric(x0[seq(i, length.out = spec$p)])
  i <- i + spec$p
  elag <- as.numeric(x0[seq(i, length.out = spec$q)])

  resid <- numeric(n); fitted <- numeric(n)
  for (t in seq_len(n)) {
    if (trig) seas <- sum(unlist(s))
    else seas <- sum(vapply(buf, function(b) b[1], numeric(1)))
    darma <- sum(ar * dlag) + sum(ma * elag)
    pred <- lev + phi * trend + seas + darma
    eps <- z[t] - pred
    d <- darma + eps
    fitted[t] <- pred
    resid[t] <- eps

    new_lev <- lev + phi * trend + alpha * d
    new_trend <- if (spec$use_trend)
      (1 - phi) * long_run_b + phi * trend + beta * d else 0

    if (trig) {
      for (a in seq_along(spec$periods)) {
        g1 <- gam[[1]][a]; g2 <- gam[[2]][a]
        for (j in seq_len(spec$harmonics[a])) {
          c <- cos(lam[[a]][j]); sn <- sin(lam[[a]][j])
          sj <- s[[a]][j]; sjs <- sstar[[a]][j]
          s[[a]][j] <- sj * c + sjs * sn + g1 * d
          sstar[[a]][j] <- -sj * sn + sjs * c + g2 * d
        }
      }
    } else {
      for (a in seq_along(spec$periods)) {
        oldest <- buf[[a]][1]
        buf[[a]] <- c(buf[[a]][-1], oldest + gam[a] * d)
      }
    }
    lev <- new_lev
    trend <- new_trend
    if (spec$p > 0L) dlag <- c(d, head(dlag, -1))
    if (spec$q > 0L) elag <- c(eps, head(elag, -1))
  }
  carry <- list(level = lev, trend = trend, dlag = dlag, elag = elag)
  if (trig) {
    # deep-copy the inner numeric vectors so callers cannot alias state
    carry$s <- lapply(s, function(v) as.numeric(v))
    carry$sstar <- lapply(sstar, function(v) as.numeric(v))
    carry$lam <- lam
  } else {
    carry$buf <- lapply(buf, function(v) as.numeric(v))
  }
  list(resid = resid, fitted = fitted, carry = carry)
}

#' fit_seed_state
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z A vector; its length is taken.
#' @param spec See Usage.
#' @param theta See Usage.
#' @param long_run_b Defaults to \code{0}.
#' @return A vector, from \code{as.numeric}.
#' @export
fit_seed_state <- function(z, spec, theta, long_run_b = 0) {
  n <- length(z)
  ns <- n_states(spec)
  zero <- rep(0, ns)
  base <- bats_filter(z, spec, theta, zero, long_run_b)$resid
  cols <- matrix(0, nrow = n, ncol = ns)
  zeros_z <- rep(0, n)
  for (j in seq_len(ns)) {
    e <- rep(0, ns); e[j] <- 1
    cols[, j] <- bats_filter(zeros_z, spec, theta, e, long_run_b)$resid
  }
  # design @ x0 = base where design = -cols; minimum-norm via truncated SVD,
  # matching numpy.linalg.lstsq with rcond=None.
  design <- -cols
  sv <- svd(design)
  cutoff <- max(dim(design)) * .Machine$double.eps * max(sv$d)
  keep <- sv$d > cutoff
  inv_d <- numeric(length(sv$d))
  inv_d[keep] <- 1 / sv$d[keep]
  sol <- sv$v %*% (inv_d * crossprod(sv$u, base))
  as.numeric(sol)
}

#' .flatten_carry
#'
#' A step of the bats_native implementation. Called by \code{state_matrices}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec A list; the body reads \code{$harmonics}, \code{$periods}, \code{$use_trend} from it.
#' @param carry A list; the body reads \code{$buf}, \code{$dlag}, \code{$elag}, \code{$level}, \code{$s}, \code{$sstar}, \code{$trend} from it.
#' @return A vector, from \code{c}.
#' @export
.flatten_carry <- function(spec, carry) {
  out <- c(carry$level)
  if (spec$use_trend) out <- c(out, carry$trend)
  if (!is.null(spec$harmonics))
    for (a in seq_along(spec$periods))
      out <- c(out, carry$s[[a]], carry$sstar[[a]])
  else
    for (a in seq_along(spec$periods))
      out <- c(out, carry$buf[[a]])
  c(out, carry$dlag, carry$elag)
}

#' state_matrices
#'
#' A step of the bats_native implementation. Called by \code{all_eigenvalues}, \code{spectral_radius}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec Passed to \code{.flatten_carry}.
#' @param theta See Usage.
#' @return A list with \code{w}, \code{fmat}, \code{g}.
#' @export
state_matrices <- function(spec, theta) {
  ns <- n_states(spec)
  zero <- rep(0, ns)
  w <- numeric(ns)
  fcols <- matrix(0, nrow = ns, ncol = ns)
  for (j in seq_len(ns)) {
    e <- rep(0, ns); e[j] <- 1
    # z chosen so that eps = 0, which leaves x_1 = F e_j
    r1 <- bats_filter(0, spec, theta, e)
    wj <- r1$fitted[1]
    w[j] <- wj
    r2 <- bats_filter(wj, spec, theta, e)
    fcols[, j] <- .flatten_carry(spec, r2$carry)
  }
  # x0 = 0 and z = 1 gives eps = 1, so x_1 = g
  r3 <- bats_filter(1, spec, theta, zero)
  g <- .flatten_carry(spec, r3$carry)
  list(w = w, fmat = fcols, g = g)
}

#' spectral_radius
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}, \code{is_forecastable}, \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec See Usage.
#' @param theta See Usage.
#' @param tol Defaults to \code{1e-06}.
#' @return One of two values, depending on the branch taken.
#' @export
spectral_radius <- function(spec, theta, tol = 1e-6) {
  sm <- state_matrices(spec, theta)
  ns <- length(sm$w)
  D <- sm$fmat - sm$g %o% sm$w
  ev <- Mod(eigen(D, only.values = TRUE)$values)
  rest <- ev[abs(ev - 1) >= tol]
  if (length(rest) == 0L) 0 else max(rest)
}

#' all_eigenvalues
#'
#' A step of the bats_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec See Usage.
#' @param theta See Usage.
#' @return A vector, from \code{sort}.
#' @export
all_eigenvalues <- function(spec, theta) {
  sm <- state_matrices(spec, theta)
  ns <- length(sm$w)
  D <- sm$fmat - sm$g %o% sm$w
  ev <- Mod(eigen(D, only.values = TRUE)$values)
  sort(ev, decreasing = TRUE)
}

#' is_forecastable
#'
#' A step of the bats_native implementation. Called by \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec See Usage.
#' @param theta See Usage.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @return A logical value.
#' @export
is_forecastable <- function(spec, theta, tol = 1e-8) {
  spectral_radius(spec, theta) < 1 - tol
}

#' concentrated_loglik
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param resid A vector; its length is taken.
#' @param omega Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
concentrated_loglik <- function(y, resid, omega) {
  n <- length(resid)
  sse <- sum(resid^2)
  if (sse <= 0) return(Inf)
  out <- -0.5 * n * log(sse)
  if (omega != 1) out <- out + (omega - 1) * sum(log(as.numeric(y)))
  out
}

#' .bats_bounds
#'
#' A step of the bats_native implementation. Called by \code{.fit_spec}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param spec A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.
#' @return A list with \code{lo}, \code{hi}.
#' @export
.bats_bounds <- function(spec) {
  lo <- 0; hi <- 1
  if (spec$use_trend) {
    lo <- c(lo, 0); hi <- c(hi, 1)
    if (spec$damped) { lo <- c(lo, 0.8); hi <- c(hi, 1) }
  }
  ns <- if (!is.null(spec$harmonics)) 2L * length(spec$periods)
        else length(spec$periods)
  # The paper does not put a box on gamma; the forecastability region
  # is enforced in the objective below.
  lo <- c(lo, rep(-1, ns)); hi <- c(hi, rep(1, ns))
  lo <- c(lo, rep(-0.99, spec$p + spec$q))
  hi <- c(hi, rep(0.99, spec$p + spec$q))
  if (spec$use_box_cox) { lo <- c(lo, 0); hi <- c(hi, 1.5) }
  list(lo = lo, hi = hi)
}

#' Several starting points for the smoothing parameters. Nelder-Mead
#'
#' builds its initial simplex by nudging each coordinate by 5% of its
#' own value (the same rule as scipy), so a single start of alpha = 0.09
#' only ever explores a box 0.004 wide. Starting from a small grid
#' instead is what makes the search actually search.
#'
#' @param spec A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.starts <- function(spec) {
  # Several starting points for the smoothing parameters. Nelder-Mead
  # builds its initial simplex by nudging each coordinate by 5% of its
  # own value (the same rule as scipy), so a single start of alpha =
  # 0.09 only ever explores a box 0.004 wide. Starting from a small
  # grid instead is what makes the search actually search.
  out <- list()
  ns <- if (!is.null(spec$harmonics)) 2L * length(spec$periods)
        else length(spec$periods)
  for (a in c(0.02, 0.1, 0.3))
    for (gseed in c(0.005, 0.05, 0.2)) {
      x <- a
      if (spec$use_trend) {
        x <- c(x, min(0.5 * a, 0.05))
        if (spec$damped) x <- c(x, 0.98)
      }
      x <- c(x, rep(gseed, ns))
      x <- c(x, rep(0, spec$p + spec$q))
      if (spec$use_box_cox) x <- c(x, 1)
      out[[length(out) + 1L]] <- x
    }
  out
}

#' .fit_spec
#'
#' A step of the bats_native implementation. Called by \code{morie_bats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param spec A list; the body reads \code{$use_box_cox} from it.
#' @param long_run_b Defaults to \code{0}.
#' @param maxiter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.
#' @return A list with \code{theta}, \code{x0}, \code{resid}, \code{fitted}, \code{loglik}, \code{omega}, \code{aic}, \code{n_par}.
#' @export
.fit_spec <- function(y, spec, long_run_b = 0, maxiter = 2000) {
  bds <- .bats_bounds(spec)
  lo <- bds$lo; hi <- bds$hi
  clamp <- function(th) pmin(pmax(as.numeric(th), lo), hi)

  negll <- function(th) {
    th <- clamp(th)
    omega <- if (spec$use_box_cox) th[length(th)] else 1
    val <- tryCatch({
      # "We can constrain the estimation to the forecastibility region
      # (Hyndman et al. 2007) so that the characteristic roots of D lie
      # within the unit circle." Without this the optimiser buys
      # in-sample fit with a seasonal state that tracks the noise.
      rho <- spectral_radius(spec, th)
      if (rho >= 1) return(1e12 * (1 + rho))
      z <- box_cox(y, omega)
      x0 <- fit_seed_state(z, spec, th, long_run_b)
      r <- bats_filter(z, spec, th, x0, long_run_b)
      -concentrated_loglik(y, r$resid, omega)
    }, error = function(e) 1e18)
    if (is.na(val) || is.infinite(val)) 1e18 else val
  }

  best_x <- NULL; best_f <- Inf
  for (st in .starts(spec)) {
    res <- tryCatch(
      optim(st, negll, method = "Nelder-Mead",
            control = list(maxit = as.integer(maxiter))),
      error = function(e) list(par = st))
    xr <- as.numeric(res$par)
    fr <- negll(xr)
    if (fr < best_f) { best_f <- fr; best_x <- xr }
  }
  th <- clamp(best_x)
  omega <- if (spec$use_box_cox) th[length(th)] else 1
  z <- box_cox(y, omega)
  x0 <- fit_seed_state(z, spec, th, long_run_b)
  r <- bats_filter(z, spec, th, x0, long_run_b)
  ll <- concentrated_loglik(y, r$resid, omega)
  k <- n_free(spec) + n_states(spec)
  list(theta = th, x0 = x0, resid = r$resid, fitted = r$fitted,
       loglik = ll, omega = omega, aic = -2 * ll + 2 * k, n_par = k)
}

#' Iterate the state equations with epsilon = 0. Future innovations
#'
#' have mean zero, so the point forecast just runs equations 3b-3f
#' forward from the final state. The MA terms die after q steps; the AR
#' recursion on d continues.
#'
#' @param spec A list; the body reads \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_trend} from it.
#' @param theta Passed to \code{.unpack}.
#' @param x0 See Usage.
#' @param z See Usage.
#' @param h A count; the body uses it as \code{seq_len(...)}.
#' @param long_run_b Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
.forecast <- function(spec, theta, x0, z, h, long_run_b = 0) {
  # Iterate the state equations with epsilon = 0. Future innovations
  # have mean zero, so the point forecast just runs equations 3b-3f
  # forward from the final state. The MA terms die after q steps; the
  # AR recursion on d continues.
  h <- as.integer(h)
  if (h < 0L)
    stop("bats: the forecast horizon must not be negative")
  if (h == 0L) return(numeric(0))

  u <- .unpack(spec, theta)
  alpha <- u$alpha; beta <- u$beta; phi <- u$phi
  gam <- u$gam; ar <- u$ar; ma <- u$ma
  c <- bats_filter(z, spec, theta, x0, long_run_b)$carry
  lev <- c$level; trend <- c$trend
  dlag <- c$dlag; elag <- c$elag
  trig <- !is.null(spec$harmonics)
  if (trig) {
    s <- lapply(c$s, function(v) as.numeric(v))
    sstar <- lapply(c$sstar, function(v) as.numeric(v))
    lam <- c$lam
  } else {
    buf <- lapply(c$buf, function(v) as.numeric(v))
  }

  out <- numeric(h)
  for (k in seq_len(h)) {
    if (trig) seas <- sum(unlist(s))
    else seas <- sum(vapply(buf, function(b) b[1], numeric(1)))
    d <- sum(ar * dlag) + sum(ma * elag)
    out[k] <- lev + phi * trend + seas + d

    new_lev <- lev + phi * trend + alpha * d
    new_trend <- if (spec$use_trend)
      (1 - phi) * long_run_b + phi * trend + beta * d else 0

    if (trig) {
      for (a in seq_along(spec$periods)) {
        g1 <- gam[[1]][a]; g2 <- gam[[2]][a]
        for (j in seq_len(spec$harmonics[a])) {
          cc <- cos(lam[[a]][j]); sn <- sin(lam[[a]][j])
          sj <- s[[a]][j]; sjs <- sstar[[a]][j]
          s[[a]][j] <- sj * cc + sjs * sn + g1 * d
          sstar[[a]][j] <- -sj * sn + sjs * cc + g2 * d
        }
      }
    } else {
      for (a in seq_along(spec$periods)) {
        oldest <- buf[[a]][1]
        buf[[a]] <- c(buf[[a]][-1], oldest + gam[a] * d)
      }
    }
    lev <- new_lev
    trend <- new_trend
    if (spec$p > 0L) dlag <- c(d, head(dlag, -1))
    if (spec$q > 0L) elag <- c(0, head(elag, -1))
  }
  out
}

#' .bats_cheatsheet
#'
#' A step of the bats_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.bats_cheatsheet <- function() {
  paste0("bats: De Livera, Hyndman & Snyder (2010). BATS = Box-Cox ",
         "transform, ARMA errors, Trend, Seasonal -- an innovations ",
         "state space model with one seasonal index per period ",
         "(eq. 3a-3f), a damped trend that converges to a long-run ",
         "trend b rather than to zero, and ARMA(p, q) errors. TBATS ",
         "swaps the seasonal index for a trigonometric one ",
         "(eq. 4a-4c), which needs 2*sum(k_i) seeds instead of ",
         "sum(m_i), handles NON-INTEGER periods such as 365.25/7, ",
         "and gives deterministic seasonality when the smoothing ",
         "parameters are zero. k_i = m_i/2 (even) or (m_i-1)/2 (odd) ",
         "reproduces the index form exactly. The seed state is ",
         "concentrated out by least squares.")
}
