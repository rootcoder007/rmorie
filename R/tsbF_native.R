# morie.fn -- function file (rootcoder007/morie)
# TSB: intermittent demand without the obsolescence blind spot.
#
# Intermittent demand is two processes at once -- whether a demand occurs
# and how big it is -- so Croston's method splits them and smooths each
# separately. The trouble is *when* it smooths.
#
# **Croston updates only when demand occurs, and that is the flaw.**
# It tracks the demand size :math:`z` and the inter-demand *interval*
# :math:`x`, both updated at demand epochs, and forecasts
# :math:`\hat Y = z'/x'`. An item that stops selling entirely therefore
# keeps its last forecast **forever**: nothing updates, because nothing
# happens. For obsolescence -- the case that matters most in inventory --
# the method is silent by construction.
#
# **TSB replaces the interval with the probability, and updates every
# period.** Writing :math:`p_t = 1\{Y_t > 0\}`,
#
# .. math::
#    p'_t &= p'_{t-1} + \beta\,(p_t - p'_{t-1}) \quad\text{every period},\\
#    z'_t &= z'_{t-1} + \alpha\,(z_t - z'_{t-1})
#    \quad\text{only when } Y_t > 0,\\
#    \hat Y_t &= p'_t\, z'_t.
#
# The probability can be updated on a zero; an interval cannot. So a dying
# item decays toward zero at rate :math:`(1-\beta)` per period, which the
# anchor measures directly against the closed form.
#
# **And the product form is unbiased where the ratio is not.** Because
# :math:`p'` and :math:`z'` are independent under stationary demand,
# :math:`E[\hat Y] = E[p']E[z'] = p\mu` exactly. Croston's ratio suffers
# an inversion bias, :math:`1/E[X] \ne E[1/X]`, which over-forecasts; SBA
# patches it with a deflator :math:`(1-\alpha/2)` that is linear in the
# smoothing constant and leaves some bias behind. All three are here, and
# the anchor measures the bias of each against a known :math:`p\mu`
# rather than repeating the claim.
#
# References
# ----------
# Teunter, R. H., Syntetos, A. A. & Babai, M. Z. (2011) "Intermittent
# demand: Linking forecasting to inventory obsolescence", *European
# Journal of Operational Research* 214(3), 606-615,
# doi:10.1016/j.ejor.2011.05.018. Secs. 2-3: the method, its
# unbiasedness, and the obsolescence argument.
#
# Croston, J. D. (1972) "Forecasting and Stock Control for Intermittent
# Demands", *Operational Research Quarterly* 23(3), 289-303,
# doi:10.2307/3007885. The method TSB modifies.
#
# Syntetos, A. A. & Boylan, J. E. (2005) "The accuracy of intermittent
# demand estimates", *International Journal of Forecasting* 21(2),
# 303-314, doi:10.1016/j.ijforecast.2004.10.001. The SBA deflator.
#
# Syntetos, A. A. & Boylan, J. E. (2001) "On the bias of intermittent
# demand estimates", *International Journal of Production Economics*
# 71(1-3), 457-466, doi:10.1016/S0925-5273(00)00143-2. The inversion
# bias itself -- an ASYMPTOTIC result, which is why the anchor measures
# it under ``init="known"``.
#
# Prak, D., Teunter, R., Babai, M. Z., Boylan, J. E. & Syntetos, A.
# (2021) "Robust compound Poisson parameter estimation for inventory
# control", *Omega* 104, 102481, doi:10.1016/j.omega.2021.102481. That
# the standard intermittent-demand estimators are severely biased in
# finite samples, which is the effect ``burn_in`` and the ``init``
# routes exist to separate from the asymptotic bias above.
#
# Teunter, R. H. & Duncan, L. (2009) "Forecasting intermittent demand:
# a comparative study", *Journal of the Operational Research Society*
# 60(3), 321-329, doi:10.1057/palgrave.jors.2602569. That per-period
# error measures are the wrong yardstick for intermittent demand, which
# is why the anchor compares bias against a known p*mu rather than
# ranking methods on RMSE.
#
# Kourentzes, N. (2014) "On intermittent demand model optimisation and
# selection", *International Journal of Production Economics* 156,
# 180-190. The article prints no DOI. That the smoothing constants
# and the initial states should be estimated together, and that the
# usual squared-error optimisation misbehaves on intermittent series.
#
# Babai, M. Z., Syntetos, A. & Teunter, R. (2014) "Intermittent demand
# forecasting: An empirical study on accuracy and the risk of
# obsolescence", *International Journal of Production Economics* 157,
# 212-219. The article prints no DOI. The empirical comparison of
# TSB against Croston and SBA under obsolescence risk.
#
# Babai, M. Z., Dallery, Y., Boubaker, S. & Kalai, R. (2019) "A new
# method to forecast intermittent demand in the presence of inventory
# obsolescence", *International Journal of Production Economics* 209,
# 30-41, doi:10.1016/j.ijpe.2018.01.026. A later obsolescence-aware
# alternative to TSB.
#
# Yang, Y., Ding, C., Lee, S., Yu, L. & Ma, F. (2021) "A modified
# Teunter-Syntetos-Babai method for intermittent demand forecasting",
# *Journal of Management Science and Engineering* 6(1), 53-63,
# doi:10.1016/j.jmse.2021.02.008. A modification of the TSB update.
#
# Svetunkov, I. & Boylan, J. E. (2023) "iETS: State space model for
# intermittent demand forecasting", *International Journal of
# Production Economics* 265, 109013, doi:10.1016/j.ijpe.2023.109013.
# The state-space formulation that puts Croston and TSB in one family.

.tsbF_EPS <- 1e-12
.tsbF_METHODS <- c("tsb", "croston", "sba")
.tsbF_INITS <- c("global", "heuristic", "known")

#' .tsbF_init
#'
#' A step of the tsbF_native implementation. Called by \code{morie_tsbF_croston_forecast}, \code{morie_tsbF_tsb_forecast}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param init One of \code{"global"}, \code{"known"}. Defaults to \code{"global"}.
#' @param z0 Defaults to \code{NULL}.
#' @param x0 Defaults to \code{NULL}.
#' @param p0 Defaults to \code{NULL}.
#' @return A list with \code{first}, \code{Z}, \code{X}, \code{P}.
#' @export
.tsbF_init <- function(y, init = "global", z0 = NULL, x0 = NULL, p0 = NULL) {
  yv <- as.numeric(y)
  pos <- yv[yv > 0]
  if (length(pos) == 0) stop("tsbF: the series has no positive demand")
  if (!(init %in% .tsbF_INITS)) {
    stop(sprintf("tsbF: init must be one of %s, got %s",
                 paste(.tsbF_INITS, collapse = ", "), init))
  }
  first <- which(yv > 0)[1]
  if (init == "known") {
    if (is.null(z0) || (is.null(x0) && is.null(p0))) {
      stop("tsbF: init='known' needs z0 and one of x0 / p0")
    }
    Z <- as.numeric(z0)
    if (is.null(x0)) {
      p0v <- as.numeric(p0)
      if (!(p0v > 0 && p0v <= 1)) {
        stop(sprintf("tsbF: p0 must be in (0, 1], got %s", p0))
      }
      X <- 1.0 / p0v
      P <- p0v
    } else {
      if (as.numeric(x0) < 1.0) {
        stop(sprintf("tsbF: x0 must be at least 1, got %s", x0))
      }
      X <- as.numeric(x0)
      P <- if (is.null(p0)) 1.0 / X else as.numeric(p0)
    }
    if (Z <= 0) {
      stop(sprintf("tsbF: z0 must be positive, got %s", z0))
    }
    return(list(first = first, Z = Z, X = X, P = P))
  }
  if (init == "global") {
    Z <- sum(pos) / length(pos)
    X <- length(yv) / as.numeric(length(pos))
    P <- length(pos) / as.numeric(length(yv))
    return(list(first = first, Z = Z, X = max(X, 1.0), P = P))
  }
  # heuristic
  gaps <- numeric(0)
  last <- first
  if (first < length(yv)) {
    for (i in (first + 1):length(yv)) {
      if (yv[i] > 0) {
        gaps <- c(gaps, i - last)
        last <- i
      }
    }
  }
  X <- if (length(gaps) > 0) sum(gaps) / length(gaps) else 1.0
  return(list(first = first, Z = pos[1], X = max(X, 1.0),
              P = length(pos) / as.numeric(length(yv))))
}

#' .tsbF_burn
#'
#' A step of the tsbF_native implementation. Called by \code{morie_tsbF_croston_forecast}, \code{morie_tsbF_tsb_forecast}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq A vector; its length is taken and its elements indexed.
#' @param burn_in See Usage.
#' @return The value of \code{[}.
#' @export
.tsbF_burn <- function(seq, burn_in) {
  b <- as.integer(burn_in)
  if (b < 0) {
    stop(sprintf("tsbF: burn_in must be non-negative, got %s", burn_in))
  }
  if (b >= length(seq)) {
    stop(sprintf("tsbF: burn_in %d discards the whole series of length %d",
                 b, length(seq)))
  }
  return(seq[(b + 1):length(seq)])
}

#' morie_tsbF_tsb_forecast
#'
#' A step of the tsbF_native implementation. Called by \code{morie_tsbF_intermittent_forecast}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param alpha Defaults to \code{0.1}.
#' @param beta Defaults to \code{0.05}.
#' @param horizon Defaults to \code{1}.
#' @param init Passed to \code{.tsbF_init}. Defaults to \code{"global"}.
#' @param z0 Passed to \code{.tsbF_init}.
#' @param p0 Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param burn_in Passed to \code{.tsbF_burn}. Defaults to \code{0}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_tsbF_tsb_forecast <- function(y, alpha = 0.1, beta = 0.05, horizon = 1,
                                    init = "global", z0 = NULL, p0 = NULL,
                                    burn_in = 0) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n < 2) stop(sprintf("tsbF: need at least 2 observations, got %d", n))
  if (!(alpha > 0 && alpha <= 1)) {
    stop(sprintf("tsbF: alpha must be in (0, 1], got %s", alpha))
  }
  if (!(beta > 0 && beta <= 1)) {
    stop(sprintf("tsbF: beta must be in (0, 1], got %s", beta))
  }
  x0_arg <- if (is.null(p0)) NULL else 1.0 / p0
  st <- .tsbF_init(yv, init = init, z0 = z0, p0 = p0, x0 = x0_arg)
  zi <- st$Z
  pi <- st$P

  a <- as.numeric(alpha)
  b <- as.numeric(beta)
  z <- zi
  p <- pi
  fitted <- numeric(n)
  probs <- numeric(n)
  sizes <- numeric(n)
  for (t in 1:n) {
    occ <- if (yv[t] > 0) 1.0 else 0.0
    p <- p + b * (occ - p)
    if (occ > 0) {
      z <- z + a * (yv[t] - z)
    }
    probs[t] <- p
    sizes[t] <- z
    fitted[t] <- p * z
  }

  h <- as.integer(horizon)
  result <- list(
    estimate = rep(fitted[n], h),
    forecast = rep(fitted[n], h),
    fitted = .tsbF_burn(fitted, burn_in),
    fitted_full = fitted,
    probability = .tsbF_burn(probs, burn_in),
    size = .tsbF_burn(sizes, burn_in),
    init = init,
    burn_in = as.integer(burn_in),
    z_init = zi,
    p_init = pi,
    p_final = p,
    z_final = z,
    alpha = a,
    beta = b,
    method = "TSB, Teunter, Syntetos & Babai (2011)",
    updates_on_zeros = TRUE
  )
  return(result)
}

#' morie_tsbF_croston_forecast
#'
#' A step of the tsbF_native implementation. Called by \code{morie_tsbF_intermittent_forecast}, \code{morie_tsbF_sba_forecast}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param alpha Defaults to \code{0.1}.
#' @param horizon Defaults to \code{1}.
#' @param init Passed to \code{.tsbF_init}. Defaults to \code{"global"}.
#' @param z0 Passed to \code{.tsbF_init}.
#' @param x0 Passed to \code{.tsbF_init}.
#' @param burn_in Passed to \code{.tsbF_burn}. Defaults to \code{0}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_tsbF_croston_forecast <- function(y, alpha = 0.1, horizon = 1,
                                        init = "global", z0 = NULL, x0 = NULL,
                                        burn_in = 0) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n < 2) stop(sprintf("tsbF: need at least 2 observations, got %d", n))
  if (!(alpha > 0 && alpha <= 1)) {
    stop(sprintf("tsbF: alpha must be in (0, 1], got %s", alpha))
  }
  st <- .tsbF_init(yv, init = init, z0 = z0, x0 = x0, p0 = NULL)
  zi <- st$Z
  xi <- st$X

  a <- as.numeric(alpha)
  z <- zi
  x <- xi
  since <- 0
  fitted <- numeric(n)
  for (t in 1:n) {
    since <- since + 1
    if (yv[t] > 0) {
      z <- z + a * (yv[t] - z)
      x <- x + a * (since - x)
      since <- 0
    }
    fitted[t] <- z / max(x, .tsbF_EPS)
  }

  h <- as.integer(horizon)
  result <- list(
    estimate = rep(fitted[n], h),
    forecast = rep(fitted[n], h),
    fitted = .tsbF_burn(fitted, burn_in),
    fitted_full = fitted,
    z_final = z,
    x_final = x,
    alpha = a,
    init = init,
    burn_in = as.integer(burn_in),
    z_init = zi,
    x_init = xi,
    method = "Croston (1972)",
    updates_on_zeros = FALSE
  )
  return(result)
}

#' morie_tsbF_sba_forecast
#'
#' A step of the tsbF_native implementation. Called by \code{morie_tsbF_intermittent_forecast}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{morie_tsbF_croston_forecast}.
#' @param alpha Passed to \code{morie_tsbF_croston_forecast}. Defaults to \code{0.1}.
#' @param horizon Passed to \code{morie_tsbF_croston_forecast}. Defaults to \code{1}.
#' @param init Passed to \code{morie_tsbF_croston_forecast}. Defaults to \code{"global"}.
#' @param z0 Passed to \code{morie_tsbF_croston_forecast}.
#' @param x0 Passed to \code{morie_tsbF_croston_forecast}.
#' @param burn_in Passed to \code{morie_tsbF_croston_forecast}. Defaults to \code{0}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_tsbF_sba_forecast <- function(y, alpha = 0.1, horizon = 1,
                                    init = "global", z0 = NULL, x0 = NULL,
                                    burn_in = 0) {
  c <- morie_tsbF_croston_forecast(y, alpha = alpha, horizon = horizon,
                                   init = init, z0 = z0, x0 = x0,
                                   burn_in = burn_in)
  d <- 1.0 - as.numeric(alpha) / 2.0
  result <- list(
    estimate = c$forecast * d,
    forecast = c$forecast * d,
    fitted = c$fitted * d,
    fitted_full = c$fitted_full * d,
    init = init,
    burn_in = as.integer(burn_in),
    deflator = d,
    alpha = as.numeric(alpha),
    method = "Syntetos-Boylan Approximation (2005)",
    updates_on_zeros = FALSE
  )
  return(result)
}

#' morie_tsbF_demand_classification
#'
#' A step of the tsbF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param adi_cut Defaults to \code{1.32}.
#' @param cv2_cut Defaults to \code{0.49}.
#' @return A list with \code{class}, \code{adi}, \code{cv2}, \code{n_positive}, \code{n}.
#' @export
morie_tsbF_demand_classification <- function(y, adi_cut = 1.32, cv2_cut = 0.49) {
  yv <- as.numeric(y)
  pos <- yv[yv > 0]
  if (length(pos) < 2) stop("tsbF: need at least 2 positive demands")
  adi <- length(yv) / as.numeric(length(pos))
  mu <- sum(pos) / length(pos)
  cv2 <- if (mu > 0) (sd(pos) / mu)^2 else 0.0
  cls <- if (adi <= adi_cut && cv2 <= cv2_cut) {
    "smooth"
  } else if (adi <= adi_cut) {
    "erratic"
  } else if (cv2 <= cv2_cut) {
    "intermittent"
  } else {
    "lumpy"
  }
  return(list(class = cls, adi = adi, cv2 = cv2,
              n_positive = length(pos), n = length(yv)))
}

#' morie_tsbF_intermittent_forecast
#'
#' A step of the tsbF_native implementation. Called by \code{morie_tsbF}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{morie_tsbF_tsb_forecast}.
#' @param method One of \code{"croston"}, \code{"tsb"}. Defaults to \code{"tsb"}.
#' @param alpha Passed to \code{morie_tsbF_tsb_forecast}. Defaults to \code{0.1}.
#' @param beta Passed to \code{morie_tsbF_tsb_forecast}. Defaults to \code{0.05}.
#' @param horizon Passed to \code{morie_tsbF_tsb_forecast}. Defaults to \code{1}.
#' @param init Passed to \code{morie_tsbF_tsb_forecast}. Defaults to \code{"global"}.
#' @param z0 Passed to \code{morie_tsbF_tsb_forecast}.
#' @param x0 Passed to \code{morie_tsbF_croston_forecast}.
#' @param p0 Passed to \code{morie_tsbF_tsb_forecast}.
#' @param burn_in Passed to \code{morie_tsbF_tsb_forecast}. Defaults to \code{0}.
#' @return The value of \code{morie_tsbF_sba_forecast}.
#' @export
morie_tsbF_intermittent_forecast <- function(y, method = "tsb", alpha = 0.1,
                                             beta = 0.05, horizon = 1,
                                             init = "global", z0 = NULL,
                                             x0 = NULL, p0 = NULL,
                                             burn_in = 0) {
  if (!(method %in% .tsbF_METHODS)) {
    stop(sprintf("tsbF: method must be one of %s, got %s",
                 paste(.tsbF_METHODS, collapse = ", "), method))
  }
  if (method == "tsb") {
    return(morie_tsbF_tsb_forecast(y, alpha = alpha, beta = beta,
                                   horizon = horizon, init = init, z0 = z0,
                                   p0 = p0, burn_in = burn_in))
  }
  if (method == "croston") {
    return(morie_tsbF_croston_forecast(y, alpha = alpha, horizon = horizon,
                                       init = init, z0 = z0, x0 = x0,
                                       burn_in = burn_in))
  }
  return(morie_tsbF_sba_forecast(y, alpha = alpha, horizon = horizon,
                                 init = init, z0 = z0, x0 = x0,
                                 burn_in = burn_in))
}

#' morie_tsbF_cheatsheet
#'
#' A step of the tsbF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_tsbF_cheatsheet <- function() {
  return(paste0(
    "tsbF: TSB updates the PROBABILITY every period (p' += ",
    "beta(occ - p')) and the SIZE only on demand; forecast is ",
    "the PRODUCT p'z', which is unbiased because the two are ",
    "independent. Croston smooths the INTERVAL and forecasts ",
    "z'/x' -- nothing updates on a zero, so an obsolete item ",
    "keeps its forecast forever, and the ratio carries an ",
    "inversion bias. SBA deflates by (1 - alpha/2). The ",
    "inversion bias is ASYMPTOTIC: use init='known' to see ",
    "it, because with init='heuristic' the initial state ",
    "decays as (1-alpha)^t and takes ~3/alpha periods to ",
    "clear, which can flip the measured sign (Prak et al. ",
    "2021). burn_in drops that transient."))
}

# Main entry point -- dispatch to the requested method.
#' Main entry point -- dispatch to the requested method
#'
#' A step of the tsbF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{morie_tsbF_intermittent_forecast}.
#' @param method Passed to \code{morie_tsbF_intermittent_forecast}. Defaults to \code{"tsb"}.
#' @param alpha Passed to \code{morie_tsbF_intermittent_forecast}. Defaults to \code{0.1}.
#' @param beta Passed to \code{morie_tsbF_intermittent_forecast}. Defaults to \code{0.05}.
#' @param horizon Passed to \code{morie_tsbF_intermittent_forecast}. Defaults to \code{1}.
#' @param init Passed to \code{morie_tsbF_intermittent_forecast}. Defaults to \code{"global"}.
#' @param z0 Passed to \code{morie_tsbF_intermittent_forecast}.
#' @param x0 Passed to \code{morie_tsbF_intermittent_forecast}.
#' @param p0 Passed to \code{morie_tsbF_intermittent_forecast}.
#' @param burn_in Passed to \code{morie_tsbF_intermittent_forecast}. Defaults to \code{0}.
#' @return The value of \code{morie_tsbF_intermittent_forecast}.
#' @export
morie_tsbF <- function(y, method = "tsb", alpha = 0.1, beta = 0.05,
                       horizon = 1, init = "global", z0 = NULL,
                       x0 = NULL, p0 = NULL, burn_in = 0) {
  morie_tsbF_intermittent_forecast(y, method = method, alpha = alpha,
                                   beta = beta, horizon = horizon,
                                   init = init, z0 = z0, x0 = x0, p0 = p0,
                                   burn_in = burn_in)
}
