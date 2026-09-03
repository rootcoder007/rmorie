# morie.fn -- function file (rootcoder007/morie)
# Stochastic treatment regimes.
#
# Static regimes assign one treatment level to everyone; dynamic regimes
# assign it as a function of measured history. Both are deterministic --
# fully fixed by pre-treatment variables -- and both are the wrong frame
# for a great deal of applied work.
#
# Two reasons the chapter gives, and they are different. First, realistic
# interventions often cannot put treatment into a deterministic state:
# setting an individual's exercise regime by a fixed rule is not something
# anyone can do. A mass-media campaign is deterministic at the community
# level and stochastic at the individual level, because each person adopts
# or not for reasons outside the intervention. Second, even where a
# deterministic regime is conceivable, its effect may be unidentifiable
# because the regime is not supported in the observed data -- nobody in
# the data behaves that way.
#
# A stochastic regime shifts the treatment distribution instead of
# setting it. For a continuous exposure the natural version is a shift:
# d(a, w) = a + delta, so everyone's exposure moves by delta from
# wherever it was. The estimand is the mean outcome under the shifted
# distribution, Psi(P) = E[ int Qbar(a, W) g_delta(a | W) da ], and for
# the shift intervention this equals E[ Qbar(A + delta, W) ] -- an
# average of the outcome regression over the observed exposure
# distribution, translated.
#
# Positivity becomes a support condition, and a milder one. A
# deterministic regime needs positive probability of the assigned value
# for every covariate pattern; a shift needs only that the shifted value
# stays inside the support of the conditional exposure distribution.
# positivity_shift reports the fraction that leaves it, because the
# whole appeal of a stochastic regime is lost if the shift walks off the
# edge of the data.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 14 (Diaz &
# van der Laan): static and dynamic regimes are both deterministic
# because they are completely determined by pre-treatment variables;
# deterministic regimes are the wrong framework for phenomena not
# subject to direct intervention -- the exercise-regime and mass-media
# examples, where an intervention is deterministic at the community
# level and stochastic at the individual level; causal effects for
# deterministic regimes may be unidentifiable because the regime is not
# supported in the observed data; the data, notation and parameter of
# interest with its identification and positivity assumption; the
# optimality theory for stochastic regimes; the TMLE and its asymptotic
# distribution; and super learning for a conditional density as the
# initial estimator.
#
# Diaz, I. & van der Laan, M. J. (2012) "Population Intervention Causal
# Effects Based on Stochastic Interventions", Biometrics 68(2), 541-549,
# doi:10.1111/j.1541-0420.2011.01685.x.
#
# Haneuse, S. & Rotnitzky, A. (2013) "Estimation of the effect of
# interventions that modify the received treatment", Statistics in
# Medicine 32(30), 5260-5277, doi:10.1002/sim.5907. Modified-treatment
# policies.

# Private helpers (shared environment: prefix avoids collisions)
#' Private helpers (shared environment: prefix avoids collisions)
#'
#' A step of the tlstoch_native implementation. Called by \code{.tlstoch_density_ratio},
#' \code{.tlstoch_positivity_shift}, \code{.tlstoch_shift_regime} and 2 others in the
#' module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlstoch_vec(x = x)
#' res
.tlstoch_vec <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }
  as.numeric(x)
}

#' .tlstoch_mat
#'
#' A step of the tlstoch_native implementation. Called by \code{.tlstoch_density_ratio},
#' \code{.tlstoch_shift_tmle}, \code{.tlstoch_stochastic_estimand}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return A matrix, from \code{matrix}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlstoch_mat(x = x)
#' res
.tlstoch_mat <- function(x) {
  if (is.null(x)) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
    return(x)
  }
  if (is.list(x)) {
    if (length(x) == 0L) {
      return(matrix(numeric(0), nrow = 0, ncol = 0))
    }
    nr <- length(x)
    nc <- max(vapply(x, length, integer(1)))
    m <- matrix(0, nrow = nr, ncol = nc)
    for (i in seq_len(nr)) {
      li <- length(x[[i]])
      if (li > 0L) m[i, seq_len(li)] <- as.numeric(x[[i]])
    }
    return(m)
  }
  matrix(as.numeric(x), ncol = 1L)
}

.tlstoch_eps <- 1e-12

#' .tlstoch_shift_regime
#'
#' A step of the tlstoch_native implementation. Called by \code{.tlstoch_shift_tmle},
#' \code{.tlstoch_stochastic_estimand}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Passed to \code{.tlstoch_vec}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @param lower Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param upper Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{shifted}, \code{delta}, \code{n_clipped}, \code{fraction_clipped}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .tlstoch_shift_regime(A = A, delta = 0.5)
#' res
.tlstoch_shift_regime <- function(A, delta, lower = NULL, upper = NULL) {
  a <- .tlstoch_vec(A)
  d <- as.numeric(delta)
  n <- length(a)
  out <- numeric(n)
  clipped <- 0L
  for (i in seq_len(n)) {
    s <- a[i] + d
    if (!is.null(lower) && s < as.numeric(lower)) {
      s <- as.numeric(lower)
      clipped <- clipped + 1L
    }
    if (!is.null(upper) && s > as.numeric(upper)) {
      s <- as.numeric(upper)
      clipped <- clipped + 1L
    }
    out[i] <- s
  }
  list(
    shifted = out, delta = d, n_clipped = clipped,
    fraction_clipped = clipped / n
  )
}

#' .tlstoch_positivity_shift
#'
#' A step of the tlstoch_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Passed to \code{.tlstoch_vec}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @param W Optional; may be \code{NULL}. Passed to \code{.tlstoch_vec}.
#' @param bins Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.
#' @return A list with \code{fraction_outside}, \code{delta}, \code{bins},
#' \code{satisfied}, \code{note}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .tlstoch_positivity_shift(A = A, delta = 0.5)
#' res
.tlstoch_positivity_shift <- function(A, delta, W = NULL, bins = 5) {
  a <- .tlstoch_vec(A)
  d <- as.numeric(delta)
  n <- length(a)
  if (is.null(W)) {
    lo <- min(a)
    hi <- max(a)
    out <- sum(a + d < lo | a + d > hi)
    return(list(
      fraction_outside = out / n, support = c(lo, hi), delta = d,
      satisfied = (out == 0L)
    ))
  }
  w <- .tlstoch_vec(W)
  lo_w <- min(w)
  hi_w <- max(w)
  nbins <- as.integer(bins)
  width <- (hi_w - lo_w) / nbins
  if (width == 0) width <- 1.0
  out <- 0L
  bin_idx <- pmin(floor((w - lo_w) / width), nbins - 1L)
  for (i in seq_len(n)) {
    same <- a[bin_idx == bin_idx[i]]
    if (a[i] + d < min(same) || a[i] + d > max(same)) {
      out <- out + 1L
    }
  }
  list(
    fraction_outside = out / n, delta = d, bins = nbins,
    satisfied = (out == 0L),
    note = paste(
      "milder than deterministic positivity: the shift",
      "only has to stay inside the CONDITIONAL support"
    )
  )
}

#' .tlstoch_stochastic_estimand
#'
#' A step of the tlstoch_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q_fn Accepted by the signature and not used anywhere in the body.
#' @param A Passed to \code{.tlstoch_vec}.
#' @param W Passed to \code{.tlstoch_mat}.
#' @param delta Passed to \code{.tlstoch_shift_regime}.
#' @param lower Passed to \code{.tlstoch_shift_regime}.
#' @param upper Passed to \code{.tlstoch_shift_regime}.
#' @return A list with \code{psi}, \code{observed_mean}, \code{contrast}, \code{delta}.
#' @export
.tlstoch_stochastic_estimand <- function(Q_fn, A, W, delta,
                                         lower = NULL, upper = NULL) {
  a <- .tlstoch_vec(A)
  W_mat <- .tlstoch_mat(W)
  nr <- nrow(W_mat)
  if (nr != length(a)) {
    stop(sprintf(
      "tlstoch: %d exposures but %d covariate rows",
      length(a), nr
    ))
  }
  sh <- .tlstoch_shift_regime(a, delta, lower, upper)$shifted
  vals <- numeric(nr)
  obs <- numeric(nr)
  for (i in seq_len(nr)) {
    vals[i] <- as.numeric(Q_fn(sh[i], W_mat[i, ]))
    obs[i] <- as.numeric(Q_fn(a[i], W_mat[i, ]))
  }
  list(
    psi = mean(vals), observed_mean = mean(obs),
    contrast = mean(vals) - mean(obs), delta = as.numeric(delta)
  )
}

#' .tlstoch_density_ratio
#'
#' A step of the tlstoch_native implementation. Called by \code{.tlstoch_shift_tmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Passed to \code{.tlstoch_vec}.
#' @param W Passed to \code{.tlstoch_mat}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @param g_fn Accepted by the signature and not used anywhere in the body.
#' @param lower Accepted by the signature and not used anywhere in the body.
#' @param upper Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{H}, \code{max}, \code{mean}.
#' @export
.tlstoch_density_ratio <- function(A, W, delta, g_fn,
                                   lower = NULL, upper = NULL) {
  a <- .tlstoch_vec(A)
  W_mat <- .tlstoch_mat(W)
  nr <- nrow(W_mat)
  d <- as.numeric(delta)
  out <- numeric(nr)
  for (i in seq_len(nr)) {
    num <- as.numeric(g_fn(a[i] - d, W_mat[i, ]))
    den <- as.numeric(g_fn(a[i], W_mat[i, ]))
    if (den <= .tlstoch_eps) {
      stop(sprintf(paste(
        "tlstoch: the observed exposure has zero",
        "density at observation %d -- the conditional",
        "density estimate is degenerate"
      ), i))
    }
    out[i] <- num / den
  }
  list(H = out, max = max(out), mean = mean(out))
}

#' .tlstoch_shift_tmle
#'
#' A step of the tlstoch_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Passed to \code{.tlstoch_vec}.
#' @param A Passed to \code{.tlstoch_vec}.
#' @param W Passed to \code{.tlstoch_mat}.
#' @param Q_fn Accepted by the signature and not used anywhere in the body.
#' @param g_fn Passed to \code{.tlstoch_density_ratio}.
#' @param delta Passed to \code{.tlstoch_density_ratio}.
#' @param lower Passed to \code{.tlstoch_density_ratio}.
#' @param upper Passed to \code{.tlstoch_density_ratio}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.
#' @return A list with \code{estimate}, \code{psi}, \code{epsilon}, \code{se}, \code{ci},
#' \code{mean_eic}, \code{delta}, \code{max_density_ratio}, \code{method}, \code{note}.
#' @export
.tlstoch_shift_tmle <- function(Y, A, W, Q_fn, g_fn, delta,
                                lower = NULL, upper = NULL, iters = 60) {
  y <- .tlstoch_vec(Y)
  a <- .tlstoch_vec(A)
  W_mat <- .tlstoch_mat(W)
  n <- length(y)
  H <- .tlstoch_density_ratio(a, W_mat, delta, g_fn, lower, upper)$H
  q <- numeric(n)
  for (i in seq_len(n)) {
    q[i] <- as.numeric(Q_fn(a[i], W_mat[i, ]))
  }
  e <- 0.0
  niters <- as.integer(iters)
  for (iter in seq_len(niters)) {
    pred <- q + e * H
    gr <- sum(H * (y - pred))
    he <- sum(H * H)
    if (he < 1e-12) break
    step <- gr / he
    e <- e + step
    if (abs(step) < 1e-12) break
  }
  sh <- .tlstoch_shift_regime(a, delta, lower, upper)$shifted
  qs <- numeric(n)
  for (i in seq_len(n)) {
    qs[i] <- as.numeric(Q_fn(sh[i], W_mat[i, ])) + e
  }
  psi <- mean(qs)
  d_vec <- H * (y - (q + e * H)) + qs - psi
  m <- mean(d_vec)
  se <- sqrt(sum((d_vec - m)^2) / n^2)
  ci_lo <- psi - 1.96 * se
  ci_hi <- psi + 1.96 * se
  list(
    estimate = psi, psi = psi, epsilon = e, se = se,
    ci = c(ci_lo, ci_hi),
    mean_eic = m, delta = as.numeric(delta),
    max_density_ratio = max(H),
    method = paste(
      "TMLE for a stochastic (shift) regime;",
      "van der Laan & Rose (2018) Chap. 14"
    ),
    note = paste(
      "the clever covariate is a DENSITY RATIO,",
      "not an inverse probability"
    )
  )
}

#' .tlstoch_cheatsheet
#'
#' A step of the tlstoch_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .tlstoch_cheatsheet()
#' res
.tlstoch_cheatsheet <- function() {
  paste(
    "tlstoch: static and dynamic regimes are both DETERMINISTIC,",
    "and that is the wrong frame twice over -- you cannot set",
    "someone's exercise regime by a rule, and a media campaign is",
    "deterministic at the community level but stochastic at the",
    "individual one; and a deterministic regime may be",
    "UNIDENTIFIABLE because nobody in the data behaves that way.",
    "A stochastic regime SHIFTS the treatment distribution:",
    "Psi = E[Q(A + delta, W)]. Positivity becomes a support",
    "condition -- the shift need only stay inside the conditional",
    "support -- and the clever covariate is a DENSITY RATIO",
    "rather than an inverse probability."
  )
}

# Entry point for the tlstoch module
morie_tlstoch <- list(
  shift_regime         = .tlstoch_shift_regime,
  positivity_shift     = .tlstoch_positivity_shift,
  stochastic_estimand  = .tlstoch_stochastic_estimand,
  density_ratio        = .tlstoch_density_ratio,
  shift_tmle           = .tlstoch_shift_tmle,
  stochasticregime     = .tlstoch_shift_tmle,
  cheatsheet           = .tlstoch_cheatsheet
)
