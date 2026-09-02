# Quarantine efficacy: how much transmission a duration actually
# prevents.
# Sources: Ashcroft, P., Lehtinen, S., Angst, D. C., Low, N. &
# Bonhoeffer, S. (2021) "Quantifying the impact of quarantine
# duration on COVID-19 transmission", eLife 10, e63704,
# doi:10.7554/eLife.63704 (eq. 1 efficacy, eq. 2 test-and-release,
# eq. 4 utility, the infected-fraction cancellation). Kucirka et al.
# (2020) Ann. Intern. Med. 173(4) (time-varying false-negative rate,
# supplied as an input). Native R mirroring morie.fn.qrntcq: same
# generation-time density, same integral-as-prevention, same ceiling,
# same utility ratio cancellation.

.qrntcq_EPS <- 1e-12

#' morie_qrntcq_gamma_generation_time
#'
#' A step of the qrntcq_native implementation. Called by \code{morie_qrntcq_efficacy_test_and_release}, \code{morie_qrntcq_optimal_duration}, \code{morie_qrntcq_quarantine_efficacy} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param shape Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2.83}.
#' @param scale Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.86}.
#' @param grid Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param t.max Passed to \code{seq}. Defaults to \code{30}.
#' @param n Passed to \code{seq}. Defaults to \code{3001L}.
#' @return A list with \code{t}, \code{density}.
#' @export
morie_qrntcq_gamma_generation_time <- function(shape = 2.83, scale = 1.86,
                                              grid = NULL, t.max = 30,
                                              n = 3001L) {
  if (shape <= 0 || scale <= 0)
    stop("qrntcq: the gamma shape and scale must be positive")
  ts <- if (is.null(grid)) seq(0, t.max, length.out = n)
        else as.numeric(grid)
  a <- as.numeric(shape)
  b <- as.numeric(scale)
  dens <- numeric(length(ts))
  for (i in seq_along(ts)) {
    t <- ts[i]
    dens[i] <- if (t <= 0) 0
      else exp((a - 1) * log(t) - t / b - lgamma(a) - a * log(b))
  }
  z <- 0.5 * sum((head(dens, -1) + tail(dens, -1)) *
                 diff(ts))
  if (z <= .qrntcq_EPS)
    stop("qrntcq: the generation-time density integrates to zero")
  list(t = ts, density = dens / z)
}

#' .mass
#'
#' A step of the qrntcq_native implementation. Called by \code{morie_qrntcq_quarantine_efficacy}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ts A vector; its length is taken and its elements indexed.
#' @param ys A vector; indexed elementwise.
#' @param lo Numeric; passed to \code{max}.
#' @param hi Numeric; passed to \code{min}.
#' @return The value of \code{tot}, as built in the body.
#' @export
.mass <- function(ts, ys, lo, hi) {
  if (hi <= lo) return(0)
  tot <- 0
  for (i in seq_len(length(ts) - 1L)) {
    a <- ts[i]
    b <- ts[i + 1]
    if (b <= lo || a >= hi) next
    l <- max(a, lo)
    r <- min(b, hi)
    if (r <= l) next
    w <- b - a
    ya <- ys[i] + (ys[i + 1] - ys[i]) * (if (w) (l - a) / w else 0)
    yb <- ys[i] + (ys[i + 1] - ys[i]) * (if (w) (r - a) / w else 0)
    tot <- tot + 0.5 * (ya + yb) * (r - l)
  }
  tot
}

#' morie_qrntcq_quarantine_efficacy
#'
#' A step of the qrntcq_native implementation. Called by \code{morie_qrntcq_efficacy_test_and_release}, \code{morie_qrntcq_optimal_duration}, \code{morie_qrntcq_relative_utility}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t.Q Coerced to numeric by the body, with \code{as.numeric}.
#' @param t.R Coerced to numeric by the body, with \code{as.numeric}.
#' @param generation.time Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param t.E Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return A list with \code{efficacy}, \code{prevented.mass}, \code{remaining.mass}, \code{t.Q}, \code{t.R}, \code{max.attainable}, \code{pre.quarantine.mass}.
#' @export
morie_qrntcq_quarantine_efficacy <- function(t.Q, t.R,
                                              generation.time = NULL,
                                              t.E = 0) {
  g <- if (is.null(generation.time))
    morie_qrntcq_gamma_generation_time() else generation.time
  ts <- g$t
  ys <- g$density
  q <- as.numeric(t.Q)
  r <- as.numeric(t.R)
  if (r < q) stop(paste0("qrntcq: release at ", r, " precedes quarantine start at ", q))
  if (q < as.numeric(t.E))
    stop(paste0("qrntcq: quarantine cannot start before exposure (t_Q ",
                q, " < t_E ", t.E, ")"))
  remaining <- .mass(ts, ys, q, ts[length(ts)])
  if (remaining <= .qrntcq_EPS)
    return(list(efficacy = 0, remaining.mass = remaining,
                prevented.mass = 0,
                note = "no transmission remains after t_Q, so quarantine can prevent nothing"))
  prevented <- .mass(ts, ys, q, r)
  list(efficacy = prevented / remaining,
       prevented.mass = prevented, remaining.mass = remaining,
       t.Q = q, t.R = r, max.attainable = 1,
       pre.quarantine.mass = .mass(ts, ys, ts[1], q))
}

#' morie_qrntcq_efficacy_test_and_release
#'
#' A step of the qrntcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t.Q Passed to \code{morie_qrntcq_quarantine_efficacy}.
#' @param t.T Carried through into a list the body builds.
#' @param t.R Passed to \code{morie_qrntcq_quarantine_efficacy}.
#' @param false.negative Coerced to numeric by the body, with \code{as.numeric}.
#' @param generation.time Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param t.R.positive Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{efficacy}, \code{efficacy.detained}, \code{efficacy.released}, \code{false.negative}, \code{t.T}, \code{t.R}, \code{bound}, \code{note}.
#' @export
morie_qrntcq_efficacy_test_and_release <- function(t.Q, t.T, t.R,
                                                   false.negative,
                                                   generation.time = NULL,
                                                   t.R.positive = NULL) {
  g <- if (is.null(generation.time))
    morie_qrntcq_gamma_generation_time() else generation.time
  p <- as.numeric(false.negative)
  if (p < 0 || p > 1)
    stop(paste0("qrntcq: the false-negative probability must lie in [0, 1], got ",
                false.negative))
  if (t.T < t.Q)
    stop("qrntcq: the test cannot precede the start of quarantine")
  if (t.R < t.T)
    stop("qrntcq: release cannot precede the test")
  stay <- if (is.null(t.R.positive)) g$t[length(g$t)] else t.R.positive
  released <- morie_qrntcq_quarantine_efficacy(t.Q, t.R, g)$efficacy
  detained <- morie_qrntcq_quarantine_efficacy(t.Q, stay, g)$efficacy
  eff <- (1 - p) * detained + p * released
  list(efficacy = eff, efficacy.detained = detained,
       efficacy.released = released, false.negative = p,
       t.T = t.T, t.R = t.R, bound = detained,
       note = "always at or below the efficacy of detaining everyone until t_R_positive, because a false negative releases an infectious person")
}

#' morie_qrntcq_utility
#'
#' A step of the qrntcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param efficacy Coerced to numeric by the body, with \code{as.numeric}.
#' @param days.in.quarantine Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
morie_qrntcq_utility <- function(efficacy, days.in.quarantine) {
  d <- as.numeric(days.in.quarantine)
  if (d <= 0) stop("qrntcq: the time in quarantine must be positive")
  as.numeric(efficacy) / d
}

#' morie_qrntcq_relative_utility
#'
#' A step of the qrntcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t.R.a Numeric; combined arithmetically in the body.
#' @param t.R.b Numeric; combined arithmetically in the body.
#' @param t.Q Numeric; combined arithmetically in the body. Defaults to \code{3}.
#' @param generation.time Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param infected.fraction Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{relative.utility}, \code{utility.a}, \code{utility.b}, \code{efficacy.a}, \code{efficacy.b}, \code{independent.of.infected.fraction}, \code{note}.
#' @export
morie_qrntcq_relative_utility <- function(t.R.a, t.R.b, t.Q = 3,
                                          generation.time = NULL,
                                          infected.fraction = NULL) {
  g <- if (is.null(generation.time))
    morie_qrntcq_gamma_generation_time() else generation.time
  ea <- morie_qrntcq_quarantine_efficacy(t.Q, t.R.a, g)$efficacy
  eb <- morie_qrntcq_quarantine_efficacy(t.Q, t.R.b, g)$efficacy
  da <- t.R.a - t.Q
  db <- t.R.b - t.Q
  if (da <= 0 || db <= 0)
    stop("qrntcq: both quarantines must have positive duration")
  list(relative.utility = (ea / da) / (eb / db),
       utility.a = ea / da, utility.b = eb / db,
       efficacy.a = ea, efficacy.b = eb,
       independent.of.infected.fraction = TRUE,
       note = "the infected fraction cancels for standard quarantine, so 'most quarantined people are not infected' is not an argument for shortening it")
}

#' morie_qrntcq_optimal_duration
#'
#' A step of the qrntcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t.Q Numeric; combined arithmetically in the body. Defaults to \code{3}.
#' @param generation.time Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param t.max Numeric; combined arithmetically in the body. Defaults to \code{20}.
#' @param step Numeric; combined arithmetically in the body. Defaults to \code{0.25}.
#' @return A list with \code{estimate}, \code{optimal.t.R}, \code{efficacy.at.optimum}, \code{utility.at.optimum}, \code{curve}, \code{t.Q}, \code{method}.
#' @export
morie_qrntcq_optimal_duration <- function(t.Q = 3, generation.time = NULL,
                                          t.max = 20, step = 0.25) {
  g <- if (is.null(generation.time))
    morie_qrntcq_gamma_generation_time() else generation.time
  best <- NULL
  curve <- list()
  t <- t.Q + step
  while (t <= t.max + .qrntcq_EPS) {
    e <- morie_qrntcq_quarantine_efficacy(t.Q, t, g)$efficacy
    u <- e / (t - t.Q)
    curve[[length(curve) + 1L]] <- list(t.R = t, efficacy = e, utility = u)
    if (is.null(best) || u > best$utility)
      best <- list(t.R = t, efficacy = e, utility = u)
    t <- t + step
  }
  list(estimate = best$t.R, optimal.t.R = best$t.R,
       efficacy.at.optimum = best$efficacy,
       utility.at.optimum = best$utility,
       curve = curve, t.Q = t.Q,
       method = "utility maximisation, Ashcroft et al. (2021) eq. (4)")
}

# house entry point: the package exports one morie_<module>
morie_qrntcq <- morie_qrntcq_gamma_generation_time
