# Rangayyan batch A -- Chapter 3: statistics of random processes, the
# delta and step functions, and continuous-time convolution.  Mirror of
# the Python arm (rng001..rng040); formulas read off the PDF of
# Biomedical Signal Analysis (Rangayyan, 2024).  Nothing here calls
# stats:: -- the moments are integrated, not delegated.

.morie_rg_aslist <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }
  as.numeric(x)
}

.morie_rg_gridint <- function(y, x = NULL) {
  # Composite Simpson on a uniform grid with an even panel count (error
  # O(h^4)); trapezoid otherwise, which is what a non-uniform grid admits
  # without interpolating.
  y <- as.numeric(y)
  n <- length(y)
  if (n < 2L) stop("need at least two grid points")
  if (is.null(x)) x <- seq_len(n) - 1 else x <- as.numeric(x)
  if (length(x) != n) stop("x and y must have the same length")
  h <- diff(x)
  if (any(h <= 0)) stop("x must be strictly increasing")
  uniform <- all(abs(h - h[1]) <= 1e-12 * max(1, abs(h[1])))
  if (uniform && (n - 1L) %% 2L == 0L) {
    odd <- seq(2L, n - 1L, by = 2L)
    even <- if (n >= 4L) seq(3L, n - 1L, by = 2L) else integer(0)
    s <- .morie_fsum(c(
      y[1], y[n], 4 * y[odd],
      if (length(even)) 2 * y[even] else numeric(0)
    ))
    return(s * h[1] / 3)
  }
  .morie_fsum(0.5 * (y[-n] + y[-1]) * h)
}

.morie_rg_quad <- function(f, a, b, tol = 1.49e-8, maxdepth = 50L) {
  # Adaptive Simpson, the same rule the Python arm's quad() uses, so the
  # two languages agree to their shared tolerance rather than to whatever
  # a black-box integrator happens to do.
  simp <- function(fa, fm, fb, a, b) (b - a) / 6 * (fa + 4 * fm + fb)
  rec <- function(a, b, fa, fm, fb, whole, tol, depth) {
    m <- 0.5 * (a + b)
    lm <- 0.5 * (a + m)
    rm <- 0.5 * (m + b)
    flm <- f(lm)
    frm <- f(rm)
    left <- simp(fa, flm, fm, a, m)
    right <- simp(fm, frm, fb, m, b)
    if (depth >= maxdepth || abs(left + right - whole) <= 15 * tol) {
      return(left + right + (left + right - whole) / 15)
    }
    rec(a, m, fa, flm, fm, left, tol / 2, depth + 1L) +
      rec(m, b, fm, frm, fb, right, tol / 2, depth + 1L)
  }
  m <- 0.5 * (a + b)
  fa <- f(a)
  fm <- f(m)
  fb <- f(b)
  rec(a, b, fa, fm, fb, simp(fa, fm, fb, a, b), tol, 0L)
}

.morie_rg_pdfint <- function(f, pdf = NULL, x = NULL,
                             lower = -Inf, upper = Inf) {
  if (!is.null(x)) {
    xs <- as.numeric(x)
    ps <- if (is.function(pdf)) {
      vapply(
        xs, function(v) as.numeric(pdf(v)),
        numeric(1)
      )
    } else {
      as.numeric(pdf)
    }
    if (length(ps) != length(xs)) {
      stop("pdf and x must have the same length")
    }
    return(.morie_rg_gridint(vapply(
      seq_along(xs),
      function(i) f(xs[i]) * ps[i],
      numeric(1)
    ), xs))
  }
  if (!is.function(pdf)) stop("give either a grid (x=) or a callable pdf")
  lo <- if (is.finite(lower)) lower else -40
  hi <- if (is.finite(upper)) upper else 40
  .morie_rg_quad(function(v) f(v) * as.numeric(pdf(v)), lo, hi)
}

.morie_rg_checkpdf <- function(mass, tol = 1e-6) {
  list(pdf_mass = as.numeric(mass), pdf_mass_ok = abs(mass - 1) <= tol)
}

# ------------------------------------------------------------- eqs 3.1-3.6

#' Eq (3.1): mu = E[eta] = integral eta p(eta) d eta
#'
#' Part of the rangayyan_ch3 implementation; see the file header for the
#' source it follows.
#'
#' @param pdf Defaults to \code{NULL}.
#' @param x Defaults to \code{NULL}.
#' @param lower Defaults to \code{-Inf}.
#' @param upper Defaults to \code{Inf}.
#' @return A vector, from \code{c}.
#' @export
PdfMean <- function(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) {
  # eq (3.1): mu = E[eta] = integral eta p(eta) d eta
  mass <- .morie_rg_pdfint(function(v) 1, pdf, x, lower, upper)
  mu <- .morie_rg_pdfint(function(v) v, pdf, x, lower, upper)
  c(
    list(mean = as.numeric(mu), method = "Rangayyan (2024) eq. (3.1)"),
    .morie_rg_checkpdf(mass)
  )
}

#' Eq (3.2): E[eta^2].  Equal to the variance only when mu = 0, so both
#'
#' are returned rather than one being assumed for the other.
#'
#' @param pdf Defaults to \code{NULL}.
#' @param x Defaults to \code{NULL}.
#' @param lower Defaults to \code{-Inf}.
#' @param upper Defaults to \code{Inf}.
#' @return A vector, from \code{c}.
#' @export
PdfMS <- function(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) {
  # eq (3.2): E[eta^2].  Equal to the variance only when mu = 0, so both
  # are returned rather than one being assumed for the other.
  mass <- .morie_rg_pdfint(function(v) 1, pdf, x, lower, upper)
  mu <- .morie_rg_pdfint(function(v) v, pdf, x, lower, upper)
  ms <- .morie_rg_pdfint(function(v) v * v, pdf, x, lower, upper)
  c(
    list(
      ms = as.numeric(ms), mean = as.numeric(mu),
      variance_from_identity = as.numeric(ms - mu * mu),
      method = "Rangayyan (2024) eq. (3.2)"
    ),
    .morie_rg_checkpdf(mass)
  )
}

#' Eq (3.3): sigma^2 = integral (eta - mu)^2 p(eta) d eta.  CV =
#' sigma/mu
#'
#' is left NULL once mu is negligible against sigma; the book warns it
#' diverges as mu -> 0.
#'
#' @param pdf Defaults to \code{NULL}.
#' @param x Defaults to \code{NULL}.
#' @param lower Defaults to \code{-Inf}.
#' @param upper Defaults to \code{Inf}.
#' @return A vector, from \code{c}.
#' @export
PdfVar <- function(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) {
  # eq (3.3): sigma^2 = integral (eta - mu)^2 p(eta) d eta.  CV = sigma/mu
  # is left NULL once mu is negligible against sigma; the book warns it
  # diverges as mu -> 0.
  mass <- .morie_rg_pdfint(function(v) 1, pdf, x, lower, upper)
  mu <- .morie_rg_pdfint(function(v) v, pdf, x, lower, upper)
  v <- .morie_rg_pdfint(function(z) (z - mu)^2, pdf, x, lower, upper)
  sd <- if (v > 0) sqrt(v) else 0
  cv <- if (abs(mu) > 1e-9 * max(sd, 1)) as.numeric(sd / mu) else NULL
  c(
    list(
      variance = as.numeric(v), sd = as.numeric(sd),
      mean = as.numeric(mu), cv = cv,
      method = "Rangayyan (2024) eq. (3.3)"
    ),
    .morie_rg_checkpdf(mass)
  )
}

#' Eq (3.4): S = (1/sigma^3) integral (eta - mu)^3 p(eta) d eta
#'
#' Part of the rangayyan_ch3 implementation; see the file header for the
#' source it follows.
#'
#' @param pdf Defaults to \code{NULL}.
#' @param x Defaults to \code{NULL}.
#' @param lower Defaults to \code{-Inf}.
#' @param upper Defaults to \code{Inf}.
#' @return A vector, from \code{c}.
#' @export
PdfSkew <- function(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) {
  # eq (3.4): S = (1/sigma^3) integral (eta - mu)^3 p(eta) d eta
  mass <- .morie_rg_pdfint(function(v) 1, pdf, x, lower, upper)
  mu <- .morie_rg_pdfint(function(v) v, pdf, x, lower, upper)
  v <- .morie_rg_pdfint(function(z) (z - mu)^2, pdf, x, lower, upper)
  sd <- sqrt(v)
  if (sd <= 0) stop("skewness is undefined for a degenerate density")
  m3 <- .morie_rg_pdfint(function(z) (z - mu)^3, pdf, x, lower, upper)
  c(
    list(
      skewness = as.numeric(m3 / sd^3), m3 = as.numeric(m3),
      sd = as.numeric(sd), mean = as.numeric(mu),
      method = "Rangayyan (2024) eq. (3.4)"
    ),
    .morie_rg_checkpdf(mass)
  )
}

#' Eq (3.5): K = (1/sigma^4) integral (eta - mu)^4 p(eta) d eta.  The
#' book
#'
#' gives 3 for a Gaussian and defines the excess K\' = K - 3.
#'
#' @param pdf Defaults to \code{NULL}.
#' @param x Defaults to \code{NULL}.
#' @param lower Defaults to \code{-Inf}.
#' @param upper Defaults to \code{Inf}.
#' @return A vector, from \code{c}.
#' @export
PdfKurt <- function(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) {
  # eq (3.5): K = (1/sigma^4) integral (eta - mu)^4 p(eta) d eta.  The book
  # gives 3 for a Gaussian and defines the excess K' = K - 3.
  mass <- .morie_rg_pdfint(function(v) 1, pdf, x, lower, upper)
  mu <- .morie_rg_pdfint(function(v) v, pdf, x, lower, upper)
  v <- .morie_rg_pdfint(function(z) (z - mu)^2, pdf, x, lower, upper)
  sd <- sqrt(v)
  if (sd <= 0) stop("kurtosis is undefined for a degenerate density")
  m4 <- .morie_rg_pdfint(function(z) (z - mu)^4, pdf, x, lower, upper)
  k <- as.numeric(m4 / sd^4)
  c(
    list(
      kurtosis = k, excess = k - 3, m4 = as.numeric(m4),
      sd = as.numeric(sd), mean = as.numeric(mu),
      method = "Rangayyan (2024) eq. (3.5)"
    ),
    .morie_rg_checkpdf(mass)
  )
}

#' Eq (3.6): H = - integral p log2(p) d eta, in bits.  p log p -> 0 as
#'
#' p -> 0, so zero-density points contribute nothing.  This is a density
#' inside the log, so unlike eq (3.11) the result may be negative.
#'
#' @param pdf Defaults to \code{NULL}.
#' @param x Defaults to \code{NULL}.
#' @param lower Defaults to \code{-Inf}.
#' @param upper Defaults to \code{Inf}.
#' @return A vector, from \code{c}.
#' @export
DiffEnt <- function(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) {
  # eq (3.6): H = - integral p log2(p) d eta, in bits.  p log p -> 0 as
  # p -> 0, so zero-density points contribute nothing.  This is a density
  # inside the log, so unlike eq (3.11) the result may be negative.
  term <- function(p) if (p <= 0) 0 else -p * log(p) / log(2)
  if (!is.null(x)) {
    xs <- as.numeric(x)
    ps <- if (is.function(pdf)) {
      vapply(
        xs, function(v) as.numeric(pdf(v)),
        numeric(1)
      )
    } else {
      as.numeric(pdf)
    }
    h <- .morie_rg_gridint(vapply(ps, term, numeric(1)), xs)
    mass <- .morie_rg_gridint(ps, xs)
  } else {
    h <- .morie_rg_pdfint(
      function(v) 1,
      function(v) term(as.numeric(pdf(v))),
      NULL, lower, upper
    )
    mass <- .morie_rg_pdfint(function(v) 1, pdf, NULL, lower, upper)
  }
  c(
    list(
      entropy = as.numeric(h), units = "bits",
      method = "Rangayyan (2024) eq. (3.6)"
    ),
    .morie_rg_checkpdf(mass)
  )
}

# ----------------------------------------------------------- eqs 3.7-3.11

#' Eq (3.7): mu = (1/N) sum eta(n) -- the DC component of the signal
#'
#' Part of the rangayyan_ch3 implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A list with \code{mean}, \code{n}, \code{method}.
#' @export
Smean <- function(x) {
  # eq (3.7): mu = (1/N) sum eta(n) -- the DC component of the signal
  xs <- .morie_rg_aslist(x)
  if (!length(xs)) stop("need at least one sample")
  list(
    mean = .morie_fsum(xs) / length(xs), n = length(xs),
    method = "Rangayyan (2024) eq. (3.7)"
  )
}

#' Eqs (3.8)-(3.10): MS, RMS, SD.  The divisor is N in all three; eq
#'
#' (3.10) is the population SD, not the N-1 unbiased one.
#'
#' @param x See Usage.
#' @return A list with \code{rms}, \code{ms}, \code{sd}, \code{mean}, \code{n}, \code{ddof}, \code{method}.
#' @export
Srms <- function(x) {
  # eqs (3.8)-(3.10): MS, RMS, SD.  The divisor is N in all three; eq
  # (3.10) is the population SD, not the N-1 unbiased one.
  xs <- .morie_rg_aslist(x)
  n <- length(xs)
  if (!n) stop("need at least one sample")
  mu <- .morie_fsum(xs) / n
  ms <- .morie_fsum(xs * xs) / n
  v <- .morie_fsum((xs - mu)^2) / n
  list(
    rms = sqrt(ms), ms = ms, sd = sqrt(v), mean = mu, n = n, ddof = 0L,
    method = "Rangayyan (2024) eqs. (3.8)-(3.10)"
  )
}

#' Eq (3.11): H = - sum p(eta_l) log2 p(eta_l), over L quantized values
#'
#' Part of the rangayyan_ch3 implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param levels Defaults to \code{NULL}.
#' @return A list with \code{entropy}, \code{units}, \code{levels}, \code{max_entropy}, \code{probabilities}, \code{method}.
#' @export
Shannon <- function(p, levels = NULL) {
  # eq (3.11): H = - sum p(eta_l) log2 p(eta_l), over L quantized values
  vals <- .morie_rg_aslist(p)
  if (!length(vals)) stop("need at least one value")
  if (!is.null(levels)) {
    lv <- as.integer(levels)
    if (lv < 1L) stop("levels must be positive")
    lo <- min(vals)
    hi <- max(vals)
    span <- hi - lo
    counts <- integer(lv)
    for (v in vals) {
      k <- if (span == 0) 1L else min(lv, as.integer((v - lo) / span * lv) + 1L)
      counts[k] <- counts[k] + 1L
    }
    probs <- counts / length(vals)
  } else {
    if (any(vals < 0)) stop("probabilities must be nonnegative")
    total <- .morie_fsum(vals)
    if (total <= 0) stop("probabilities must sum to a positive value")
    probs <- vals / total
  }
  nz <- probs[probs > 0]
  h <- -.morie_fsum(nz * log(nz) / log(2))
  lv <- length(probs)
  list(
    entropy = as.numeric(h), units = "bits", levels = lv,
    max_entropy = if (lv > 1) log(lv) / log(2) else 0,
    probabilities = probs, method = "Rangayyan (2024) eq. (3.11)"
  )
}

# ---------------------------------------------------------- eqs 3.12-3.22

#' Eqs (3.12)-(3.14).  Eq (3.14) holds only if x and eta are
#'
#' uncorrelated, so the sample correlation is measured and both the
#' observed and the additive variance are returned side by side.
#'
#' @param x See Usage.
#' @param eta See Usage.
#' @return A list with \code{y}, \code{mean_signal}, \code{mean_noise}, \code{mean_observed}, \code{mean_additive}, \code{variance_observed}, \code{variance_additive}, \code{covariance}, \code{correlation}, \code{n}, \code{method}.
#' @export
NoiseModel <- function(x, eta) {
  # eqs (3.12)-(3.14).  Eq (3.14) holds only if x and eta are
  # uncorrelated, so the sample correlation is measured and both the
  # observed and the additive variance are returned side by side.
  xs <- .morie_rg_aslist(x)
  es <- .morie_rg_aslist(eta)
  if (length(xs) != length(es)) {
    stop("signal and noise must have the same length")
  }
  n <- length(xs)
  if (!n) stop("need at least one sample")
  y <- xs + es
  mx <- .morie_fsum(xs) / n
  me <- .morie_fsum(es) / n
  vx <- .morie_fsum((xs - mx)^2) / n
  ve <- .morie_fsum((es - me)^2) / n
  my <- .morie_fsum(y) / n
  vy <- .morie_fsum((y - my)^2) / n
  cov <- .morie_fsum((xs - mx) * (es - me)) / n
  rho <- if (vx > 0 && ve > 0) cov / sqrt(vx * ve) else 0
  list(
    y = y, mean_signal = mx, mean_noise = me, mean_observed = my,
    mean_additive = mx + me, variance_observed = vy,
    variance_additive = vx + ve, covariance = cov, correlation = rho,
    n = n, method = "Rangayyan (2024) eqs. (3.12)-(3.14)"
  )
}

#' Eq (3.13): mu_y = mu_x + mu_eta.  Linearity of expectation needs no
#'
#' independence -- that is what separates it from eq (3.14).
#'
#' @param ... Passed through.
#' @return A list with \code{mean}, \code{component_means}, \code{n_processes}, \code{method}.
#' @export
MeanSum <- function(...) {
  # eq (3.13): mu_y = mu_x + mu_eta.  Linearity of expectation needs no
  # independence -- that is what separates it from eq (3.14).
  procs <- list(...)
  if (!length(procs)) stop("need at least one process")
  means <- vapply(procs, function(p) {
    v <- .morie_rg_aslist(p)
    if (!length(v)) stop("every process needs at least one sample")
    .morie_fsum(v) / length(v)
  }, numeric(1))
  list(
    mean = .morie_fsum(means), component_means = means,
    n_processes = length(means), method = "Rangayyan (2024) eq. (3.13)"
  )
}

#' Eq (3.15): mu_x(t1) = (1/M) sum_k x_k(t1)
#'
#' Part of the rangayyan_ch3 implementation; see the file header for the
#' source it follows.
#'
#' @param observations See Usage.
#' @param index Defaults to \code{NULL}.
#' @return A list with \code{mean}, \code{m}, \code{sd}, \code{se}, \code{method}.
#' @export
EnsMean <- function(observations, index = NULL) {
  # eq (3.15): mu_x(t1) = (1/M) sum_k x_k(t1)
  if (is.null(index)) {
    vals <- .morie_rg_aslist(observations)
  } else {
    i <- as.integer(index)
    vals <- vapply(observations, function(rec) {
      r <- .morie_rg_aslist(rec)
      if (i < 1L || i > length(r)) {
        stop(sprintf("index %d outside a record of length %d", i, length(r)))
      }
      r[i]
    }, numeric(1))
  }
  m <- length(vals)
  if (!m) stop("need at least one observation")
  mu <- .morie_fsum(vals) / m
  v <- .morie_fsum((vals - mu)^2) / m
  list(
    mean = mu, m = m, sd = sqrt(v), se = sqrt(v / m),
    method = "Rangayyan (2024) eq. (3.15)"
  )
}

#' Eq (3.18): x_bar(t) = (1/M) sum_k x_k(t) for all t -- the prototype
#'
#' signal, a filtered version of the M observations.
#'
#' @param observations See Usage.
#' @return A list with \code{average}, \code{sd}, \code{m}, \code{n}, \code{se}, \code{method}.
#' @export
EnsAvg <- function(observations) {
  # eq (3.18): x_bar(t) = (1/M) sum_k x_k(t) for all t -- the prototype
  # signal, a filtered version of the M observations.
  recs <- lapply(observations, .morie_rg_aslist)
  m <- length(recs)
  if (!m) stop("need at least one observation")
  n <- length(recs[[1]])
  if (!n) stop("records must be nonempty")
  if (any(vapply(recs, length, integer(1)) != n)) {
    stop("all records must have the same length")
  }
  mat <- matrix(unlist(recs), nrow = m, byrow = TRUE)
  avg <- vapply(seq_len(n), function(i) .morie_fsum(mat[, i]) / m, numeric(1))
  sd <- vapply(
    seq_len(n),
    function(i) sqrt(.morie_fsum((mat[, i] - avg[i])^2) / m),
    numeric(1)
  )
  list(
    average = avg, sd = sd, m = m, n = n, se = sd / sqrt(m),
    method = "Rangayyan (2024) eq. (3.18)"
  )
}

#' Eqs (3.21)-(3.22): C_xy and rho = C_xy / (sigma_x sigma_y)
#'
#' Part of the rangayyan_ch3 implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param ddof Defaults to \code{0}.
#' @return A list with \code{covariance}, \code{correlation}, \code{sd_x}, \code{sd_y}, \code{mean_x}, \code{mean_y}, \code{n}, \code{ddof}, \code{method}.
#' @export
CovXY <- function(x, y, ddof = 0) {
  # eqs (3.21)-(3.22): C_xy and rho = C_xy / (sigma_x sigma_y)
  xs <- .morie_rg_aslist(x)
  ys <- .morie_rg_aslist(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  n <- length(xs)
  d <- n - as.integer(ddof)
  if (d <= 0) stop(sprintf("not enough samples for ddof=%d", ddof))
  mx <- .morie_fsum(xs) / n
  my <- .morie_fsum(ys) / n
  cov <- .morie_fsum((xs - mx) * (ys - my)) / d
  vx <- .morie_fsum((xs - mx)^2) / d
  vy <- .morie_fsum((ys - my)^2) / d
  rho <- if (vx > 0 && vy > 0) cov / sqrt(vx * vy) else NULL
  list(
    covariance = cov, correlation = rho, sd_x = sqrt(vx), sd_y = sqrt(vy),
    mean_x = mx, mean_y = my, n = n, ddof = as.integer(ddof),
    method = "Rangayyan (2024) eqs. (3.21)-(3.22)"
  )
}

# ---------------------------------------------------------- eqs 3.24-3.35

#' Eq (3.24): delta(t) is undefined at t = 0 and 0 elsewhere.  A
#'
#' generalized function has no pointwise value at the origin, so NA is
#' returned there.  With width= the unit-area rectangle of Figure 3.10
#' is returned instead -- the approximating family, not the delta.
#'
#' @param t See Usage.
#' @param width Defaults to \code{NULL}.
#' @return A list with \code{delta}, \code{t}, \code{width}, \code{height}, \code{undefined_at_zero}, \code{method}.
#' @export
DiracDelta <- function(t, width = NULL) {
  # eq (3.24): delta(t) is undefined at t = 0 and 0 elsewhere.  A
  # generalized function has no pointwise value at the origin, so NA is
  # returned there.  With width= the unit-area rectangle of Figure 3.10
  # is returned instead -- the approximating family, not the delta.
  ts <- .morie_rg_aslist(t)
  if (is.null(width)) {
    v <- ifelse(ts == 0, NA_real_, 0)
    return(list(
      delta = v, t = ts, undefined_at_zero = TRUE,
      method = "Rangayyan (2024) eq. (3.24)"
    ))
  }
  w <- as.numeric(width)
  if (w <= 0) stop("width must be positive")
  h <- 1 / w
  list(
    delta = ifelse(abs(ts) <= w / 2, h, 0), t = ts, width = w, height = h,
    undefined_at_zero = FALSE,
    method = "Rangayyan (2024) eq. (3.24), rectangular approximation"
  )
}

#' Eq (3.25): integral delta(t) dt = 1.  The property defines the delta,
#'
#' so the useful computation is the check on a candidate approximation.
#'
#' @param t Defaults to \code{NULL}.
#' @param values Defaults to \code{NULL}.
#' @param width Defaults to \code{NULL}.
#' @return A list with \code{area}, \code{unit_area}, \code{method}.
#' @export
DeltaArea <- function(t = NULL, values = NULL, width = NULL) {
  # eq (3.25): integral delta(t) dt = 1.  The property defines the delta,
  # so the useful computation is the check on a candidate approximation.
  if (!is.null(values)) {
    if (is.null(t)) stop("give the grid t alongside values")
    area <- .morie_rg_gridint(values, t)
    return(list(
      area = as.numeric(area), unit_area = abs(area - 1) <= 1e-6,
      method = "Rangayyan (2024) eq. (3.25)"
    ))
  }
  if (!is.null(width)) {
    w <- as.numeric(width)
    if (w <= 0) stop("width must be positive")
    # panel edges placed on +/- w/2 so no panel straddles the jump; the
    # midpoint rule is then exact for a piecewise-constant pulse.
    span <- 2 * w
    edges <- sort(unique(c(
      seq(-span, span, length.out = 801L),
      -w / 2, w / 2
    )))
    mid <- 0.5 * (edges[-length(edges)] + edges[-1])
    area <- .morie_fsum(diff(edges) * ifelse(abs(mid) <= w / 2, 1 / w, 0))
    return(list(
      area = as.numeric(area), width = w,
      unit_area = abs(area - 1) <= 1e-9,
      method = "Rangayyan (2024) eq. (3.25)"
    ))
  }
  list(area = 1, unit_area = TRUE, method = "Rangayyan (2024) eq. (3.25)")
}

#' Eq (3.26): delta(t) = 0.5 lim_{a->0} a |t|^(a-1).  The exponent is
#'
#' negative for every a in (0,1), so the function diverges at t = 0 (NA,
#' not a large finite number).  Its integral over [-L, L] is L^a, which
#' tends to 1 as a -> 0 -- the unit-area property in the limit.
#'
#' @param t See Usage.
#' @param a See Usage.
#' @return A list with \code{values}, \code{t}, \code{a}, \code{area_symmetric}, \code{half_width}, \code{method}.
#' @export
DeltaLim <- function(t, a) {
  # eq (3.26): delta(t) = 0.5 lim_{a->0} a |t|^(a-1).  The exponent is
  # negative for every a in (0,1), so the function diverges at t = 0 (NA,
  # not a large finite number).  Its integral over [-L, L] is L^a, which
  # tends to 1 as a -> 0 -- the unit-area property in the limit.
  ts <- .morie_rg_aslist(t)
  av <- as.numeric(a)
  if (av <= 0) stop("a must be positive")
  v <- ifelse(ts == 0, NA_real_, 0.5 * av * abs(ts)^(av - 1))
  lim <- if (length(ts)) max(abs(ts)) else 1
  if (lim == 0) lim <- 1
  list(
    values = v, t = ts, a = av, area_symmetric = lim^av,
    half_width = lim, method = "Rangayyan (2024) eq. (3.26)"
  )
}

#' Eq (3.27): u(t) = 1 for t > 0, 0 otherwise.  Strict: u(0) = 0 here,
#'
#' whereas the discrete step of eq (3.35) has u(0) = 1.  They are
#' separate definitions, not one sampled from the other.
#'
#' @param t See Usage.
#' @param shift Defaults to \code{0}.
#' @return A list with \code{u}, \code{t}, \code{shift}, \code{value_at_origin}, \code{method}.
#' @export
Ustep <- function(t, shift = 0) {
  # eq (3.27): u(t) = 1 for t > 0, 0 otherwise.  Strict: u(0) = 0 here,
  # whereas the discrete step of eq (3.35) has u(0) = 1.  They are
  # separate definitions, not one sampled from the other.
  ts <- .morie_rg_aslist(t)
  s <- as.numeric(shift)
  list(
    u = ifelse(ts - s > 0, 1, 0), t = ts, shift = s,
    value_at_origin = 0, method = "Rangayyan (2024) eq. (3.27)"
  )
}

#' Eq (3.28): integral_{T1}^{T2} x(t) delta(t - to) dt = x(to) if
#'
#' T1 < to < T2, else 0.  Both inequalities are strict, so an impulse
#' sitting on a limit contributes nothing.
#'
#' @param x See Usage.
#' @param t0 See Usage.
#' @param lower See Usage.
#' @param upper See Usage.
#' @return A list with \code{value}, \code{inside}, \code{t0}, \code{lower}, \code{upper}, \code{method}.
#' @export
Sifting <- function(x, t0, lower, upper) {
  # eq (3.28): integral_{T1}^{T2} x(t) delta(t - to) dt = x(to) if
  # T1 < to < T2, else 0.  Both inequalities are strict, so an impulse
  # sitting on a limit contributes nothing.
  if (!is.function(x)) stop("x must be a function continuous at t0")
  lo <- as.numeric(lower)
  hi <- as.numeric(upper)
  tt <- as.numeric(t0)
  if (hi <= lo) stop("upper must exceed lower")
  inside <- lo < tt && tt < hi
  list(
    value = if (inside) as.numeric(x(tt)) else 0, inside = inside,
    t0 = tt, lower = lo, upper = hi,
    method = "Rangayyan (2024) eq. (3.28)"
  )
}

#' Eq (3.29): x(t) = integral x(alpha) delta(t - alpha) d alpha.  The
#'
#' weight at alpha_i is x(alpha_i) times a trapezoidal spacing, so the
#' weights sum to the integral of x rather than to the sum of samples.
#'
#' @param x See Usage.
#' @param t Defaults to \code{NULL}.
#' @return A list with \code{locations}, \code{weights}, \code{amplitudes}, \code{total_weight}, \code{integral}, \code{reconstruction_error}, \code{method}.
#' @export
DeltaDecomp <- function(x, t = NULL) {
  # eq (3.29): x(t) = integral x(alpha) delta(t - alpha) d alpha.  The
  # weight at alpha_i is x(alpha_i) times a trapezoidal spacing, so the
  # weights sum to the integral of x rather than to the sum of samples.
  xs <- .morie_rg_aslist(x)
  n <- length(xs)
  if (!n) stop("need at least one sample")
  ts <- if (is.null(t)) seq_len(n) - 1 else .morie_rg_aslist(t)
  if (length(ts) != n) stop("t and x must have the same length")
  if (n == 1L) {
    dt <- 1
  } else {
    dt <- vapply(seq_len(n), function(i) {
      lo <- if (i > 1L) ts[i] - ts[i - 1L] else 0
      hi <- if (i < n) ts[i + 1L] - ts[i] else 0
      0.5 * (lo + hi)
    }, numeric(1))
  }
  weights <- xs * dt
  recon <- ifelse(dt != 0, weights / dt, xs)
  list(
    locations = ts, weights = weights, amplitudes = xs,
    total_weight = sum(weights),
    integral = if (n > 1L) .morie_rg_gridint(xs, ts) else 0,
    reconstruction_error = max(abs(recon - xs)),
    method = "Rangayyan (2024) eq. (3.29)"
  )
}

#' Eq (3.30): y(t) = integral x(tau) h(t - tau) d tau.  Tabulated on a
#'
#' uniform grid this is the discrete convolution SCALED BY dt; dropping
#' the dt is how a continuous-time convolution comes out wrong by a
#' factor of the sampling interval.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param dt Defaults to \code{1}.
#' @param t Defaults to \code{NULL}.
#' @return A list with \code{y}, \code{t}, \code{dt}, \code{n}, \code{m}, \code{integral}, \code{method}.
#' @export
ContConv <- function(x, h, dt = 1, t = NULL) {
  # eq (3.30): y(t) = integral x(tau) h(t - tau) d tau.  Tabulated on a
  # uniform grid this is the discrete convolution SCALED BY dt; dropping
  # the dt is how a continuous-time convolution comes out wrong by a
  # factor of the sampling interval.
  xs <- .morie_rg_aslist(x)
  hs <- .morie_rg_aslist(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  step <- as.numeric(dt)
  ts <- NULL
  if (!is.null(t)) {
    ts <- .morie_rg_aslist(t)
    if (length(ts) != length(xs)) stop("t must match x in length")
    if (length(ts) > 1L) step <- ts[2] - ts[1]
  }
  if (step <= 0) stop("dt must be positive")
  n <- length(xs)
  m <- length(hs)
  y <- numeric(n + m - 1L)
  for (k in seq_len(n + m - 1L)) {
    lo <- max(1L, k - m + 1L)
    hi <- min(k, n)
    idx <- lo:hi
    y[k] <- .morie_fsum(xs[idx] * hs[k - idx + 1L]) * step
  }
  t_out <- (seq_along(y) - 1) * step
  if (!is.null(ts) && length(ts)) t_out <- ts[1] + (seq_along(y) - 1) * step
  list(
    y = y, t = t_out, dt = step, n = n, m = m,
    integral = if (length(y) > 1L) .morie_rg_gridint(y, t_out) else 0,
    method = "Rangayyan (2024) eq. (3.30)"
  )
}

#' Eq (3.31): y(t) = integral h(tau) x(t - tau) d tau, given by the book
#'
#' as an equivalent result.  Computed the other way round and compared
#' against eq (3.30) rather than asserted equivalent.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param dt Defaults to \code{1}.
#' @param t Defaults to \code{NULL}.
#' @return The value of \code{swapped}, as built in the body.
#' @export
ContConvAlt <- function(x, h, dt = 1, t = NULL) {
  # eq (3.31): y(t) = integral h(tau) x(t - tau) d tau, given by the book
  # as an equivalent result.  Computed the other way round and compared
  # against eq (3.30) rather than asserted equivalent.
  swapped <- ContConv(h, x, dt = dt, t = t)
  direct <- ContConv(x, h, dt = dt, t = t)
  diff <- max(abs(swapped$y - direct$y))
  swapped$max_difference <- diff
  swapped$commutes <- diff <= 1e-12 * max(1, max(abs(direct$y)))
  swapped$method <- "Rangayyan (2024) eq. (3.31)"
  swapped
}

#' Eq (3.34): delta(n) = 1 if n = 0, 0 otherwise.  Unlike eq (3.24) this
#'
#' is an ordinary sequence, evaluable at the origin.
#'
#' @param n See Usage.
#' @param shift Defaults to \code{0}.
#' @param amplitude Defaults to \code{1}.
#' @return A list with \code{delta}, \code{n}, \code{shift}, \code{amplitude}, \code{method}.
#' @export
KDelta <- function(n, shift = 0, amplitude = 1) {
  # eq (3.34): delta(n) = 1 if n = 0, 0 otherwise.  Unlike eq (3.24) this
  # is an ordinary sequence, evaluable at the origin.
  idx <- if (length(n) == 1L && n == as.integer(n) && n > 1L) {
    seq.int(0L, as.integer(n) - 1L)
  } else {
    as.integer(n)
  }
  s <- as.integer(shift)
  a <- as.numeric(amplitude)
  list(
    delta = ifelse(idx == s, a, 0), n = idx, shift = s, amplitude = a,
    method = "Rangayyan (2024) eq. (3.34)"
  )
}

#' Eq (3.35): u(n) = 1 for n >= 0, 0 otherwise.  Non-strict, so
#'
#' u(0) = 1 -- the opposite of eq (3.27).
#'
#' @param n See Usage.
#' @param shift Defaults to \code{0}.
#' @return A list with \code{u}, \code{n}, \code{shift}, \code{first_difference}, \code{value_at_origin}, \code{method}.
#' @export
StepSeq <- function(n, shift = 0) {
  # eq (3.35): u(n) = 1 for n >= 0, 0 otherwise.  Non-strict, so
  # u(0) = 1 -- the opposite of eq (3.27).
  idx <- if (length(n) == 1L && n == as.integer(n) && n > 1L) {
    seq.int(0L, as.integer(n) - 1L)
  } else {
    as.integer(n)
  }
  s <- as.integer(shift)
  u <- ifelse(idx - s >= 0L, 1, 0)
  d <- c(u[1], if (length(u) > 1L) diff(u) else numeric(0))
  list(
    u = u, n = idx, shift = s, first_difference = d,
    value_at_origin = 1, method = "Rangayyan (2024) eq. (3.35)"
  )
}

#' Eq (3.42): h(t) = 10 (0.25 - t), 0 <= t <= 0.25 s, at fs = 2 kHz.
#' The
#'
#' text immediately after states the output was divided by the sum of
#' all values of h(n) -- the normalization is part of the method; the
#' raw taps sum to 626.25 for the book\'s constants.
#'
#' @param x Defaults to \code{NULL}.
#' @param fs Defaults to \code{2000}.
#' @param duration Defaults to \code{0.25}.
#' @param slope Defaults to \code{10}.
#' @return The value of \code{out}, as built in the body.
#' @export
RampFilt <- function(x = NULL, fs = 2000, duration = 0.25, slope = 10) {
  # eq (3.42): h(t) = 10 (0.25 - t), 0 <= t <= 0.25 s, at fs = 2 kHz.  The
  # text immediately after states the output was divided by the sum of
  # all values of h(n) -- the normalization is part of the method; the
  # raw taps sum to 626.25 for the book's constants.
  if (fs <= 0 || duration <= 0) stop("fs and duration must be positive")
  n_taps <- as.integer(round(duration * fs)) + 1L
  h <- slope * (duration - (seq_len(n_taps) - 1) / fs)
  gain <- .morie_fsum(h)
  if (gain <= 0) stop("ramp has nonpositive total weight")
  hn <- h / gain
  out <- list(
    h = h, h_normalized = hn, gain = gain, n_taps = n_taps,
    fs = as.numeric(fs), duration = as.numeric(duration),
    method = "Rangayyan (2024) eq. (3.42)"
  )
  if (!is.null(x)) {
    xs <- .morie_rg_aslist(x)
    y <- vapply(seq_along(xs), function(k) {
      lo <- max(1L, k - n_taps + 1L)
      idx <- lo:k
      .morie_fsum(xs[idx] * hn[k - idx + 1L])
    }, numeric(1))
    out$y <- y
    out$n <- length(xs)
  }
  out
}

# pre-policy spellings
morie_ch3_pdf_mean <- PdfMean
morie_ch3_pdf_ms <- PdfMS
morie_ch3_pdf_variance <- PdfVar
morie_ch3_pdf_skewness <- PdfSkew
morie_ch3_pdf_kurtosis <- PdfKurt
morie_ch3_differential_entropy <- DiffEnt
morie_ch3_sample_mean <- Smean
morie_ch3_sample_rms <- Srms
morie_ch3_shannon_entropy <- Shannon
morie_ch3_signal_plus_noise <- NoiseModel
morie_ch3_mean_of_sum <- MeanSum
morie_ch3_ensemble_mean <- EnsMean
morie_ch3_ensemble_average <- EnsAvg
morie_ch3_covariance <- CovXY
morie_ch3_dirac_delta <- DiracDelta
morie_ch3_delta_unit_area <- DeltaArea
morie_ch3_delta_limit <- DeltaLim
morie_ch3_unit_step <- Ustep
morie_ch3_sifting <- Sifting
morie_ch3_delta_decomposition <- DeltaDecomp
morie_ch3_continuous_convolution <- ContConv
morie_ch3_continuous_convolution_alt <- ContConvAlt
morie_ch3_discrete_delta <- KDelta
morie_ch3_discrete_unit_step <- StepSeq
morie_ch3_ramp_filter <- RampFilt
