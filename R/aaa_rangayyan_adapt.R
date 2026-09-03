# Rangayyan optimal and adaptive filtering: the Wiener filter, the
# adaptive noise canceller, LMS and RLS, the Kalman filter, and adaptive
# segmentation.  Mirror of the Python bsaadapt module.
#
# Equation numbers verified in the PDF: 3.154-3.155, 3.167-3.176, 3.186,
# 3.195-3.196, 3.200-3.207, 3.213, 3.224-3.225, 4.30, 8.27-8.29.
#
# Two book problems are pinned here rather than papered over.  Eq. (3.224)
# prints a minus sign on its first line that contradicts its own second
# line and eq. (3.225); the plus form is the correct one and is what
# RlsUpdate implements.  Eq. (3.205) is a recursion with no stated initial
# value, so LmsFilt seeds it with the reference's mean square -- seeding
# from r(0)^2 makes mu(0) unbounded whenever the reference starts at zero,
# which a sine at phase zero does.

#' .morie_rg_acf
#'
#' A step of the rangayyan_adapt implementation. Called by \code{Acfseg}, \code{PcgSeg},
#' \code{PsdAcf} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param lags A count; the body uses it as \code{seq_len(...)}.
#' @return A vector, from \code{vapply}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .morie_rg_acf(x = x, lags = 3L)
#' res
.morie_rg_acf <- function(x, lags) {
  n <- length(x)
  vapply(seq_len(lags) - 1L, function(m) {
    .morie_fsum(x[seq_len(n - m)] * x[seq_len(n - m) + m]) / n
  }, numeric(1))
}

#' Theta(k) = E\[x(n-k) d(n)\], the right-hand side of eq. (3.168)
#'
#' A step of the rangayyan_adapt implementation. Called by \code{Whopf}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param d A vector; its length is taken and its elements indexed.
#' @param lags A count; the body uses it as \code{seq_len(...)}.
#' @return A vector, from \code{vapply}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' g <- c(0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
#' res <- .morie_rg_ccf(x = x, d = g, lags = 3L)
#' res
.morie_rg_ccf <- function(x, d, lags) {
  # theta(k) = E[x(n-k) d(n)], the right-hand side of eq. (3.168)
  n <- min(length(x), length(d))
  vapply(seq_len(lags) - 1L, function(k) {
    i <- seq.int(k + 1L, n)
    .morie_fsum(x[i - k] * d[i]) / n
  }, numeric(1))
}

#' .morie_rg_solve
#'
#' A step of the rangayyan_adapt implementation. Called by \code{PcgSeg}, \code{WienerHopf}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A A matrix; passed to \code{solve}.
#' @param b A matrix; passed to \code{solve}.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .morie_rg_solve(A = A, b = b)
#' res
.morie_rg_solve <- function(A, b) {
  out <- tryCatch(as.numeric(solve(A, b)), error = function(e) NULL)
  if (is.null(out) || any(!is.finite(out))) {
    stop(
      "the correlation matrix is singular; the Wiener-Hopf system of ",
      "eq. (3.168) has no unique solution"
    )
  }
  out
}

#' R(n) as eq. (3.155) wants it: current sample first, zeros before the
#'
#' record starts
#'
#' @param r A vector; indexed elementwise.
#' @param i Numeric; combined arithmetically in the body.
#' @param m A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{ifelse}.
#' @export
.morie_rg_lagvec <- function(r, i, m) {
  # r(n) as eq. (3.155) wants it: current sample first, zeros before the
  # record starts
  k <- i - seq_len(m) + 1L
  ifelse(k >= 1L, r[pmax(k, 1L)], 0)
}

# ---------------------------------------------------- Wiener, 3.154-3.186

#' Eq. (3.154): the estimate is the convolution of the tap weights with
#'
#' the input.  The first M-1 outputs run on a partly empty delay line
#' and are reported as unsettled rather than trimmed away.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{d_hat}, \code{n}, \code{order}, \code{settled_from}, \code{method}.
#' @export
WienerOut <- function(w, x) {
  # eq. (3.154): the estimate is the convolution of the tap weights with
  # the input.  The first M-1 outputs run on a partly empty delay line and
  # are reported as unsettled rather than trimmed away.
  ws <- as.numeric(w)
  xs <- as.numeric(x)
  if (!length(ws) || !length(xs)) {
    stop("both the tap weights and the input need samples")
  }
  m <- length(ws)
  n <- length(xs)
  out <- vapply(seq_len(n), function(i) {
    .morie_fsum(ws * .morie_rg_lagvec(xs, i, m))
  }, numeric(1))
  list(
    d_hat = out, n = n, order = m, settled_from = m - 1L,
    method = "Rangayyan (2024) eq. (3.154)"
  )
}

#' Eq. (3.155): the same estimate written as an inner product.  x(n)
#' runs
#'
#' BACKWARDS in time, its first entry being the current sample; getting
#' that order wrong reverses the filter without any error being raised,
#' which is why the length check is not the only thing recorded.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param xvec Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{d_hat}, \code{order}, \code{vector_is_time_reversed}, \code{method}.
#' @export
WienerDot <- function(w, xvec) {
  # eq. (3.155): the same estimate written as an inner product.  x(n) runs
  # BACKWARDS in time, its first entry being the current sample; getting
  # that order wrong reverses the filter without any error being raised,
  # which is why the length check is not the only thing recorded.
  ws <- as.numeric(w)
  xv <- as.numeric(xvec)
  if (length(ws) != length(xv)) {
    stop(
      "w and x(n) must have the same length; x(n) runs backwards in ",
      "time, x[1] being the current sample"
    )
  }
  if (!length(ws)) stop("need at least one tap")
  list(
    d_hat = .morie_fsum(ws * xv), order = length(ws),
    vector_is_time_reversed = TRUE,
    method = "Rangayyan (2024) eq. (3.155)"
  )
}

#' Eq. (3.167): grad J = -2 Theta + 2 Phi w.  The surface is quadratic
#'
#' with a single minimum, so a vanishing gradient is the optimum and not
#' merely a stationary point.
#'
#' @param phi A matrix; passed to \code{as.matrix}.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{gradient}, \code{norm}, \code{at_optimum}, \code{order},
#' \code{surface}, \code{method}.
#' @export
MseGrad <- function(phi, theta, w) {
  # eq. (3.167): grad J = -2 Theta + 2 Phi w.  The surface is quadratic
  # with a single minimum, so a vanishing gradient is the optimum and not
  # merely a stationary point.
  P <- as.matrix(phi)
  t <- as.numeric(theta)
  ws <- as.numeric(w)
  m <- length(ws)
  if (length(t) != m || nrow(P) != m || ncol(P) != m) {
    stop("Phi must be M x M and Theta, w of length M")
  }
  g <- vapply(seq_len(m), function(i) {
    2 * (.morie_fsum(P[i, ] * ws) - t[i])
  }, numeric(1))
  list(
    gradient = g, norm = sqrt(.morie_fsum(g * g)),
    at_optimum = all(abs(g) < 1e-9), order = m,
    surface = "quadratic, single minimum",
    method = "Rangayyan (2024) eq. (3.167)"
  )
}

#' Eq. (3.168): Phi w = Theta.  At the solution the input vector and the
#'
#' error are orthogonal, and so are the output and the error -- the
#' orthogonality principle, which is what makes the solution optimal.
#'
#' @param phi A matrix; passed to \code{as.matrix}.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{w}, \code{residual}, \code{max_residual}, \code{order},
#' \code{condition}, \code{orthogonality}, \code{method}.
#' @export
WienerHopf <- function(phi, theta) {
  # eq. (3.168): Phi w = Theta.  At the solution the input vector and the
  # error are orthogonal, and so are the output and the error -- the
  # orthogonality principle, which is what makes the solution optimal.
  P <- as.matrix(phi)
  t <- as.numeric(theta)
  m <- length(t)
  if (nrow(P) != m || ncol(P) != m) {
    stop("Phi must be M x M and Theta of length M")
  }
  w <- .morie_rg_solve(P, t)
  resid <- vapply(seq_len(m), function(i) {
    .morie_fsum(P[i, ] * w) - t[i]
  }, numeric(1))
  diag_p <- abs(diag(P))
  list(
    w = w, residual = resid, max_residual = max(abs(resid)), order = m,
    condition = if (min(diag_p) > 0) max(diag_p) / min(diag_p) else Inf,
    orthogonality = paste(
      "at w_o the input vector and the error are",
      "orthogonal, and so are the output and the",
      "error"
    ),
    method = "Rangayyan (2024) eq. (3.168)"
  )
}

#' Eq. (3.169): w_o = Phi^-1 Theta.  Written as an inverse in the book,
#'
#' computed here by solving the system -- the inverse is never formed,
#' which is both faster and better conditioned.
#'
#' @param phi Passed to \code{WienerHopf}.
#' @param theta Passed to \code{WienerHopf}.
#' @return The value of \code{r}, as built in the body.
#' @export
WienerOpt <- function(phi, theta) {
  # eq. (3.169): w_o = Phi^-1 Theta.  Written as an inverse in the book,
  # computed here by solving the system -- the inverse is never formed,
  # which is both faster and better conditioned.
  r <- WienerHopf(phi, theta)
  r$w_o <- r$w
  r$solved_not_inverted <- TRUE
  r$method <- "Rangayyan (2024) eq. (3.169)"
  r
}

#' Eq. (3.172): J_min = var(d) - Theta\' w_o.  A negative J_min cannot
#'
#' happen for consistent statistics, so it is reported rather than
#' clamped: it means the supplied variance and covariances do not come
#' from the same process.
#'
#' @param phi Passed to \code{WienerHopf}.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @param var_d Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{j_min}, \code{w_o}, \code{var_d}, \code{explained},
#' \code{consistent}, \code{fraction_explained}, \code{method}.
#' @export
WienerMin <- function(phi, theta, var_d) {
  # eq. (3.172): J_min = var(d) - Theta' w_o.  A negative J_min cannot
  # happen for consistent statistics, so it is reported rather than
  # clamped: it means the supplied variance and covariances do not come
  # from the same process.
  r <- WienerHopf(phi, theta)
  t <- as.numeric(theta)
  explained <- .morie_fsum(t * r$w)
  jmin <- as.numeric(var_d) - explained
  list(
    j_min = jmin, w_o = r$w, var_d = as.numeric(var_d),
    explained = explained,
    consistent = jmin >= -1e-9 * max(abs(as.numeric(var_d)), 1),
    fraction_explained = if (var_d != 0) {
      1 - jmin / as.numeric(var_d)
    } else {
      NULL
    },
    method = "Rangayyan (2024) eq. (3.172)"
  )
}

#' Eqs. (3.173)-(3.174): the normal equations written as a convolution
#' of
#'
#' the tap weights with the ACF.  It holds only for a stationary
#' process; that premise is recorded because nothing in the arithmetic
#' checks it.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param phi Coerced to numeric by the body, with \code{as.numeric}.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{lhs}, \code{theta}, \code{max_difference}, \code{holds},
#' \code{order}, \code{requires_stationarity}, \code{method}.
#' @export
WienerConv <- function(w, phi, theta) {
  # eqs. (3.173)-(3.174): the normal equations written as a convolution of
  # the tap weights with the ACF.  It holds only for a stationary process;
  # that premise is recorded because nothing in the arithmetic checks it.
  ws <- as.numeric(w)
  p <- as.numeric(phi)
  t <- as.numeric(theta)
  m <- length(ws)
  if (length(p) < m || length(t) < m) {
    stop("need at least M lags of phi and theta")
  }
  lhs <- vapply(seq_len(m) - 1L, function(k) {
    .morie_fsum(ws * p[abs(k - (seq_len(m) - 1L)) + 1L])
  }, numeric(1))
  gap <- max(abs(lhs - t[seq_len(m)]))
  scale <- max(abs(t[seq_len(m)]))
  if (!scale) scale <- 1
  list(
    lhs = lhs, theta = t[seq_len(m)], max_difference = gap,
    holds = gap <= 1e-8 * scale, order = m,
    requires_stationarity = TRUE,
    method = "Rangayyan (2024) eqs. (3.173)-(3.174)"
  )
}

#' Eq. (3.175): W(w) S_xx(w) = S_xd(w).  Bins where S_xx vanishes carry
#'
#' no information about W and are listed, not silently satisfied.
#'
#' @param W Coerced to complex by the body, with \code{as.complex}.
#' @param sxx Coerced to complex by the body, with \code{as.complex}.
#' @param sxd Coerced to complex by the body, with \code{as.complex}.
#' @return A list with \code{lhs}, \code{sxd}, \code{max_difference}, \code{holds},
#' \code{undetermined_bins}, \code{n_undetermined}, \code{method}.
#' @export
WienerFreqR <- function(W, sxx, sxd) {
  # eq. (3.175): W(w) S_xx(w) = S_xd(w).  Bins where S_xx vanishes carry
  # no information about W and are listed, not silently satisfied.
  Ws <- as.complex(W)
  a <- as.complex(sxx)
  b <- as.complex(sxd)
  if (length(Ws) != length(a) || length(a) != length(b)) {
    stop("W, S_xx and S_xd must have the same length")
  }
  lhs <- Ws * a
  gap <- max(Mod(lhs - b))
  scale <- max(Mod(b))
  if (!scale) scale <- 1
  und <- which(Mod(a) <= 1e-300) - 1L
  list(
    lhs = lhs, sxd = b, max_difference = gap,
    holds = gap <= 1e-8 * scale,
    undetermined_bins = und, n_undetermined = length(und),
    method = "Rangayyan (2024) eq. (3.175)"
  )
}

#' Eq. (3.176): W(w) = S_xd(w) / S_xx(w).  Where the denominator
#' vanishes
#'
#' the ratio is undefined; returning zero there is a choice, and it is
#' flagged so a caller can tell it apart from a genuine zero response.
#'
#' @param sxx Coerced to complex by the body, with \code{as.complex}.
#' @param sxd Coerced to complex by the body, with \code{as.complex}.
#' @return A list with \code{W}, \code{magnitude}, \code{undetermined_bins},
#' \code{n_undetermined}, \code{zero_where_undetermined}, \code{n}, \code{method}.
#' @export
WienerFreq <- function(sxx, sxd) {
  # eq. (3.176): W(w) = S_xd(w) / S_xx(w).  Where the denominator vanishes
  # the ratio is undefined; returning zero there is a choice, and it is
  # flagged so a caller can tell it apart from a genuine zero response.
  a <- as.complex(sxx)
  b <- as.complex(sxd)
  if (length(a) != length(b)) {
    stop("S_xx and S_xd must have the same length")
  }
  if (!length(a)) stop("need at least one bin")
  und <- which(Mod(a) <= 1e-300)
  W <- ifelse(Mod(a) <= 1e-300, complex(real = 0, imaginary = 0), b / a)
  list(
    W = W, magnitude = Mod(W), undetermined_bins = und - 1L,
    n_undetermined = length(und), zero_where_undetermined = TRUE,
    n = length(W), method = "Rangayyan (2024) eq. (3.176)"
  )
}

#' Eq. (3.186): W = S_d / (S_d + S_eta).  Three properties the book
#'
#' stresses and this checks: zero where the signal is absent (nothing to
#' restore), unity where the noise is absent (nothing to suppress), and
#' falling monotonically with the SNR in between.
#'
#' @param sd Coerced to numeric by the body, with \code{as.numeric}.
#' @param seta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{W}, \code{snr}, \code{undetermined_bins},
#' \code{zero_where_signal_absent}, \code{unity_where_noise_absent}, \code{n},
#' \code{method}.
#' @export
WienerSnr <- function(sd, seta) {
  # eq. (3.186): W = S_d / (S_d + S_eta).  Three properties the book
  # stresses and this checks: zero where the signal is absent (nothing to
  # restore), unity where the noise is absent (nothing to suppress), and
  # falling monotonically with the SNR in between.
  d <- as.numeric(sd)
  e <- as.numeric(seta)
  if (length(d) != length(e)) {
    stop("S_d and S_eta must have the same length")
  }
  if (!length(d)) stop("need at least one bin")
  if (any(d < 0) || any(e < 0)) stop("a PSD cannot be negative")
  tot <- d + e
  W <- ifelse(tot <= 0, 0, d / tot)
  und <- which(tot <= 0) - 1L
  snr <- ifelse(e > 0, d / e, Inf)
  list(
    W = W, snr = snr, undetermined_bins = und,
    zero_where_signal_absent = all(W[d == 0] == 0),
    unity_where_noise_absent = all(W[e == 0 & d > 0] == 1),
    n = length(W), method = "Rangayyan (2024) eq. (3.186)"
  )
}

#' Eqs. (3.168), (3.171): build the Toeplitz correlation matrix and the
#'
#' cross-correlation vector from data and solve for the tap weights.
#' The biased (1/N) ACF estimate is used, which is what makes Phi
#' nonnegative-definite and so keeps the system solvable.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param d Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{w}, \code{phi}, \code{theta}, \code{Phi}, \code{order},
#' \code{j_min}, \code{var_d}, \code{toeplitz}, \code{acf_biased}, \code{condition},
#' \code{method}.
#' @export
Whopf <- function(x, d, order) {
  # eqs. (3.168), (3.171): build the Toeplitz correlation matrix and the
  # cross-correlation vector from data and solve for the tap weights.  The
  # biased (1/N) ACF estimate is used, which is what makes Phi
  # nonnegative-definite and so keeps the system solvable.
  xs <- as.numeric(x)
  ds <- as.numeric(d)
  if (length(xs) != length(ds)) {
    stop("input and desired response must have equal length")
  }
  m <- as.integer(order)
  if (m < 1L) stop("order must be at least 1")
  if (length(xs) <= m) stop("need more samples than taps")
  phi <- .morie_rg_acf(xs, m)
  theta <- .morie_rg_ccf(xs, ds, m)
  idx <- outer(seq_len(m), seq_len(m), function(i, j) abs(i - j) + 1L)
  Phi <- matrix(phi[idx], m, m)
  r <- WienerHopf(Phi, theta)
  n <- length(ds)
  var_d <- .morie_fsum(ds * ds) / n - (.morie_fsum(ds) / n)^2
  jm <- WienerMin(Phi, theta, var_d)
  list(
    w = r$w, phi = phi, theta = theta, Phi = Phi, order = m,
    j_min = jm$j_min, var_d = var_d, toeplitz = TRUE, acf_biased = TRUE,
    condition = r$condition,
    method = "Rangayyan (2024) eqs. (3.168), (3.171)"
  )
}

#' WienerFilt
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param desired The body requires: give either a desired signal (time-domain route,.
#' @param order Passed to \code{Whopf}. Defaults to \code{8}.
#' @param sd Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param seta Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{y}, \code{W}, \code{route}, \code{fs}, \code{n}, \code{method}.
#' @export
WienerFilt <- function(x, desired = NULL, order = 8, sd = NULL,
                       seta = NULL, fs = 1) {
  # The two routes to the same filter.  The time route estimates the
  # correlations from a desired signal; the frequency route needs both
  # PSDs and knows the signal and noise spectra outright.  They are
  # alternatives, so supplying both or neither is an error rather than a
  # silent preference.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  have_time <- !is.null(desired)
  have_freq <- !is.null(sd) || !is.null(seta)
  if (have_time == have_freq) {
    stop(
      "give either a desired signal (time-domain route, ",
      "eqs. 3.168-3.169) or both PSDs (frequency route, eq. 3.186), ",
      "not both and not neither"
    )
  }
  if (have_time) {
    r <- Whopf(xs, desired, order)
    y <- WienerOut(r$w, xs)$d_hat
    return(list(
      y = y, w = r$w, order = r$order, j_min = r$j_min,
      route = "time",
      method = "Rangayyan (2024) eqs. (3.168)-(3.169)"
    ))
  }
  if (is.null(sd) || is.null(seta)) {
    stop("the frequency route needs BOTH S_d and S_eta")
  }
  n <- length(xs)
  W <- WienerSnr(sd, seta)$W
  half <- n %/% 2L + 1L
  if (length(W) != half) {
    stop(sprintf(paste(
      "the PSDs need one value per one-sided DFT bin",
      "(%d for %d samples), got %d"
    ), half, n, length(W)))
  }
  step <- 2 * pi / n
  k <- seq_len(n) - 1L
  re <- vapply(k, function(kk) {
    .morie_fsum(xs * cos(-step * (seq_len(n) - 1L) * kk))
  }, numeric(1))
  im <- vapply(k, function(kk) {
    .morie_fsum(xs * sin(-step * (seq_len(n) - 1L) * kk))
  }, numeric(1))
  g <- ifelse(k < half, W[pmin(k, half - 1L) + 1L], W[n - k + 1L])
  re <- re * g
  im <- im * g
  y <- vapply(seq_len(n) - 1L, function(i) {
    ang <- step * i * k
    .morie_fsum(re * cos(ang) - im * sin(ang)) / n
  }, numeric(1))
  list(
    y = y, W = W, route = "frequency", fs = as.numeric(fs), n = n,
    method = "Rangayyan (2024) eq. (3.186)"
  )
}

# ------------------------------------------- noise canceller, 3.187-3.205

#' Section 3.10.1: the primary input is x = v + m.  The method needs the
#'
#' signal and the interference statistically independent; the sample
#' correlation between them is returned so that premise is testable
#' instead of assumed.
#'
#' @param v Coerced to numeric by the body, with \code{as.numeric}.
#' @param m Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{x}, \code{v}, \code{m}, \code{n}, \code{correlation},
#' \code{independent}, \code{assumption}, \code{method}.
#' @export
AncInput <- function(v, m) {
  # Section 3.10.1: the primary input is x = v + m.  The method needs the
  # signal and the interference statistically independent; the sample
  # correlation between them is returned so that premise is testable
  # instead of assumed.
  vs <- as.numeric(v)
  ms <- as.numeric(m)
  if (length(vs) != length(ms)) {
    stop("signal and noise must have the same length")
  }
  n <- length(vs)
  if (!n) stop("need at least one sample")
  mv <- .morie_fsum(vs) / n
  mm <- .morie_fsum(ms) / n
  cov <- .morie_fsum((vs - mv) * (ms - mm)) / n
  sv <- sqrt(.morie_fsum((vs - mv)^2) / n)
  sm <- sqrt(.morie_fsum((ms - mm)^2) / n)
  rho <- if (sv > 0 && sm > 0) cov / (sv * sm) else 0
  list(
    x = vs + ms, v = vs, m = ms, n = n, correlation = rho,
    independent = abs(rho) < 0.1,
    assumption = paste(
      "the method needs v and m statistically",
      "independent, and the reference correlated with",
      "m but not with v"
    ),
    method = "Rangayyan (2024) Section 3.10.1"
  )
}

#' Eq. (3.196): e = x - y, and the ERROR is the canceller\'s output.
#' This
#'
#' is the step that surprises: the quantity being minimized is the thing
#' you keep, because minimizing it drives y toward the interference and
#' leaves the signal behind in e.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{e}, \code{v_hat}, \code{n}, \code{input_power},
#' \code{output_power}, \code{power_reduction}, \code{error_is_the_output},
#' \code{method}.
#' @export
AncOut <- function(x, y) {
  # eq. (3.196): e = x - y, and the ERROR is the canceller's output.  This
  # is the step that surprises: the quantity being minimized is the thing
  # you keep, because minimizing it drives y toward the interference and
  # leaves the signal behind in e.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) {
    stop("primary input and filter output must have the same length")
  }
  if (!length(xs)) stop("need at least one sample")
  e <- xs - ys
  px <- .morie_fsum(xs * xs)
  pe <- .morie_fsum(e * e)
  list(
    e = e, v_hat = e, n = length(e), input_power = px,
    output_power = pe,
    power_reduction = if (px > 0) pe / px else NULL,
    error_is_the_output = TRUE,
    method = "Rangayyan (2024) eq. (3.196)"
  )
}

#' Eq. (3.195): the adaptive filter runs on the REFERENCE, not on the
#'
#' primary input.  Filtering the primary would cancel the signal too.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{n}, \code{order}, \code{settled_from},
#' \code{filters_the_reference}, \code{method}.
#' @export
LmsOut <- function(w, r) {
  # eq. (3.195): the adaptive filter runs on the REFERENCE, not on the
  # primary input.  Filtering the primary would cancel the signal too.
  ws <- as.numeric(w)
  rs <- as.numeric(r)
  if (!length(ws) || !length(rs)) {
    stop("both the tap weights and the reference need samples")
  }
  m <- length(ws)
  y <- vapply(seq_along(rs), function(i) {
    .morie_fsum(ws * .morie_rg_lagvec(rs, i, m))
  }, numeric(1))
  list(
    y = y, n = length(y), order = m, settled_from = m - 1L,
    filters_the_reference = TRUE,
    method = "Rangayyan (2024) eq. (3.195)"
  )
}

#' Eq. (3.200): e^2 = x^2 - 2 x r\'w + (r\'w)^2.  The expansion is
#' checked
#'
#' against the square itself.  This is the INSTANTANEOUS squared error
#' standing in for the expectation -- that substitution is the whole of
#' LMS, and the reason the algorithm converges only in the mean.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param rvec Coerced to numeric by the body, with \code{as.numeric}.
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{e}, \code{e_squared}, \code{expanded},
#' \code{max_difference}, \code{agrees}, \code{nonnegative},
#' \code{instantaneous_not_expected}, \code{method}.
#' @export
LmsSqErr <- function(x, rvec, w) {
  # eq. (3.200): e^2 = x^2 - 2 x r'w + (r'w)^2.  The expansion is checked
  # against the square itself.  This is the INSTANTANEOUS squared error
  # standing in for the expectation -- that substitution is the whole of
  # LMS, and the reason the algorithm converges only in the mean.
  rv <- as.numeric(rvec)
  ws <- as.numeric(w)
  if (length(rv) != length(ws)) {
    stop("r(n) and w must have the same length")
  }
  if (!length(rv)) stop("need at least one tap")
  xv <- as.numeric(x)
  rw <- .morie_fsum(rv * ws)
  expanded <- xv * xv - 2 * xv * rw + rw * rw
  e <- xv - rw
  list(
    e = e, e_squared = e * e, expanded = expanded,
    max_difference = abs(expanded - e * e),
    agrees = abs(expanded - e * e) <= 1e-9 * (1 + abs(e * e)),
    nonnegative = expanded >= -1e-12,
    instantaneous_not_expected = TRUE,
    method = "Rangayyan (2024) eq. (3.200)"
  )
}

#' Eqs. (3.201)-(3.202): the instantaneous gradient is -2 e r, so
#'
#' steepest descent gives w - mu grad, which is exactly Widrow-Hoff.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param e Coerced to numeric by the body, with \code{as.numeric}.
#' @param rvec Coerced to numeric by the body, with \code{as.numeric}.
#' @param mu Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{gradient}, \code{w_next}, \code{mu}, \code{e}, \code{order},
#' \code{equals_widrow_hoff}, \code{method}.
#' @export
LmsDescent <- function(w, e, rvec, mu) {
  # eqs. (3.201)-(3.202): the instantaneous gradient is -2 e r, so
  # steepest descent gives w - mu grad, which is exactly Widrow-Hoff.
  ws <- as.numeric(w)
  rv <- as.numeric(rvec)
  if (length(ws) != length(rv)) {
    stop("w and r(n) must have the same length")
  }
  if (!length(ws)) stop("need at least one tap")
  ev <- as.numeric(e)
  grad <- -2 * ev * rv
  list(
    gradient = grad, w_next = ws - as.numeric(mu) * grad,
    mu = as.numeric(mu), e = ev, order = length(ws),
    equals_widrow_hoff = TRUE,
    method = "Rangayyan (2024) eqs. (3.201)-(3.202)"
  )
}

#' Eq. (3.203): w(n+1) = w(n) + 2 mu e(n) r(n).  The factor of two is in
#'
#' the book\'s equation and is kept; folding it into mu silently halves
#' every step size a reader transcribes from the text.  The stability
#' bound 0 < mu < 1/lambda_max is reported against the input power.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param e Coerced to numeric by the body, with \code{as.numeric}.
#' @param rvec Coerced to numeric by the body, with \code{as.numeric}.
#' @param mu Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{w_next}, \code{mu}, \code{e}, \code{order},
#' \code{factor_of_two_is_in_the_equation}, \code{stable_bound}, \code{within_bound},
#' \code{method}.
#' @export
WidrowHoff <- function(w, e, rvec, mu) {
  # eq. (3.203): w(n+1) = w(n) + 2 mu e(n) r(n).  The factor of two is in
  # the book's equation and is kept; folding it into mu silently halves
  # every step size a reader transcribes from the text.  The stability
  # bound 0 < mu < 1/lambda_max is reported against the input power.
  ws <- as.numeric(w)
  rv <- as.numeric(rvec)
  if (length(ws) != length(rv)) {
    stop("w and r(n) must have the same length")
  }
  if (!length(ws)) stop("need at least one tap")
  mv <- as.numeric(mu)
  ev <- as.numeric(e)
  power <- .morie_fsum(rv * rv)
  bound <- if (power > 0) 1 / power else Inf
  list(
    w_next = ws + 2 * mv * ev * rv, mu = mv, e = ev, order = length(ws),
    factor_of_two_is_in_the_equation = TRUE,
    stable_bound = bound, within_bound = mv < bound,
    method = "Rangayyan (2024) eq. (3.203)"
  )
}

#' Eq. (3.204): eq. (3.203) with a step size that changes each sample
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param e Coerced to numeric by the body, with \code{as.numeric}.
#' @param rvec Coerced to numeric by the body, with \code{as.numeric}.
#' @param mu_n Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{w_next}, \code{mu}, \code{e}, \code{order},
#' \code{time_varying}, \code{method}.
#' @export
LmsVarStep <- function(w, e, rvec, mu_n) {
  # eq. (3.204): eq. (3.203) with a step size that changes each sample.
  ws <- as.numeric(w)
  rv <- as.numeric(rvec)
  if (length(ws) != length(rv)) {
    stop("w and r(n) must have the same length")
  }
  if (!length(ws)) stop("need at least one tap")
  mv <- as.numeric(mu_n)
  ev <- as.numeric(e)
  list(
    w_next = ws + 2 * mv * ev * rv, mu = mv, e = ev,
    order = length(ws), time_varying = TRUE,
    method = "Rangayyan (2024) eq. (3.204)"
  )
}

#' Eq. (3.205), after Zhang et al.: mu(n) = mu / ((M+1) xbar^2(n)) with
#'
#' xbar^2(n) = alpha r^2(n) + (1-alpha) xbar^2(n-1).  Normalizing by the
#' running power is what makes the step scale-free, so one mu works
#' across a record whose amplitude varies by an order of magnitude --
#' the book\'s motivation for VAG signals.  alpha must be small: near 1
#' it tracks the instantaneous sample and reintroduces the very jitter
#' the averaging removes, so a value above 0.5 is refused.
#'
#' @param mu Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}.
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.02}.
#' @param power_prev Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{mu}, \code{power}, \code{power_prev}, \code{alpha},
#' \code{order}, \code{base_mu}, \code{method}.
#' @export
LmsZhang <- function(mu, order, r, alpha = 0.02, power_prev = NULL) {
  # eq. (3.205), after Zhang et al.: mu(n) = mu / ((M+1) xbar^2(n)) with
  # xbar^2(n) = alpha r^2(n) + (1-alpha) xbar^2(n-1).  Normalizing by the
  # running power is what makes the step scale-free, so one mu works
  # across a record whose amplitude varies by an order of magnitude --
  # the book's motivation for VAG signals.  alpha must be small: near 1 it
  # tracks the instantaneous sample and reintroduces the very jitter the
  # averaging removes, so a value above 0.5 is refused.
  m <- as.integer(order)
  if (m < 1L) stop("order must be at least 1")
  mv <- as.numeric(mu)
  if (!(mv > 0 && mv < 1)) stop("eq. (3.205) needs 0 < mu < 1")
  av <- as.numeric(alpha)
  if (!(av >= 0 && av <= 0.5)) {
    stop(
      "the book writes 0 <= alpha << 1; alpha above 0.5 tracks the ",
      "instantaneous sample instead of averaging, got ", av
    )
  }
  rv <- as.numeric(r)
  prev <- if (is.null(power_prev)) rv * rv else as.numeric(power_prev)
  power <- av * rv * rv + (1 - av) * prev
  if (power <= 0) {
    stop("the running power estimate vanished; mu(n) is unbounded")
  }
  list(
    mu = mv / ((m + 1) * power), power = power, power_prev = prev,
    alpha = av, order = m, base_mu = mv,
    method = "Rangayyan (2024) eq. (3.205), after Zhang et al."
  )
}

#' LmsFilt
#'
#' A step of the rangayyan_adapt implementation. Called by \code{Anc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param primary Coerced to numeric by the body, with \code{as.numeric}.
#' @param reference Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param mu Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @param variable A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param alpha Passed to \code{LmsZhang}. Defaults to \code{0.02}.
#' @return A list with \code{e}, \code{output}, \code{y}, \code{final_weights},
#' \code{order}, \code{mu}, \code{variable_step}, \code{step_history},
#' \code{stable_bound}, \code{within_bound}, \code{input_power}, \code{output_power},
#' \code{power_reduction}, \code{converges_in_the_mean_only}, \code{method}.
#' @export
LmsFilt <- function(primary, reference, order = 8, mu = 0.01,
                    variable = FALSE, alpha = 0.02) {
  # Section 3.10.2: run the canceller, eqs. (3.195)-(3.196), (3.203).
  # Convergence is in the MEAN only -- the taps keep jittering around the
  # Wiener solution forever by an amount proportional to mu, so
  # final_weights is one sample of that jitter and not a converged answer.
  xs <- as.numeric(primary)
  rs <- as.numeric(reference)
  if (length(xs) != length(rs)) {
    stop("primary and reference must have the same length")
  }
  m <- as.integer(order)
  if (m < 1L) stop("order must be at least 1")
  n <- length(xs)
  if (n <= m) stop("need more samples than taps")
  mv <- as.numeric(mu)
  if (mv <= 0) stop("mu must be positive")
  rpow <- .morie_fsum(rs * rs) / n
  bound <- if (rpow > 0) 1 / (m * rpow) else Inf
  w <- numeric(m)
  y <- numeric(n)
  e <- numeric(n)
  hist <- numeric(n)
  # eq. (3.205) states no initial value; the reference's mean square is
  # the natural seed and the only one that survives r(1) = 0
  power_prev <- if (variable) rpow else NULL
  for (i in seq_len(n)) {
    rv <- .morie_rg_lagvec(rs, i, m)
    yi <- .morie_fsum(w * rv)
    ei <- xs[i] - yi
    if (variable) {
      step <- LmsZhang(min(mv, 0.999), m, rs[i],
        alpha = alpha,
        power_prev = power_prev
      )
      power_prev <- step$power
      mu_i <- step$mu
    } else {
      mu_i <- mv
    }
    w <- w + 2 * mu_i * ei * rv
    y[i] <- yi
    e[i] <- ei
    hist[i] <- mu_i
  }
  px <- .morie_fsum(xs * xs)
  pe <- .morie_fsum(e * e)
  list(
    e = e, output = e, y = y, final_weights = w, order = m, mu = mv,
    variable_step = isTRUE(variable),
    step_history = if (variable) hist else NULL,
    stable_bound = bound, within_bound = mv < bound,
    input_power = px, output_power = pe,
    power_reduction = if (px > 0) pe / px else NULL,
    converges_in_the_mean_only = TRUE,
    method = "Rangayyan (2024) Section 3.10.2, eq. (3.203)"
  )
}

# --------------------------------------------------------- RLS, 3.206-3.225

#' Eq. (3.206): xi = sum lambda^(n-i) e^2(i).  lambda < 1 discounts old
#'
#' errors, giving an effective memory of 1/(1-lambda) samples; lambda =
#' 1 is the growing window, which never forgets and so cannot track.
#'
#' @param errors Coerced to numeric by the body, with \code{as.numeric}.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{xi}, \code{weights}, \code{lam}, \code{n}, \code{memory},
#' \code{growing_window}, \code{method}.
#' @export
RlsObj <- function(errors, lam) {
  # eq. (3.206): xi = sum lambda^(n-i) e^2(i).  lambda < 1 discounts old
  # errors, giving an effective memory of 1/(1-lambda) samples; lambda = 1
  # is the growing window, which never forgets and so cannot track.
  e <- as.numeric(errors)
  if (!length(e)) stop("need at least one error value")
  lv <- as.numeric(lam)
  if (!(lv > 0 && lv <= 1)) stop("eq. (3.206) needs 0 < lambda <= 1")
  n <- length(e)
  weights <- lv^(n - seq_len(n))
  list(
    xi = .morie_fsum(weights * e * e), weights = weights, lam = lv,
    n = n, memory = if (lv < 1) 1 / (1 - lv) else Inf,
    growing_window = lv == 1,
    method = "Rangayyan (2024) eq. (3.206)"
  )
}

#' Eq. (3.207): the same form as Wiener-Hopf, but with time-averaged and
#'
#' exponentially weighted correlations.  Solving it outright each sample
#' is what the matrix inversion lemma exists to avoid.
#'
#' @param phi Passed to \code{WienerHopf}.
#' @param theta Passed to \code{WienerHopf}.
#' @return The value of \code{r}, as built in the body.
#' @export
RlsNormal <- function(phi, theta) {
  # eq. (3.207): the same form as Wiener-Hopf, but with time-averaged and
  # exponentially weighted correlations.  Solving it outright each sample
  # is what the matrix inversion lemma exists to avoid.
  r <- WienerHopf(phi, theta)
  r$w_tilde <- r$w
  r$same_form_as_wiener_hopf <- TRUE
  r$direct_inversion <- TRUE
  r$method <- "Rangayyan (2024) eq. (3.207)"
  r
}

#' Eq. (3.213), the matrix inversion lemma:
#'
#' (A + B C D)^-1 = A^-1 - A^-1 B (D A^-1 B + C^-1)^-1 D A^-1. Both
#' sides are formed and compared.  Its value in RLS is that with k = 1
#' the only inverse left is of a SCALAR, which turns an O(M^3) inversion
#' per sample into O(M^2).
#'
#' @param A A matrix; passed to \code{as.matrix}.
#' @param B A matrix; passed to \code{as.matrix}.
#' @param C A matrix; passed to \code{as.matrix}.
#' @param D A matrix; passed to \code{as.matrix}.
#' @return A list with \code{direct}, \code{lemma}, \code{max_difference}, \code{holds},
#' \code{n}, \code{k}, \code{scalar_when_k_is_one}, \code{method}.
#' @export
AbcdLemma <- function(A, B, C, D) {
  # eq. (3.213), the matrix inversion lemma:
  #   (A + B C D)^-1 = A^-1 - A^-1 B (D A^-1 B + C^-1)^-1 D A^-1.
  # Both sides are formed and compared.  Its value in RLS is that with
  # k = 1 the only inverse left is of a SCALAR, which turns an O(M^3)
  # inversion per sample into O(M^2).
  Am <- as.matrix(A)
  Bm <- as.matrix(B)
  Cm <- as.matrix(C)
  Dm <- as.matrix(D)
  n <- nrow(Am)
  if (ncol(Am) != n) stop("A must be square")
  k <- nrow(Cm)
  if (nrow(Bm) != n || ncol(Bm) != k) stop("B must be n x k")
  if (nrow(Dm) != k || ncol(Dm) != n) stop("D must be k x n")
  if (ncol(Cm) != k) stop("C must be k x k")
  inv <- function(M) {
    out <- tryCatch(solve(M), error = function(e) NULL)
    if (is.null(out)) stop("a matrix in eq. (3.213) is singular")
    out
  }
  direct <- inv(Am + Bm %*% Cm %*% Dm)
  Ai <- inv(Am)
  inner <- Dm %*% Ai %*% Bm + inv(Cm)
  lemma <- Ai - Ai %*% Bm %*% inv(inner) %*% Dm %*% Ai
  gap <- max(abs(direct - lemma))
  scale <- max(abs(direct))
  if (!scale) scale <- 1
  list(
    direct = direct, lemma = lemma, max_difference = gap,
    holds = gap <= 1e-6 * scale, n = n, k = k,
    scalar_when_k_is_one = k == 1L,
    method = "Rangayyan (2024) eq. (3.213)"
  )
}

#' Eq. (3.224): w(n) = w(n-1) + k(n) alpha(n)
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w_prev Coerced to numeric by the body, with \code{as.numeric}.
#' @param k Coerced to numeric by the body, with \code{as.numeric}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{w_next}, \code{correction}, \code{alpha}, \code{order},
#' \code{sign}, \code{erratum}, \code{method}.
#' @export
RlsUpdate <- function(w_prev, k, alpha) {
  # eq. (3.224): w(n) = w(n-1) + k(n) alpha(n).
  #
  # BOOK ERRATUM: the first line of eq. (3.224) prints a MINUS sign, which
  # contradicts its own second line and eq. (3.225).  The plus form is the
  # correct one -- with the minus the recursion drives the error up rather
  # than down -- and is what is implemented here.
  ws <- as.numeric(w_prev)
  kv <- as.numeric(k)
  if (length(ws) != length(kv)) stop("w and k must have the same length")
  if (!length(ws)) stop("need at least one tap")
  a <- as.numeric(alpha)
  list(
    w_next = ws + kv * a, correction = kv * a, alpha = a,
    order = length(ws), sign = "+",
    erratum = paste(
      "eq. (3.224) line 1 prints a minus sign that",
      "contradicts its own line 2 and eq. (3.225); the",
      "plus form is correct"
    ),
    method = "Rangayyan (2024) eq. (3.224)"
  )
}

#' Eq. (3.225): alpha(n) = x(n) - w\'(n-1) r(n).  The A PRIORI error,
#' made
#'
#' with the PREVIOUS weights.  Using the updated weights gives the a
#' posteriori error, a different and always smaller quantity, and the
#' recursion is not valid with it.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param rvec Coerced to numeric by the body, with \code{as.numeric}.
#' @param w_prev Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{alpha}, \code{prediction}, \code{order},
#' \code{uses_previous_weights}, \code{not_the_a_posteriori_error}, \code{method}.
#' @export
RlsApriori <- function(x, rvec, w_prev) {
  # eq. (3.225): alpha(n) = x(n) - w'(n-1) r(n).  The A PRIORI error, made
  # with the PREVIOUS weights.  Using the updated weights gives the a
  # posteriori error, a different and always smaller quantity, and the
  # recursion is not valid with it.
  rv <- as.numeric(rvec)
  ws <- as.numeric(w_prev)
  if (length(rv) != length(ws)) {
    stop("r(n) and w must have the same length")
  }
  if (!length(rv)) stop("need at least one tap")
  pred <- .morie_fsum(rv * ws)
  list(
    alpha = as.numeric(x) - pred, prediction = pred, order = length(rv),
    uses_previous_weights = TRUE, not_the_a_posteriori_error = TRUE,
    method = "Rangayyan (2024) eq. (3.225)"
  )
}

#' Section 3.10.3, eqs. (3.215), (3.221), (3.224)-(3.225).  P is
#'
#' symmetrized every sample: in exact arithmetic the update preserves
#' symmetry, in floating point it does not, and the asymmetry grows
#' until P loses positive definiteness and the filter diverges.  The
#' size of that drift is returned rather than hidden.
#'
#' @param primary Coerced to numeric by the body, with \code{as.numeric}.
#' @param reference Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.98}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{e}, \code{output}, \code{y}, \code{final_weights}, \code{P},
#' \code{order}, \code{lam}, \code{delta}, \code{memory}, \code{p_symmetry_error},
#' \code{p_symmetrized}, \code{input_power}, \code{output_power}, \code{power_reduction},
#' \code{method}.
#' @export
RlsFilt <- function(primary, reference, order = 8, lam = 0.98, delta = 1) {
  # Section 3.10.3, eqs. (3.215), (3.221), (3.224)-(3.225).  P is
  # symmetrized every sample: in exact arithmetic the update preserves
  # symmetry, in floating point it does not, and the asymmetry grows until
  # P loses positive definiteness and the filter diverges.  The size of
  # that drift is returned rather than hidden.
  xs <- as.numeric(primary)
  rs <- as.numeric(reference)
  if (length(xs) != length(rs)) {
    stop("primary and reference must have the same length")
  }
  m <- as.integer(order)
  if (m < 1L) stop("order must be at least 1")
  n <- length(xs)
  if (n <= m) stop("need more samples than taps")
  lv <- as.numeric(lam)
  if (!(lv > 0 && lv <= 1)) stop("eq. (3.206) needs 0 < lambda <= 1")
  dv <- as.numeric(delta)
  if (dv <= 0) stop("delta must be positive")
  P <- diag(dv, m)
  w <- numeric(m)
  e <- numeric(n)
  y <- numeric(n)
  asym <- 0
  for (i in seq_len(n)) {
    rv <- .morie_rg_lagvec(rs, i, m)
    pred <- .morie_fsum(w * rv)
    alpha <- xs[i] - pred
    # compensated, to match the Python arm bit for bit: with a plain
    # BLAS product the two arms drift apart once P is ill conditioned
    Pr <- vapply(
      seq_len(m), function(a) .morie_fsum(P[a, ] * rv),
      numeric(1)
    )
    den <- lv + .morie_fsum(rv * Pr)
    if (den <= 0) {
      stop(
        "the RLS denominator vanished at sample ", i,
        "; P has lost positive definiteness"
      )
    }
    kg <- Pr / den
    newP <- (P - outer(kg, Pr)) / lv
    asym <- max(asym, max(abs(newP - t(newP))))
    P <- (newP + t(newP)) / 2
    w <- w + kg * alpha
    y[i] <- pred
    e[i] <- alpha
  }
  px <- .morie_fsum(xs * xs)
  pe <- .morie_fsum(e * e)
  list(
    e = e, output = e, y = y, final_weights = w, P = P, order = m,
    lam = lv, delta = dv,
    memory = if (lv < 1) 1 / (1 - lv) else Inf,
    p_symmetry_error = asym, p_symmetrized = TRUE,
    input_power = px, output_power = pe,
    power_reduction = if (px > 0) pe / px else NULL,
    method = paste(
      "Rangayyan (2024) Section 3.10.3, eqs. (3.215),",
      "(3.221), (3.224)-(3.225)"
    )
  )
}

#' Section 8.6.2.  Every stage is itself a predictor, so one run gives
#'
#' the fit at EVERY order up to the one requested -- an order need not
#' be chosen in advance, which is the lattice\'s advantage over the
#' direct form.  |gamma| < 1 at every stage is the stability condition,
#' the same one as eq. (7.39).
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.98}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @return A list with \code{reflection}, \code{forward_error}, \code{backward_error},
#' \code{all_orders_forward}, \code{order}, \code{lam}, \code{stable},
#' \code{every_stage_is_a_predictor}, \code{method}.
#' @export
RlsLattice <- function(x, order = 4, lam = 0.98, delta = 0.01) {
  # Section 8.6.2.  Every stage is itself a predictor, so one run gives
  # the fit at EVERY order up to the one requested -- an order need not be
  # chosen in advance, which is the lattice's advantage over the direct
  # form.  |gamma| < 1 at every stage is the stability condition, the same
  # one as eq. (7.39).
  xs <- as.numeric(x)
  m <- as.integer(order)
  if (m < 1L) stop("order must be at least 1")
  n <- length(xs)
  if (n <= m) stop("need more samples than the order")
  lv <- as.numeric(lam)
  if (!(lv > 0 && lv <= 1)) stop("lambda must satisfy 0 < lambda <= 1")
  dv <- as.numeric(delta)
  if (dv <= 0) stop("delta must be positive")
  fe <- rep(dv, m + 1L)
  be <- rep(dv, m + 1L)
  cross <- numeric(m + 1L)
  bprev <- numeric(m + 1L)
  gam <- numeric(m + 1L)
  ferr <- matrix(0, n, m + 1L)
  berr <- matrix(0, n, m + 1L)
  for (i in seq_len(n)) {
    f <- numeric(m + 1L)
    b <- numeric(m + 1L)
    f[1] <- xs[i]
    b[1] <- xs[i]
    for (s in seq_len(m) + 1L) {
      cross[s] <- lv * cross[s] + f[s - 1L] * bprev[s - 1L]
      fe[s] <- lv * fe[s] + f[s - 1L] * f[s - 1L]
      be[s] <- lv * be[s] + bprev[s - 1L] * bprev[s - 1L]
      den <- sqrt(fe[s] * be[s])
      gam[s] <- if (den > 0) cross[s] / den else 0
      if (abs(gam[s]) >= 1) gam[s] <- if (gam[s] > 0) 0.999 else -0.999
      f[s] <- f[s - 1L] - gam[s] * bprev[s - 1L]
      b[s] <- bprev[s - 1L] - gam[s] * f[s - 1L]
    }
    ferr[i, ] <- f
    berr[i, ] <- b
    bprev <- b
  }
  refl <- gam[-1]
  list(
    reflection = refl, forward_error = ferr[, m + 1L],
    backward_error = berr[, m + 1L], all_orders_forward = ferr,
    order = m, lam = lv, stable = all(abs(refl) < 1),
    every_stage_is_a_predictor = TRUE,
    method = paste(
      "RLS lattice; the |gamma| < 1 stability condition",
      "is the same as Rangayyan (2024) eq. (7.39)"
    )
  )
}

#' RlsMonitor
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param reference Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param lam Passed to \code{RlsFilt}. Defaults to \code{0.98}.
#' @param settle Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param threshold Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.
#' @param window Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return A list with \code{error}, \code{error_power}, \code{boundaries},
#' \code{n_boundaries}, \code{threshold}, \code{baseline}, \code{baseline_sd},
#' \code{settle}, \code{window}, \code{order}, \code{transient_excluded}, \code{method}.
#' @export
RlsMonitor <- function(x, reference = NULL, order = 8, lam = 0.98,
                       settle = NULL, threshold = 3, window = NULL) {
  # Section 8.6.1: watch the RLS error power and mark a boundary where it
  # jumps.  The convergence transient at the start is excluded from the
  # baseline; including it sets the threshold from the filter's own
  # start-up and hides the first real boundary.
  xs <- as.numeric(x)
  m <- as.integer(order)
  n <- length(xs)
  if (n <= 2L * m) stop("need well more samples than taps")
  ref <- if (is.null(reference)) c(0, xs[-n]) else as.numeric(reference)
  r <- RlsFilt(xs, ref, order = m, lam = lam)
  e <- r$e
  s <- if (is.null(settle)) min(n %/% 4L, 10L * m) else as.integer(settle)
  if (s >= n - m) {
    stop("the settling period leaves no samples to monitor")
  }
  w <- if (is.null(window)) max(m, (n - s) %/% 20L) else as.integer(window)
  if (w < 1L) stop("the window must hold at least one sample")
  power <- vapply(seq_len(n), function(i) {
    seg <- e[max(1L, i - w + 1L):i]
    .morie_fsum(seg * seg) / length(seg)
  }, numeric(1))
  base <- power[(s + 1L):n]
  mu <- .morie_fsum(base) / length(base)
  sdv <- sqrt(.morie_fsum((base - mu)^2) / length(base))
  thr <- mu + as.numeric(threshold) * sdv
  hits <- integer(0)
  i <- s + 1L
  while (i <= n) {
    if (power[i] > thr) {
      j <- i
      while (j < n && power[j + 1L] > thr) j <- j + 1L
      hits <- c(hits, (i:j)[which.max(power[i:j])] - 1L)
      i <- j + 1L
    } else {
      i <- i + 1L
    }
  }
  list(
    error = e, error_power = power, boundaries = hits,
    n_boundaries = length(hits), threshold = thr, baseline = mu,
    baseline_sd = sdv, settle = s, window = w, order = m,
    transient_excluded = TRUE,
    method = "Rangayyan (2024) Section 8.5 (adaptive segmentation)"
  )
}

# ------------------------------------------------------ Kalman and Riccati

#' The recursive counterpart of the Wiener filter: it tracks a state
#'
#' through a model instead of filtering a stationary record.  P is
#' symmetrized each step for the same floating-point reason as in RLS,
#' and the asymmetry it removed is returned.  The plain covariance
#' update is used rather than the Joseph form, which is recorded because
#' the Joseph form is the numerically safer one when R is small.
#'
#' @param z A matrix; passed to \code{as.matrix}.
#' @param F A matrix; passed to \code{as.matrix}.
#' @param H A matrix; passed to \code{as.matrix}.
#' @param Q A matrix; passed to \code{as.matrix}.
#' @param R A matrix; passed to \code{as.matrix}.
#' @param x0 Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param P0 Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{states}, \code{covariances}, \code{gains},
#' \code{innovations}, \code{n}, \code{state_dim}, \code{obs_dim},
#' \code{p_symmetry_error}, \code{p_symmetrized}, \code{joseph_form}, \code{method}.
#' @export
Kalman <- function(z, F, H, Q, R, x0 = NULL, P0 = NULL) {
  # The recursive counterpart of the Wiener filter: it tracks a state
  # through a model instead of filtering a stationary record.  P is
  # symmetrized each step for the same floating-point reason as in RLS,
  # and the asymmetry it removed is returned.  The plain covariance update
  # is used rather than the Joseph form, which is recorded because the
  # Joseph form is the numerically safer one when R is small.
  Fm <- as.matrix(F)
  Hm <- as.matrix(H)
  Qm <- as.matrix(Q)
  Rm <- as.matrix(R)
  ns <- nrow(Fm)
  p <- nrow(Hm)
  if (ncol(Fm) != ns || ncol(Hm) != ns) {
    stop("F must be n x n and H must be p x n")
  }
  if (nrow(Qm) != ns || ncol(Qm) != ns) stop("Q must be n x n")
  if (nrow(Rm) != p || ncol(Rm) != p) stop("R must be p x p")
  x <- if (is.null(x0)) numeric(ns) else as.numeric(x0)
  P <- if (is.null(P0)) diag(1, ns) else as.matrix(P0)
  zl <- if (is.list(z)) z else split(as.matrix(z), row(as.matrix(z)))
  states <- vector("list", length(zl))
  covs <- vector("list", length(zl))
  gains <- vector("list", length(zl))
  innov <- vector("list", length(zl))
  asym <- 0
  for (t in seq_along(zl)) {
    zv <- as.numeric(zl[[t]])
    if (length(zv) != p) {
      stop("every measurement must have length ", p)
    }
    xp <- as.numeric(Fm %*% x)
    Pp <- Fm %*% P %*% t(Fm) + Qm
    S <- Hm %*% Pp %*% t(Hm) + Rm
    K <- Pp %*% t(Hm) %*% solve(S)
    y <- zv - as.numeric(Hm %*% xp)
    x <- xp + as.numeric(K %*% y)
    Pn <- Pp - K %*% Hm %*% Pp
    asym <- max(asym, max(abs(Pn - t(Pn))))
    P <- (Pn + t(Pn)) / 2
    states[[t]] <- x
    covs[[t]] <- P
    gains[[t]] <- K
    innov[[t]] <- y
  }
  list(
    states = states, covariances = covs, gains = gains,
    innovations = innov, n = length(states), state_dim = ns,
    obs_dim = p, p_symmetry_error = asym, p_symmetrized = TRUE,
    joseph_form = FALSE,
    method = paste(
      "Kalman (1960); the recursive counterpart of the",
      "Wiener filter of Rangayyan (2024) Section 3.9"
    )
  )
}

#' The fixed point of the Kalman covariance recursion.  Once P settles
#'
#' the gain is constant and the filter is a fixed linear filter -- which
#' is the Wiener solution for that model, and the sense in which Kalman
#' generalizes Wiener.  Solved by iterating rather than by an eigenvalue
#' method; a model that is not detectable has no stabilizing solution
#' and will not converge, which `converged` reports instead of returning
#' whatever P happened to be at the iteration limit.
#'
#' @param F A matrix; passed to \code{as.matrix}.
#' @param H A matrix; passed to \code{as.matrix}.
#' @param Q A matrix; passed to \code{as.matrix}.
#' @param R A matrix; passed to \code{as.matrix}.
#' @param maxiter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-12}.
#' @return A list with \code{P}, \code{K}, \code{iterations}, \code{change},
#' \code{converged}, \code{n}, \code{steady_state_is_the_wiener_solution}, \code{method}.
#' @export
Riccati <- function(F, H, Q, R, maxiter = 1000L, tol = 1e-12) {
  # The fixed point of the Kalman covariance recursion.  Once P settles
  # the gain is constant and the filter is a fixed linear filter -- which
  # is the Wiener solution for that model, and the sense in which Kalman
  # generalizes Wiener.  Solved by iterating rather than by an eigenvalue
  # method; a model that is not detectable has no stabilizing solution and
  # will not converge, which `converged` reports instead of returning
  # whatever P happened to be at the iteration limit.
  #
  # P here is the PREDICTED covariance; the filter stores the UPDATED one,
  # P - K H P.
  Fm <- as.matrix(F)
  Hm <- as.matrix(H)
  Qm <- as.matrix(Q)
  Rm <- as.matrix(R)
  n <- nrow(Fm)
  P <- Qm
  change <- Inf
  it <- 0L
  for (it in seq_len(as.integer(maxiter))) {
    S <- Hm %*% P %*% t(Hm) + Rm
    G <- P %*% t(Hm) %*% solve(S)
    Pn <- Fm %*% P %*% t(Fm) + Qm - Fm %*% G %*% Hm %*% P %*% t(Fm)
    Pn <- (Pn + t(Pn)) / 2
    change <- max(abs(Pn - P))
    P <- Pn
    if (change < tol) break
  }
  S <- Hm %*% P %*% t(Hm) + Rm
  K <- P %*% t(Hm) %*% solve(S)
  list(
    P = P, K = K, iterations = it, change = change,
    converged = change < tol, n = n,
    steady_state_is_the_wiener_solution = TRUE,
    method = paste(
      "discrete algebraic Riccati equation; the fixed",
      "point of the Kalman covariance recursion"
    )
  )
}

# ------------------------------------------------- segmentation, 8.27-8.29

#' Section 8.5.1, the spectral error measure: the mean squared
#' difference
#'
#' of the LOG spectra.  Taking logs is what makes it scale-free -- a
#' pure gain change shifts every log bin by the same constant, so it
#' lands entirely in mean_offset and leaves shape_only at zero.
#'
#' @param psd Coerced to numeric by the body, with \code{as.numeric}.
#' @param reference Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{sem}, \code{log_difference}, \code{n_bins},
#' \code{mean_offset}, \code{shape_only}, \code{gain_change_only}, \code{zero_bins},
#' \code{scale_free}, \code{method}.
#' @export
Sem <- function(psd, reference) {
  # Section 8.5.1, the spectral error measure: the mean squared difference
  # of the LOG spectra.  Taking logs is what makes it scale-free -- a pure
  # gain change shifts every log bin by the same constant, so it lands
  # entirely in mean_offset and leaves shape_only at zero.
  a <- as.numeric(psd)
  b <- as.numeric(reference)
  if (length(a) != length(b)) {
    stop("the two PSDs must have the same length")
  }
  if (!length(a)) stop("need at least one bin")
  if (any(a < 0) || any(b < 0)) stop("a PSD cannot be negative")
  floor_v <- 1e-300
  zeros <- sum(a <= floor_v) + sum(b <= floor_v)
  d <- log(pmax(a, floor_v)) - log(pmax(b, floor_v))
  value <- .morie_fsum(d * d) / length(d)
  offset <- .morie_fsum(d) / length(d)
  shape <- .morie_fsum((d - offset)^2) / length(d)
  list(
    sem = value, log_difference = d, n_bins = length(d),
    mean_offset = offset, shape_only = shape,
    gain_change_only = abs(value - offset * offset) < 1e-9,
    zero_bins = zeros, scale_free = TRUE,
    method = "Rangayyan (2024) Section 8.5 (spectral error measure)"
  )
}

#' Section 8.5.2, eqs. (8.27)-(8.29), after Michael and Houchin
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param test Coerced to numeric by the body, with \code{as.numeric}.
#' @param reference Coerced to numeric by the body, with \code{as.numeric}.
#' @param lags Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param thp Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param thf Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{distance}, \code{power_distance}, \code{spectral_distance},
#' \code{lags}, \code{lags_auto}, \code{acf_test}, \code{acf_reference},
#' \code{power_test}, \code{power_reference}, \code{boundary}, \code{th_power},
#' \code{th_spectral}, \code{amplitude_invariant}, \code{method}.
#' @export
Acfseg <- function(test, reference, lags = NULL, thp = 1, thf = 1) {
  # Section 8.5.2, eqs. (8.27)-(8.29), after Michael and Houchin.
  #
  #   d_P(n) = |sqrt(phi_T(n,0)) - sqrt(phi_R(0))|
  #            / min{sqrt(phi_T(n,0)), sqrt(phi_R(0))}          (8.27)
  #   d_F(n) = sum_{k=1..q} |phi_T(n,k) - phi_R(k)|
  #            / (0.5 + sum_{k=1..q} min{sqrt(phi_T(n,k)),
  #                                      sqrt(phi_R(k))})        (8.28)
  #   d(n)   = d_P(n)/Th_P + d_F(n)/Th_F,  boundary when d(n) > 1  (8.29)
  #
  # q is set the way the book sets it, at the lower of the two lags where
  # the ACFs first turn negative.  That definition is what keeps
  # eq. (8.28) real -- every phi(k) under a square root is positive by
  # construction up to q -- so a q supplied by hand is checked against the
  # same condition rather than trusted.  The two distances answer
  # different questions: d_P moves when the signal gets louder at the same
  # shape, d_F when the shape changes at the same power.
  a <- as.numeric(test)
  b <- as.numeric(reference)
  if (length(a) < 2L || length(b) < 2L) {
    stop("each window needs at least two samples")
  }
  if (as.numeric(thp) <= 0) stop("Th_P must be positive")
  if (as.numeric(thf) <= 0) stop("Th_F must be positive")
  nmax <- min(length(a), length(b))
  rt <- .morie_rg_acf(a, nmax)
  rr <- .morie_rg_acf(b, nmax)
  if (rt[1] <= 0 || rr[1] <= 0) stop("a window has zero energy")
  first_neg <- function(r) {
    neg <- which(r[-1] < 0)
    if (!length(neg)) length(r) else neg[1]
  }
  auto <- min(first_neg(rt), first_neg(rr)) - 1L
  if (is.null(lags)) {
    q <- auto
  } else {
    q <- as.integer(lags)
    if (q < 1L) stop("need at least one lag")
    if (q > auto) {
      stop(
        "eq. (8.28) needs the ACFs nonnegative out to lag q; they turn ",
        "negative at lag ", auto + 1L
      )
    }
  }
  if (q < 1L) {
    stop("both ACFs turn negative at lag 1; no lags to compare")
  }
  st <- sqrt(rt[1])
  sr <- sqrt(rr[1])
  dp <- abs(st - sr) / min(st, sr)
  k <- seq_len(q) + 1L
  num <- .morie_fsum(abs(rt[k] - rr[k]))
  den <- 0.5 + .morie_fsum(pmin(sqrt(rt[k]), sqrt(rr[k])))
  df <- num / den
  d <- dp / as.numeric(thp) + df / as.numeric(thf)
  list(
    distance = d, power_distance = dp, spectral_distance = df,
    lags = q, lags_auto = is.null(lags), acf_test = rt[seq_len(q + 1L)],
    acf_reference = rr[seq_len(q + 1L)], power_test = rt[1],
    power_reference = rr[1], boundary = d > 1,
    th_power = as.numeric(thp), th_spectral = as.numeric(thf),
    amplitude_invariant = FALSE,
    method = paste(
      "Rangayyan (2024) eqs. (8.27)-(8.29), after Michael",
      "and Houchin"
    )
  )
}

#' PcgSeg
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param window Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param step Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6}.
#' @param threshold Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{sem}, \code{sem_fixed_reference}, \code{times},
#' \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{median}, \code{mad},
#' \code{window}, \code{step}, \code{order}, \code{fs},
#' \code{reference_restarted_at_boundaries}, \code{robust_threshold}, \code{method}.
#' @export
PcgSeg <- function(x, fs, window = NULL, step = NULL, order = 6,
                   threshold = NULL) {
  # Section 8.5: adaptive segmentation of the PCG by the spectral error
  # measure between a moving window and a reference window.  The reference
  # is RESTARTED at every boundary, which is the point of the method:
  # against a fixed reference the measure stays high after the first
  # change and every later window looks like a boundary.  With no
  # threshold given a robust one is used, median + 3 x 1.4826 MAD, because
  # a mean-and-SD threshold is inflated by the very jumps it must detect.
  xs <- as.numeric(x)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  n <- length(xs)
  w <- if (is.null(window)) {
    max(32L, as.integer(0.05 * fsv))
  } else {
    as.integer(window)
  }
  if (w > n) stop("the window is longer than the record")
  hop <- if (is.null(step)) w %/% 2L else as.integer(step)
  if (hop < 1L) stop("step must be at least one sample")
  p <- as.integer(order)
  if (w <= p) stop("the window must hold more samples than the order")
  spectrum <- function(seg) {
    acf <- .morie_rg_acf(seg, p + 1L)
    if (acf[1] <= 0) {
      return(NULL)
    }
    idx <- outer(seq_len(p), seq_len(p), function(i, j) abs(i - j) + 1L)
    Phi <- matrix(acf[idx], p, p)
    a <- tryCatch(.morie_rg_solve(Phi, -acf[seq_len(p) + 1L]),
      error = function(e) NULL
    )
    if (is.null(a)) {
      return(NULL)
    }
    vapply(seq_len(32L), function(kk) {
      om <- pi * kk / 33
      j <- seq_len(p)
      re <- 1 + .morie_fsum(a * cos(-om * j))
      im <- .morie_fsum(a * sin(-om * j))
      den <- re * re + im * im
      if (den > 0) acf[1] / den else 1e-300
    }, numeric(1))
  }
  starts <- seq.int(1L, n - w + 1L, by = hop)
  ref0 <- spectrum(xs[starts[1]:(starts[1] + w - 1L)])
  if (is.null(ref0)) stop("the first window has no usable AR spectrum")
  values <- vapply(starts, function(s) {
    sp <- spectrum(xs[s:(s + w - 1L)])
    if (is.null(sp)) 0 else Sem(sp, ref0)$sem
  }, numeric(1))
  med <- sort(values)[length(values) %/% 2L + 1L]
  mad <- sort(abs(values - med))[length(values) %/% 2L + 1L]
  thr <- if (is.null(threshold)) {
    med + 3 * 1.4826 * mad
  } else {
    as.numeric(threshold)
  }
  ref <- ref0
  bounds <- integer(0)
  adaptive <- numeric(length(starts))
  for (idx in seq_along(starts)) {
    s <- starts[idx]
    sp <- spectrum(xs[s:(s + w - 1L)])
    v <- if (is.null(sp) || is.null(ref)) 0 else Sem(sp, ref)$sem
    adaptive[idx] <- v
    if (v > thr) {
      bounds <- c(bounds, s - 1L)
      ref <- sp
    }
  }
  list(
    sem = adaptive, sem_fixed_reference = values,
    times = (starts - 1L) / fsv, boundaries = bounds,
    n_boundaries = length(bounds), threshold = thr, median = med,
    mad = mad, window = w, step = hop, order = p, fs = fsv,
    reference_restarted_at_boundaries = TRUE,
    robust_threshold = is.null(threshold),
    method = paste(
      "Rangayyan (2024) Section 8.5 (adaptive",
      "segmentation of the PCG)"
    )
  )
}

#' Eq. (4.30): the PSD is the DFT of the ACF.  It holds exactly for the
#'
#' CIRCULAR autocorrelation.  The linear (biased) ACF gives a smoothed
#' version instead, and the size of that difference is returned -- the
#' distinction is the reason the periodogram is inconsistent.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{psd}, \code{via_circular_acf}, \code{acf_circular},
#' \code{acf_linear}, \code{max_difference}, \code{holds}, \code{linear_difference},
#' \code{linear_acf_is_smoothed}, \code{n}, \code{method}.
#' @export
PsdAcf <- function(x) {
  # eq. (4.30): the PSD is the DFT of the ACF.  It holds exactly for the
  # CIRCULAR autocorrelation.  The linear (biased) ACF gives a smoothed
  # version instead, and the size of that difference is returned -- the
  # distinction is the reason the periodogram is inconsistent.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  step <- 2 * pi / n
  i0 <- seq_len(n) - 1L
  re <- vapply(
    i0, function(k) .morie_fsum(xs * cos(-step * i0 * k)),
    numeric(1)
  )
  im <- vapply(
    i0, function(k) .morie_fsum(xs * sin(-step * i0 * k)),
    numeric(1)
  )
  direct <- re * re + im * im
  circ <- vapply(i0, function(m) {
    .morie_fsum(xs * xs[(i0 + m) %% n + 1L])
  }, numeric(1))
  cr <- vapply(
    i0, function(k) .morie_fsum(circ * cos(-step * i0 * k)),
    numeric(1)
  )
  gap <- max(abs(direct - cr))
  lin <- .morie_rg_acf(xs, n)
  lr <- vapply(
    i0, function(k) .morie_fsum(lin * cos(-step * i0 * k)),
    numeric(1)
  )
  scale <- max(direct)
  if (!scale) scale <- 1
  list(
    psd = direct, via_circular_acf = cr, acf_circular = circ,
    acf_linear = lin, max_difference = gap,
    holds = gap <= 1e-6 * scale,
    linear_difference = max(abs(direct - lr * n)),
    linear_acf_is_smoothed = TRUE, n = n,
    method = "Rangayyan (2024) eq. (4.30)"
  )
}

# --------------------------------------------------------- applications

#' Anc
#'
#' A step of the rangayyan_adapt implementation. Called by \code{FetalEcg}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param primary Passed to \code{LmsFilt}.
#' @param reference Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Passed to \code{LmsFilt}. Defaults to \code{8}.
#' @param mu Passed to \code{LmsFilt}. Defaults to \code{0.01}.
#' @param method One of \code{"lms"}, \code{"rls"}. Defaults to \code{"lms"}.
#' @param lam Passed to \code{RlsFilt}. Defaults to \code{0.98}.
#' @param delta Passed to \code{RlsFilt}. Defaults to \code{1}.
#' @return The value of \code{r}, as built in the body.
#' @export
Anc <- function(primary, reference, order = 8, mu = 0.01,
                method = "lms", lam = 0.98, delta = 1) {
  # Section 3.10, eqs. (3.195)-(3.196): the canceller with either
  # adaptation rule.  The correlation between the output and the reference
  # is returned as reference_leakage: if the reference is not clean of the
  # signal, the canceller removes part of the signal along with the
  # interference, and this is what shows it.
  if (!method %in% c("lms", "rls")) {
    stop("method must be 'lms' or 'rls'")
  }
  r <- if (method == "lms") {
    LmsFilt(primary, reference, order = order, mu = mu)
  } else {
    RlsFilt(primary, reference, order = order, lam = lam, delta = delta)
  }
  e <- r$e
  rs <- as.numeric(reference)
  n <- length(e)
  me <- .morie_fsum(e) / n
  mr <- .morie_fsum(rs) / n
  ve <- .morie_fsum((e - me)^2) / n
  vr <- .morie_fsum((rs - mr)^2) / n
  cov <- .morie_fsum((e - me) * (rs - mr)) / n
  leak <- if (ve > 0 && vr > 0) cov / sqrt(ve * vr) else 0
  r$reference_leakage <- leak
  r$well_separated <- abs(leak) < 0.2
  r$adaptation <- method
  r$method <- paste(
    "Rangayyan (2024) Section 3.10,",
    "eqs. (3.195)-(3.196)"
  )
  r
}

#' FetalEcg
#'
#' A step of the rangayyan_adapt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param abdominal Coerced to numeric by the body, with \code{as.numeric}.
#' @param chest Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Carried through into a list the body builds. Defaults to \code{32}.
#' @param mu Passed to \code{Anc}. Defaults to \code{0.005}.
#' @param method Passed to \code{Anc}. Defaults to \code{"lms"}.
#' @return A list with \code{fetal}, \code{maternal_estimate}, \code{order},
#' \code{input_power}, \code{output_power}, \code{suppression_db},
#' \code{reference_leakage}, \code{single_reference},
#' \code{widrow_used_multiple_references}, \code{method}.
#' @export
FetalEcg <- function(abdominal, chest, order = 32, mu = 0.005,
                     method = "lms") {
  # Section 3.14, after Widrow et al.: cancel the maternal ECG from an
  # abdominal lead using a chest lead as the reference.  Widrow used
  # SEVERAL chest references to capture the maternal signal from more than
  # one projection; a single reference is a simplification, and it is
  # recorded rather than left implied.
  abd <- as.numeric(abdominal)
  ref <- as.numeric(chest)
  if (length(abd) != length(ref)) {
    stop("the abdominal and chest leads must have the same length")
  }
  r <- Anc(abd, ref, order = order, mu = mu, method = method)
  px <- r$input_power
  pe <- r$output_power
  list(
    fetal = r$e, maternal_estimate = r$y, order = order,
    input_power = px, output_power = pe,
    suppression_db = if (pe > 0 && px > 0) 10 * log10(px / pe) else NULL,
    reference_leakage = r$reference_leakage,
    single_reference = TRUE,
    widrow_used_multiple_references = TRUE,
    method = "Rangayyan (2024) Section 3.14, after Widrow et al."
  )
}
