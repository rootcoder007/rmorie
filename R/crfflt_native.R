# The Christiano-Fitzgerald band pass filter.
# Sources: Christiano, L. J. & Fitzgerald, T. J. (2003) "The Band Pass
# Filter", International Economic Review 44(2), 435-465,
# doi:10.1111/1468-2354.t01-1-00076 (NBER working paper version
# w7257, doi:10.3386/w7257, for the random-walk filter of eqs.
# (1.2)-(1.3), the endpoint weights of footnote 3, the one-sided
# filter of eq. (1.4), the symmetric variant of footnote 5 and its
# invariance to drift, and the caveat about negatively autocorrelated
# first differences; Sec. 2.1 for the orthogonal decomposition,
# B(e^{-i omega}) = 1 on the band, B(1) = 0, and the Figure 1a
# observation that the weights remain noticeably non-zero even at
# j = 120; Sec. 2.2 for the finite-sample problem as the projection
# P[y | x]); Baxter, M. & King, R. G. (1999) Measuring Business
# Cycles, Review of Economics and Statistics 81(4), 575-593; Hodrick,
# R. J. & Prescott, E. C. (1997) Postwar U.S. Business Cycles.
#
# Native implementation mirroring Python morie.fn.crfflt exactly: the
# same ideal weights in the same order, the same drift adjustment for
# the asymmetric and one-sided filters, the same three endpoint
# routes, and the same zero-sum weight vectors (B(1) = 0).

#' Ideal band pass weights
#'
#' Equation (1.3) of Christiano & Fitzgerald (2003): \code{B_0 = (b-a)/pi}
#' and \code{B_j = (sin(jb) - sin(ja)) / (pi j)} for \code{j >= 1}, with
#' \code{a = 2 pi / p_u} and \code{b = 2 pi / p_l}.
#'
#' @param p_low Numeric, lower period bound.
#' @param p_high Numeric, upper period bound.
#' @param n Integer, number of non-zero lag weights to return.
#' @return A list with \code{B}, \code{a}, \code{b}, \code{p_low},
#'   \code{p_high}, \code{B0} and a note.
#' @references Christiano, L. J. & Fitzgerald, T. J. (2003). The Band
#'   Pass Filter. International Economic Review, 44(2), 435-465.
#' @export
morie_crfflt_ideal_weights <- function(p_low, p_high, n) {
  pl <- as.numeric(p_low)
  pu <- as.numeric(p_high)
  if (!(2 <= pl && pl < pu))
    stop(sprintf("crfflt: need 2 <= p_low < p_high, got (%.4f, %.4f)",
                 pl, pu))
  n <- as.integer(n)
  if (n < 0L) stop("crfflt: n must be non-negative")
  a <- 2 * pi / pu
  b <- 2 * pi / pl
  B <- (b - a) / pi
  if (n >= 1L) {
    js <- seq_len(n)
    B <- c(B, (sin(js * b) - sin(js * a)) / (pi * js))
  } else {
    B <- c(B)
  }
  list(B = B, a = a, b = b, p_low = pl, p_high = pu, B0 = B[1],
       note = paste("the ideal filter is infinite; these weights die",
                    "out only slowly (Fig. 1a: still non-zero at j=120)"))
}

#' Drift adjustment for the asymmetric filter
#'
#' Removes the random walk's drift \code{mu_hat = (x_T - x_1) / (T - 1)}
#' from the series. The asymmetric filter has one unit root and is not
#' drift-invariant; the symmetric one has two and does not need it.
#'
#' @param x Numeric vector.
#' @return A list with \code{adjusted} (the detrended series) and
#'   \code{drift} (the removed drift).
#' @keywords internal
#' @noRd
.drift_adjust <- function(x) {
  v <- as.numeric(x)
  T <- length(v)
  if (T < 2L) stop("crfflt: need at least 2 observations")
  mu <- (v[T] - v[1]) / (T - 1L)
  list(adjusted = v - (seq_len(T) - 1L) * mu, drift = mu)
}

#' Endpoint weight that absorbs the truncated tail
#'
#' \code{tilde_B_m = -1/2 B_0 - sum_{j=1}^{m-1} B_j}, footnote 3 of
#' Christiano & Fitzgerald (2003).
#'
#' @param B Numeric vector of ideal weights.
#' @param m Integer, distance from the centre.
#' @return A single numeric.
#' @keywords internal
#' @noRd
.tail <- function(B, m) {
  if (m <= 0L) return(-0.5 * B[1])
  -0.5 * B[1] - sum(B[seq_len(m) + 1L - 1L])
}

#' Extract the band by one of three routes
#'
#' @param x Numeric vector, the series.
#' @param p_low Numeric, lower period bound.
#' @param p_high Numeric, upper period bound.
#' @param method One of \code{"asymmetric"}, \code{"symmetric"} or
#'   \code{"one_sided"}.
#' @param p Integer, leads-and-lags count for the symmetric route.
#' @param drift Logical, drift-adjust the data first.
#' @return A list mirroring the Python arm's payload.
#' @references Christiano, L. J. & Fitzgerald, T. J. (2003). The Band
#'   Pass Filter. International Economic Review, 44(2), 435-465.
#' @export
morie_crfflt_cf_filter <- function(x, p_low = 6.0, p_high = 32.0,
                                   method = "asymmetric", p = NULL,
                                   drift = TRUE) {
  if (!(method %in% c("asymmetric", "symmetric", "one_sided")))
    stop(sprintf("crfflt: method must be one of asymmetric, symmetric, one_sided, got %s",
                 method))
  v <- as.numeric(x)
  T <- length(v)
  if (T < 5L) stop(sprintf("crfflt: need at least 5 observations, got %d", T))
  mu <- 0
  if (isTRUE(drift) && method != "symmetric") {
    da <- .drift_adjust(v)
    v <- da$adjusted
    mu <- da$drift
  }
  B <- morie_crfflt_ideal_weights(p_low, p_high, T)$B

  if (method == "symmetric") {
    pp <- if (is.null(p)) min(12L, (T - 1L) %/% 2L) else as.integer(p)
    if (pp < 1L || 2L * pp >= T)
      stop(sprintf("crfflt: p must satisfy 1 <= p < T/2, got %d for T = %d",
                   pp, T))
    w <- B[seq_len(pp)]
    end <- .tail(B, pp)
    out <- rep(NA_real_, T)
    for (t in (pp + 1L):(T - pp)) {
      s <- w[1] * v[t]
      if (pp >= 2L)
        for (j in 2:pp) s <- s + w[j] * (v[t + j - 1L] + v[t - j + 1L])
      s <- s + end * (v[t + pp - 1L] + v[t - pp - 1L + 1L])
      out[t] <- s
    }
    wts <- c(end, w[seq.int(pp - 1L, 2L)], w[1], w[seq.int(2L, pp)], end)
    return(list(estimate = out, cycle = out, method = "symmetric",
                p = pp, weights = wts, weight_sum = sum(wts),
                n_missing = 2L * pp, drift_removed = 0.0,
                note = paste("two unit roots, so the output is invariant",
                             "to drift; costs the first and last p",
                             "observations"),
                reference = "Christiano & Fitzgerald (2003) footnote 5"))
  }

  if (method == "one_sided") {
    out <- rep(NA_real_, T)
    for (t in seq_len(T)) {
      back <- t - 1L
      if (back < 2L) next
      s <- 0.5 * B[1] * v[t]
      if (back >= 2L)
        for (j in 1:(back - 1L)) s <- s + B[j + 1L] * v[t - j]
      s <- s + .tail(B, back) * v[1]
      out[t] <- s
    }
    return(list(estimate = out, cycle = out, method = "one_sided",
                drift_removed = mu,
                note = paste("eq. (1.4): current and past data only,",
                             "for real time estimation"),
                reference = "Christiano & Fitzgerald (2003) eq. (1.4)"))
  }

  out <- numeric(T)
  sums <- numeric(T)
  for (t in seq_len(T)) {
    f <- T - t
    b <- t - 1L
    w <- numeric(T)
    w[t] <- w[t] + if (f >= 1L && b >= 1L) B[1] else 0.5 * B[1]
    if (f >= 1L)
      for (j in 1:(f - 1L)) w[t + j] <- w[t + j] + B[j + 1L]
    if (b >= 1L)
      for (j in 1:(b - 1L)) w[t - j] <- w[t - j] + B[j + 1L]
    if (f >= 1L) w[T] <- w[T] + .tail(B, f)
    if (b >= 1L) w[1] <- w[1] + .tail(B, b)
    out[t] <- sum(w * v)
    sums[t] <- sum(w)
  }
  list(estimate = out, cycle = out, method = "asymmetric",
       trend = v - out, drift_removed = mu, weight_sums = sums,
       max_abs_weight_sum = max(abs(sums)),
       note = paste("eq. (1.2): time-varying weights, uses every",
                    "observation; ONE unit root, so the data must be",
                    "drift-adjusted first"),
       reference = "Christiano & Fitzgerald (2003) eqs. (1.2)-(1.3)")
}

#' Magnitude of the frequency response of a symmetric weight vector
#'
#' Computes \code{|sum_j w_j exp(-i omega j)|} with the weight vector
#' centred on its middle element.
#'
#' @param weights Numeric vector of weights.
#' @param omega Numeric, frequency.
#' @return Numeric magnitude.
#' @export
morie_crfflt_frequency_response <- function(weights, omega) {
  w <- as.numeric(weights)
  n <- length(w)
  c <- (n - 1L) %/% 2L
  re <- sum(w * cos(omega * (seq_len(n) - 1L - c)))
  im <- sum(-w * sin(omega * (seq_len(n) - 1L - c)))
  sqrt(re * re + im * im)
}

# house entry point: the package exports one morie_<module>
morie_crfflt <- morie_crfflt_ideal_weights
