# Robust bias-corrected inference for regression-discontinuity designs.
#
# Calonico, S., Cattaneo, M. D., & Titiunik, R. (2014) "Robust Nonparametric
# Confidence Intervals for Regression-Discontinuity Designs", *Econometrica*
# 82(6), 2295-2326.
#
# Local polynomial RD estimators need a bandwidth, and the bandwidth selectors
# in use -- cross-validation, or minimising asymptotic MSE -- deliberately
# balance squared bias against variance. That makes them "large" in the sense
# that n h_n^5 ->/-> 0 for the local-linear estimator, so the leading bias
# does not vanish from the distributional approximation and the conventional
# interval undercovers. The paper's fix is two-part: (1) bias-correct with a
# higher-order local polynomial at a pilot bandwidth, and (2) rescale by a
# variance that includes the bias estimate's own variability.

.causrddc_kernels <- c("triangular", "uniform", "epanechnikov")

#' .causrddc_kern
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param u See Usage.
#' @param kernel See Usage.
#' @return A numeric value.
#' @export
.causrddc_kern <- function(u, kernel) {
  a <- abs(u)
  if (a > 1.0) return(0.0)
  if (kernel == "uniform") return(1.0)
  if (kernel == "triangular") return(1.0 - a)
  0.75 * (1.0 - a * a)
}

#' .causrddc_solve
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param M See Usage.
#' @param b See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.causrddc_solve <- function(M, b) {
  as.numeric(solve(M, b))
}

#' .causrddc_local_poly_weights
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param p See Usage.
#' @param nu See Usage.
#' @param kernel Defaults to \code{"triangular"}.
#' @param side Defaults to \code{1}.
#' @return A list with \code{w}, \code{omega}.
#' @export
.causrddc_local_poly_weights <- function(x, h, p, nu, kernel = "triangular", side = 1) {
  n <- length(x)
  side_cond <- if (side > 0) x >= 0.0 else x < 0.0
  keep <- which(side_cond & (abs(x) <= h))
  d <- p + 1
  M <- matrix(0.0, d, d)
  RW <- matrix(0.0, d, n)
  xp1 <- rep(0.0, n)
  for (i in keep) {
    k <- .causrddc_kern(x[i] / h, kernel)
    if (k <= 0.0) next
    r <- (x[i] / h) ^ (0:(d - 1))
    RW[, i] <- k * r
    M <- M + k * outer(r, r)
    xp1[i] <- (x[i] / h) ^ d
  }
  e <- as.numeric((0:(d - 1)) == nu)
  c_vec <- tryCatch(.causrddc_solve(M, e), error = function(err) {
    stop(sprintf("causrddc: the local polynomial design is singular at h = %g on side %+d -- too few points inside the bandwidth", h, side))
  })
  scale <- factorial(nu) / (h ^ nu)
  w <- as.numeric(scale * (t(c_vec) %*% RW))
  omega <- scale * sum(c_vec * (RW %*% xp1)) * (h ^ (p + 1))
  list(w = w, omega = omega)
}

#' .causrddc_kernel_constants
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param p See Usage.
#' @param q See Usage.
#' @param kernel Defaults to \code{"triangular"}.
#' @param n_grid Defaults to \code{2001}.
#' @return A list with \code{G}, \code{th}, \code{P}.
#' @export
.causrddc_kernel_constants <- function(p, q, kernel = "triangular", n_grid = 2001) {
  d <- p + 1
  G <- matrix(0.0, d, d)
  P <- matrix(0.0, d, d)
  th <- rep(0.0, d)
  m <- as.integer(n_grid)
  m <- bitwOr(m, 1L)
  step <- 1.0 / (m - 1)
  for (g in 0:(m - 1)) {
    u <- g * step
    if (g == 0 || g == m - 1) {
      wq <- step / 3.0
    } else if (bitwAnd(g, 1L) == 1L) {
      wq <- 4.0 * step / 3.0
    } else {
      wq <- 2.0 * step / 3.0
    }
    k <- .causrddc_kern(u, kernel)
    u_pow <- u ^ (0:(d - 1))
    th <- th + wq * k * (u ^ q) * u_pow
    G <- G + wq * k * outer(u_pow, u_pow)
    P <- P + wq * k * k * outer(u_pow, u_pow)
  }
  list(G = G, th = th, P = P)
}

#' .causrddc_global_derivative
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param side See Usage.
#' @param order See Usage.
#' @param deriv See Usage.
#' @return A list with \code{deriv}, \code{sigma2}.
#' @export
.causrddc_global_derivative <- function(x, y, side, order, deriv) {
  side_cond <- if (side > 0) x >= 0.0 else x < 0.0
  idx <- which(side_cond)
  d <- order + 1
  if (length(idx) <= d) {
    stop(sprintf("causrddc: too few observations on side %+d for a preliminary polynomial of order %d", side, order))
  }
  M <- matrix(0.0, d, d)
  v <- rep(0.0, d)
  for (i in idx) {
    r <- x[i] ^ (0:(d - 1))
    v <- v + r * y[i]
    M <- M + outer(r, r)
  }
  beta <- .causrddc_solve(M, v)
  fitted <- rep(0.0, length(idx))
  for (k in seq_along(idx)) {
    i <- idx[k]
    fitted[k] <- sum(beta * (x[i] ^ (0:(d - 1))))
  }
  resid <- y[idx] - fitted
  sigma2 <- sum(resid^2) / max(1, length(idx) - d)
  list(deriv = beta[deriv + 1] * factorial(deriv), sigma2 = sigma2)
}

#' .causrddc_nn_sigma2
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param J See Usage.
#' @param side_of See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.causrddc_nn_sigma2 <- function(x, y, J, side_of) {
  n <- length(x)
  out <- rep(0.0, n)
  idx_pos <- which(side_of > 0)
  idx_neg <- which(side_of <= 0)
  for (group in list(idx_pos, idx_neg)) {
    if (length(group) < J + 1) next
    order_idx <- group[order(x[group])]
    L <- length(order_idx)
    for (i in group) {
      t <- match(i, order_idx) - 1
      cand <- integer(0)
      lo <- t - 1
      hi <- t + 1
      while (length(cand) < J && (lo >= 0 || hi < L)) {
        if (lo < 0) {
          cand <- c(cand, order_idx[hi + 1])
          hi <- hi + 1
        } else if (hi >= L) {
          cand <- c(cand, order_idx[lo + 1])
          lo <- lo - 1
        } else if (abs(x[order_idx[lo + 1]] - x[i]) <= abs(x[order_idx[hi + 1]] - x[i])) {
          cand <- c(cand, order_idx[lo + 1])
          lo <- lo - 1
        } else {
          cand <- c(cand, order_idx[hi + 1])
          hi <- hi + 1
        }
      }
      mean_y <- mean(y[cand])
      out[i] <- (length(cand) / (length(cand) + 1.0)) * (y[i] - mean_y) ^ 2
    }
  }
  out
}

#' .causrddc_density_at_zero
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param h Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
.causrddc_density_at_zero <- function(x, h = NULL) {
  n <- length(x)
  xs <- sort(x)
  mean_x <- mean(xs)
  sd_x <- sqrt(max(1e-300, sum((xs - mean_x)^2) / (n - 1)))
  if (is.null(h)) {
    h <- 1.06 * sd_x * n ^ (-0.2)
  }
  if (h <= 0) {
    stop("causrddc: the running variable has no spread")
  }
  tot <- 0.0
  for (v in x) {
    u <- v / h
    if (abs(u) <= 1.0) {
      tot <- tot + 0.75 * (1.0 - u * u)
    }
  }
  max(tot / (n * h), 1e-12)
}

#' morie_causrddc_rd_bandwidth
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param nu Defaults to \code{0}.
#' @param p Defaults to \code{1}.
#' @param kernel Defaults to \code{"triangular"}.
#' @param s Defaults to \code{0}.
#' @param prelim_order Defaults to \code{NULL}.
#' @return A list with \code{h}, \code{h_unclamped}, \code{at_bound}, \code{C}, \code{B}, \code{V}, \code{f}, \code{mu_plus}, \code{mu_minus}.
#' @export
morie_causrddc_rd_bandwidth <- function(x, y, nu = 0, p = 1, kernel = "triangular", s = 0, prelim_order = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  if (n != length(y)) {
    stop("causrddc: x and y must have the same length")
  }
  if (!(kernel %in% .causrddc_kernels)) {
    stop(sprintf("causrddc: kernel must be one of %s", paste(.causrddc_kernels, collapse = ", ")))
  }
  if (nu < 0 || nu > p) {
    stop("causrddc: need 0 <= nu <= p")
  }
  r <- p + 1
  po <- if (is.null(prelim_order)) r + 1 else as.integer(prelim_order)
  res_p <- .causrddc_global_derivative(x, y, +1, po, r)
  res_m <- .causrddc_global_derivative(x, y, -1, po, r)
  mu_p <- res_p$deriv
  mu_m <- res_m$deriv
  s2p <- res_p$sigma2
  s2m <- res_m$sigma2
  kc <- .causrddc_kernel_constants(p, r, kernel)
  G <- kc$G
  th <- kc$th
  P <- kc$P
  e <- as.numeric((0:p) == nu)
  Ginv_e <- .causrddc_solve(G, e)
  diff <- mu_p - ((-1.0) ^ (nu + r + s)) * mu_m
  B <- (diff / factorial(r)) * factorial(nu) * sum(Ginv_e * th)
  PG <- as.numeric(P %*% Ginv_e)
  quad <- sum(Ginv_e * PG)
  f <- .causrddc_density_at_zero(x)
  V <- (s2p + s2m) * (factorial(nu) ^ 2) * quad / f
  if (abs(B) < 1e-300) {
    stop("causrddc: the leading bias constant is zero, so the MSE-optimal bandwidth is not defined; supply h")
  }
  C <- ((1.0 + 2.0 * nu) * V / (2.0 * (p + 1.0 - nu) * B * B)) ^ (1.0 / (2.0 * p + 3.0))
  h <- C * n ^ (-1.0 / (2.0 * p + 3.0))
  span <- max(max(x), -min(x))
  at_bound <- h > span
  list(h = min(h, span), h_unclamped = h, at_bound = at_bound, C = C, B = B, V = V, f = f, mu_plus = mu_p, mu_minus = mu_m)
}

#' .causrddc_hc_sigma2
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param h See Usage.
#' @param p See Usage.
#' @param kernel See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.causrddc_hc_sigma2 <- function(x, y, h, p, kernel) {
  n <- length(x)
  out <- rep(0.0, n)
  for (side in c(+1, -1)) {
    side_cond <- if (side > 0) x >= 0.0 else x < 0.0
    idx <- which(side_cond & (abs(x) <= h))
    d <- p + 1
    if (length(idx) <= d) next
    M <- matrix(0.0, d, d)
    v <- rep(0.0, d)
    for (i in idx) {
      k <- .causrddc_kern(x[i] / h, kernel)
      r <- (x[i] / h) ^ (0:(d - 1))
      v <- v + k * r * y[i]
      M <- M + k * outer(r, r)
    }
    beta <- .causrddc_solve(M, v)
    for (i in idx) {
      fit <- sum(beta * ((x[i] / h) ^ (0:(d - 1))))
      out[i] <- (y[i] - fit) ^ 2
    }
  }
  out
}

#' morie_causrddc
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @param y See Usage.
#' @param x See Usage.
#' @param treatment Defaults to \code{NULL}.
#' @param cutoff Defaults to \code{0}.
#' @param nu Defaults to \code{0}.
#' @param p Defaults to \code{1}.
#' @param q Defaults to \code{NULL}.
#' @param h Defaults to \code{NULL}.
#' @param b Defaults to \code{NULL}.
#' @param kernel Defaults to \code{"triangular"}.
#' @param alpha Defaults to \code{0.05}.
#' @param vce Defaults to \code{"nn"}.
#' @param J Defaults to \code{3}.
#' @return A list with \code{estimate}, \code{bias_corrected}, \code{se_conventional}, \code{se_robust}, \code{ci_conventional}, \code{ci_bias_corrected}, \code{ci_robust}, \code{pvalue_robust}, \code{h}, \code{b}, \code{rho}, \code{p}, \code{q}, \code{nu}, \code{kernel}, \code{vce}, \code{alpha}, \code{n}, \code{n_left}, \code{n_right}, \code{weights_conventional}, \code{weights_bias_corrected}, \code{fuzzy}, \code{method}.
#' @export
morie_causrddc <- function(y, x, treatment = NULL, cutoff = 0.0, nu = 0, p = 1, q = NULL, h = NULL, b = NULL, kernel = "triangular", alpha = 0.05, vce = "nn", J = 3) {
  y <- as.numeric(y)
  x <- as.numeric(x) - as.numeric(cutoff)
  n <- length(x)
  if (n != length(y)) {
    stop("causrddc: y and x must have the same length")
  }
  if (!(kernel %in% .causrddc_kernels)) {
    stop(sprintf("causrddc: kernel must be one of %s", paste(.causrddc_kernels, collapse = ", ")))
  }
  if (!(vce %in% c("nn", "hc"))) {
    stop("causrddc: vce must be 'nn' or 'hc'")
  }
  p <- as.integer(p)
  nu <- as.integer(nu)
  if (nu < 0 || nu > p) {
    stop("causrddc: need 0 <= nu <= p")
  }
  q <- if (is.null(q)) p + 1 else as.integer(q)
  if (q <= p) {
    stop("causrddc: need q > p (the bias estimator must be of higher order than the point estimator)")
  }
  if (alpha <= 0.0 || alpha >= 1.0) {
    stop("causrddc: alpha must lie in (0, 1)")
  }
  if (is.null(h)) {
    h <- morie_causrddc_rd_bandwidth(x, y, nu, p, kernel, s = 0)$h
  }
  h <- as.numeric(h)
  if (h <= 0) {
    stop("causrddc: h must be positive")
  }
  if (is.null(b)) {
    b <- morie_causrddc_rd_bandwidth(x, y, p + 1, q, kernel, s = 2)$h
  }
  b <- as.numeric(b)
  if (b <= 0) {
    stop("causrddc: b must be positive")
  }

  weights_for <- function(vec) {
    wp <- .causrddc_local_poly_weights(x, h, p, nu, kernel, +1)
    wm <- .causrddc_local_poly_weights(x, h, p, nu, kernel, -1)
    vp <- .causrddc_local_poly_weights(x, b, q, p + 1, kernel, +1)
    vm <- .causrddc_local_poly_weights(x, b, q, p + 1, kernel, -1)
    fac <- 1.0 / factorial(p + 1)
    w_conv <- wp$w - wm$w
    w_bc <- w_conv - fac * (wp$omega * vp$w - wm$omega * vm$w)
    tau <- sum(w_conv * vec)
    tau_bc <- sum(w_bc * vec)
    list(w_conv = w_conv, w_bc = w_bc, tau = tau, tau_bc = tau_bc)
  }

  resY <- weights_for(y)
  wY <- resY$w_conv
  wYbc <- resY$w_bc
  tauY <- resY$tau
  tauYbc <- resY$tau_bc

  if (is.null(treatment)) {
    w_conv <- wY
    w_bc <- wYbc
    tau <- tauY
    tau_bc <- tauYbc
    resid_source <- y
  } else {
    t <- as.numeric(treatment)
    if (length(t) != n) {
      stop("causrddc: treatment must have the same length as y")
    }
    resT <- weights_for(t)
    tauT <- resT$tau
    tauTbc <- resT$tau_bc
    if (abs(tauT) < 1e-12) {
      stop("causrddc: the first-stage jump is zero, so the fuzzy estimand is not identified")
    }
    tau <- tauY / tauT
    tau_bc <- tauYbc / tauTbc
    w_conv <- (wY - tau * resT$w_conv) / tauT
    w_bc <- (wYbc - tau_bc * resT$w_bc) / tauTbc
    resid_source <- y - tau * t
  }

  side_of <- ifelse(x >= 0.0, 1, -1)
  if (vce == "nn") {
    sig2 <- .causrddc_nn_sigma2(x, resid_source, as.integer(J), side_of)
  } else {
    sig2 <- .causrddc_hc_sigma2(x, resid_source, h, p, kernel)
  }

  v_conv <- sum(w_conv^2 * sig2)
  v_rbc <- sum(w_bc^2 * sig2)
  z <- qnorm(1.0 - alpha / 2.0)
  se_c <- sqrt(max(v_conv, 0.0))
  se_r <- sqrt(max(v_rbc, 0.0))
  inside <- which(abs(x) <= h)

  pvalue_robust <- if (se_r > 0) 2.0 * (1.0 - pnorm(abs(tau_bc) / se_r)) else NA_real_

  list(
    estimate = tau,
    bias_corrected = tau_bc,
    se_conventional = se_c,
    se_robust = se_r,
    ci_conventional = c(tau - z * se_c, tau + z * se_c),
    ci_bias_corrected = c(tau_bc - z * se_c, tau_bc + z * se_c),
    ci_robust = c(tau_bc - z * se_r, tau_bc + z * se_r),
    pvalue_robust = pvalue_robust,
    h = h, b = b, rho = h / b, p = p, q = q, nu = nu,
    kernel = kernel, vce = vce, alpha = alpha,
    n = n,
    n_left = sum(x[inside] < 0.0),
    n_right = sum(x[inside] >= 0.0),
    weights_conventional = w_conv,
    weights_bias_corrected = w_bc,
    fuzzy = !is.null(treatment),
    method = "robust bias-corrected RD (Calonico, Cattaneo & Titiunik 2014)"
  )
}

# Aliases per ledger/NAMING.md
morie_causrddc_rdrobust <- morie_causrddc
morie_causrddc_causal_rdd_ccft_bw <- morie_causrddc

# Re-export the other public entry points under the morie_ prefix
morie_causrddc_kernel_constants <- .causrddc_kernel_constants
morie_causrddc_local_poly_weights <- .causrddc_local_poly_weights

#' morie_causrddc_cheatsheet
#'
#' Part of the causrddc_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_causrddc_cheatsheet <- function() {
  "causrddc: robust bias-corrected RD inference (Calonico, Cattaneo & Titiunik 2014). MSE-optimal bandwidths are 'large' on purpose, so the conventional CI carries a first-order bias and undercovers. Fix: recentre by an estimated bias from a higher-order local polynomial at pilot bandwidth b, AND rescale by V + C^bc, a variance that includes the bias estimate's own variability -- which is what lets rho = h/b stay non-zero. Remark 7: at h = b the bias-corrected estimator IS the local-quadratic estimator (Frisch-Waugh). Bandwidths from Lemma 1; variance nearest-neighbour (J=3) or plug-in residuals. Sharp, kink (nu=1) and fuzzy all from one code path."
}
