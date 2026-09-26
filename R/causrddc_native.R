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
#' A step of the causrddc_native implementation. Called by \code{.causrddc_hc_sigma2},
#' \code{.causrddc_kernel_constants}, \code{.causrddc_local_poly_weights}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u Numeric; passed to \code{abs}.
#' @param kernel One of \code{"triangular"}, \code{"uniform"}.
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
#' A step of the causrddc_native implementation. Called by
#' \code{.causrddc_global_derivative}, \code{.causrddc_hc_sigma2},
#' \code{.causrddc_local_poly_weights} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param M A matrix; passed to \code{solve}.
#' @param b A matrix; passed to \code{solve}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .causrddc_solve(M = A, b = b)
#' res
.causrddc_solve <- function(M, b) {
  as.numeric(solve(M, b))
}

#' .causrddc_local_poly_weights
#'
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param h Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @param nu Numeric; combined arithmetically in the body.
#' @param kernel Passed to \code{.causrddc_kern}. Defaults to \code{"triangular"}.
#' @param side Passed to \code{>}. Defaults to \code{1}.
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
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc_rd_bandwidth}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @param q Numeric; combined arithmetically in the body.
#' @param kernel Passed to \code{.causrddc_kern}. Defaults to \code{"triangular"}.
#' @param n_grid Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2001}.
#' @return A list with \code{G}, \code{th}, \code{P}.
#' @export
#' @examples
#' res <- .causrddc_kernel_constants(p = 0.5, q = 0.5)
#' res
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
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc_rd_bandwidth}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; indexed elementwise.
#' @param y A vector; indexed elementwise.
#' @param side Passed to \code{>}.
#' @param order Numeric; combined arithmetically in the body.
#' @param deriv Numeric; combined arithmetically in the body.
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
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param y A vector; indexed elementwise.
#' @param J Numeric; combined arithmetically in the body.
#' @param side_of Passed to \code{>}.
#' @param window Neighbours are drawn from units with \code{abs(x) <= window}.
#' @return The value of \code{out}, as built in the body.
#' @export
.causrddc_nn_sigma2 <- function(x, y, J, side_of, window = Inf) {
  ## Neighbours come from the estimation sample |x| <= max(h, b), and ties
  ## follow rdrobust: units sharing x are matched together, and an
  ## equal-distance step takes both sides (so a unit can get more than J).
  n <- length(x)
  out <- rep(0.0, n)
  inw <- abs(x) <= window
  for (group in list(which(side_of > 0 & inw), which(side_of <= 0 & inw))) {
    m <- length(group)
    if (m < J + 1) next
    ord <- group[order(x[group])]
    xs <- x[ord]
    ys <- y[ord]
    runs <- rle(xs)
    dups <- rep(runs$lengths, runs$lengths)
    dupsid <- sequence(runs$lengths)
    for (t in seq_len(m)) {
      rpos <- dups[t] - dupsid[t]
      lpos <- dupsid[t] - 1
      while (lpos + rpos < min(J, m - 1)) {
        if (t - lpos - 1 <= 0) {
          rpos <- rpos + dups[t + rpos + 1]
        } else if (t + rpos + 1 > m) {
          lpos <- lpos + dups[t - lpos - 1]
        } else {
          dl <- xs[t] - xs[t - lpos - 1]
          dr <- xs[t + rpos + 1] - xs[t]
          if (dl > dr) {
            rpos <- rpos + dups[t + rpos + 1]
          } else if (dl < dr) {
            lpos <- lpos + dups[t - lpos - 1]
          } else {
            rpos <- rpos + dups[t + rpos + 1]
            lpos <- lpos + dups[t - lpos - 1]
          }
        }
      }
      ji <- lpos + rpos
      mean_y <- (sum(ys[(t - lpos):(t + rpos)]) - ys[t]) / ji
      out[ord[t]] <- (ji / (ji + 1.0)) * (ys[t] - mean_y)^2
    }
  }
  out
}

#' .causrddc_density_at_zero
#'
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc_rd_bandwidth}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken.
#' @param h Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .causrddc_density_at_zero(x = x)
#' res
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
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken.
#' @param y A vector; its length is taken.
#' @param nu Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param p Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param kernel Passed to \code{.causrddc_kernel_constants}. Defaults to \code{"triangular"}.
#' @param s Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param prelim_order Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return A list with \code{h}, \code{h_unclamped}, \code{at_bound}, \code{C}, \code{B},
#' \code{V}, \code{f}, \code{mu_plus}, \code{mu_minus}.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(200, -1, 1)
#' y <- 0.5 * x + (x >= 0) * 0.3 + rnorm(200) * 0.1
#' morie_causrddc_rd_bandwidth(x, y)
#' @keywords internal
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
#' A step of the causrddc_native implementation. Called by \code{morie_causrddc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param y A vector; indexed elementwise.
#' @param h Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @param kernel Passed to \code{.causrddc_kern}.
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
#' A step of the causrddc_native implementation. Called by \code{morie_rdrobu}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A vector; its length is taken.
#' @param x A vector; its length is taken and its elements indexed.
#' @param treatment Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param cutoff Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param nu Passed to \code{morie_causrddc_rd_bandwidth}. Defaults to \code{0}.
#' @param p Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param q Optional; may be \code{NULL}. Passed to \code{morie_causrddc_rd_bandwidth}.
#' @param h Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param b Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param kernel Passed to \code{morie_causrddc_rd_bandwidth}. Defaults to \code{"triangular"}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.05}.
#' @param vce One of \code{"hc"}, \code{"nn"}. Defaults to \code{"nn"}.
#' @param J Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.
#' @return A list with \code{estimate}, \code{bias_corrected}, \code{se_conventional},
#' \code{se_robust}, \code{ci_conventional}, \code{ci_bias_corrected}, \code{ci_robust},
#' \code{pvalue_robust}, \code{h}, \code{b}, \code{rho}, \code{p}, \code{q}, \code{nu},
#' \code{kernel}, \code{vce}, \code{alpha}, \code{n}, \code{n_left}, \code{n_right},
#' \code{weights_conventional}, \code{weights_bias_corrected}, \code{fuzzy},
#' \code{method}.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(200, -1, 1)
#' y <- 0.5 * x + (x >= 0) * 0.3 + rnorm(200) * 0.1
#' morie_causrddc(y, x)
#' @keywords internal
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
    # one linearisation at the conventional estimates, s = (1/tau_T,
    # -tau_Y/tau_T^2), for the bias correction and both variances, as
    # rdrobust: tau_bc = tau - s'(bias_Y, bias_T). The local-polynomial
    # weights depend on x alone, so s'(w y, w t) = w (y - tau t) / tau_T
    tau_bc <- tau - ((tauY - tauYbc) / tauT - tauY * (tauT - tauTbc) / tauT^2)
    w_conv <- wY / tauT
    w_bc <- wYbc / tauT
    resid_source <- y - tau * t
  }

  side_of <- ifelse(x >= 0.0, 1, -1)
  if (vce == "nn") {
    sig2 <- .causrddc_nn_sigma2(x, resid_source, as.integer(J), side_of, max(h, b))
    sig2_b <- sig2
  } else {
    ## conventional: residuals of the order-p fit at h; robust: of the
    ## order-q fit at b, which covers every unit the bias weights touch
    sig2 <- .causrddc_hc_sigma2(x, resid_source, h, p, kernel)
    sig2_b <- .causrddc_hc_sigma2(x, resid_source, b, q, kernel)
  }

  v_conv <- sum(w_conv^2 * sig2)
  v_rbc <- sum(w_bc^2 * sig2_b)
  z <- qnorm(1.0 - alpha / 2.0)
  se_c <- sqrt(max(v_conv, 0.0))
  se_r <- sqrt(max(v_rbc, 0.0))
  inside <- which(abs(x) <= h)

  pvalue_robust <- if (se_r > 0) 2.0 * pnorm(abs(tau_bc) / se_r, lower.tail = FALSE) else NA_real_

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
#' A step of the causrddc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' morie_causrddc_cheatsheet()
#' @keywords internal
morie_causrddc_cheatsheet <- function() {
  paste0(
    "causrddc: robust bias-corrected RD inference (Calonico, Catt",
    "aneo & Titiunik 2014). MSE-optimal bandwidths are 'large' on",
    " purpose, so the conventional CI carries a first-order bias ",
    "and undercovers. Fix: recentre by an estimated bias from a h",
    "igher-order local polynomial at pilot bandwidth b, AND resca",
    "le by V + C^bc, a variance that includes the bias estimate's",
    " own variability -- which is what lets rho = h/b stay non-ze",
    "ro. Remark 7: at h = b the bias-corrected estimator IS the l",
    "ocal-quadratic estimator (Frisch-Waugh). Bandwidths from Lem",
    "ma 1; variance nearest-neighbour (J=3) or plug-in residuals.",
    " Sharp, kink (nu=1) and fuzzy all from one code path."
  )
}

#' rdrobust's MSE-optimal constants on one side of the cutoff
#'
#' rdrobust:::rdrobust_bw: the variance constant V, bias constant B,
#' regularisation R and rate for an order-o fit of the nu-th derivative
#' (x already centred at the cutoff).
#' @noRd
.causrddc_bw_part <- function(X, Y, Tt, o, nu, o_B, h_V, h_B, scale, kernel,
                              vce, nnmatch) {
  kw <- function(h) {
    u <- X / h
    w <- if (kernel %in% c("epanechnikov", "epa")) 0.75 * (1 - u^2) * (abs(u) <= 1)
         else if (kernel %in% c("uniform", "uni")) 0.5 * (abs(u) <= 1)
         else (1 - abs(u)) * (abs(u) <= 1)
    w / h
  }
  fit <- function(h, order) {
    w <- kw(h)
    ind <- which(w > 0)
    R <- outer(X[ind], 0:order, `^`)
    list(ind = ind, R = R, W = w[ind], invG = solve(crossprod(R * w[ind], R)))
  }
  coef_ <- function(f, v) as.numeric(f$invG %*% crossprod(f$R * f$W, v[f$ind]))
  resid2 <- function(f, comp, order) {
    if (vce == "nn") {
      return(.causrddc_nn_sigma2(X[f$ind], comp[f$ind], nnmatch, rep(1, length(f$ind))))
    }
    e <- comp[f$ind] - as.numeric(f$R %*% coef_(f, comp))
    n_ <- length(f$ind)
    k_ <- order + 1
    hii <- f$W * rowSums((f$R %*% f$invG) * f$R)
    fac <- switch(vce, hc0 = 1, hc1 = n_ / (n_ - k_),
                  hc2 = 1 / pmax(1 - hii, 1e-8), hc3 = 1 / pmax(1 - hii, 1e-8)^2)
    fac * e^2
  }
  sandwich <- function(f, r2, j) {
    RW <- f$R * f$W
    M <- crossprod(RW * r2, RW)
    as.numeric(f$invG[j, ] %*% M %*% f$invG[j, ])
  }
  fV <- fit(h_V, o)
  s_vec <- 1
  comp <- Y
  if (!is.null(Tt)) {
    bY <- coef_(fV, Y)
    bT <- coef_(fV, Tt)
    tY <- factorial(nu) * bY[nu + 1]
    tT <- factorial(nu) * bT[nu + 1]
    s_vec <- c(1 / tT, -tY / tT^2)
    comp <- s_vec[1] * Y + s_vec[2] * Tt
  }
  V_V <- sandwich(fV, resid2(fV, comp, o), nu + 1)
  v <- crossprod(fV$R * fV$W, (X[fV$ind] / h_V)^(o + 1))
  BConst <- h_V^nu * as.numeric(fV$invG %*% v)[nu + 1]
  fB <- fit(h_B, o_B)
  bcomp <- coef_(fB, comp)
  BWreg <- 0
  if (scale > 0) {
    V_B <- sandwich(fB, resid2(fB, comp, o_B), o + 2)
    BWreg <- 3 * BConst^2 * V_B
  }
  list(V = (2 * nu + 1) * h_V^(2 * nu + 1) * V_V,
       B = sqrt(2 * (o + 1 - nu)) * BConst * bcomp[o + 2],
       R = scale * (2 * (o + 1 - nu)) * BWreg,
       rate = 1 / (2 * o + 3))
}

#' MSE-optimal RD bandwidths (rdrobust's mserd)
#'
#' The common MSE-optimal bandwidths of Calonico, Cattaneo and Farrell
#' (2020), as \code{rdrobust::rdbwselect(bwselect = "mserd")} with its
#' defaults (stdvars = FALSE, masspoints = "adjust", bwrestrict = TRUE):
#' \code{h} for the order-p estimate of the deriv-th derivative jump (fuzzy
#' with \code{treatment}) and \code{b} for its bias correction.
#'
#' @param y Outcome.
#' @param x Running variable.
#' @param cutoff Cutoff.
#' @param p Polynomial order.
#' @param deriv Derivative (1 for a kink).
#' @param q Order of the bias fit, default p + 1.
#' @param kernel "triangular", "epanechnikov" or "uniform".
#' @param vce "nn" (default) or "hc0"-"hc3".
#' @param nnmatch Nearest neighbours for vce = "nn".
#' @param treatment Treatment received, for fuzzy designs.
#' @param scaleregul Regularisation scale.
#' @return list(h, b).
#' @references Calonico, S., Cattaneo, M. D. and Farrell, M. H. (2020).
#'   Optimal bandwidth choice for robust bias-corrected inference in
#'   regression discontinuity designs. Econometrics Journal 23, 192-210.
#' @examples
#' x <- sin(1.37 * 0:199); y <- 1 + x + (x >= 0) + 0.3 * cos(3.1 * 0:199)
#' morie_rd_mserd_bandwidth(y, x)
#' @export
morie_rd_mserd_bandwidth <- function(y, x, cutoff = 0, p = 1, deriv = 0,
                                     q = NULL, kernel = "triangular",
                                     vce = "nn", nnmatch = 3,
                                     treatment = NULL, scaleregul = 1) {
  if (is.null(q)) q <- p + 1
  o <- order(x)
  xs <- as.numeric(x)[o] - cutoff
  ys <- as.numeric(y)[o]
  ts <- if (is.null(treatment)) NULL else as.numeric(treatment)[o]
  n <- length(xs)
  x_iq <- stats::quantile(xs, 0.75, type = 2, names = FALSE) -
    stats::quantile(xs, 0.25, type = 2, names = FALSE)
  BWp <- min(stats::sd(xs), x_iq / 1.349)
  C_c <- if (kernel %in% c("epanechnikov", "epa")) 2.34
         else if (kernel %in% c("uniform", "uni")) 1.843 else 2.576
  L <- xs < 0
  Xl <- xs[L]
  Xr <- xs[!L]
  Yl <- ys[L]
  Yr <- ys[!L]
  Tl <- if (is.null(ts)) NULL else ts[L]
  Tr <- if (is.null(ts)) NULL else ts[!L]
  if (!is.null(ts) && (stats::var(Tl) == 0 || stats::var(Tr) == 0)) Tl <- Tr <- NULL
  M_l <- length(unique(Xl))
  M_r <- length(unique(Xr))
  c_bw <- C_c * BWp * (M_l + M_r)^(-1 / 5)
  bw_max <- max(abs(min(xs)), abs(max(xs)))
  c_bw <- min(c_bw, bw_max)
  bw_min <- NULL
  if (1 - M_l / length(Xl) >= 0.2 || 1 - M_r / length(Xr) >= 0.2) {
    ul <- sort(unique(Xl), decreasing = TRUE)
    ur <- sort(unique(Xr))
    bw_min <- max(abs(ul[min(10, M_l)]) + 1e-8, abs(ur[min(10, M_r)]) + 1e-8)
    c_bw <- max(c_bw, bw_min)
  }
  both <- function(o_, nu, o_B, hb_l, hb_r, scale) {
    list(.causrddc_bw_part(Xl, Yl, Tl, o_, nu, o_B, c_bw, hb_l, scale, kernel, vce, nnmatch),
         .causrddc_bw_part(Xr, Yr, Tr, o_, nu, o_B, c_bw, hb_r, scale, kernel, vce, nnmatch))
  }
  d <- both(q + 1, q + 1, q + 2, abs(min(xs)), abs(max(xs)), 0)
  d_bw <- min(((d[[1]]$V + d[[2]]$V) / (d[[2]]$B - d[[1]]$B)^2)^d[[1]]$rate, bw_max)
  if (!is.null(bw_min)) d_bw <- max(d_bw, bw_min)
  bb <- both(q, p + 1, q + 1, d_bw, d_bw, scaleregul)
  b_bw <- min(((bb[[1]]$V + bb[[2]]$V) / ((bb[[2]]$B - bb[[1]]$B)^2 +
    scaleregul * (bb[[2]]$R + bb[[1]]$R)))^bb[[1]]$rate, bw_max)
  hh <- both(p, deriv, q, b_bw, b_bw, scaleregul)
  h_bw <- min(((hh[[1]]$V + hh[[2]]$V) / ((hh[[2]]$B - hh[[1]]$B)^2 +
    scaleregul * (hh[[2]]$R + hh[[1]]$R)))^hh[[1]]$rate, bw_max)
  list(h = as.numeric(h_bw), b = as.numeric(b_bw))
}
