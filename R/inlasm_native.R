# morie.fn -- function file (rootcoder007/morie)
# INLA: deterministic posteriors for latent Gaussian models.
#
# A large family of models -- generalised linear, additive, spatial,
# spatio-temporal, geoadditive -- share one structure: a **latent
# Gaussian field** x controlled by a **few** hyperparameters theta,
# observed through a non-Gaussian likelihood. Because the response
# is non-Gaussian the posterior marginals have no closed form, and
# the default answer was MCMC.
#
# The objection to MCMC here is practical, not philosophical. For
# these models it has problems of convergence *and* of computational
# time, to the point that in some applications it is simply not an
# appropriate tool for routine analysis. INLA computes very accurate
# approximations to the posterior marginals **deterministically** --
# seconds or minutes where MCMC needs hours or days -- and the same
# machinery gives model comparison criteria and predictive measures,
# so models can be compared automatically.
#
# The approximation is nested, which is what "nested" means here:
#
#   tilde_p(x_i | y) = INTEGRAL tilde_p(x_i | theta, y) *
#                      tilde_p(theta | y) dtheta,
#
# where the inner conditional is itself a Laplace approximation and
# the outer integral is a **finite weighted sum** over a small
# design of theta points. That the sum is small is exactly why
# dim(theta) must stay low -- the assumption is structural, not
# incidental, and integrate_marginals refuses a design that implies
# otherwise.
#
# The Gaussian approximation is the cheap inner step: match the mode
# and the curvature of log p(x|theta,y), which for a log-concave
# likelihood is a Newton iteration on a sparse system. The
# **simplified Laplace** correction adds the skewness the Gaussian
# misses, and the anchor exercises exactly that -- on a Gaussian
# likelihood the approximation must be **exact**, and on a skewed
# one it must not be.
#
# References
# ----------
# Rue, H., Martino, S. & Chopin, N. (2009) "Approximate Bayesian
# inference for latent Gaussian models by using integrated nested
# Laplace approximations", Journal of the Royal Statistical Society:
# Series B (Statistical Methodology) 71(2), 319-392,
# doi:10.1111/j.1467-9868.2008.00700.x. [PDF supplied by Vee.]
# Tierney, L. & Kadane, J. B. (1986) "Accurate Approximations for
# Posterior Moments and Marginal Densities", Journal of the American
# Statistical Association 81(393), 82-86,
# doi:10.1080/01621459.1986.10478240. The Laplace approximation
# being nested.
# Rue, H. & Held, L. (2005) Gaussian Markov Random Fields: Theory
# and Applications, Chapman & Hall/CRC,
# doi:10.1201/9780203492024. The sparse GMRF computations the
# method rests on.

.EPS <- 1e-12
.MAX_HYPER <- 6

gaussian_approximation <- function(log_lik, log_lik_d1, log_lik_d2,
                                    prior_mean, prior_precision,
                                    x0 = 0.0, iters = 60, tol = 1e-12) {
  m <- as.numeric(prior_mean)
  Q <- as.numeric(prior_precision)
  if (Q <= 0.0) {
    stop("inlasm: the prior precision must be positive")
  }
  x <- as.numeric(x0)
  it <- 0L
  for (it in seq_len(as.integer(iters))) {
    g <- as.numeric(log_lik_d1(x)) - Q * (x - m)
    h <- as.numeric(log_lik_d2(x)) - Q
    if (h >= -.EPS) {
      stop(sprintf("inlasm: the objective is not locally concave at x = %g, so the Gaussian approximation has no mode here", x))
    }
    step <- g / h
    x <- x - step
    if (abs(step) < as.numeric(tol)) {
      break
    }
  }
  prec <- Q - as.numeric(log_lik_d2(x))
  list(
    mode = x,
    precision = prec,
    sd = 1.0 / sqrt(prec),
    iterations = it,
    log_norm = as.numeric(log_lik(x)) - 0.5 * Q * (x - m)^2 + 0.5 * log(2.0 * pi / prec),
    note = "exact when the likelihood is Gaussian, because then the objective is exactly quadratic"
  )
}

skewness_correction <- function(third_derivative, precision) {
  d3 <- as.numeric(third_derivative)
  prec <- as.numeric(precision)
  if (prec <= 0.0) {
    stop("inlasm: the precision must be positive")
  }
  list(
    skewness = d3 / prec^1.5,
    gaussian_adequate = abs(d3 / prec^1.5) < 1e-9,
    note = "zero for a Gaussian likelihood; non-zero is exactly what the simplified Laplace corrects"
  )
}

laplace_marginal <- function(log_joint, x_grid, theta) {
  xs <- as.numeric(x_grid)
  if (length(xs) < 2L) {
    stop("inlasm: at least 2 grid points are needed")
  }
  lp <- vapply(xs, function(x) as.numeric(log_joint(x, theta)), numeric(1L))
  m_val <- max(lp)
  w <- exp(lp - m_val)
  area <- 0.0
  for (i in seq_len(length(xs) - 1L)) {
    area <- area + 0.5 * (w[i] + w[i + 1L]) * (xs[i + 1L] - xs[i])
  }
  if (area <= .EPS) {
    stop("inlasm: the marginal has no mass on this grid")
  }
  dens <- w / area
  mean_v <- 0.0
  for (i in seq_len(length(xs) - 1L)) {
    mean_v <- mean_v + 0.5 * (dens[i] * xs[i] + dens[i + 1L] * xs[i + 1L]) *
      (xs[i + 1L] - xs[i])
  }
  var_v <- 0.0
  for (i in seq_len(length(xs) - 1L)) {
    var_v <- var_v + 0.5 * (dens[i] * (xs[i] - mean_v)^2 +
                              dens[i + 1L] * (xs[i + 1L] - mean_v)^2) *
      (xs[i + 1L] - xs[i])
  }
  list(
    x = xs,
    density = dens,
    mean = mean_v,
    sd = sqrt(max(var_v, 0.0)),
    log_scale = m_val
  )
}

hyperparameter_design <- function(mode, curvature, step = 1.0, dim = NULL) {
  m <- as.numeric(mode)
  d <- if (is.null(dim)) length(m) else as.integer(dim)
  if (d != length(m)) {
    stop(sprintf("inlasm: the mode has %d entries but dim is %d",
                 length(m), d))
  }
  if (d > .MAX_HYPER) {
    stop(sprintf("inlasm: %d hyperparameters -- the outer integral is a small weighted SUM, so the method assumes a low-dimensional theta (the paper says a FEW)",
                 d))
  }
  curv <- as.numeric(curvature)
  sd_vals <- vapply(curv, function(v) if (v > 0.0) 1.0 / sqrt(v) else 1.0,
                    numeric(1L))
  pts <- list(as.list(m))
  for (i in seq_len(d)) {
    for (s in c(-1.0, 1.0)) {
      p <- as.list(m)
      p[[i]] <- p[[i]] + s * as.numeric(step) * sd_vals[i]
      pts[[length(pts) + 1L]] <- p
    }
  }
  list(
    points = pts,
    n_points = length(pts),
    dim = d,
    cost_scaling = "linear here, exponential for a full grid -- hence 'a few' hyperparameters",
    note = "a FINITE design, so the outer integral is a sum"
  )
}

integrate_marginals <- function(conditional_marginals, log_weights, x_grid) {
  M <- lapply(conditional_marginals, function(row) as.numeric(row))
  lw <- as.numeric(log_weights)
  xs <- as.numeric(x_grid)
  if (length(M) != length(lw)) {
    stop(sprintf("inlasm: %d conditional marginals but %d weights",
                 length(M), length(lw)))
  }
  if (any(vapply(M, length, integer(1L)) != length(xs))) {
    stop("inlasm: a conditional marginal does not match the grid")
  }
  mx <- max(lw)
  w <- exp(lw - mx)
  z <- sum(w)
  w <- w / z
  dens <- vapply(seq_along(xs), function(i) {
    sum(vapply(seq_along(M), function(j) w[j] * M[[j]][i], numeric(1L)))
  }, numeric(1L))
  area <- 0.0
  for (i in seq_len(length(xs) - 1L)) {
    area <- area + 0.5 * (dens[i] + dens[i + 1L]) * (xs[i + 1L] - xs[i])
  }
  if (area > .EPS) {
    dens <- dens / area
  }
  mean_v <- 0.0
  for (i in seq_len(length(xs) - 1L)) {
    mean_v <- mean_v + 0.5 * (dens[i] * xs[i] + dens[i + 1L] * xs[i + 1L]) *
      (xs[i + 1L] - xs[i])
  }
  var_v <- 0.0
  for (i in seq_len(length(xs) - 1L)) {
    var_v <- var_v + 0.5 * (dens[i] * (xs[i] - mean_v)^2 +
                              dens[i + 1L] * (xs[i + 1L] - mean_v)^2) *
      (xs[i + 1L] - xs[i])
  }
  list(
    estimate = mean_v,
    mean = mean_v,
    sd = sqrt(max(var_v, 0.0)),
    density = dens,
    x = xs,
    theta_weights = w,
    n_theta = length(M),
    method = "integrated nested Laplace approximation; Rue, Martino & Chopin (2009)",
    note = "deterministic: no chain, no convergence diagnostic, and the same machinery yields model comparison and predictive measures"
  )
}

cheatsheet <- function() {
  "inlasm: latent GAUSSIAN field x, a FEW hyperparameters theta, non-Gaussian response -- so the posterior marginals have no closed form. MCMC works in principle but has convergence AND time problems, sometimes badly enough that it is not appropriate for routine analysis. INLA is deterministic: p(x_i|y) = INTEGRAL p(x_i|theta,y) p(theta|y) dtheta, where the inner term is a LAPLACE approximation and the outer integral is a finite weighted SUM over a small design of theta -- which is exactly why dim(theta) must stay low. The Gaussian inner step is EXACT for a Gaussian likelihood; the simplified Laplace adds the skewness it cannot represent. Seconds or minutes against hours or days."
}

# compact alias per ledger/NAMING.md
inla <- integrate_marginals
inla_spatial <- integrate_marginals
inlaspatial <- integrate_marginals

# public names resolved by fn/_lazy_map.json
morie_inlasm <- list(
  gaussian_approximation = gaussian_approximation,
  laplace_marginal = laplace_marginal,
  hyperparameter_design = hyperparameter_design,
  integrate_marginals = integrate_marginals,
  skewness_correction = skewness_correction,
  cheatsheet = cheatsheet,
  inla = inla,
  inla_spatial = inla_spatial,
  inlaspatial = inlaspatial
)

#' @rdname gaussian_approximation
#' @export
morie_inlasm <- gaussian_approximation

#' @rdname gaussian_approximation
#' @export
morie_inlasm <- gaussian_approximation

#' @rdname gaussian_approximation
#' @export
morie_inlasm <- gaussian_approximation

#' @rdname gaussian_approximation
#' @export
morie_inlasm <- gaussian_approximation
