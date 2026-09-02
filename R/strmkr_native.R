# R arm of strmkr -- the Strauss inhibition process fitted by maximum
# pseudolikelihood through the Baddeley-Turner quadrature device.
# Strauss, D. J. (1975) Biometrika 62(2), 467-475; Besag, J. (1977) Bull.
# ISI 47(2), 77-92; Baddeley, A. & Turner, R. (2000) ANZJS 42(3), 283-322.
# Mirrors src/morie/fn/strmkr.py.

.strmkr_EPS <- 1e-12

#' .strmkr_rows
#'
#' A step of the strmkr_native implementation. Called by \code{morie_strmkr_strauss_process}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return The value of \code{m}, as built in the body.
#' @export
.strmkr_rows <- function(x) {
  if (is.matrix(x)) {
    m <- x
  } else if (is.data.frame(x)) {
    m <- as.matrix(x)
  } else if (is.list(x)) {
    m <- do.call(rbind, lapply(x, as.numeric))
  } else {
    m <- matrix(as.numeric(x), nrow = 1L)
  }
  storage.mode(m) <- "double"
  m
}

#' morie_strmkr_strauss_process
#'
#' A step of the strmkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param coords Passed to \code{.strmkr_rows}.
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param gamma Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param window Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param nx A count; the body uses it as \code{seq_len(...)}. Defaults to \code{12L}.
#' @param ny A count; the body uses it as \code{seq_len(...)}. Defaults to \code{12L}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-11}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_strmkr_strauss_process <- function(coords, r, gamma = NULL,
                                         window = NULL, nx = 12L, ny = 12L,
                                         max_iter = 100L, tol = 1e-11) {
  P <- .strmkr_rows(coords)
  n <- nrow(P)
  if (n == 0L)
    stop(paste0("strmkr: an empty pattern carries no information about ",
                "interaction"))
  if (ncol(P) != 2L) stop("strmkr: coords must be two-dimensional")
  rr <- as.numeric(r)
  if (rr <= 0.0) stop("strmkr: the interaction radius must be positive")
  nx <- as.integer(nx)
  ny <- as.integer(ny)
  if (nx < 1L || ny < 1L)
    stop("strmkr: the dummy grid must be at least 1 by 1")

  if (is.null(window)) {
    win <- c(min(P[, 1]), max(P[, 1]), min(P[, 2]), max(P[, 2]))
    if (win[2] - win[1] <= .strmkr_EPS) { win[1] <- win[1] - 0.5
                                          win[2] <- win[2] + 0.5 }
    if (win[4] - win[3] <= .strmkr_EPS) { win[3] <- win[3] - 0.5
                                          win[4] <- win[4] + 0.5 }
    window_source <- "bounding box of the pattern"
  } else {
    win <- as.numeric(window)
    if (length(win) != 4L)
      stop("strmkr: window must be (xmin, xmax, ymin, ymax)")
    if (win[2] <= win[1] || win[4] <= win[3])
      stop("strmkr: the window has non-positive area")
    window_source <- "supplied"
  }
  area <- (win[2] - win[1]) * (win[4] - win[3])

  # sufficient statistic: pairs closer together than r
  npairs <- 0L
  if (n > 1L) for (i in seq_len(n - 1L)) for (j in seq.int(i + 1L, n)) {
    dx <- P[i, 1] - P[j, 1]
    dy <- P[i, 2] - P[j, 2]
    if (sqrt(dx * dx + dy * dy) < rr) npairs <- npairs + 1L
  }

  # ---- Baddeley-Turner quadrature: data points plus a dummy grid
  dummy <- matrix(0.0, nx * ny, 2L)
  idx <- 0L
  for (a in seq_len(nx) - 1L) for (b in seq_len(ny) - 1L) {
    idx <- idx + 1L
    dummy[idx, 1] <- win[1] + (a + 0.5) * (win[2] - win[1]) / nx
    dummy[idx, 2] <- win[3] + (b + 0.5) * (win[4] - win[3]) / ny
  }
  quad <- rbind(P, dummy)
  m <- nrow(quad)
  isdata <- c(rep(1.0, n), rep(0.0, nx * ny))

  ta <- pmin(pmax(as.integer((quad[, 1] - win[1]) / (win[2] - win[1]) * nx),
                  0L), nx - 1L)
  tb <- pmin(pmax(as.integer((quad[, 2] - win[3]) / (win[4] - win[3]) * ny),
                  0L), ny - 1L)
  counts <- matrix(0L, nx, ny)
  for (i in seq_len(m)) counts[ta[i] + 1L, tb[i] + 1L] <-
    counts[ta[i] + 1L, tb[i] + 1L] + 1L
  tile_area <- area / (nx * ny)
  w <- vapply(seq_len(m), function(i) tile_area / counts[ta[i] + 1L,
                                                         tb[i] + 1L], 0)

  # t_r(u): data points within r of u, never counting u against itself
  tstat <- numeric(m)
  for (i in seq_len(m)) {
    c0 <- 0L
    for (j in seq_len(n)) {
      if (i == j) next
      dx <- quad[i, 1] - P[j, 1]
      dy <- quad[i, 2] - P[j, 2]
      if (sqrt(dx * dx + dy * dy) < rr) c0 <- c0 + 1L
    }
    tstat[i] <- c0
  }

  # ---- weighted Poisson regression, log link, response z/w
  X <- cbind(1.0, tstat)
  yq <- isdata / w
  beta <- c(log(max(n, 1L) / area), 0.0)
  it <- 0L
  converged <- FALSE
  A <- matrix(0.0, 2L, 2L)
  for (it in seq_len(as.integer(max_iter))) {
    eta <- as.numeric(X %*% beta)
    mu <- exp(pmax(-500.0, pmin(500.0, eta)))
    ww <- w * mu
    zi <- eta + (yq - mu) / pmax(mu, 1e-300)
    A <- crossprod(X * ww, X)
    rhs <- as.numeric(crossprod(X, ww * zi))
    det <- A[1, 1] * A[2, 2] - A[1, 2] * A[2, 1]
    if (abs(det) < 1e-300)
      stop(paste0("strmkr: the pseudolikelihood information matrix is ",
                  "singular -- no quadrature point has a close neighbour, ",
                  "so gamma is not identified at this radius"))
    new <- c((A[2, 2] * rhs[1] - A[1, 2] * rhs[2]) / det,
             (A[1, 1] * rhs[2] - A[2, 1] * rhs[1]) / det)
    shift <- max(abs(new - beta))
    beta <- new
    if (shift < tol) { converged <- TRUE
    break }
  }

  det <- A[1, 1] * A[2, 2] - A[1, 2] * A[2, 1]
  cov2 <- matrix(c(A[2, 2] / det, -A[2, 1] / det,
                   -A[1, 2] / det, A[1, 1] / det), 2L, 2L)
  se <- c(sqrt(max(cov2[1, 1], 0.0)), sqrt(max(cov2[2, 2], 0.0)))

  eta <- as.numeric(X %*% beta)
  mu <- exp(pmax(-500.0, pmin(500.0, eta)))
  logpl <- sum(w * (yq * eta - mu))

  beta_hat <- exp(beta[1])
  gamma_hat <- exp(beta[2])
  logpl_pois <- n * log(max(n / area, 1e-300)) - n
  out <- list(
    estimate = c(beta_hat, gamma_hat),
    beta = beta_hat, gamma = gamma_hat,
    log_beta = beta[1], log_gamma = beta[2],
    se_log_beta = se[1], se_log_gamma = se[2],
    gamma_ci_lower = exp(beta[2] - 1.959963984540054 * se[2]),
    gamma_ci_upper = exp(beta[2] + 1.959963984540054 * se[2]),
    n_points = as.integer(n), n_close_pairs = as.integer(npairs),
    radius = rr, area = area, window = win, window_source = window_source,
    n_quadrature = as.integer(m), n_dummy = as.integer(nx * ny),
    log_pseudolikelihood = logpl,
    log_pseudolikelihood_poisson = logpl_pois,
    iterations = as.integer(it), converged = converged,
    valid_density = gamma_hat <= 1.0,
    interaction = if (gamma_hat < 1.0 - 1e-8) "inhibition" else
      if (abs(gamma_hat - 1.0) <= 1e-8) "none (Poisson)" else
        "attraction -- NOT a valid Strauss density")
  if (!is.null(gamma)) {
    g <- as.numeric(gamma)
    if (g <= 0.0) stop("strmkr: gamma must be positive")
    out$gamma_given <- g
    out$log_density_unnormalised <- n * log(beta_hat) + npairs * log(g)
    out$valid_density_given <- g <= 1.0
  }
  out$method <- paste0("Strauss process fitted by maximum pseudolikelihood ",
                       "through the Baddeley-Turner quadrature device ",
                       "(Strauss 1975; Besag 1977; Baddeley & Turner 2000)")
  out$note <- paste0("gamma < 1 is inhibition, gamma = 1 is Poisson, and ",
                     "gamma > 1 is not an integrable density at all (Kelly ",
                     "& Ripley 1976) -- valid_density says which case the ",
                     "fit landed in instead of clamping it")
  out
}

#' .strmkr_cheatsheet
#'
#' A step of the strmkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.strmkr_cheatsheet <- function() {
  paste0("strmkr: morie_strmkr_strauss_process(coords, r, gamma) -> ",
         "pseudolikelihood beta and gamma for the Strauss inhibition model ",
         "(Strauss 1975; Baddeley & Turner 2000)")
}

morie_strmkr <- morie_strmkr_strauss_process
