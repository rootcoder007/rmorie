# REML estimation of variance components (Searle, Casella & McCulloch 1992).
# Reference: Searle, S. R., Casella, G. & McCulloch, C. E. (1992).
# Variance Components. Wiley.  REML Sec. 3.8 and Ch. 6 (Sec. 6.6);
# the balanced-data identity "REML solutions = ANOVA estimators" in
# Sec. 4.8; EM computation Ch. 8.
# Patterson, H. D. & Thompson, R. (1971). Recovery of inter-block
# information when block sizes are unequal. Biometrika 58(3), 545-554
# (the original REML).

#' Group y values by group label, preserving first-appearance order
#'
#' A step of the remlfn_native implementation. Called by \code{.remlfn_ranova}, \code{morie_remlfn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param group A vector; its length is taken and its elements indexed.
#' @return A list with \code{keys}, \code{gs}.
#' @export
.remlfn_groups <- function(y, group) {
  # Group y values by group label, preserving first-appearance order.
  if (length(y) != length(group)) {
    stop("y and group must have equal length")
  }
  keys <- character(0)
  gs <- list()
  for (i in seq_along(group)) {
    k <- as.character(group[i])
    if (!(k %in% keys)) {
      keys <- c(keys, k)
      gs[[k]] <- numeric(0)
    }
    gs[[k]] <- c(gs[[k]], y[i])
  }
  list(keys = keys, gs = gs[keys])
}

#' .remlfn_ranova
#'
#' A step of the remlfn_native implementation. Called by \code{morie_remlfn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; passed to \code{mean}.
#' @param group Passed to \code{.remlfn_groups}.
#' @return A list with \code{msa}, \code{mse}, \code{sigma2_a_raw}, \code{sigma2_a}, \code{balanced}, \code{dfa}, \code{dfe}, \code{ssa}, \code{sse}.
#' @export
.remlfn_ranova <- function(y, group) {
  grp <- .remlfn_groups(y, group)
  keys <- grp$keys
  gs <- grp$gs
  a <- length(keys)
  ns <- sapply(gs, length)
  N <- sum(ns)

  grand_mean <- mean(y)
  group_means <- sapply(gs, mean)

  ssa <- sum(ns * (group_means - grand_mean)^2)
  sse <- 0
  for (i in seq_along(gs)) {
    sse <- sse + sum((gs[[i]] - group_means[i])^2)
  }

  dfa <- a - 1
  dfe <- N - a
  msa <- ssa / dfa
  mse <- sse / dfe

  c_factor <- N - sum(ns * ns) / N
  sigma2_a_raw <- (msa - mse) / c_factor
  sigma2_a <- max(sigma2_a_raw, 0)

  balanced <- (max(ns) == min(ns))

  list(
    msa = msa,
    mse = mse,
    sigma2_a_raw = sigma2_a_raw,
    sigma2_a = sigma2_a,
    balanced = balanced,
    dfa = dfa,
    dfe = dfe,
    ssa = ssa,
    sse = sse
  )
}

#' .remlfn_loglik
#'
#' A step of the remlfn_native implementation. Called by \code{morie_remlfn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gs A vector; its length is taken and its elements indexed.
#' @param ns A vector; indexed elementwise.
#' @param s2a Numeric; combined arithmetically in the body.
#' @param s2e Numeric; passed to \code{log}.
#' @return A list with \code{loglik}, \code{mu}.
#' @export
.remlfn_loglik <- function(gs, ns, s2a, s2e) {
  logdetV <- 0
  xvx <- 0
  xvy <- 0
  yvy <- 0
  for (i in seq_along(gs)) {
    g <- gs[[i]]
    n <- ns[i]
    d <- s2e + n * s2a
    logdetV <- logdetV + (n - 1) * log(s2e) + log(d)
    s <- sum(g)
    ss <- sum(g * g)
    xvx <- xvx + n / d
    xvy <- xvy + s / d
    yvy <- yvy + ss / s2e - (s2a / (s2e * d)) * s * s
  }
  mu <- xvy / xvx
  ypy <- yvy - xvy * xvy / xvx
  list(loglik = -0.5 * (logdetV + log(xvx) + ypy), mu = mu)
}

#' .remlfn_nelder_mead
#'
#' A step of the remlfn_native implementation. Called by \code{morie_remlfn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fn See Usage.
#' @param x0 A vector; its length is taken and its elements indexed.
#' @param xatol Defaults to \code{1e-10}.
#' @param fatol Defaults to \code{1e-10}.
#' @param maxiter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{5000}.
#' @return A list with \code{x}, \code{fun}, \code{nit}, \code{success}.
#' @export
.remlfn_nelder_mead <- function(fn, x0, xatol = 1e-10, fatol = 1e-10, maxiter = 5000) {
  n <- length(x0)
  alpha <- 1
  gamma <- 2
  rho <- 0.5
  sigma <- 0.5

  # Initialize simplex
  simplex <- matrix(0, nrow = n + 1, ncol = n)
  simplex[1, ] <- x0
  for (i in seq_len(n)) {
    simplex[i + 1, ] <- x0
    if (x0[i] == 0) {
      simplex[i + 1, i] <- 0.00025
    } else {
      simplex[i + 1, i] <- x0[i] * 1.05
    }
  }

  fv <- numeric(n + 1)
  for (i in seq_len(n + 1)) {
    fv[i] <- fn(simplex[i, ])
  }

  nit <- 0
  converged <- FALSE

  for (iter in seq_len(maxiter)) {
    nit <- iter
    ord <- order(fv)
    simplex <- simplex[ord, , drop = FALSE]
    fv <- fv[ord]

    # Check convergence. The Python arm (_sci_core._nelder_mead) measures
    # the spread of EVERY vertex against the best one, not just the worst
    # against the best, and uses <= rather than <. With more than one
    # parameter the two rules stop on different iterations -- which is why
    # n_iter came out 117 against 125 while the estimates agreed.
    spread <- max(abs(fv[2:(n + 1)] - fv[1]))
    width <- max(abs(sweep(simplex[2:(n + 1), , drop = FALSE], 2,
                           simplex[1, ], "-")))
    if (spread <= fatol && width <= xatol) {
      converged <- TRUE
      break
    }

    # Centroid (excluding worst point)
    xbar <- colSums(simplex[seq_len(n), , drop = FALSE]) / n

    # Reflection
    xr <- xbar + alpha * (xbar - simplex[n + 1, ])
    fr <- fn(xr)

    if (fv[1] <= fr && fr < fv[n]) {
      simplex[n + 1, ] <- xr
      fv[n + 1] <- fr
    } else if (fr < fv[1]) {
      # Expansion
      xe <- xbar + gamma * (xr - xbar)
      fe <- fn(xe)
      if (fe < fr) {
        simplex[n + 1, ] <- xe
        fv[n + 1] <- fe
      } else {
        simplex[n + 1, ] <- xr
        fv[n + 1] <- fr
      }
    } else {
      # Contraction
      if (fr < fv[n + 1]) {
        xc <- xbar + rho * (xr - xbar)
      } else {
        xc <- xbar - rho * (xbar - simplex[n + 1, ])
      }
      fc <- fn(xc)
      if (fc < min(fr, fv[n + 1])) {
        simplex[n + 1, ] <- xc
        fv[n + 1] <- fc
      } else {
        # Shrink
        for (i in 2:(n + 1)) {
          simplex[i, ] <- simplex[1, ] + sigma * (simplex[i, ] - simplex[1, ])
          fv[i] <- fn(simplex[i, ])
        }
      }
    }
  }

  ord <- order(fv)
  simplex <- simplex[ord, , drop = FALSE]
  fv <- fv[ord]

  list(
    x = simplex[1, ],
    fun = fv[1],
    nit = nit,
    success = converged
  )
}

#' morie_remlfn
#'
#' A step of the remlfn_native implementation. Called by \code{morie_vcomp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param group A vector; its length is taken.
#' @param tol Passed to \code{.remlfn_nelder_mead}. Defaults to \code{1e-10}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5000}.
#' @param solver One of \code{"auto"}, \code{"closed"}, \code{"optim"}. Defaults to \code{"auto"}.
#' @return A list with \code{sigma2_a}, \code{sigma2_e}, \code{mu}, \code{loglik}, \code{n_iter}, \code{converged}, \code{icc}, \code{a}, \code{N}, \code{closed_form}, \code{solver}, \code{method}.
#' @export
morie_remlfn <- function(y, group, tol = 1e-10, max_iter = 5000, solver = "auto") {
  y <- as.numeric(y)

  if (length(y) != length(group)) {
    stop("y and group must have equal length")
  }

  grp <- .remlfn_groups(y, group)
  keys <- grp$keys
  gs <- grp$gs
  a <- length(keys)

  if (a < 2) {
    stop("need at least two classes")
  }

  ns <- sapply(gs, length)
  N <- sum(ns)

  if (N == a) {
    stop("need replication within classes")
  }

  # Start from the ANOVA solution, floored away from zero
  st <- .remlfn_ranova(y, group)
  s2e <- st$mse
  if (s2e <= 0) s2e <- 1e-8
  s2a <- st$sigma2_a
  if (s2a <= 0) s2a <- s2e / max(a, 2)

  if (!(solver %in% c("auto", "closed", "optim"))) {
    stop("solver must be 'auto', 'closed' or 'optim'")
  }

  use_closed <- (solver == "closed" ||
                 (solver == "auto" && st$balanced && st$sigma2_a_raw > 0))

  if (solver == "closed" && !st$balanced) {
    stop("solver='closed' is only valid for balanced data; Searle Sec. 4.8 states REML = ANOVA for balanced data only")
  }

  if (use_closed) {
    s2a <- st$sigma2_a_raw
    s2e <- st$mse
    ll_mu <- .remlfn_loglik(gs, ns, s2a, s2e)
    ll <- ll_mu$loglik
    mu <- ll_mu$mu
    denom <- s2a + s2e
    return(list(
      sigma2_a = s2a,
      sigma2_e = s2e,
      mu = mu,
      loglik = ll,
      n_iter = 0,
      converged = TRUE,
      icc = if (denom > 0) s2a / denom else 0,
      a = a,
      N = N,
      closed_form = TRUE,
      solver = solver,
      method = "REML variance components (Searle et al. 1992, Sec. 4.8 closed form: REML = ANOVA on balanced data)"
    ))
  }

  # Maximize the restricted log-likelihood by Nelder-Mead on log-variances
  neg <- function(par) {
    va <- exp(par[1])
    ve <- exp(par[2])
    if (is.infinite(va) || is.infinite(ve) || va <= 0 || ve <= 0) {
      return(1e300)
    }
    res <- tryCatch({
      ll <- .remlfn_loglik(gs, ns, va, ve)
      -ll$loglik
    }, error = function(e) 1e300)
    if (is.nan(res)) return(1e300)
    res
  }

  x0 <- c(log(max(s2a, 1e-12)), log(max(s2e, 1e-12)))
  res <- .remlfn_nelder_mead(neg, x0, xatol = tol, fatol = tol, maxiter = as.integer(max_iter))
  xb <- res$x

  # Coordinate-wise golden-section polish to handle flat objective
  gr <- (sqrt(5) - 1) / 2
  for (iter in seq_len(60)) {
    moved <- 0
    for (k in c(1, 2)) {
      lo <- xb[k] - 0.5
      hi <- xb[k] + 0.5
      c <- hi - gr * (hi - lo)
      d <- lo + gr * (hi - lo)
      pc <- xb; pc[k] <- c
      pd <- xb; pd[k] <- d
      fc <- neg(pc); fd <- neg(pd)
      for (j in seq_len(200)) {
        if (fc < fd) {
          hi <- d
          d <- c
          fd <- fc
          c <- hi - gr * (hi - lo)
          pc <- xb; pc[k] <- c
          fc <- neg(pc)
        } else {
          lo <- c
          c <- d
          fc <- fd
          d <- lo + gr * (hi - lo)
          pd <- xb; pd[k] <- d
          fd <- neg(pd)
        }
        if (hi - lo < 1e-14) break
      }
      best <- 0.5 * (lo + hi)
      moved <- max(moved, abs(best - xb[k]))
      xb[k] <- best
    }
    if (moved < 1e-13) break
  }

  s2a <- exp(xb[1])
  s2e <- exp(xb[2])
  ll_mu <- .remlfn_loglik(gs, ns, s2a, s2e)
  ll <- ll_mu$loglik
  mu <- ll_mu$mu
  it <- res$nit
  converged <- res$success
  denom <- s2a + s2e
  return(list(
    sigma2_a = s2a,
    sigma2_e = s2e,
    mu = mu,
    loglik = ll,
    n_iter = it,
    converged = converged,
    icc = if (denom > 0) s2a / denom else 0,
    a = a,
    N = N,
    closed_form = FALSE,
    solver = solver,
    method = "REML variance components (Searle et al. 1992, Sec. 6.6)"
  ))
}

# Long descriptive alias (stub-era name)
morie_reml_variance_components <- morie_remlfn

# Cheatsheet
#' Cheatsheet
#'
#' A step of the remlfn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.remlfn_cheatsheet <- function() {
  return("remlfn: REML for the one-way random model; balanced data REML solutions = ANOVA estimators (Searle Sec. 4.8)")
}

# Public name resolutions
morie_reml_loglik <- morie_remlfn
morie_remlloglik <- morie_remlfn
