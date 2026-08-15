# polkrn -- MSM with polynomial or kernel exposure basis
# Hernan, Brumback & Robins (2002) fit a marginal structural model for a
# repeated-measures outcome under time-varying treatment, and the part
# this module implements is their treatment of the exposure summary:
# the MSM regresses the outcome on a flexible function of cumulative
# treatment rather than on treatment linearly, because the effect of
# staying on therapy is not assumed to accumulate at a constant rate.
#
# Two bases, both here.
#
# **Polynomial.** The MSM is
#
#   E[Y^a] = beta_0 + sum_{d=1}^{D} beta_d (sum_k a_k)^d
#
# fitted by weighted least squares in the pseudo-population created by
# the Sec. 21.2 stabilized weights. Degree 1 is the linear MSM and the
# higher degrees are what "nonlinear effects" means here.
#
# **Kernel.** The same regression run through a radial basis expansion
# of cumulative exposure, with centres placed at the exposure quantiles
# so they follow the data rather than a grid. Useful when the shape is
# not polynomial -- a threshold or a plateau -- which a low-degree
# polynomial cannot represent and a high-degree one represents by
# oscillating.
#
# The two are reported together, along with the fitted curve, because
# the choice between them is a modelling decision and printing a single
# coefficient would conceal it.
#
# The exposure summary is a modelling assumption, not a summary.
# Collapsing a treatment history to its total says the order of
# treatment does not matter. That is exactly the assumption Hernan,
# Brumback & Robins make for cumulative zidovudine, and it is stated
# here rather than left implicit; summary="final" and summary="duration"
# are provided for the cases where it does not hold.
#
# References
# ----------
# Hernan, M. A., Brumback, B. & Robins, J. M. (2002) "Estimating the
# causal effect of zidovudine on CD4 count with a marginal structural
# model for repeated measures", Statistics in Medicine 21(12),
# 1689-1709, doi:10.1002/sim.1144.
#
# Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
# models and causal inference in epidemiology", Epidemiology 11(5),
# 550-560, doi:10.1097/00001648-200009000-00011 -- the weights.
#
# Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
# Chapman & Hall/CRC, Sec. 21.2.

# ---- private helpers (prefixed .polkrn_ to avoid name collisions) ----

.polkrn_vec <- function(x) {
  as.numeric(x)
}

.polkrn_quantile7 <- function(x, q) {
  # type 7 quantile: linear interpolation, the R default
  as.numeric(quantile(x, probs=q, type=7, names=FALSE))
}

.polkrn_wls <- function(X, y, w) {
  # Weighted least squares with an intercept added automatically.
  # Returns a list with coef, se, vcov.
  X <- as.matrix(X)
  y <- as.numeric(y)
  w <- as.numeric(w)
  X <- cbind(1, X)
  n <- nrow(X)
  p <- ncol(X)
  XtWX <- crossprod(X, w * X)
  XtWy <- crossprod(X, w * y)
  beta <- solve(XtWX, XtWy)
  resid <- y - X %*% beta
  wrss <- sum(w * resid^2)
  sigma2 <- wrss / (n - p)
  vcov <- sigma2 * solve(XtWX)
  se <- sqrt(diag(vcov))
  list(coef=as.numeric(beta), se=se, vcov=vcov)
}

.polkrn_logistic <- function(X, y) {
  # Iteratively reweighted least squares for logistic regression.
  # X: design matrix (with intercept), y: 0/1 vector.
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  for (iter in 1:25) {
    eta <- as.numeric(X %*% beta)
    mu <- 1 / (1 + exp(-eta))
    mu <- pmin(pmax(mu, 1e-10), 1 - 1e-10)
    v <- mu * (1 - mu)
    z <- eta + (y - mu) / v
    XtWX <- crossprod(X, v * X)
    XtWz <- crossprod(X, v * z)
    beta_new <- solve(XtWX, XtWz)
    if (max(abs(beta_new - beta)) < 1e-8) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  eta <- as.numeric(X %*% beta)
  mu <- 1 / (1 + exp(-eta))
  list(beta=as.numeric(beta), mu=mu)
}

.polkrn_ip_weights_history <- function(A_hist, L_hist, kind, stabilize, trim) {
  # Compute inverse-probability weights for a treatment history.
  n <- length(A_hist[[1]])
  T <- length(A_hist)
  w <- rep(1, n)
  per_time <- vector("list", T)

  for (t in seq_len(T)) {
    A_t <- .polkrn_vec(A_hist[[t]])

    if (t == 1L) {
      # First time: no past, intercept only for both.
      X_num <- matrix(1, nrow=n, ncol=1L)
      X_den <- matrix(1, nrow=n, ncol=1L)
    } else {
      A_prev <- .polkrn_vec(A_hist[[t - 1L]])
      if (is.null(L_hist[[t - 1L]])) {
        X_num <- cbind(1, A_prev)
        X_den <- cbind(1, A_prev)
      } else {
        L_prev <- .polkrn_vec(L_hist[[t - 1L]])
        X_num <- cbind(1, A_prev)
        X_den <- cbind(1, A_prev, L_prev)
      }
    }

    fit_num <- .polkrn_logistic(X_num, A_t)
    prob_num <- ifelse(A_t == 1, fit_num$mu, 1 - fit_num$mu)

    fit_den <- .polkrn_logistic(X_den, A_t)
    prob_den <- ifelse(A_t == 1, fit_den$mu, 1 - fit_den$mu)

    if (isTRUE(stabilize)) {
      w_t <- prob_num / prob_den
    } else {
      w_t <- 1 / prob_den
    }

    if (!is.null(trim)) {
      w_t <- pmin(w_t, trim)
    }

    w <- w * w_t
    per_time[[t]] <- list(weight=w_t)
  }

  list(w=w, per_time=per_time)
}

# ---- public API ----

exposure_summary <- function(A_history, how="cumulative") {
  if (!how %in% c("cumulative", "final", "duration")) {
    stop(sprintf("exposure_summary: how must be one of %s, got %s",
                 paste(c("cumulative", "final", "duration"), collapse=", "),
                 how))
  }
  cols <- lapply(A_history, .polkrn_vec)
  n <- length(cols[[1]])
  for (j in seq_along(cols)) {
    if (length(cols[[j]]) != n) {
      stop(sprintf("exposure_summary: time 0 has %d rows but time %d has %d",
                   n, j - 1L, length(cols[[j]])))
    }
  }
  if (how == "cumulative") {
    return(rowSums(do.call(cbind, cols)))
  }
  if (how == "final") {
    return(cols[[length(cols)]])
  }
  mat <- do.call(cbind, cols)
  return(rowSums(mat != 0))
}

rbf_basis <- function(x, n_centres=5, width=NULL) {
  xs <- .polkrn_vec(x)
  m <- as.integer(n_centres)
  if (m < 1L) {
    stop(sprintf("rbf_basis: need at least one centre, got %s", n_centres))
  }
  centres <- sapply(seq_len(m), function(j) {
    .polkrn_quantile7(xs, (j + 0.5) / m)
  })
  uniq <- sort(unique(centres))
  if (length(uniq) < 2L) {
    stop("rbf_basis: the exposure takes one distinct value at the requested quantiles, so no basis can be built")
  }
  if (is.null(width)) {
    gaps <- sapply(seq_len(length(uniq) - 1L), function(j) {
      uniq[j + 1L] - uniq[j]
    })
    width <- sum(gaps) / length(gaps)
  }
  h <- as.numeric(width)
  if (h <= 0.0) {
    stop(sprintf("rbf_basis: width must be positive, got %s", width))
  }
  n <- length(xs)
  X <- matrix(NA_real_, nrow=n, ncol=m)
  for (j in seq_len(m)) {
    X[, j] <- exp(-0.5 * ((xs - centres[j]) / h)^2)
  }
  list(X=X, centres=centres, width=h)
}

morie_polkrn <- function(y, A_history, H_history, degree=2,
                          basis="both", summary="cumulative",
                          n_centres=5, width=NULL, kind="binary",
                          stabilize=TRUE, trim=NULL, grid=NULL) {
  if (!basis %in% c("polynomial", "kernel", "both")) {
    stop(sprintf("polynomial_kernel_msm: basis must be one of %s, got %s",
                 paste(c("polynomial", "kernel", "both"), collapse=", "),
                 basis))
  }
  deg <- as.integer(degree)
  if (deg < 1L) {
    stop(sprintf("polynomial_kernel_msm: degree must be at least 1, got %s",
                 degree))
  }
  A_hist <- as.list(A_history)
  if (is.null(H_history)) {
    L_hist <- rep(list(NULL), length(A_hist))
  } else {
    L_hist <- as.list(H_history)
  }
  if (length(L_hist) != length(A_hist)) {
    stop(sprintf("polynomial_kernel_msm: %d treatment times but %d covariate blocks",
                 length(A_hist), length(L_hist)))
  }

  yv <- .polkrn_vec(y)
  n <- length(yv)

  ipw <- .polkrn_ip_weights_history(A_hist, L_hist, kind=kind,
                                     stabilize=stabilize, trim=trim)
  w <- ipw$w
  per_time <- ipw$per_time

  e <- exposure_summary(A_hist, summary)

  if (is.null(grid)) {
    lo <- min(e)
    hi <- max(e)
    if (hi > lo) {
      grid <- lo + (hi - lo) * (0:20) / 20.0
    } else {
      grid <- lo
    }
  }
  grid <- .polkrn_vec(grid)

  out <- list(
    exposure = e,
    weights = w,
    grid = grid,
    n = n,
    n_times = length(A_hist),
    degree = deg,
    summary = summary,
    basis = basis,
    mean_weight = sum(w) / n,
    max_weight = max(w),
    per_time_mean_weight = vapply(per_time,
                                   function(p) sum(p$weight) / n,
                                   numeric(1))
  )

  if (basis %in% c("polynomial", "both")) {
    Xp <- sapply(seq_len(deg), function(d) e^d)
    fp <- .polkrn_wls(Xp, yv, w)
    bp <- fp$coef
    out$coef_polynomial <- bp
    out$se_polynomial <- fp$se
    out$vcov_polynomial <- fp$vcov
    out$curve_polynomial <- vapply(grid, function(g) {
      bp[1] + sum(bp[1 + seq_len(deg)] * g^seq_len(deg))
    }, numeric(1))
    out$estimate <- bp[2]
    out$se <- fp$se[2]
  }

  if (basis %in% c("kernel", "both")) {
    rbf <- rbf_basis(e, n_centres=n_centres, width=width)
    Xk <- rbf$X
    centres <- rbf$centres
    h <- rbf$width
    fk <- .polkrn_wls(Xk, yv, w)
    bk <- fk$coef
    out$coef_kernel <- bk
    out$se_kernel <- fk$se
    out$centres <- centres
    out$width <- h
    n_c <- length(centres)
    out$curve_kernel <- vapply(grid, function(g) {
      bk[1] + sum(bk[1 + seq_len(n_c)] *
                  exp(-0.5 * ((g - centres) / h)^2))
    }, numeric(1))
    if (basis == "kernel") {
      slopes <- numeric(length(grid) - 1L)
      count <- 0L
      for (tt in seq_len(length(grid) - 1L)) {
        if (grid[tt + 1L] != grid[tt]) {
          count <- count + 1L
          slopes[count] <- (out$curve_kernel[tt + 1L] -
                            out$curve_kernel[tt]) /
                           (grid[tt + 1L] - grid[tt])
        }
      }
      if (count > 0L) {
        out$estimate <- sum(slopes[seq_len(count)]) / count
      } else {
        out$estimate <- NaN
      }
      out$se <- NaN
    }
  }

  out$method <- sprintf("marginal structural model with a %s exposure basis, Hernan, Brumback & Robins (2002); weights by Robins, Hernan & Brumback (2000)", basis)
  out
}

.polkrn_cheatsheet <- function() {
  "polkrn: MSM on a flexible function of cumulative exposure (Hernan-Brumback-Robins 2002). polynomial degree D or RBF with quantile centres; weights are the Sec.21.2 product. summary = cumulative | final | duration."
}

# compact alias per ledger/NAMING.md
polynomialkernelmsm <- morie_polkrn
