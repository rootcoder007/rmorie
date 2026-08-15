```r
# Synthetic control estimates and placebo (permutation) inference.
#
# Abadie, A., Diamond, A., & Hainmueller, J. (2015) "Comparative Politics and
# the Synthetic Control Method", *American Journal of Political Science* 59(2),
# 495-510.

.plcbsc_simplex_project <- function(v) {
  n <- length(v)
  if (n == 0L) return(numeric(0))
  u <- sort(v, decreasing = TRUE)
  css <- 0.0
  rho <- 0L
  theta <- 0.0
  for (i in seq_len(n)) {
    css <- css + u[i]
    t <- (css - 1.0) / i
    if (u[i] - t > 0) {
      rho <- i
      theta <- t
    }
  }
  pmax(v - theta, 0.0)
}

.plcbsc_synthetic_control <- function(x_treated, x_donors, v = NULL,
                                      max_iter = 5000, tol = 1e-12,
                                      step = NULL) {
  X1 <- as.numeric(x_treated)
  D <- do.call(rbind, lapply(x_donors, as.numeric))
  k <- length(X1)
  J <- nrow(D)

  if (J == 0L) stop("plcbsc: the donor pool is empty")
  if (ncol(D) != k) {
    stop("plcbsc: every donor needs the same predictors as the treated unit")
  }

  if (is.null(v)) {
    vv <- rep(1.0, k)
  } else {
    vv <- as.numeric(v)
    if (length(vv) != k || any(vv < 0)) {
      stop("plcbsc: v must be one non-negative weight per predictor")
    }
    if (sum(vv) <= 0) {
      stop("plcbsc: v must have some positive weight")
    }
  }

  w <- rep(1.0 / J, J)

  resid <- function(ws) {
    X1 - as.numeric(D %*% ws)
  }

  loss <- function(ws) {
    r <- resid(ws)
    sum(vv * r * r)
  }

  if (is.null(step)) {
    norms <- as.numeric((D ^ 2) %*% vv)
    norm <- max(norms)
    step <- if (norm > 0) 1.0 / (2.0 * norm * J) else 1e-3
  }

  cur <- loss(w)
  converged <- FALSE
  it <- 0L
  new <- cur + 1
  cand <- w

  for (it in seq_len(as.integer(max_iter))) {
    r <- resid(w)
    grad <- -2.0 * as.numeric(D %*% (vv * r))

    s <- step
    cand <- w
    new <- cur + 1
    for (inner in seq_len(60L)) {
      cand <- .plcbsc_simplex_project(w - s * grad)
      new <- loss(cand)
      if (new <= cur) break
      s <- s * 0.5
    }

    if (new > cur - tol * max(1.0, abs(cur))) {
      w <- cand
      cur <- new
      converged <- TRUE
      break
    }
    w <- cand
    cur <- new
  }

  list(weights = w, loss = cur,
       fitted = as.numeric(D %*% w),
       n_iter = it, converged = converged)
}

.plcbsc_gaps <- function(y_treated, y_donors, weights) {
  Y0 <- do.call(rbind, lapply(y_donors, as.numeric))
  y_treated - as.numeric(Y0 %*% weights)
}

.plcbsc_rmspe <- function(gaps) {
  if (length(gaps) == 0L) return(NaN)
  sqrt(sum(gaps ^ 2) / length(gaps))
}

.plcbsc_effect <- function(gaps, t0, statistic, pre_gaps = NULL) {
  T <- length(gaps)
  post <- if (t0 < T) gaps[(t0 + 1):T] else numeric(0)
  if (statistic == "effect") {
    if (length(post) == 0L) return(NaN)
    return(sum(post) / length(post))
  }
  pre <- if (is.null(pre_gaps)) gaps[seq_len(t0)] else pre_gaps
  denom <- .plcbsc_rmspe(pre)
  if (denom <= 0) return(Inf)
  .plcbsc_rmspe(post) / denom
}

morie_plcbsc <- function(y_treated, y_donors, t0, x_treated = NULL,
                         x_donors = NULL, v = NULL,
                         statistic = "effect", ...) {

  y1 <- as.numeric(y_treated)
  Y0 <- do.call(rbind, lapply(y_donors, as.numeric))
  T <- length(y1)
  J <- nrow(Y0)

  if (J == 0L) stop("plcbsc: the donor pool is empty")
  if (ncol(Y0) != T) {
    stop("plcbsc: every donor needs the same number of periods as the treated unit")
  }
  t0 <- as.integer(t0)
  if (!(t0 >= 1L && t0 < T)) {
    stop("plcbsc: t0 must leave at least one pre- and one post-intervention period")
  }
  if (!(statistic %in% c("effect", "rmspe_ratio"))) {
    stop("plcbsc: statistic must be 'effect' or 'rmspe_ratio'")
  }

  fit_kwargs <- list(...)

  fit <- function(unit_y, others_y, unit_x, others_x) {
    if (is.null(unit_x)) {
      unit_x <- unit_y[seq_len(t0)]
      others_x <- lapply(others_y, function(o) o[seq_len(t0)])
    }
    args <- c(list(x_treated = unit_x, x_donors = others_x, v = v), fit_kwargs)
    do.call(.plcbsc_synthetic_control, args)
  }

  others_y_list <- lapply(seq_len(J), function(j) Y0[j, ])

  main <- fit(y1, others_y_list, x_treated, x_donors)
  gaps <- .plcbsc_gaps(y1, others_y_list, main$weights)
  est <- .plcbsc_effect(gaps, t0, statistic)

  placebo <- numeric(J)
  for (j in seq_len(J)) {
    others_idx <- setdiff(seq_len(J), j)
    others <- lapply(others_idx, function(m) Y0[m, ])
    ox <- NULL
    if (!is.null(x_donors)) {
      ox <- x_donors[others_idx]
    }
    pf <- fit(Y0[j, ], others,
              if (is.null(x_donors)) NULL else x_donors[[j]],
              ox)
    pg <- .plcbsc_gaps(Y0[j, ], others, pf$weights)
    placebo[j] <- .plcbsc_effect
