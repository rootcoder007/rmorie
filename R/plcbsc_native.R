# Synthetic control estimates and placebo (permutation) inference.
#
# Abadie, A., Diamond, A., & Hainmueller, J. (2015) "Comparative Politics and
# the Synthetic Control Method", *American Journal of Political Science* 59(2),
# 495-510.

#' .plcbsc_simplex_project
#'
#' A step of the plcbsc_native implementation. Called by \code{.plcbsc_synthetic_control}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return The value of \code{pmax}.
#' @export
.plcbsc_simplex_project <- function(v) {
  n <- length(v)
  if (n == 0L) return(numeric(0))
  u <- sort(v, decreasing = TRUE)
  css <- 0.0
  theta <- 0.0
  for (i in seq_len(n)) {
    css <- css + u[i]
    t <- (css - 1.0) / i
    if (u[i] - t > 0) theta <- t
  }
  pmax(v - theta, 0.0)
}

#' .plcbsc_synthetic_control
#'
#' A step of the plcbsc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x_treated Coerced to numeric by the body, with \code{as.numeric}.
#' @param x_donors Iterated over elementwise, with \code{lapply}.
#' @param v Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5000}.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.
#' @param step Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{weights}, \code{loss}, \code{fitted}, \code{n_iter}, \code{converged}.
#' @export
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

  resid <- function(ws) X1 - as.numeric(crossprod(D, ws))
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
  cand <- w
  new <- cur + 1

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
       fitted = as.numeric(crossprod(D, w)),
       n_iter = it, converged = converged)
}

#' .plcbsc_gaps
#'
#' A step of the plcbsc_native implementation. Called by \code{.plcbsc_in_time_placebo}, \code{morie_plcbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_treated Numeric; combined arithmetically in the body.
#' @param y_donors Iterated over elementwise, with \code{lapply}.
#' @param weights A matrix; passed to \code{crossprod}.
#' @return A numeric value.
#' @export
.plcbsc_gaps <- function(y_treated, y_donors, weights) {
  Y0 <- do.call(rbind, lapply(y_donors, as.numeric))
  y_treated - as.numeric(crossprod(Y0, weights))
}

#' .plcbsc_rmspe
#'
#' A step of the plcbsc_native implementation. Called by \code{.plcbsc_effect}, \code{.plcbsc_in_time_placebo}, \code{morie_plcbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gaps A vector; its length is taken.
#' @return A numeric value.
#' @export
.plcbsc_rmspe <- function(gaps) {
  if (length(gaps) == 0L) return(NaN)
  sqrt(sum(gaps ^ 2) / length(gaps))
}

#' .plcbsc_effect
#'
#' A step of the plcbsc_native implementation. Called by \code{morie_plcbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gaps A vector; its length is taken and its elements indexed.
#' @param t0 A count; the body uses it as \code{seq_len(...)}.
#' @param statistic Compared against \code{"effect"}.
#' @param pre_gaps Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A numeric value.
#' @export
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

#' morie_plcbsc
#'
#' A step of the plcbsc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_treated Coerced to numeric by the body, with \code{as.numeric}.
#' @param y_donors Iterated over elementwise, with \code{lapply}.
#' @param t0 A count; the body uses it as \code{seq_len(...)}.
#' @param x_treated Passed to \code{fit}.
#' @param x_donors Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param v Carried through into a list the body builds.
#' @param statistic One of \code{"effect"}, \code{"rmspe_ratio"}. Defaults to \code{"effect"}.
#' @param ... Passed through.
#' @return A list with \code{estimate}, \code{gaps}, \code{weights}, \code{fit_loss}, \code{placebo}, \code{pvalue}, \code{rank}, \code{n_donors}, \code{t0}, \code{statistic}, \code{rmspe_pre}, \code{rmspe_post}, \code{note}, \code{method}.
#' @export
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
    placebo[j] <- .plcbsc_effect(pg, t0, statistic)
  }

  abs_est <- abs(est)
  abs_placebo <- abs(placebo)
  all_stats <- c(abs_est, abs_placebo)
  at_least <- sum(all_stats >= abs_est - 1e-12)
  pvalue <- at_least / length(all_stats)
  rank <- match(abs_est, sort(all_stats, decreasing = TRUE))

  list(
    estimate = est,
    gaps = gaps,
    weights = main$weights,
    fit_loss = main$loss,
    placebo = placebo,
    pvalue = pvalue,
    rank = rank,
    n_donors = J,
    t0 = t0,
    statistic = statistic,
    rmspe_pre = .plcbsc_rmspe(gaps[seq_len(t0)]),
    rmspe_post = .plcbsc_rmspe(gaps[(t0 + 1):T]),
    note = paste0("inference is by permutation over the donor pool, so the ",
                  "smallest attainable p-value is 1/(J+1) = ",
                  sprintf("%.4g", 1.0 / (J + 1))),
    method = "synthetic control with in-space placebos (Abadie, Diamond & Hainmueller 2015)"
  )
}

#' .plcbsc_in_time_placebo
#'
#' A step of the plcbsc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_treated Coerced to numeric by the body, with \code{as.numeric}.
#' @param y_donors Iterated over elementwise, with \code{lapply}.
#' @param t0 Numeric; combined arithmetically in the body.
#' @param fake_t0 A count; the body uses it as \code{seq_len(...)}.
#' @param v Carried through into a list the body builds.
#' @param ... Passed through.
#' @return A list with \code{weights}, \code{gaps}, \code{placebo_effect}, \code{rmspe_pre}, \code{rmspe_placebo}.
#' @export
.plcbsc_in_time_placebo <- function(y_treated, y_donors, t0, fake_t0, v = NULL, ...) {
  y1 <- as.numeric(y_treated)
  Y0 <- do.call(rbind, lapply(y_donors, as.numeric))
  fake_t0 <- as.integer(fake_t0)
  t0 <- as.integer(t0)
  if (!(fake_t0 >= 1L && fake_t0 < t0)) {
    stop("plcbsc: fake_t0 must fall before the real t0 and leave a pre-period")
  }
  fit_kwargs <- list(...)
  others_x_list <- lapply(seq_len(nrow(Y0)), function(j) Y0[j, seq_len(fake_t0)])
  args <- c(list(x_treated = y1[seq_len(fake_t0)],
                 x_donors = others_x_list,
                 v = v), fit_kwargs)
  fit <- do.call(.plcbsc_synthetic_control, args)
  others_y_list <- lapply(seq_len(nrow(Y0)), function(j) Y0[j, ])
  gaps <- .plcbsc_gaps(y1, others_y_list, fit$weights)
  list(weights = fit$weights, gaps = gaps,
       placebo_effect = sum(gaps[(fake_t0 + 1):t0]) / (t0 - fake_t0),
       rmspe_pre = .plcbsc_rmspe(gaps[seq_len(fake_t0)]),
       rmspe_placebo = .plcbsc_rmspe(gaps[(fake_t0 + 1):t0]))
}

#' .plcbsc_cheatsheet
#'
#' A step of the plcbsc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.plcbsc_cheatsheet <- function() {
  paste("plcbsc: synthetic control + placebo inference (Abadie, Diamond & Hainmueller 2015).",
        "Weights live on the SIMPLEX -- non-negative, summing to one -- which is what stops the",
        "counterfactual extrapolating outside the donors' support, unlike regression weights.",
        "No standard errors: run the whole procedure pretending each donor was treated, and the",
        "p-value is the fraction of placebo effects at least as large as the real one, so the",
        "smallest attainable p-value is 1/(J+1). in_time_placebo moves the date instead of the",
        "unit. The post/pre RMSPE ratio is offered as an option, not attributed to this paper.")
}

placebo_inference <- morie_plcbsc
placebo_scm_inference <- morie_plcbsc
