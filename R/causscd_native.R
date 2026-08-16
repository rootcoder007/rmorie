# Sources:
#   Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S.
#   (2021) "Synthetic Difference-in-Differences", American Economic Review
#   111(12), 4088-4118.

#' .causscd_grid
#'
#' A step of the causscd_native implementation. Called by \code{sdid}, \code{time_weights}, \code{unit_weights}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y See Usage.
#' @param treated See Usage.
#' @param t_post See Usage.
#' @return A list with \code{rows}, \code{n}, \code{T}, \code{tr}, \code{t_post}.
#' @export
.causscd_grid <- function(Y, treated, t_post) {
  rows <- lapply(Y, function(r) as.numeric(r))
  n <- length(rows)
  if (n < 2L) stop("causscd: need at least two units")
  T <- length(rows[[1L]])
  for (r in rows) {
    if (length(r) != T) stop("causscd: Y is ragged")
    if (any(!is.finite(r))) stop("causscd: Y contains a non-finite value")
  }
  tr <- as.logical(treated)
  if (length(tr) != n) stop("causscd: treated must have one flag per unit")
  t_post <- as.integer(t_post)
  if (!(t_post >= 1L && t_post < T))
    stop("causscd: t_post must lie in 1..T-1 (it is the number of pre-treatment periods)")
  if (!any(tr)) stop("causscd: no treated units")
  if (all(tr)) stop("causscd: no control units")
  list(rows = rows, n = n, T = T, tr = tr, t_post = t_post)
}

#' .causscd_project_simplex
#'
#' A step of the causscd_native implementation. Called by \code{.causscd_simplex_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param v A vector; its length is taken.
#' @return The value of \code{pmax}.
#' @export
.causscd_project_simplex <- function(v) {
  m <- length(v)
  u <- sort(v, decreasing = TRUE)
  css <- 0.0
  rho <- 0L
  theta <- 0.0
  for (k in seq_len(m)) {
    css <- css + u[k]
    t <- (css - 1.0) / k
    if (u[k] - t > 0) {
      rho <- k
      theta <- t
    }
  }
  pmax(0.0, v - theta)
}

#' .causscd_simplex_fit
#'
#' A step of the causscd_native implementation. Called by \code{time_weights}, \code{unit_weights}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param cols A vector; its length is taken and its elements indexed.
#' @param target A vector; its length is taken.
#' @param penalty Numeric; combined arithmetically in the body.
#' @param iters Defaults to \code{2000L}.
#' @param tol Defaults to \code{1e-12}.
#' @return A list with \code{w}, \code{intercept}.
#' @export
.causscd_simplex_fit <- function(cols, target, penalty, iters = 2000L, tol = 1e-12) {
  m <- length(cols)
  L <- length(target)
  w <- rep(1.0 / m, m)
  step <- NULL
  for (iter in seq_len(as.integer(iters))) {
    fit <- numeric(L)
    for (t in seq_len(L)) {
      s <- 0.0
      for (k in seq_len(m)) s <- s + w[k] * cols[[k]][t]
      fit[t] <- s
    }
    icept <- sum(target - fit) / L
    resid <- icept + fit - target
    grad <- numeric(m)
    for (k in seq_len(m)) {
      s <- 0.0
      for (t in seq_len(L)) s <- s + resid[t] * cols[[k]][t]
      grad[k] <- 2.0 * s + 2.0 * penalty * w[k]
    }
    if (is.null(step)) {
      gnorm <- sqrt(sum(grad * grad))
      if (gnorm == 0) gnorm <- 1.0
      step <- 1.0 / gnorm
    }
    cand <- w - step * grad
    cand <- .causscd_project_simplex(cand)
    if (max(abs(cand - w)) < tol) {
      w <- cand
      break
    }
    w <- cand
  }
  fit <- numeric(L)
  for (t in seq_len(L)) {
    s <- 0.0
    for (k in seq_len(m)) s <- s + w[k] * cols[[k]][t]
    fit[t] <- s
  }
  icept <- sum(target - fit) / L
  list(w = w, intercept = icept)
}

#' unit_weights
#'
#' A step of the causscd_native implementation. Called by \code{sdid}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Passed to \code{.causscd_grid}.
#' @param treated Passed to \code{.causscd_grid}.
#' @param t_post A count; the body uses it as \code{seq_len(...)}.
#' @param zeta Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{weights}, \code{intercept}, \code{zeta}.
#' @export
unit_weights <- function(Y, treated, t_post, zeta = NULL) {
  g <- .causscd_grid(Y, treated, t_post)
  rows <- g$rows; n <- g$n; T <- g$T; tr <- g$tr; t_post <- g$t_post
  co_idx <- which(!tr)
  trt_idx <- which(tr)
  pre <- seq_len(t_post) - 1L
  target <- numeric(length(pre))
  for (tt in seq_along(pre)) {
    s <- 0.0
    for (i in trt_idx) s <- s + rows[[i]][pre[tt] + 1L]
    target[tt] <- s / length(trt_idx)
  }
  cols <- lapply(co_idx, function(i) rows[[i]][pre + 1L])
  if (is.null(zeta)) {
    diffs <- numeric(0)
    for (i in co_idx) {
      for (t in seq.int(2L, t_post)) {
        diffs <- c(diffs, rows[[i]][t] - rows[[i]][t - 1L])
      }
    }
    if (length(diffs) > 1L) {
      mu <- sum(diffs) / length(diffs)
      sd <- sqrt(sum((diffs - mu)^2) / (length(diffs) - 1L))
    } else {
      sd <- 1.0
    }
    zeta <- (length(trt_idx) * (T - t_post))^0.25 * sd
  }
  fit <- .causscd_simplex_fit(cols, target, (zeta^2) * t_post)
  full <- rep(0.0, n)
  for (k in seq_along(co_idx)) full[co_idx[k]] <- fit$w[k]
  list(weights = full, intercept = fit$intercept, zeta = zeta)
}

#' time_weights
#'
#' A step of the causscd_native implementation. Called by \code{sdid}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Passed to \code{.causscd_grid}.
#' @param treated Passed to \code{.causscd_grid}.
#' @param t_post A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{weights}, \code{intercept}.
#' @export
time_weights <- function(Y, treated, t_post) {
  g <- .causscd_grid(Y, treated, t_post)
  rows <- g$rows; n <- g$n; T <- g$T; tr <- g$tr; t_post <- g$t_post
  co_idx <- which(!tr)
  post <- seq.int(t_post + 1L, T) - 1L
  target <- numeric(length(co_idx))
  for (ii in seq_along(co_idx)) {
    s <- 0.0
    for (t in post) s <- s + rows[[co_idx[ii]]][t + 1L]
    target[ii] <- s / length(post)
  }
  cols <- lapply(pre_seq <- seq_len(t_post) - 1L,
                 function(t) sapply(co_idx, function(i) rows[[i]][t + 1L]))
  fit <- .causscd_simplex_fit(cols, target, 0.0)
  full <- rep(0.0, T)
  for (t in seq_len(t_post)) full[t] <- fit$w[t]
  list(weights = full, intercept = fit$intercept)
}

#' sdid
#'
#' A step of the causscd_native implementation. Called by \code{causscd}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Passed to \code{.causscd_grid}.
#' @param treated Passed to \code{.causscd_grid}.
#' @param t_post A count; the body uses it as \code{seq_len(...)}.
#' @param method One of \code{"did"}, \code{"sc"}, \code{"sdid"}. Defaults to \code{"sdid"}.
#' @param zeta Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{tau}, \code{unit_weights}, \code{time_weights}, \code{zeta}, \code{delta_treated}, \code{delta_control}, \code{method_name}, \code{n_treated}, \code{n_control}, \code{t_pre}, \code{t_post}, \code{method}, \code{note}.
#' @export
sdid <- function(Y, treated, t_post, method = "sdid", zeta = NULL) {
  g <- .causscd_grid(Y, treated, t_post)
  rows <- g$rows; n <- g$n; T <- g$T; tr <- g$tr; t_post <- g$t_post
  if (!(method %in% c("sdid", "did", "sc")))
    stop("causscd: method must be 'sdid', 'did' or 'sc'")
  co_idx <- which(!tr)
  trt_idx <- which(tr)
  pre <- seq_len(t_post) - 1L
  post <- seq.int(t_post + 1L, T) - 1L

  if (method == "did") {
    om <- rep(0.0, n)
    om[co_idx] <- 1.0 / length(co_idx)
    lam <- rep(0.0, T)
    lam[pre + 1L] <- 1.0 / length(pre)
    zeta_used <- 0.0
  } else {
    uw <- unit_weights(Y, treated, t_post, zeta)
    om <- uw$weights
    zeta_used <- uw$zeta
    if (method == "sc") {
      lam <- rep(0.0, T)
      lam[pre + 1L] <- 1.0 / length(pre)
    } else {
      tw <- time_weights(Y, treated, t_post)
      lam <- tw$weights
    }
  }

  wavg_pre <- function(i) sum(lam[pre + 1L] * rows[[i]][pre + 1L])
  avg_post <- function(i) sum(rows[[i]][post + 1L]) / length(post)

  delta <- numeric(n)
  for (i in seq_len(n)) delta[i] <- avg_post(i) - wavg_pre(i)
  d_tr <- sum(delta[trt_idx]) / length(trt_idx)
  d_co <- sum(om * delta)

  tau <- d_tr - d_co

  list(
    estimate = tau,
    tau = tau,
    unit_weights = om,
    time_weights = lam,
    zeta = zeta_used,
    delta_treated = d_tr,
    delta_control = d_co,
    method_name = method,
    n_treated = length(trt_idx),
    n_control = length(co_idx),
    t_pre = t_post,
    t_post = T - t_post,
    method = sprintf("synthetic DID (Arkhangelsky, Athey, Hirshberg, Imbens & Wager 2021), weighting '%s'", method),
    note = paste0("all three weightings are the same estimator of eq. ",
                  "2.4; DID uses 1/N_co and uniform time weights, SC ",
                  "fitted unit weights only, SDID both")
  )
}

#' causscd
#'
#' A step of the causscd_native implementation. Called by \code{morie_causscd}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y See Usage.
#' @param treated See Usage.
#' @param t_post See Usage.
#' @param zeta Defaults to \code{NULL}.
#' @return The value of \code{p}, as built in the body.
#' @export
causscd <- function(Y, treated, t_post, zeta = NULL) {
  out <- sdid(Y, treated, t_post, "sdid", zeta)
  p <- out
  p$did <- sdid(Y, treated, t_post, "did")$tau
  p$sc <- sdid(Y, treated, t_post, "sc", zeta)$tau
  p$sdid <- out$tau
  p
}

#' morie_causscd
#'
#' A step of the causscd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y See Usage.
#' @param treated See Usage.
#' @param t_post See Usage.
#' @param zeta Defaults to \code{NULL}.
#' @return The value of \code{causscd}.
#' @export
morie_causscd <- function(Y, treated, t_post, zeta = NULL) {
  causscd(Y, treated, t_post, zeta)
}
