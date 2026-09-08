# Bounds in a short dynamic discrete-choice panel. Honore & Tamer
# (2006) Econometrica 74(3), 611-629. Leave G(alpha|x) and the
# initial-condition distribution unrestricted; theta is in the
# identified set iff the observed sequence frequencies lie in the
# convex hull of the model's sequence probabilities over (alpha, y_0).
# This is a linear feasibility problem.

.bnshrt_GHC_EPS <- 1e-9

#' .bnshrt_logistic
#'
#' A step of the bnshrt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{min}.
#' @return A numeric value.
#' @export
#' @examples
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .bnshrt_logistic(z = y)
#' res
.bnshrt_logistic <- function(z) 1 / (1 + exp(-max(-500, min(500, z))))

#' morie_sequence_probabilities
#'
#' A step of the bnshrt_native implementation. Called by \code{morie_in_identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}.
#' @param x A matrix; passed to \code{as.matrix}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param y0 Coerced to integer by the body, with \code{as.integer}.
#' @param link One of \code{"logit"}, \code{"probit"}. Defaults to \code{"logit"}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_sequence_probabilities <- function(beta, gamma, x, alpha, y0,
                                         link = "logit") {
  xs <- as.matrix(x)
  storage.mode(xs) <- "double"
  T_ <- nrow(xs)
  if (T_ < 1L) stop("bnshrt: need at least one period")
  b <- as.numeric(beta)
  if (length(b) != ncol(xs))
    stop("bnshrt: beta has ", length(b), " entries for ", ncol(xs), " covariates")
  g <- as.numeric(gamma)
  a <- as.numeric(alpha)
  if (!(link %in% c("logit", "probit")))
    stop("bnshrt: link must be logit or probit")
  Ff <- if (link == "logit") .bnshrt_logistic else pnorm
  out <- list()
  for (code in seq_len(2L ^ T_) - 1L) {
    seq_ <- integer(T_)
    for (t in seq_len(T_)) seq_[t] <- (code %/% (2L ^ (t - 1L))) %% 2L
    p <- 1
    prev <- as.integer(y0)
    for (t in seq_len(T_)) {
      idx <- sum(xs[t, ] * b) + g * prev + a
      pt <- Ff(idx)
      p <- p * if (seq_[t] == 1L) pt else (1 - pt)
      prev <- seq_[t]
    }
    out[[paste(seq_, collapse = "")]] <- p
  }
  out
}

#' morie_sequence_frequencies
#'
#' A step of the bnshrt_native implementation. Called by \code{morie_identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y A matrix; passed to \code{as.matrix}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_sequence_frequencies <- function(Y) {
  Ym <- as.matrix(Y)
  if (nrow(Ym) == 0L) stop("bnshrt: no observations")
  T_ <- ncol(Ym)
  rows <- lapply(seq_len(nrow(Ym)), function(i) Ym[i, ])
  for (r in rows) if (length(r) != T_) stop("bnshrt: all sequences must have the same length")
  for (r in rows) for (v in r) if (!(v %in% c(0, 1))) stop("bnshrt: choices must be 0/1")
  counts <- list()
  for (r in rows) {
    key <- paste(r, collapse = "")
    counts[[key]] <- if (is.null(counts[[key]])) 1L else counts[[key]] + 1L
  }
  n <- nrow(Ym)
  out <- list()
  for (code in seq_len(2L ^ T_) - 1L) {
    seq_ <- integer(T_)
    for (t in seq_len(T_)) seq_[t] <- (code %/% (2L ^ (t - 1L))) %% 2L
    key <- paste(seq_, collapse = "")
    out[[key]] <- if (is.null(counts[[key]])) 0 else counts[[key]] / n
  }
  out
}

#' .bnshrt_project_simplex
#'
#' A step of the bnshrt_native implementation. Called by \code{morie_in_identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return The value of \code{pmax}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .bnshrt_project_simplex(v = x)
#' res
.bnshrt_project_simplex <- function(v) {
  n <- length(v)
  u <- sort(v, decreasing = TRUE)
  css <- 0
  rho <- 0
  theta <- 0
  for (i in seq_len(n)) {
    css <- css + u[i]
    t <- (css - 1) / i
    if (u[i] - t > 0) { rho <- i
    theta <- t }
  }
  pmax(v - theta, 0)
}

#' morie_in_identified_set
#'
#' A step of the bnshrt_native implementation. Called by \code{morie_identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param freq A vector; indexed elementwise.
#' @param beta Passed to \code{morie_sequence_probabilities}.
#' @param gamma Passed to \code{morie_sequence_probabilities}.
#' @param x Passed to \code{morie_sequence_probabilities}.
#' @param alpha_grid See Usage.
#' @param y0_values Defaults to \code{c(0, 1)}.
#' @param link Passed to \code{morie_sequence_probabilities}. Defaults to \code{"logit"}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-04}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4000L}.
#' @return A list with \code{discrepancy}, \code{feasible}, \code{weights},
#' \code{fitted}, \code{target}.
#' @export
morie_in_identified_set <- function(freq, beta, gamma, x, alpha_grid,
                                    y0_values = c(0, 1),
                                    link = "logit", tol = 1e-4,
                                    iters = 4000L) {
  cols <- list()
  for (a in alpha_grid) for (y0 in y0_values) {
    cols[[length(cols) + 1L]] <- morie_sequence_probabilities(beta, gamma,
                                                              x, a, y0, link)
  }
  if (length(cols) == 0L) stop("bnshrt: the alpha grid is empty")
  keys <- sort(names(freq))
  A <- matrix(0, nrow = length(keys), ncol = length(cols))
  for (j in seq_along(cols)) for (r in seq_along(keys))
    A[r, j] <- cols[[j]][[keys[r]]]
  target <- as.numeric(freq[keys])
  m <- length(cols)
  R_ <- length(keys)
  w <- rep(1 / m, m)
  v <- rep(1, m)
  L <- 1
  for (kk in seq_len(60)) {
    Av <- as.numeric(A %*% v)
    AtAv <- as.numeric(2 * crossprod(A, Av))
    nrm <- sqrt(sum(AtAv^2))
    if (nrm <= .bnshrt_GHC_EPS) break
    v <- AtAv / nrm
    L <- nrm
  }
  step <- 1 / max(L, .bnshrt_GHC_EPS)
  y_acc <- w
  t_acc <- 1
  prev <- w
  for (it in seq_len(as.integer(iters))) {
    pred <- as.numeric(A %*% y_acc)
    grad <- as.numeric(2 * crossprod(A, pred - target))
    w <- .bnshrt_project_simplex(y_acc - step * grad)
    t_new <- 0.5 * (1 + sqrt(1 + 4 * t_acc^2))
    mom <- (t_acc - 1) / t_new
    y_acc <- w + mom * (w - prev)
    prev <- w
    t_acc <- t_new
  }
  pred <- as.numeric(A %*% w)
  disc <- sqrt(sum((pred - target)^2))
  list(discrepancy = disc, feasible = disc <= as.numeric(tol),
       weights = w, fitted = pred, target = target)
}

#' morie_identified_set
#'
#' A step of the bnshrt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y Passed to \code{morie_sequence_frequencies}.
#' @param x Passed to \code{morie_in_identified_set}.
#' @param beta_grid See Usage.
#' @param gamma_grid See Usage.
#' @param alpha_grid Passed to \code{morie_in_identified_set}.
#' @param beta_fixed Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param link Passed to \code{morie_in_identified_set}. Defaults to \code{"logit"}.
#' @param tol Passed to \code{morie_in_identified_set}. Defaults to \code{0.001}.
#' @return A list with \code{estimate}, \code{set}, \code{n_feasible},
#' \code{beta_bounds}, \code{gamma_bounds}, \code{beta_width}, \code{gamma_width},
#' \code{point_identified}, \code{discrepancy}, \code{method}, \code{assumes}.
#' @export
morie_identified_set <- function(Y, x, beta_grid, gamma_grid, alpha_grid,
                                 beta_fixed = NULL, link = "logit",
                                 tol = 1e-3) {
  freq <- morie_sequence_frequencies(Y)
  keep <- list()
  disc <- list()
  for (bv in beta_grid) for (gv in gamma_grid) {
    b <- if (is.null(beta_fixed)) c(bv) else c(bv, as.numeric(beta_fixed))
    r <- morie_in_identified_set(freq, b, gv, x, alpha_grid, link = link,
                                 tol = tol)
    # collapse the beta vector first: paste() vectorises over a
    # multi-entry beta and [[<- cannot take two keys
    disc[[paste(paste(bv, collapse = ","), gv, sep = "|")]] <- r$discrepancy
    if (r$feasible) keep[[length(keep) + 1L]] <- c(bv, gv)
  }
  if (length(keep) == 0L)
    return(list(estimate = NULL, set = list(), n_feasible = 0L,
                discrepancy = disc,
                note = "no grid point is feasible at this tolerance"))
  bs <- vapply(keep, function(p) p[1], numeric(1))
  gs <- vapply(keep, function(p) p[2], numeric(1))
  list(estimate = list(beta = mean(bs), gamma = mean(gs)),
       set = keep, n_feasible = length(keep),
       beta_bounds = c(min(bs), max(bs)), gamma_bounds = c(min(gs), max(gs)),
       beta_width = max(bs) - min(bs), gamma_width = max(gs) - min(gs),
       point_identified = (max(bs) - min(bs) < .bnshrt_GHC_EPS &&
                           max(gs) - min(gs) < .bnshrt_GHC_EPS),
       discrepancy = disc,
       method = "identified set by mixture feasibility over (alpha, y0); Honore & Tamer (2006) Sec. 2.1",
       assumes = "nothing about G(alpha | x) or the initial condition distribution")
}

morie_shortpanelbound <- morie_identified_set
morie_bound_short_panel <- morie_identified_set

# house entry point: the package exports one morie_<module>
morie_bnshrt <- morie_identified_set
