# Penalised doubly robust TMLE.
# Sources: Belloni, A. & Chernozhukov, V. (2013) Least squares after
# model selection in high-dimensional sparse models, Bernoulli 19(2),
# 521-547, doi:10.3150/11-BEJ410 (post-lasso removes the shrinkage
# bias on the selected coefficients); van der Laan, M. J. & Gruber,
# S. (2016) One-step targeted minimum loss-based estimation based on
# universal least favorable one-dimensional submodels, International
# Journal of Biostatistics 12(1), 351-378, doi:10.1515/ijb-2015-0054
# (the one-dimensional fluctuation whose MLE must not be penalised);
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, Chaps. 4 and 7 (second-order remainder as a
# product of nuisance errors, rate conditions for a penalised
# initial estimator).
#
# Native implementation mirroring Python morie.fn.tmldgp exactly: the
# same coordinate-descent lasso, the same post-lasso OLS refit, the
# same penalised nuisances with an unpenalised targeting step, the
# same validation messages.

.tmldgp_EPS <- 1e-12

#' .tmldgp_logit
#'
#' A step of the tmldgp_native implementation. Called by \code{morie_tmldgp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .tmldgp_logit(p = 0.5)
#' res
.tmldgp_logit <- function(p) {
  q <- pmin(pmax(as.numeric(p), 1e-9), 1 - 1e-9)
  log(q / (1 - q))
}

#' .tmldgp_expit
#'
#' A step of the tmldgp_native implementation. Called by \code{morie_tmldgp},
#' \code{shrunk_targeting_unsafe}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tmldgp_expit(x = x)
#' res
.tmldgp_expit <- function(x) {
  # vectorised clamp: the scalar if() errors on any vector input
  xc <- pmax(x, -700)
  1 / (1 + exp(-xc))
}

#' .soft
#'
#' A step of the tmldgp_native implementation. Called by \code{lasso_path}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; passed to \code{abs}.
#' @param t Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.soft <- function(x, t) sign(x) * max(abs(x) - t, 0)

#' Coordinate descent for the L1-penalised least squares fit
#'
#' @param X Design matrix.
#' @param y Outcome vector.
#' @param lam Regularisation strength.
#' @param iters Maximum coordinate-descent passes.
#' @param tol Convergence tolerance on the maximum coordinate update.
#' @return A list with \code{beta}, \code{intercept}, \code{support},
#'   \code{lambda}.
#' @references Belloni, A. & Chernozhukov, V. (2013).
#' @export
#' @examples
#' lasso_path(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
#' @keywords internal
lasso_path <- function(X, y, lam, iters = 500, tol = 1e-9) {
  rows <- as.matrix(X)
  storage.mode(rows) <- "double"
  t <- as.numeric(y)
  n <- nrow(rows)
  p <- ncol(rows)
  if (length(t) != n) stop("tmldgp: rows and outcomes differ in length")
  lam <- as.numeric(lam)
  if (lam < 0) stop("tmldgp: lambda cannot be negative")
  b <- rep(0, p)
  b0 <- mean(t)
  for (it in seq_len(as.integer(iters))) {
    big <- 0
    for (j in seq_len(p)) {
      r <- t - b0 - (rows %*% b - rows[, j] * b[j])
      zj <- sum(rows[, j]^2)
      if (zj < .tmldgp_EPS) next
      new <- .soft(sum(rows[, j] * r) / n, lam) / (zj / n)
      big <- max(big, abs(new - b[j]))
      b[j] <- new
    }
    b0 <- mean(t - rows %*% b)
    if (big < as.numeric(tol)) break
  }
  list(beta = b, intercept = b0,
       support = which(abs(b) > 1e-10), lambda = lam)
}

#' Refit by least squares on the selected support
#'
#' @param X Design matrix.
#' @param y Outcome vector.
#' @param lam Lasso regularisation.
#' @return A list with \code{support}, \code{coef}, \code{intercept},
#'   \code{predict}, \code{lasso_beta}, \code{selected_by},
#'   \code{note}.
#' @references Belloni, A. & Chernozhukov, V. (2013).
#' @export
#' @examples
#' post_lasso(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
#' @keywords internal
post_lasso <- function(X, y, lam) {
  rows <- as.matrix(X)
  storage.mode(rows) <- "double"
  t <- as.numeric(y)
  sel <- lasso_path(rows, t, lam)
  S <- sel$support
  if (length(S) == 0L) {
    m <- mean(t)
    return(list(support = integer(0), coef = numeric(0),
                intercept = m, predict = function(row) m,
                lasso_beta = sel$beta,
                selected_by = "lasso",
                note = "the lasso selected nothing"))
  }
  Xs <- rows[, S, drop = FALSE]
  XsI <- cbind(1, Xs)
  co <- as.numeric(solve(crossprod(XsI), crossprod(XsI, t)))
  predict_fn <- function(row) {
    v <- as.numeric(row)
    co[1] + sum(co[-1] * v[S])
  }
  list(support = S, coef = co, intercept = co[1],
       predict = predict_fn, lasso_beta = sel$beta,
       selected_by = "lasso, refitted by OLS",
       note = "post-lasso removes the shrinkage bias on the selected coefficients")
}

#' Penalise epsilon itself -- the thing not to do
#'
#' Kept so the cost is measurable: shrinking the fluctuation pulls
#' the estimator back toward the untargeted plug-in and leaves
#' P_n D^* non-zero.
#'
#' @param Q Initial outcome regression.
#' @param H Clever covariate.
#' @param Y Outcome.
#' @param ridge Ridge added to both the score and the Hessian.
#' @return A list with \code{epsilon}, \code{Q_star}, \code{score},
#'   \code{caveat}.
#' @export
#' @examples
#' shrunk_targeting_unsafe(Q = 0.5, H = 0.5, Y = 5L)
#' @keywords internal
shrunk_targeting_unsafe <- function(Q, H, Y, ridge = 1) {
  q <- as.numeric(Q)
  h <- as.numeric(H)
  yy <- as.numeric(Y)
  n <- length(q)
  if (!(length(h) == length(yy) && length(yy) == n))
    stop("tmldgp: Q, H, Y must have the same length")
  off <- vapply(q, .tmldgp_logit, numeric(1))
  e <- 0
  for (it in seq_len(60L)) {
    p <- .tmldgp_expit(off + e * h)
    gr <- sum(h * (yy - p)) - as.numeric(ridge) * e
    he <- sum(h * h * p * (1 - p)) + as.numeric(ridge)
    if (he < 1e-12) break
    e <- e + gr / he
  }
  upd <- .tmldgp_expit(off + e * h)
  list(epsilon = e, Q_star = upd,
       score = sum(h * (yy - upd)) / n,
       caveat = "the score equation is NOT solved when the fluctuation is penalised")
}

#' Penalised nuisance fits, unpenalised targeting
#'
#' Both bar Q and g are fitted by post-lasso; the one-dimensional
#' fluctuation is then fitted by maximum likelihood, unregularised.
#'
#' @param y Outcome vector in \[0,1\].
#' @param D Treatment indicator vector.
#' @param X Covariate matrix.
#' @param penalty Lasso regularisation strength.
#' @param iters Maximum Newton steps.
#' @return A list with \code{estimate}, \code{psi}, \code{epsilon},
#'   \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic},
#'   \code{g_support}, \code{Q_support}, \code{penalty},
#'   \code{method}, \code{note}.
#' @references Belloni, A. & Chernozhukov, V. (2013); van der Laan, M.
#'   J. & Gruber, S. (2016).
#' @export
#' @keywords internal
morie_tmldgp <- function(y, D, X, penalty = 0.05, iters = 100) {
  yv <- as.numeric(y)
  a <- as.numeric(D)
  W <- as.matrix(X)
  storage.mode(W) <- "double"
  n <- length(yv)
  if (!(length(a) == nrow(W) && nrow(W) == n))
    stop("tmldgp: the inputs differ in length")
  if (any(yv < 0 | yv > 1))
    stop("tmldgp: the outcome must lie in [0,1]; rescale it first (see tmlcou)")
  gfit <- post_lasso(W, a, penalty)
  gg <- pmin(pmax(vapply(seq_len(n), function(i)
    gfit$predict(W[i, ]), numeric(1)), 0.02), 0.98)
  Xa <- cbind(a, W)
  qfit <- post_lasso(Xa, yv, penalty)
  row1 <- c(1, rep(0, ncol(W)))
  row0 <- c(0, rep(0, ncol(W)))
  q1 <- pmin(pmax(vapply(seq_len(n), function(i) {
    r <- row1
    r[-1] <- W[i, ]
    qfit$predict(r)
  }, numeric(1)), 1e-6), 1 - 1e-6)
  q0 <- pmin(pmax(vapply(seq_len(n), function(i) {
    r <- row0
    r[-1] <- W[i, ]
    qfit$predict(r)
  }, numeric(1)), 1e-6), 1 - 1e-6)
  H <- a / gg - (1 - a) / (1 - gg)
  qa <- ifelse(a == 1, q1, q0)
  off <- vapply(qa, .tmldgp_logit, numeric(1))
  e <- 0
  for (k in seq_len(as.integer(iters))) {
    p <- .tmldgp_expit(off + e * H)
    gr <- sum(H * (yv - p))
    he <- sum(H * H * p * (1 - p))
    if (he < 1e-12) break
    step <- gr / he
    e <- e + step
    if (abs(step) < 1e-12) break
  }
  q1s <- .tmldgp_expit(.tmldgp_logit(q1) + e / gg)
  q0s <- .tmldgp_expit(.tmldgp_logit(q0) - e / (1 - gg))
  psi <- sum(q1s - q0s) / n
  d <- numeric(n)
  for (i in seq_len(n)) {
    qas <- if (a[i] == 1) q1s[i] else q0s[i]
    d[i] <- H[i] * (yv[i] - qas) + q1s[i] - q0s[i] - psi
  }
  m <- sum(d) / n
  se <- sqrt(sum((d - m)^2) / n^2)
  list(estimate = psi, psi = psi, epsilon = e, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       mean_eic = m, solves_eic = abs(m) < 1e-6,
       g_support = gfit$support, Q_support = qfit$support,
       penalty = as.numeric(penalty),
       method = paste0("penalised doubly robust TMLE with post-lasso ",
                       "nuisance fits; Belloni & Chernozhukov (2013), ",
                       "van der Laan & Gruber (2016)"),
       note = paste0("the PENALTY is on the nuisances only; ",
                     "penalising the fluctuation would break the ",
                     "score equation"))
}

#' Compact alias per ledger/NAMING.md
#' @rdname morie_tmldgp
#' @export
morie_penalisedtmle <- morie_tmldgp

#' Public alias resolved by fn/_lazy_map.json
#' @rdname morie_tmldgp
#' @export
morie_tmle_doubly_robust_pen <- morie_tmldgp

# -- restored: morie-only definition kept through the rmorie sync --
#' Coordinate-descent lasso path
#'
#' Returns the L1-penalised least squares fit at a single
#' \code{lambda}, with intercept and the selected support.
#'
#' @param X Numeric matrix of predictors (without intercept).
#' @param y Numeric response vector.
#' @param lam Non-negative penalty strength.
#' @param iters Maximum coordinate-descent iterations.
#' @tol Convergence tolerance on the maximum coefficient update.
#' @param tol See Usage.
#' @return A list with \code{beta}, \code{intercept}, \code{support},
#'   \code{lambda}.
#' @references Belloni, A. & Chernozhukov, V. (2013).
#' @export
morie_lasso_path <- function(X, y, lam, iters = 500L, tol = 1e-9) {
  rows <- as.matrix(X)
  storage.mode(rows) <- "double"
  t_ <- as.numeric(y)
  n <- nrow(rows)
  p <- ncol(rows)
  if (length(t_) != n) stop("tmldgp: row/outcome length mismatch")
  if (as.numeric(lam) < 0) stop("tmldgp: lambda cannot be negative")
  b <- rep(0, p)
  b0 <- sum(t_) / n
  for (it in seq_len(as.integer(iters))) {
    big <- 0
    for (j in seq_len(p)) {
      r <- numeric(n)
      for (i in seq_len(n)) {
        s <- 0
        for (q in seq_len(p)) if (q != j) s <- s + rows[i, q] * b[q]
        r[i] <- t_[i] - b0 - s
      }
      zj <- sum(rows[, j]^2)
      if (zj < .tmldgp_EPS) next
      sxx <- 0
      for (i in seq_len(n)) sxx <- sxx + rows[i, j] * r[i]
      new <- .soft(sxx / n, as.numeric(lam)) / (zj / n)
      if (abs(new - b[j]) > big) big <- abs(new - b[j])
      b[j] <- new
    }
    s0 <- 0
    for (i in seq_len(n)) {
      s <- 0
      for (q in seq_len(p)) s <- s + rows[i, q] * b[q]
      s0 <- s0 + t_[i] - s
    }
    b0 <- s0 / n
    if (big < as.numeric(tol)) break
  }
  list(beta = b, intercept = b0,
       support = which(abs(b) > 1e-10) - 1L, lambda = as.numeric(lam))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Penalised doubly robust TMLE
#'
#' Both the propensity \code{g} and the outcome regression
#' \code{bar Q} are fitted by post-lasso; the one-dimensional
#' targeting step is unpenalised.
#'
#' @param y Numeric outcome in \code{[0,1]}.
#' @param D Numeric treatment vector.
#' @param X Numeric covariate matrix.
#' @param penalty Lasso penalty.
#' @param iters Maximum targeting iterations.
#' @return A list with the estimate, SE, CI, EIC, and support.
#' @references Belloni, A. & Chernozhukov, V. (2013); van der Laan,
#'   M. J. & Gruber, S. (2016).
#' @export
morie_penalised_tmle <- function(y, D, X, penalty = 0.05, iters = 100) {
  yv <- as.numeric(y)
  a <- as.numeric(D)
  W <- as.matrix(X)
  storage.mode(W) <- "double"
  n <- length(yv)
  if (!(length(a) == nrow(W) && nrow(W) == n))
    stop("tmldgp: the inputs differ in length")
  if (any(yv < 0 | yv > 1))
    stop("tmldgp: the outcome must lie in [0,1]; rescale it first (see tmlcou)")
  gfit <- morie_post_lasso(W, a, penalty)
  gg <- pmin(pmax(vapply(seq_len(n), function(i) gfit$predict(W[i, ]),
                          numeric(1)), 0.02), 0.98)
  Xa <- cbind(a, W)
  qfit <- morie_post_lasso(Xa, yv, penalty)
  q1 <- pmin(pmax(vapply(seq_len(n),
                         function(i) qfit$predict(c(1, W[i, ])),
                         numeric(1)), 1e-6), 1 - 1e-6)
  q0 <- pmin(pmax(vapply(seq_len(n),
                         function(i) qfit$predict(c(0, W[i, ])),
                         numeric(1)), 1e-6), 1 - 1e-6)
  H <- a / gg - (1 - a) / (1 - gg)
  qa <- ifelse(a == 1, q1, q0)
  off <- vapply(qa, .tmldgp_logit, numeric(1))
  e <- 0
  for (it in seq_len(as.integer(iters))) {
    p <- .tmldgp_expit(off + e * H)
    gr <- sum(H * (yv - p))
    he <- sum(H * H * p * (1 - p))
    if (he < 1e-12) break
    step <- gr / he
    e <- e + step
    if (abs(step) < 1e-12) break
  }
  q1s <- .tmldgp_expit(.tmldgp_logit(q1) + e / gg)
  q0s <- .tmldgp_expit(.tmldgp_logit(q0) - e / (1 - gg))
  psi <- mean(q1s - q0s)
  d <- vapply(seq_len(n), function(i) {
    qas <- if (a[i] == 1) q1s[i] else q0s[i]
    H[i] * (yv[i] - qas) + q1s[i] - q0s[i] - psi
  }, numeric(1))
  m <- mean(d)
  se <- sqrt(sum((d - m)^2) / n^2)
  list(estimate = psi, psi = psi, epsilon = e, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       mean_eic = m, solves_eic = abs(m) < 1e-6,
       g_support = gfit$support, Q_support = qfit$support,
       penalty = as.numeric(penalty),
       method = paste("penalised doubly robust TMLE with post-lasso",
                      "nuisance fits; Belloni & Chernozhukov (2013),",
                      "van der Laan & Gruber (2016)"),
       note = paste("the PENALTY is on the nuisances only; penalising",
                    "the fluctuation would break the score equation"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Refit by OLS on the lasso's selected support
#'
#' @param X Numeric predictor matrix.
#' @param y Numeric response vector.
#' @param lam Lasso penalty used for the selection step.
#' @return A list with \code{support}, \code{coef}, \code{intercept},
#'   \code{predict}, and the underlying lasso \code{beta}.
#' @references Belloni, A. & Chernozhukov, V. (2013).
#' @export
morie_post_lasso <- function(X, y, lam) {
  rows <- as.matrix(X)
  storage.mode(rows) <- "double"
  t_ <- as.numeric(y)
  sel <- morie_lasso_path(rows, t_, lam)
  S <- sel$support
  if (length(S) == 0L) {
    m_ <- mean(t_)
    return(list(support = integer(0), coef = numeric(0),
                intercept = m_, predict = function(row) m_,
                selected_by = "lasso",
                note = "the lasso selected nothing",
                lasso_beta = sel$beta))
  }
  Xs <- rows[, S + 1L, drop = FALSE]
  Xd <- cbind(1, Xs)
  co <- solve(crossprod(Xd), crossprod(Xd, t_))
  predict_row <- function(row) {
    v <- as.numeric(row)
    s <- co[1]
    for (a in seq_along(S)) s <- s + co[1L + a] * v[S[a] + 1L]
    s
  }
  list(support = S, coef = co, intercept = co[1],
       predict = predict_row, lasso_beta = sel$beta,
       selected_by = "lasso, refitted by OLS",
       note = "post-lasso removes the shrinkage bias on the selected coefficients")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' The thing not to do: penalise the fluctuation
#'
#' @param Q,H,Y Numeric vectors of equal length.
#' @param ridge Optional ridge on \code{epsilon}.
#' @return A list with the would-be \code{epsilon}, the updated fit,
#'   and the un-targeted mean score.
#' @export
morie_shrunk_targeting_unsafe <- function(Q, H, Y, ridge = 1.0) {
  q <- as.numeric(Q)
  h <- as.numeric(H)
  y <- as.numeric(Y)
  n <- length(q)
  if (!(length(h) == n && length(y) == n))
    stop("tmldgp: Q, H, Y must be the same length")
  off <- vapply(q, .tmldgp_logit, numeric(1))
  e <- 0
  for (it in seq_len(60L)) {
    p <- .tmldgp_expit(off + e * h)
    gr <- sum(h * (y - p)) - as.numeric(ridge) * e
    he <- sum(h * h * p * (1 - p)) + as.numeric(ridge)
    if (he < 1e-12) break
    e <- e + gr / he
  }
  upd <- .tmldgp_expit(off + e * h)
  list(epsilon = e, Q_star = upd,
       score = sum(h * (y - upd)) / n,
       caveat = "the score equation is NOT solved when the fluctuation is penalised")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Compact one-line summary of the tmldgp recipe
#'
#' @return A character string.
#' @export
morie_tmldgp_cheatsheet <- function() {
  paste("tmldgp: in high dimensions regularise the NUISANCES and",
        "leave the TARGETING alone -- epsilon is one-dimensional and",
        "its MLE is exactly what makes P_n D* = 0, so shrinking it",
        "pulls the estimator back to the untargeted plug-in. Use",
        "POST-LASSO for the nuisance fits: the lasso selects and",
        "shrinks, refitting by OLS on the selection keeps the",
        "selection and undoes the shrinkage. Double robustness",
        "matters MORE under penalisation, since a penalised fit is",
        "deliberately biased and the remainder is a PRODUCT of the",
        "two errors.")
}
