# C-TMLE: choosing the treatment mechanism collaboratively.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 10
# (C-TMLE for continuous tuning: the general C-TMLE algorithm
# constructing an ordered sequence of TMLEs indexed by a tuning
# parameter, with the variation norm of Chap. 6 as the continuous
# index; the use of the L-fit of the targeted outcome regression as
# the selection criterion rather than the fit of g; the one-step
# TMLE inside the sequence; the verification that C-TMLE solves the
# critical score equation; and the general theorem for C-TMLE
# asymptotic linearity). van der Laan, M. J. & Gruber, S. (2010)
# "Collaborative Double Robust Targeted Maximum Likelihood
# Estimation", International Journal of Biostatistics 6(1),
# Article 17, doi:10.2202/1557-4679.1181. The original C-TMLE.
# Gruber, S. & van der Laan, M. J. (2010) "An Application of
# Collaborative Targeted Maximum Likelihood Estimation in Causal
# Inference and Genomics", International Journal of Biostatistics
# 6(1), Article 18, doi:10.2202/1557-4679.1182.
#
# Native implementation mirroring Python morie.fn.tlctmle exactly:
# the same candidate-sequence construction with the same
# clever-covariate summary, the same targeted log-likelihood loss,
# the same Fisher-scoring fluctuation, the same V-fold
# cross-validated risk with the optional variance penalty, the
# same influence-curve diagnostic, and the same selection-criterion
# anchors.

#' morie_tlctmle
#'
#' A step of the tlctmle_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Passed to \code{ctmle}.
#' @param Y Passed to \code{ctmle}.
#' @param Q1 Passed to \code{ctmle}.
#' @param Q0 Passed to \code{ctmle}.
#' @param W Passed to \code{ctmle}.
#' @param g_models Passed to \code{ctmle}.
#' @param V Passed to \code{ctmle}. Defaults to \code{5L}.
#' @param seed Passed to \code{ctmle}. Defaults to \code{0L}.
#' @param penalty Passed to \code{ctmle}. Defaults to \code{TRUE}.
#' @return The value of \code{ctmle}.
#' @export
morie_tlctmle <- function(A, Y, Q1, Q0, W, g_models, V = 5L,
                          seed = 0L, penalty = TRUE) {
  ctmle(A, Y, Q1, Q0, W, g_models, V = V, seed = seed,
        penalty = penalty)
}

#' .ctmle_logit
#'
#' A step of the tlctmle_native implementation. Called by \code{ctmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.ctmle_logit <- function(p) {
  q <- min(max(as.numeric(p), 1e-9), 1 - 1e-9)
  log(q / (1 - q))
}

#' .ctmle_expit
#'
#' A step of the tlctmle_native implementation. Called by \code{.ctmle_fluct}, \code{ctmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.ctmle_expit <- function(x) {
  if (x > -700) 1 / (1 + exp(-x)) else 0
}

#' .ctmle_fluct
#'
#' A step of the tlctmle_native implementation. Called by \code{ctmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q Coerced to numeric by the body, with \code{as.numeric}.
#' @param H Coerced to numeric by the body, with \code{as.numeric}.
#' @param Y Coerced to numeric by the body, with \code{as.numeric}.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @return A list with \code{epsilon}, \code{q}.
#' @export
.ctmle_fluct <- function(Q, H, Y, iters = 60L) {
  q <- as.numeric(Q)
  h <- as.numeric(H)
  y <- as.numeric(Y)
  n <- length(q)
  off <- vapply(q, .ctmle_logit, numeric(1))
  e <- 0
  for (i in seq_len(iters)) {
    p <- vapply(seq_len(n), function(j)
      .ctmle_expit(off[j] + e * h[j]), numeric(1))
    gr <- sum(h * (y - p))
    he <- sum(h * h * p * (1 - p))
    if (he < 1e-12) break
    e <- e + gr / he
  }
  list(epsilon = e,
       q = vapply(seq_len(n), function(j)
         .ctmle_expit(off[j] + e * h[j]), numeric(1)))
}

#' targeted_loss
#'
#' A step of the tlctmle_native implementation. Called by \code{ctmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q_star Coerced to numeric by the body, with \code{as.numeric}.
#' @param Y Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
targeted_loss <- function(Q_star, Y) {
  q <- as.numeric(Q_star)
  y <- as.numeric(Y)
  n <- length(q)
  tot <- 0
  for (i in seq_len(n)) {
    p <- min(max(q[i], 1e-12), 1 - 1e-12)
    tot <- tot + (-(y[i] * log(p) + (1 - y[i]) * log(1 - p)))
  }
  tot / n
}

#' candidate_sequence
#'
#' A step of the tlctmle_native implementation. Called by \code{ctmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Coerced to numeric by the body, with \code{as.numeric}.
#' @param W A matrix; passed to \code{as.matrix}.
#' @param g_models A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
candidate_sequence <- function(A, W, g_models) {
  a <- as.numeric(A)
  rows <- as.matrix(W)
  if (is.null(dim(rows))) rows <- matrix(as.numeric(W), ncol = 1)
  out <- vector("list", length(g_models))
  for (k in seq_along(g_models)) {
    cols <- g_models[[k]]
    X <- rows[, cols, drop = FALSE]
    ok <- TRUE
    b <- rep(0, ncol(X) + 1L)
    tryCatch({
      # plain logistic regression by Fisher scoring
      X1 <- cbind(1, X)
      eta <- rep(0, nrow(X1))
      for (it in 1:25L) {
        p <- 1 / (1 + exp(-eta))
        p <- pmin(pmax(p, 1e-9), 1 - 1e-9)
        W <- p * (1 - p)
        z <- eta + (a - p) / W
        XtWX <- crossprod(X1, X1 * W)
        XtWz <- crossprod(X1, W * z)
        b_new <- as.numeric(solve(XtWX, XtWz))
        if (max(abs(b_new - b)) < 1e-10) { b <- b_new; break }
        b <- b_new
        eta <- as.numeric(X1 %*% b)
      }
      g <- as.numeric(1 / (1 + exp(-X1 %*% b)))
    }, error = function(e) {
      ok <<- FALSE
    })
    if (!ok) {
      m <- mean(a)
      g <- rep(m, length(a))
    }
    g <- pmin(pmax(g, 0.01), 0.99)
    out[[k]] <- list(covariates = as.integer(cols), g = g,
                     max_clever = max(1 / g, 1 / (1 - g)))
  }
  out
}

#' instrument_penalty
#'
#' A step of the tlctmle_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param g_small Coerced to numeric by the body, with \code{as.numeric}.
#' @param g_large Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{small}, \code{large}, \code{ratio}, \code{note}.
#' @export
instrument_penalty <- function(g_small, g_large) {
  gs <- as.numeric(g_small); gl <- as.numeric(g_large)
  a <- max(max(1 / gs), max(1 / (1 - gs)))
  b <- max(max(1 / gl), max(1 / (1 - gl)))
  list(small = a, large = b, ratio = b / a,
       note = "a pure instrument raises this without removing any bias")
}

#' ctmle
#'
#' A step of the tlctmle_native implementation. Called by \code{morie_tlctmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Coerced to numeric by the body, with \code{as.numeric}.
#' @param Y Coerced to numeric by the body, with \code{as.numeric}.
#' @param Q1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param Q0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param W Passed to \code{candidate_sequence}.
#' @param g_models Passed to \code{candidate_sequence}.
#' @param V Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.
#' @param penalty A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{psi}, \code{selected}, \code{selected_covariates}, \code{cv_risks}, \code{cv_losses}, \code{variance_penalties}, \code{penalized}, \code{epsilon}, \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic}, \code{max_clever_covariate}, \code{method}, \code{note}.
#' @export
ctmle <- function(A, Y, Q1, Q0, W, g_models, V = 5L, seed = 0L,
                  penalty = TRUE) {
  a <- as.numeric(A)
  y <- as.numeric(Y)
  q1 <- as.numeric(Q1)
  q0 <- as.numeric(Q0)
  n <- length(a)
  cands <- candidate_sequence(a, W, g_models)
  if (length(cands) == 0L)
    stop("tlctmle: no candidate treatment mechanisms")
  e_rng <- .ghc_rng(as.numeric(seed))
  idx <- seq_len(n)
  for (i in n:2) {
    j <- as.integer(.ghc_unif(e_rng, 1L) * (i + 1)) %% (i + 1)
    if (j == 0L) j <- 1L
    if (j == i) j <- i - 1L
    tmp <- idx[i]; idx[i] <- idx[j]; idx[j] <- tmp
  }
  folds <- lapply(seq_len(as.integer(V)), function(v)
    idx[seq(v, length(idx), by = as.integer(V))])
  risks <- losses <- pens <- numeric(length(cands))
  for (k in seq_along(cands)) {
    g <- cands[[k]]$g
    tot <- 0; m <- 0; ics <- numeric(0)
    for (f in folds) {
      tr <- setdiff(seq_len(n), f)
      H <- a[tr] / g[tr] - (1 - a[tr]) / (1 - g[tr])
      qa <- ifelse(a[tr] == 1, q1[tr], q0[tr])
      e <- .ctmle_fluct(qa, H, y[tr])$epsilon
      for (i in f) {
        h <- a[i] / g[i] - (1 - a[i]) / (1 - g[i])
        q <- if (a[i] == 1) q1[i] else q0[i]
        qs <- .ctmle_expit(.ctmle_logit(q) + e * h)
        tot <- tot + targeted_loss(qs, y[i])
        m <- m + 1
        q1i <- .ctmle_expit(.ctmle_logit(q1[i]) + e / g[i])
        q0i <- .ctmle_expit(.ctmle_logit(q0[i]) - e / (1 - g[i]))
        ics <- c(ics, h * (y[i] - qs) + q1i - q0i)
      }
    }
    mu <- mean(ics)
    var <- sum((ics - mu)^2) / (length(ics) - 1)
    losses[k] <- tot / m
    pens[k] <- var / n
    risks[k] <- tot / m + if (isTRUE(penalty)) var / n else 0
  }
  best <- which.min(risks) - 1L
  g <- cands[[best + 1L]]$g
  H <- a / g - (1 - a) / (1 - g)
  qa <- ifelse(a == 1, q1, q0)
  e <- .ctmle_fluct(qa, H, y)$epsilon
  q1s <- vapply(seq_len(n), function(i)
    .ctmle_expit(.ctmle_logit(q1[i]) + e / g[i]), numeric(1))
  q0s <- vapply(seq_len(n), function(i)
    .ctmle_expit(.ctmle_logit(q0[i]) - e / (1 - g[i])), numeric(1))
  psi <- mean(q1s - q0s)
  d <- numeric(n)
  for (i in seq_len(n)) {
    qas <- if (a[i] == 1) q1s[i] else q0s[i]
    d[i] <- H[i] * (y[i] - qas) + q1s[i] - q0s[i] - psi
  }
  m <- mean(d)
  se <- sqrt(sum((d - m)^2)) / n
  list(estimate = psi, psi = psi, selected = best,
       selected_covariates = cands[[best + 1L]]$covariates,
       cv_risks = risks, cv_losses = losses,
       variance_penalties = pens, penalized = isTRUE(penalty),
       epsilon = e, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       mean_eic = m, solves_eic = abs(m) < 1e-6,
       max_clever_covariate = max(abs(H)),
       method = "C-TMLE selecting g by the cross-validated loss of the TARGETED outcome fit; van der Laan & Rose (2018) Chap. 10",
       note = "collaboration changes WHICH g is targeted against, not whether the score equation is solved")
}

#' .tlctmle_cheatsheet
#'
#' A step of the tlctmle_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.tlctmle_cheatsheet <- function() {
  paste("tlctmle: fitting g as well as possible ON ITS OWN TERMS ",
        "is the wrong objective -- a covariate that predicts ",
        "treatment but not the outcome removes no bias and ",
        "inflates the clever covariate. Build an ORDERED sequence ",
        "of candidate g's (continuously indexed by a ",
        "variation-norm bound) and select by the cross-validated ",
        "loss of the TARGETED OUTCOME FIT, not by g's own fit. ",
        "That criterion rejects instruments automatically. The ",
        "selected C-TMLE still solves the efficient score ",
        "equation.", sep = "")
}
