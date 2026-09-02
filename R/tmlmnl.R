# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cross-fitted TMLE with pluggable machine learners for Q and g
#'
#' Cross-fitting is what makes an arbitrarily complex learner safe: each
#' observation's nuisance value comes from a fit that never saw it, so
#' the empirical-process term vanishes without a Donsker condition.  The
#' targeting step is the TMLE one, so the estimator remains a plug-in.
#'
#' Formula: with out-of-fold \code{Q} and \code{g},
#' \code{H = D/g - (1 - D)/(1 - g)}, \code{eps = sum H (y - Q)/sum H^2},
#' \code{psi = mean\[Q*(1, X) - Q*(0, X)\]}.  Folds are \code{i mod 5} on
#' the input order, so both language arms use the same partition.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param ml_q Function \code{(Xtrain, ytrain, Xtest)} returning
#'   predictions; \code{NULL} uses least squares.
#' @param ml_g Function \code{(Xtrain, Dtrain, Xtest)} returning
#'   probabilities; \code{NULL} uses logistic IRLS.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_folds}, \code{n}.
#' @references Chernozhukov, V. et al. (2018). Econometrics Journal
#'   21(1):C1-C68; van der Laan, M. J. & Rubin, D. (2006). IJB 2(1):11.
#' @export
#' @examples
#' Tmlmnl(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlmnl <- function(y, D, X, ml_q = NULL, ml_g = NULL) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  n <- length(yv)
  if (n == 0L || length(Dv) != n)
    stop("Tmlmnl: y and D must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlmnl: X must have one row per subject")
  fq <- if (is.null(ml_q)) function(Xtr, ytr, Xte)
    as.numeric(as.matrix(Xte) %*% .s4_ols(Xtr, ytr)$beta) else ml_q
  fg <- if (is.null(ml_g)) function(Xtr, dtr, Xte)
    .s4_expit(as.numeric(as.matrix(Xte) %*% .s4_glmbin(Xtr, dtr))) else ml_g
  K <- 5L
  W <- cbind(1, Xm)
  des <- cbind(Dv, W)
  g <- numeric(n)
  Qobs <- numeric(n)
  Q1 <- numeric(n)
  Q0 <- numeric(n)
  fold <- (seq_len(n) - 1L) %% K
  for (k in 0:(K - 1L)) {
    te <- which(fold == k)
    tr <- which(fold != k)
    if (length(te) == 0L || length(tr) < 2L) next
    gp <- fg(W[tr, , drop = FALSE], Dv[tr], W[te, , drop = FALSE])
    g[te] <- .s4_clip(as.numeric(gp), 0.025, 0.975)
    stack <- rbind(des[te, , drop = FALSE],
                   cbind(1, W[te, , drop = FALSE]),
                   cbind(0, W[te, , drop = FALSE]))
    qp <- as.numeric(fq(des[tr, , drop = FALSE], yv[tr], stack))
    m <- length(te)
    Qobs[te] <- qp[seq_len(m)]
    Q1[te] <- qp[m + seq_len(m)]
    Q0[te] <- qp[2L * m + seq_len(m)]
  }
  H <- Dv / g - (1 - Dv) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  psi <- sum(Q1s - Q0s) / n
  ic <- H * (yv - Qobs - eps * H) + Q1s - Q0s - psi
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, n_folds = K, n = n,
             method = "Cross-fitted TMLE with pluggable machine learners")
}
