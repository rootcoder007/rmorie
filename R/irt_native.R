# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native item-response-theory engines (feat/native-specializations,
# module 18): two-parameter logistic (2PL) model by Bock-Aitkin
# marginal maximum likelihood EM, Samejima's graded response model
# (GRM), and expected-a-posteriori (EAP) ability scoring. No mirt/ltm
# at runtime; tests/cross validates against mirt where installed.

#' Internal helper: normal quadrature grid
#' @noRd
.morie_irt_quad <- function(n_quad = 41L, range = 4) {
  theta <- seq(-range, range, length.out = n_quad)
  w <- stats::dnorm(theta)
  list(theta = theta, w = w / sum(w))
}

#' Fit a two-parameter logistic (2PL) IRT model
#'
#' Bock-Aitkin marginal maximum likelihood: the E-step computes each
#' respondent's posterior over a fixed normal quadrature grid; the
#' M-step refits each item's discrimination/difficulty by weighted
#' logistic Newton-Raphson on the expected counts. Abilities are
#' returned as EAP scores.
#'
#' @param responses A 0/1 matrix or data frame (rows = persons,
#'   columns = items). NAs are allowed and are ignored item-wise.
#' @param n_quad Number of quadrature points (default 41).
#' @param max_iter Maximum EM iterations.
#' @param tol Convergence tolerance on the marginal log-likelihood.
#' @return An object of class \code{morie_irt_2pl}: a list with
#'   \code{discrimination} (a), \code{difficulty} (b), \code{loglik},
#'   \code{n_iter}, \code{converged}, \code{theta} (EAP scores),
#'   \code{theta_se}, \code{n_persons}, \code{n_items}, \code{method}.
#' @references Bock, R. D., & Aitkin, M. (1981). Marginal maximum
#'   likelihood estimation of item parameters. \emph{Psychometrika},
#'   46(4), 443--459.
#' @examples
#' set.seed(1)
#' th <- rnorm(300)
#' a <- c(1, 1.5, 0.8); b <- c(-0.5, 0, 0.5)
#' X <- sapply(1:3, function(j) rbinom(300, 1, plogis(a\[j\] * (th - b\[j\]))))
#' fit <- morie_irt_2pl(X)
#' fit$difficulty
#' @export
morie_irt_2pl <- function(responses, n_quad = 41L, max_iter = 200L,
                          tol = 1e-6) {
  X <- as.matrix(responses)
  storage.mode(X) <- "double"
  if (!all(X %in% c(0, 1) | is.na(X))) {
    stop("`responses` must be binary 0/1 (NAs allowed).", call. = FALSE)
  }
  n <- nrow(X); k <- ncol(X)
  q <- .morie_irt_quad(n_quad)
  Q <- length(q$theta)
  a <- rep(1, k)
  b <- as.numeric(-stats::qlogis(pmin(pmax(colMeans(X, na.rm = TRUE),
                                           0.02), 0.98)))
  obs <- !is.na(X)
  X0 <- X; X0[!obs] <- 0
  ll_old <- -Inf
  n_iter <- 0L
  converged <- FALSE
  post <- NULL
  for (it in seq_len(max_iter)) {
    n_iter <- it
    # E-step: person x quadrature log-likelihood
    P <- stats::plogis(outer(q$theta, b, "-") *
                         matrix(a, Q, k, byrow = TRUE))  # Q x k
    logP <- log(pmax(P, 1e-12)); log1P <- log(pmax(1 - P, 1e-12))
    LL <- X0 %*% t(logP) + (obs - X0) %*% t(log1P)       # n x Q
    LL <- sweep(LL, 2L, log(q$w), "+")
    m <- apply(LL, 1L, max)
    W <- exp(LL - m)
    denom <- rowSums(W)
    post <- W / denom                                     # n x Q
    ll <- sum(m + log(denom))
    # Expected counts per item x quadrature
    for (j in seq_len(k)) {
      oj <- obs[, j]
      nq <- colSums(post[oj, , drop = FALSE])             # trials
      rq <- colSums(post[oj & X[, j] == 1, , drop = FALSE])
      # Weighted logistic Newton for (a_j, b_j): P = plogis(a(th-b))
      par <- c(a[j], b[j])
      for (nr in 1:25) {
        eta <- par[1] * (q$theta - par[2])
        pj <- stats::plogis(eta)
        # gradient wrt (a, b)
        resid <- rq - nq * pj
        g <- c(sum(resid * (q$theta - par[2])),
               sum(resid * (-par[1])))
        wv <- nq * pj * (1 - pj)
        H11 <- -sum(wv * (q$theta - par[2])^2)
        H22 <- -sum(wv * par[1]^2)
        H12 <- -sum(wv * (-par[1]) * (q$theta - par[2])) -
          sum(resid)
        H <- matrix(c(H11, H12, H12, H22), 2L)
        step <- tryCatch(solve(H, g), error = function(e) g * 0)
        par_new <- par - step
        par_new[1] <- min(max(par_new[1], 0.05), 8)
        par_new[2] <- min(max(par_new[2], -6), 6)
        if (max(abs(par_new - par)) < 1e-8) { par <- par_new; break }
        par <- par_new
      }
      a[j] <- par[1]; b[j] <- par[2]
    }
    if (abs(ll - ll_old) < tol * max(1, abs(ll))) {
      converged <- TRUE
      ll_old <- ll
      break
    }
    ll_old <- ll
  }
  # Recompute the posterior at the FINAL item parameters (the loop's
  # posterior lags one M-step behind), so EAP scores match
  # morie_irt_eap() on the same data exactly.
  P <- stats::plogis(outer(q$theta, b, "-") *
                       matrix(a, Q, k, byrow = TRUE))
  LL <- X0 %*% t(log(pmax(P, 1e-12))) +
    (obs - X0) %*% t(log(pmax(1 - P, 1e-12)))
  LL <- sweep(LL, 2L, log(q$w), "+")
  m <- apply(LL, 1L, max)
  W <- exp(LL - m)
  post <- W / rowSums(W)
  ll_old <- sum(m + log(rowSums(W)))
  eap <- as.numeric(post %*% q$theta)
  eap_var <- as.numeric(post %*% q$theta^2) - eap^2
  structure(
    list(discrimination = stats::setNames(a, colnames(X)),
         difficulty = stats::setNames(b, colnames(X)),
         loglik = ll_old, n_iter = n_iter, converged = converged,
         theta = eap, theta_se = sqrt(pmax(eap_var, 0)),
         n_persons = n, n_items = k,
         method = "2PL Bock-Aitkin EM (morie native)"),
    class = c("morie_irt_2pl", "list"))
}

#' Fit Samejima's graded response model (GRM)
#'
#' Marginal maximum likelihood EM for ordered polytomous items:
#' \eqn{P(X \ge c \mid \theta) = \mathrm{logit}^{-1}(a(\theta - b_c))}
#' with ordered thresholds. The M-step maximizes each item's expected
#' complete-data log-likelihood with \code{stats::optim} on an
#' order-preserving parameterization.
#'
#' @param responses Integer matrix / data frame of ordered categories
#'   (1..C per item; NAs allowed).
#' @param n_quad,max_iter,tol EM controls as in
#'   \code{\link{morie_irt_2pl}}.
#' @return An object of class \code{morie_irt_grm}: list with
#'   \code{discrimination}, \code{thresholds} (list of ordered
#'   b-vectors), \code{loglik}, \code{n_iter}, \code{converged},
#'   \code{theta}, \code{theta_se}, \code{method}.
#' @references Samejima, F. (1969). Estimation of latent ability using
#'   a response pattern of graded scores. \emph{Psychometrika
#'   Monograph}, 17.
#' @examples
#' set.seed(64)
#' n <- 1200; k <- 4; a_true <- c(1.2, 1.5, 1.0, 1.8); th <- rnorm(n)
#' X <- vapply(seq_len(k), function(j) {
#'   bj <- sort(runif(2, -1, 1))
#'   u <- runif(n)
#'   1L + (u < plogis(a_true\[j\] * (th - bj\[1\]))) + (u < plogis(a_true\[j\] * (th - bj\[2\])))
#' }, integer(n))
#' morie_irt_grm(X)
#' @export
morie_irt_grm <- function(responses, n_quad = 31L, max_iter = 100L,
                          tol = 1e-5) {
  X <- as.matrix(responses)
  storage.mode(X) <- "integer"
  n <- nrow(X); k <- ncol(X)
  q <- .morie_irt_quad(n_quad)
  Q <- length(q$theta)
  n_cat <- apply(X, 2L, function(z) max(z, na.rm = TRUE))
  if (any(n_cat < 2L)) stop("Every item needs >= 2 categories.",
                            call. = FALSE)
  # parameters: a_j > 0, thresholds b_j (length C_j - 1, increasing)
  a <- rep(1, k)
  bs <- lapply(seq_len(k), function(j) {
    props <- cumsum(rev(table(factor(X[, j], levels = seq_len(n_cat[j])))))
    props <- rev(props)[-1] / sum(!is.na(X[, j]))
    sort(as.numeric(-stats::qlogis(pmin(pmax(props, 0.05), 0.95))))
  })
  cat_prob <- function(a_j, b_j, theta) {
    # Q x C matrix of category probabilities
    Pge <- cbind(1, stats::plogis(outer(theta, b_j, "-") * a_j), 0)
    Pge[, seq_len(ncol(Pge) - 1L), drop = FALSE] -
      Pge[, -1L, drop = FALSE]
  }
  ll_old <- -Inf; converged <- FALSE; n_iter <- 0L; post <- NULL
  for (it in seq_len(max_iter)) {
    n_iter <- it
    LLq <- matrix(0, n, Q)
    for (j in seq_len(k)) {
      Pc <- cat_prob(a[j], bs[[j]], q$theta)      # Q x C
      oj <- which(!is.na(X[, j]))
      LLq[oj, ] <- LLq[oj, ] + t(log(pmax(Pc[, X[oj, j]], 1e-12)))
    }
    LLq <- sweep(LLq, 2L, log(q$w), "+")
    m <- apply(LLq, 1L, max)
    W <- exp(LLq - m)
    denom <- rowSums(W)
    post <- W / denom
    ll <- sum(m + log(denom))
    # M-step per item via optim on (log a, b1, log-diffs)
    for (j in seq_len(k)) {
      oj <- which(!is.na(X[, j]))
      # expected count of (quadrature, category)
      Ec <- vapply(seq_len(n_cat[j]), function(c_) {
        colSums(post[oj[X[oj, j] == c_], , drop = FALSE])
      }, numeric(Q))                              # Q x C
      to_par <- function(a_j, b_j) c(log(a_j), b_j[1L],
                                     log(diff(c(b_j[1L], b_j))[-1L] + 1e-9))
      from_par <- function(p) {
        a_j <- exp(p[1L])
        nb <- length(p) - 1L
        b_j <- if (nb == 1L) p[2L] else cumsum(c(p[2L], exp(p[-(1:2)])))
        list(a = a_j, b = b_j)
      }
      negll <- function(p) {
        pp <- from_par(p)
        Pc <- cat_prob(pp$a, pp$b, q$theta)
        -sum(Ec * log(pmax(Pc, 1e-12)))
      }
      opt <- tryCatch(
        stats::optim(to_par(a[j], bs[[j]]), negll, method = "BFGS",
                     control = list(maxit = 100L)),
        error = function(e) NULL)
      if (!is.null(opt)) {
        pp <- from_par(opt$par)
        a[j] <- min(max(pp$a, 0.05), 8)
        bs[[j]] <- pmin(pmax(pp$b, -6), 6)
      }
    }
    if (abs(ll - ll_old) < tol * max(1, abs(ll))) {
      converged <- TRUE; ll_old <- ll; break
    }
    ll_old <- ll
  }
  eap <- as.numeric(post %*% q$theta)
  eap_var <- as.numeric(post %*% q$theta^2) - eap^2
  structure(
    list(discrimination = stats::setNames(a, colnames(X)),
         thresholds = stats::setNames(bs, colnames(X)),
         loglik = ll_old, n_iter = n_iter, converged = converged,
         theta = eap, theta_se = sqrt(pmax(eap_var, 0)),
         n_persons = n, n_items = k,
         method = "GRM Samejima EM (morie native)"),
    class = c("morie_irt_grm", "list"))
}

#' EAP ability scores for new respondents under a fitted 2PL model
#'
#' @param fit A \code{\link{morie_irt_2pl}} fit (or any list with
#'   \code{discrimination} and \code{difficulty}).
#' @param responses 0/1 matrix of new response patterns.
#' @param n_quad Quadrature points.
#' @return A data frame with \code{theta} (EAP) and \code{se}.
#' @examples
#' set.seed(63)
#' th <- rnorm(1000)
#' a <- c(0.8, 1.2, 1.6, 1.0, 1.4); b <- c(-1, -0.5, 0, 0.5, 1)
#' X <- vapply(seq_along(a), function(j) rbinom(1000, 1, plogis(a\[j\] * (th - b\[j\]))), numeric(1000))
#' fit <- morie_irt_2pl(X)
#' morie_irt_eap(fit, X)$theta\[1:5\]
#' @export
morie_irt_eap <- function(fit, responses, n_quad = 41L) {
  X <- as.matrix(responses)
  storage.mode(X) <- "double"
  a <- as.numeric(fit$discrimination)
  b <- as.numeric(fit$difficulty)
  q <- .morie_irt_quad(n_quad)
  Q <- length(q$theta)
  k <- length(a)
  obs <- !is.na(X)
  X0 <- X; X0[!obs] <- 0
  P <- stats::plogis(outer(q$theta, b, "-") *
                       matrix(a, Q, k, byrow = TRUE))
  LL <- X0 %*% t(log(pmax(P, 1e-12))) +
    (obs - X0) %*% t(log(pmax(1 - P, 1e-12)))
  LL <- sweep(LL, 2L, log(q$w), "+")
  m <- apply(LL, 1L, max)
  W <- exp(LL - m)
  post <- W / rowSums(W)
  eap <- as.numeric(post %*% q$theta)
  v <- as.numeric(post %*% q$theta^2) - eap^2
  data.frame(theta = eap, se = sqrt(pmax(v, 0)))
}
