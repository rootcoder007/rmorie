# morie.fn -- function file (rootcoder007/morie)
# Native R port of linwlr: weighted linear learner for a structural nested
# mean model. See Robins (2004) "Optimal structural nested models for
# optimal sequential decisions", in Lin & Heagerty (eds.), Proceedings of
# the Second Seattle Symposium in Biostatistics, Springer LNS 179,
# doi:10.1007/978-1-4419-9076-1_11. Foundational SNMM and g-estimation:
# Robins (1994) doi:10.1080/03610929408831393. See also Hernan & Robins
# (2020) Causal Inference: What If, Ch. 14.

# ---- private helpers (namespaced to avoid collisions across R/) ----

#' .linwlr_vec
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.linwlr_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(x)
}

#' .linwlr_mat
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A matrix, from \code{as.matrix}.
#' @export
.linwlr_mat <- function(x) {
  if (is.null(x)) return(matrix(0, nrow = 0, ncol = 0))
  as.matrix(x)
}

#' .linwlr_design
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param Zsrc See Usage.
#' @param n See Usage.
#' @return The value of \code{cbind}.
#' @export
.linwlr_design <- function(Zsrc, n) {
  if (is.null(Zsrc)) {
    return(matrix(1, nrow = n, ncol = 1))
  }
  cbind(1, Zsrc)
}

#' .linwlr_sigmoid
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @return A numeric value.
#' @export
.linwlr_sigmoid <- function(v) {
  1 / (1 + exp(-v))
}

#' .linwlr_logit_irls
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param Z See Usage.
#' @param y See Usage.
#' @param max_iter See Usage.
#' @param ridge See Usage.
#' @return The value of \code{beta}, as built in the body.
#' @export
.linwlr_logit_irls <- function(Z, y, max_iter, ridge) {
  n <- nrow(Z)
  q <- ncol(Z)
  beta <- rep(0, q)
  for (iter in seq_len(max_iter)) {
    eta <- as.vector(Z %*% beta)
    mu <- .linwlr_sigmoid(eta)
    mu <- pmin(pmax(mu, 1e-15), 1 - 1e-15)
    w <- mu * (1 - mu)
    A_mat <- crossprod(Z, w * Z) + ridge * diag(q)
    b_vec <- crossprod(Z, w * (eta + (y - mu) / w))
    beta_new <- solve(A_mat, b_vec)
    if (max(abs(beta_new - beta)) < 1e-8) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  beta
}

#' .linwlr_lstsq
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param Z See Usage.
#' @param y See Usage.
#' @param ridge See Usage.
#' @return A matrix, from \code{solve}.
#' @export
.linwlr_lstsq <- function(Z, y, ridge) {
  q <- ncol(Z)
  A <- crossprod(Z) + ridge * diag(q)
  b <- crossprod(Z, y)
  solve(A, b)
}

#' .linwlr_ridgesolve
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @param rhs See Usage.
#' @param ridge See Usage.
#' @return A matrix, from \code{solve}.
#' @export
.linwlr_ridgesolve <- function(M, rhs, ridge) {
  q <- nrow(M)
  A <- M + ridge * diag(q)
  solve(A, as.numeric(rhs))
}

#' .linwlr_wls
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param w See Usage.
#' @return A list with \code{coef}, \code{se}, \code{resid}.
#' @export
.linwlr_wls <- function(X, y, w) {
  X <- cbind(1, X)
  n <- nrow(X)
  k <- ncol(X)
  WX <- w * X
  XWX <- crossprod(X, WX) + 1e-10 * diag(k)
  XWy <- crossprod(X, w * y)
  coef <- solve(XWX, XWy)
  resid <- y - as.vector(X %*% coef)
  sigma2 <- sum(w * resid^2) / max(1, n - k)
  var_beta <- sigma2 * solve(XWX)
  list(coef = as.numeric(coef), se = sqrt(diag(var_beta)), resid = resid)
}

#' .linwlr_sandwich_se
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param bread See Usage.
#' @param meat See Usage.
#' @param n See Usage.
#' @param ridge See Usage.
#' @return The value of \code{se}, as built in the body.
#' @export
.linwlr_sandwich_se <- function(bread, meat, n, ridge) {
  q <- nrow(bread)
  inv_mat <- solve(bread + ridge * diag(q))
  se <- numeric(q)
  for (a in seq_len(q)) {
    inv_a <- inv_mat[, a]
    var_a <- as.numeric(t(inv_a) %*% meat %*% inv_a) / n
    se[a] <- if (var_a > 0) sqrt(var_a) else NaN
  }
  se
}

# ---- public API ----

#' morie_linwlr_blip
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param w See Usage.
#' @param psi See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
morie_linwlr_blip <- function(a, w, psi) {
  av <- .linwlr_vec(a)
  if (is.null(w)) return(av * psi[1])
  Wm <- .linwlr_mat(w)
  p <- ncol(Wm)
  if (p == 0L) return(av * psi[1])
  as.numeric(av * (psi[1] + as.numeric(Wm %*% psi[2:(1 + p)])))
}

#' morie_linwlr
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param A See Usage.
#' @param W Defaults to \code{NULL}.
#' @param propensity Defaults to \code{NULL}.
#' @param method Defaults to \code{"gest"}.
#' @param baseline Defaults to \code{NULL}.
#' @param pi_covariates Defaults to \code{NULL}.
#' @param ridge Defaults to \code{1e-10}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_linwlr <- function(y, A, W = NULL, propensity = NULL, method = "gest",
                          baseline = NULL, pi_covariates = NULL,
                          ridge = 1e-10) {
  if (!(method %in% c("gest", "wls"))) {
    stop(sprintf("morie_linwlr: method must be 'gest' or 'wls', got %s",
                 sQuote(method)))
  }
  yv <- .linwlr_vec(y)
  av <- .linwlr_vec(A)
  n <- length(yv)
  if (length(av) != n) {
    stop(sprintf("morie_linwlr: %d outcomes but %d treatments",
                 n, length(av)))
  }
  Wm <- if (is.null(W)) matrix(0, nrow = n, ncol = 0) else .linwlr_mat(W)
  if (nrow(Wm) != n) {
    stop(sprintf("morie_linwlr: %d outcomes but %d history rows",
                 n, nrow(Wm)))
  }
  binary <- all(av %in% c(0, 1))

  if (is.null(propensity)) {
    if (!is.null(pi_covariates)) {
      Zsrc <- .linwlr_mat(pi_covariates)
      if (nrow(Zsrc) != n) {
        stop(sprintf("morie_linwlr: %d propensity covariate rows for %d observations",
                     nrow(Zsrc), n))
      }
    } else if (ncol(Wm) > 0L) {
      Zsrc <- Wm
    } else {
      Zsrc <- NULL
    }
    Z <- .linwlr_design(Zsrc, n)
    if (binary) {
      coefs <- .linwlr_logit_irls(Z, av, 60, ridge)
      pi <- .linwlr_sigmoid(as.numeric(Z %*% coefs))
    } else {
      coefs <- .linwlr_lstsq(Z, av, ridge)
      pi <- as.numeric(Z %*% coefs)
    }
  } else {
    pi <- as.numeric(propensity)
    if (length(pi) != n) {
      stop(sprintf("morie_linwlr: %d propensities for %d observations",
                   length(pi), n))
    }
  }
  if (binary && any(pi <= 0 | pi >= 1)) {
    stop("morie_linwlr: a propensity of 0 or 1 violates positivity and makes the blip unidentified there")
  }

  ytilde <- yv
  if (!is.null(baseline)) {
    Zb <- .linwlr_design(.linwlr_mat(baseline), n)
    bb <- .linwlr_lstsq(Zb, yv, ridge)
    fitted <- as.numeric(Zb %*% bb)
    ytilde <- yv - fitted
  }

  p <- ncol(Wm)

  # The blip is identified only through the treatment residual A - E[A|W].
  # A propensity model that reproduces the treatment exactly makes that
  # residual zero, the estimating equation 0 = 0, and psi whatever the ridge
  # returns -- so the two arms would disagree on a meaningless number.
  resid_a <- av - pi
  scale_a <- if (length(av)) max(abs(av)) else 0
  if (max(abs(resid_a)) <= 1e-9 * max(1, scale_a)) {
    stop(paste0("morie_linwlr: the propensity model reproduces the treatment ",
                "exactly, so A - E[A | W] is zero and the blip is ",
                "UNIDENTIFIED -- any psi solves the estimating equation. This ",
                "usually means the treatment was passed as one of its own ",
                "covariates, or pi_covariates determines it. Supply a ",
                "propensity that leaves variation in A."))
  }

  if (method == "gest") {
    basis <- cbind(1, Wm)
    cen <- av - pi
    q <- p + 1
    tmp <- cen * av * basis
    M <- crossprod(basis, tmp)
    rhs <- as.numeric(crossprod(basis, cen * ytilde))
    diag(M) <- diag(M) + ridge
    psi <- solve(M, rhs)
    pred <- av * as.numeric(basis %*% psi)
    resid <- ytilde - pred
    bread <- crossprod(basis, cen * av * basis) / n
    tmp3 <- cen * resid * basis
    meat <- crossprod(tmp3) / n
    se <- .linwlr_sandwich_se(bread, meat, n, ridge)
  } else {
    w <- if (binary) {
      ifelse(av > 0.5, 1 / pi, 1 / (1 - pi))
    } else {
      rep(1, n)
    }
    X <- cbind(av, av * Wm, Wm)
    fit <- .linwlr_wls(X, ytilde, w)
    psi <- c(fit$coef[2], fit$coef[3:(2 + p)])
    se <- c(fit$se[2], fit$se[3:(2 + p)])
    resid <- fit$resid
  }

  blip_vals <- if (p > 0L) {
    as.numeric(av * (psi[1] + Wm %*% psi[2:(1 + p)]))
  } else {
    av * psi[1]
  }

  result <- list(
    estimate = psi[1],
    se = if (length(se) > 0) se[1] else NaN,
    psi = psi,
    psi_se = se,
    propensity = pi,
    residual = resid,
    blip = blip_vals,
    binary_treatment = binary,
    method_used = method,
    n = n,
    method = sprintf("linear blip by %s, Robins (2004) optimal structural nested models",
                     if (method == "gest") "g-estimation" else "weighted least squares")
  )
  class(result) <- "morie_richresult"
  result
}

#' morie_linwlr_cheatsheet
#'
#' Part of the linwlr_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_linwlr_cheatsheet <- function() {
  paste0("linwlr: linear blip gamma(a,w) = a(psi0 + psi1'w) by ",
         "g-estimation on A - E[A|W] (Robins 2004), or by IP-weighted ",
         "least squares. Consistent if the PROPENSITY is right; no ",
         "double-robustness claimed without an outcome model.")
}

# compact alias per ledger/NAMING.md
morie_linwlr_linearweightedlearner <- morie_linwlr
