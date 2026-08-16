# MinT: optimal reconciliation of hierarchical forecasts.
#
# Sources:
#   Wickramasuriya, S. L., Athanasopoulos, G. & Hyndman, R. J. (2019)
#   "Optimal Forecast Reconciliation for Hierarchical and Grouped Time
#   Series Through Trace Minimization", JASA 114(526), 804-819.
#   Hyndman, R. J., Ahmed, R. A., Athanasopoulos, G. & Shang, H. L.
#   (2011) "Optimal combination forecasts for hierarchical time series",
#   CSDA 55(9), 2579-2589.
#   Schafer, J. & Strimmer, K. (2005) "A Shrinkage Approach to
#   Large-Scale Covariance Matrix Estimation and Implications for
#   Functional Genomics", SAGMB 4(1), article 32.
#   Penrose, R. (1956) "On best approximate solutions of linear matrix
#   equations", Math. Proc. Cambridge Phil. Soc. 52(1), 17-19.

#' summing_matrix
#'
#' A step of the hierF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param groups A vector; its length is taken and its elements indexed.
#' @param n_bottom A matrix; passed to \code{diag}.
#' @return The value of \code{rbind}.
#' @export
summing_matrix <- function(groups, n_bottom) {
  if (n_bottom < 1L) stop("hierF: need at least one bottom series")
  S <- matrix(0.0, length(groups), n_bottom)
  for (r in seq_along(groups)) {
    for (i in groups[[r]]) {
      if (i < 0L || i >= n_bottom) {
        stop(sprintf("hierF: bottom index %d out of range", i))
      }
      S[r, i + 1L] <- 1.0
    }
  }
  bot <- diag(1.0, n_bottom, n_bottom)
  rbind(S, bot)
}

#' is_coherent
#'
#' A step of the hierF_native implementation. Called by \code{mint_reconcile}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; indexed elementwise.
#' @param S A matrix; passed to \code{nrow}.
#' @param tol Passed to \code{<=}. Defaults to \code{1e-09}.
#' @return A logical value.
#' @export
is_coherent <- function(y, S, tol = 1e-9) {
  m <- nrow(S)
  n <- ncol(S)
  b <- y[(m - n + 1L):m]
  all(abs(y[1:m] - as.numeric(S %*% b)) <= tol)
}

#' shrink_covariance
#'
#' A step of the hierF_native implementation. Called by \code{mint_P}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param residuals A matrix; passed to \code{nrow}.
#' @param lam Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{cov}, \code{lambda}.
#' @export
shrink_covariance <- function(residuals, lam = NULL) {
  T <- nrow(residuals)
  if (T < 2L) {
    stop(sprintf("hierF: need at least 2 residual rows, got %d", T))
  }
  m <- ncol(residuals)
  mu <- colMeans(residuals)
  R <- scale(residuals, center = mu, scale = FALSE)
  Sig <- crossprod(R) / (T - 1L)
  D <- diag(diag(Sig))
  if (is.null(lam)) {
    off_sum <- 0.0
    for (a in 1:m) {
      for (b in 1:m) {
        if (a != b) off_sum <- off_sum + Sig[a, b]^2
      }
    }
    var <- 0.0
    for (a in 1:m) {
      for (b in 1:m) {
        if (a == b) next
        w <- R[, a] * R[, b]
        wm <- mean(w)
        var <- var + sum((w - wm)^2) * T / (T - 1)^3
      }
    }
    lam <- if (off_sum <= 1e-12) 1.0 else min(1.0, max(0.0, var / off_sum))
  }
  Sig_shr <- (1.0 - lam) * Sig + lam * D
  list(cov = Sig_shr, lambda = lam)
}

#' ._cholsolve
#'
#' A step of the hierF_native implementation. Called by \code{mint_P}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{solve}.
#' @param b A matrix; passed to \code{solve}.
#' @return A vector, from \code{as.numeric}.
#' @export
._cholsolve <- function(A, b) {
  A <- as.matrix(A)
  # chol() returns the UPPER factor U with A = t(U) %*% U, so the
  # forward solve must use t(U) and the back solve U. Passing U to
  # forwardsolve and t(U) to backsolve, as this did, silently solves a
  # different system: for a 3x3 it returned b[1]/A[1,1] and zeros.
  U <- tryCatch(chol(A), error = function(e) NULL)
  if (!is.null(U)) {
    return(as.numeric(backsolve(U, forwardsolve(t(U), b))))
  }
  as.numeric(solve(A, b))
}

#' mint_P
#'
#' A step of the hierF_native implementation. Called by \code{mint_reconcile}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param S A matrix; indexed by row and column.
#' @param W Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param method One of \code{"custom"}, \code{"ols"}, \code{"shrink"}, \code{"wls"}. Defaults to \code{"shrink"}.
#' @param residuals Optional; may be \code{NULL}. A matrix; passed to \code{nrow}.
#' @param ridge A matrix; passed to \code{diag}. Defaults to \code{1e-10}.
#' @return A list with \code{P}, \code{lambda}.
#' @export
mint_P <- function(S, W = NULL, method = "shrink", residuals = NULL,
                   ridge = 1e-10) {
  if (!(method %in% c("ols", "wls", "shrink", "custom"))) {
    stop(sprintf("hierF: method must be ols, wls, shrink or custom, got %r", method))
  }
  m <- nrow(S)
  n <- ncol(S)
  lam <- NULL
  if (method == "ols") {
    Wm <- diag(1.0, m, m)
  } else if (method == "wls") {
    if (is.null(residuals)) stop("hierF: wls needs residuals")
    T <- nrow(residuals)
    v <- pmax(colMeans(residuals^2), 1e-12)
    Wm <- diag(v, m, m)
  } else if (method == "shrink") {
    if (is.null(residuals)) stop("hierF: shrink needs residuals")
    sc <- shrink_covariance(residuals)
    Wm <- sc$cov
    lam <- sc$lambda
  } else {
    if (is.null(W)) stop("hierF: method='custom' needs W")
    Wm <- as.matrix(W)
  }
  WmR <- Wm + diag(ridge, m, m)
  Winv_S <- matrix(0.0, m, n)
  for (j in 1:n) {
    Winv_S[, j] <- ._cholsolve(WmR, S[, j])
  }
  A <- crossprod(S, Winv_S)
  AR <- A + diag(ridge, n, n)
  P <- matrix(0.0, n, m)
  for (i in 1:n) {
    e <- numeric(n)
    e[i] <- 1.0
    row <- ._cholsolve(AR, e)
    # row is length n and Winv_S is m x n, so row %*% Winv_S does
    # not conform. MinT wants row i of P to be W^-1 S A^-1 e_i,
    # i.e. sum_j row[j] * Winv_S[a][j] for each a -- Winv_S %*% row
    P[i, ] <- as.numeric(Winv_S %*% row)
  }
  list(P = P, lambda = lam)
}

#' mint_reconcile
#'
#' A step of the hierF_native implementation. Called by \code{morie_hierF}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param base Coerced to numeric by the body, with \code{as.numeric}.
#' @param S A matrix; passed to \code{as.matrix}.
#' @param method Carried through into a list the body builds. Defaults to \code{"shrink"}.
#' @param residuals Passed to \code{mint_P}.
#' @param W Passed to \code{mint_P}.
#' @param ridge Passed to \code{mint_P}. Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{reconciled}, \code{bottom}, \code{base}, \code{P}, \code{S}, \code{method}, \code{shrinkage}, \code{n_series}, \code{n_bottom}, \code{coherent}, \code{ps_identity_error}, \code{adjustment}, \code{cite}, \code{method_detail}.
#' @export
mint_reconcile <- function(base, S, method = "shrink", residuals = NULL,
                           W = NULL, ridge = 1e-10) {
  Sm <- as.matrix(S)
  m <- nrow(Sm)
  n <- ncol(Sm)
  yb <- as.numeric(base)
  if (length(yb) != m) {
    stop(sprintf("hierF: %d base forecasts for %d series",
                 length(yb), m))
  }
  out <- mint_P(Sm, W = W, method = method, residuals = residuals,
                ridge = ridge)
  P <- out$P
  lam <- out$lambda
  b <- as.numeric(P %*% yb)
  rec <- as.numeric(Sm %*% b)
  PS <- P %*% Sm
  ps_err <- max(abs(PS - diag(1.0, n, n)))
  list(estimate = rec, reconciled = rec, bottom = b, base = yb,
       P = P, S = Sm, method = method, shrinkage = lam,
       n_series = m, n_bottom = n, coherent = is_coherent(rec, Sm),
       ps_identity_error = ps_err,
       adjustment = rec - yb,
       cite = "MinT, Wickramasuriya, Athanasopoulos & Hyndman (2019)",
       method_detail = "P = (S' W^-1 S)^-1 S' W^-1")
}

mintreconcile <- mint_reconcile
hierarchical_forecast <- mint_reconcile

#' morie_hierF
#'
#' A step of the hierF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param base Passed to \code{mint_reconcile}.
#' @param S Passed to \code{mint_reconcile}.
#' @param method Passed to \code{mint_reconcile}. Defaults to \code{"shrink"}.
#' @param residuals Passed to \code{mint_reconcile}.
#' @param W Passed to \code{mint_reconcile}.
#' @param ridge Passed to \code{mint_reconcile}. Defaults to \code{1e-10}.
#' @return The value of \code{mint_reconcile}.
#' @export
morie_hierF <- function(base, S, method = "shrink", residuals = NULL,
                        W = NULL, ridge = 1e-10) {
  mint_reconcile(base, S, method, residuals, W, ridge)
}

#' .hierF_cheatsheet
#'
#' A step of the hierF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.hierF_cheatsheet <- function() {
  paste("hierF: y = S b, reconcile with ytilde = S P yhat where P = (S'W^-1 S)^-1 S'W^-1 minimises tr(P W P') subject to PS = I (MinT). PS = I makes SP a PROJECTION -- an already coherent forecast is left alone. Wrong P still adds up, because S forces that; it is just the wrong coherent point. W: ols=I, wls=diag, shrink=the paper's default because the full covariance is singular when m > T.")
}
