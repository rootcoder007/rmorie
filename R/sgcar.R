# SPDX-License-Identifier: AGPL-3.0-or-later
#' Valid interval for the CAR dependence parameter rho
#'
#' The joint distribution exists only where the precision matrix is
#' positive definite. The book states the bound through the eigenvalues
#' of W (eq 6.48): for Q = I - rho W the range is
#' (1/theta_min, 1/theta_max); for Q = D - rho W the same condition
#' applies to the eigenvalues of D^-1/2 W D^-1/2.
#'
#' @param W Symmetric adjacency weights (n by n).
#' @param parameterization "weighted" (Q = D - rho W) or "identity"
#'   (Q = I - rho W).
#' @return Numeric length-2 vector, the open interval (lo, hi).
#' @references Schabenberger & Gotway (2005), eq (6.48), p. 340.
#' @export
#' @examples
#' car_rho_bounds(W = 5L)
car_rho_bounds <- function(W, parameterization = "weighted") {
  W <- as.matrix(W)
  M <- if (identical(parameterization, "weighted")) {
    d <- rowSums(W)
    s <- ifelse(d > 0, 1 / sqrt(ifelse(d > 0, d, 1)), 0)
    (W * s) * rep(s, each = nrow(W))
  } else {
    W
  }
  ev <- eigen((M + t(M)) / 2, symmetric = TRUE, only.values = TRUE)$values
  c(if (min(ev) < 0) 1 / min(ev) else -Inf,
    if (max(ev) > 0) 1 / max(ev) else Inf)
}

#' Haining's least-squares estimator of the CAR dependence parameter
#'
#' rho_OLS = (e' W e) / (e' W^2 e), with e the OLS residual vector.
#' Unlike the SAR case, this estimator is CONSISTENT for a one-parameter
#' CAR model, so it is a principled estimate in its own right as well as
#' a starting value for maximum likelihood.
#'
#' @param Z Response, length n.
#' @param W Symmetric adjacency weights (n by n).
#' @param X Covariates (n by p); an intercept when NULL.
#' @return Numeric scalar.
#' @references Schabenberger & Gotway (2005), p. 340, citing
#'   Haining (1990), p. 130.
#' @export
#' @examples
#' car_rho_ols(Z = 5L, W = 5L)
car_rho_ols <- function(Z, W, X = NULL) {
  Z <- as.numeric(Z); W <- as.matrix(W)
  X <- if (is.null(X)) matrix(1, length(Z), 1) else as.matrix(X)
  e <- as.numeric(Z - X %*% qr.solve(X, Z))
  denom <- as.numeric(crossprod(e, W %*% (W %*% e)))
  if (abs(denom) < 1e-300) return(0)
  as.numeric(crossprod(e, W %*% e) / denom)
}

#' Conditional autoregressive (CAR) model, fitted by maximum likelihood
#'
#' The conditional specification (eqs 6.43-6.44) generates, by
#' Hammersley-Clifford, a valid joint Gaussian with mean X beta and
#' Sigma_CAR = (I - C)^-1 Sigma_c (eq 6.45).
#'
#' Two one-parameter forms are supported and they are NOT the same model:
#' "weighted" takes C = rho D^-1 W with Sigma_c = sigma^2 D^-1, giving
#' precision (D - rho W) / sigma^2 and conditional variance sigma^2 / d_i
#' -- inversely proportional to the number of neighbours, not constant.
#' "identity" takes C = rho W with Sigma_c = sigma^2 I, giving precision
#' (I - rho W) / sigma^2; this is the case the book carries into
#' estimation (eq 6.47) and its valid rho range is much narrower.
#'
#' For fixed rho the profile is beta = (X' Q X)^-1 X' Q Z,
#' sigma^2 = r' Q r / n, and
#' l(rho) = 1/2 log|Q| - n/2 log(sigma^2) - n/2, maximised over the VALID
#' interval for rho. That interval includes zero and negative values: a
#' CAR fit that cannot return rho <= 0 cannot represent independence or
#' competition, and will report spatial dependence in data that has none.
#'
#' @param Z Response, length n.
#' @param W Symmetric adjacency weights (n by n).
#' @param X Covariates (n by p); an intercept when NULL.
#' @param parameterization "weighted" (default) or "identity".
#' @return Named list: name, statistic (ML rho), p_value (NULL), extra
#'   with beta, sigma2, tau2 (alias), loglik, rho_ols, rho_bounds,
#'   parameterization.
#' @references Schabenberger & Gotway (2005), Sec 6.2.2.2, eqs
#'   (6.43)-(6.48), pp. 338-341; Besag (1974); Haining (1990), p. 130.
#' @examples
#' n <- 20
#' W <- matrix(0, n, n); W[cbind(1:(n - 1), 2:n)] <- 1; W <- W + t(W)
#' sgcar(sin(seq_len(n) * 0.7), W)
#' @export
sgcar <- function(Z, W, X = NULL, parameterization = "weighted") {
  Z <- as.numeric(Z)
  W <- as.matrix(W)
  n <- length(Z)
  if (nrow(W) != n || ncol(W) != n) {
    stop("`W` must be ", n, " by ", n, " to match `Z`")
  }
  if (!isTRUE(all.equal(W, t(W), tolerance = 1e-10))) {
    stop("`W` must be symmetric: an asymmetric C gives a non-symmetric ",
         "precision and no valid joint distribution (Hammersley-Clifford). ",
         "Use parameterization = \"weighted\" rather than passing a ",
         "row-standardised W.")
  }
  if (!parameterization %in% c("weighted", "identity")) {
    stop("`parameterization` must be \"weighted\" or \"identity\"")
  }
  X <- if (is.null(X)) matrix(1, n, 1) else as.matrix(X)
  if (nrow(X) != n) stop("`X` must have one row per element of `Z`")

  D <- diag(rowSums(W), nrow = n)
  prec <- function(rho) {
    if (identical(parameterization, "weighted")) D - rho * W else diag(n) - rho * W
  }
  b <- car_rho_bounds(W, parameterization)
  lo <- b[1]; hi <- b[2]
  eps <- 1e-6 * max(hi - lo, 1e-12)

  neg_ll <- function(rho) {
    Q <- prec(rho)
    dt <- determinant(Q, logarithm = TRUE)
    if (dt$sign <= 0) return(Inf)
    beta <- try(solve(crossprod(X, Q %*% X), crossprod(X, Q %*% Z)), silent = TRUE)
    if (inherits(beta, "try-error")) return(Inf)
    r <- Z - as.numeric(X %*% beta)
    s2 <- as.numeric(crossprod(r, Q %*% r)) / n
    if (s2 <= 0) return(Inf)
    -(0.5 * as.numeric(dt$modulus) - 0.5 * n * log(s2) - 0.5 * n)
  }

  o <- stats::optimize(neg_ll, lower = lo + eps, upper = hi - eps,
                       tol = 1e-10 * max(hi - lo, 1))
  rho <- o$minimum
  if (!is.finite(neg_ll(rho))) {
    warning("CAR likelihood is not finite anywhere in the valid rho ",
            "interval; falling back to rho = 0 (independence). The fit is ",
            "OLS, not a spatial model.", call. = FALSE)
    rho <- 0
  }

  Q <- prec(rho)
  beta <- as.numeric(solve(crossprod(X, Q %*% X), crossprod(X, Q %*% Z)))
  r <- Z - as.numeric(X %*% beta)
  s2 <- as.numeric(crossprod(r, Q %*% r)) / n
  dt <- determinant(Q, logarithm = TRUE)
  loglik <- 0.5 * as.numeric(dt$modulus) - 0.5 * n * log(s2) - 0.5 * n -
    0.5 * n * log(2 * pi)

  list(name = "conditional_autoregressive", statistic = rho, p_value = NULL,
       extra = list(beta = beta, sigma2 = s2, tau2 = s2, loglik = loglik,
                    rho_ols = car_rho_ols(Z, W, X), rho_bounds = c(lo, hi),
                    parameterization = parameterization))
}
