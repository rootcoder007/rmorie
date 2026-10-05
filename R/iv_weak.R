# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Weak-instrument diagnostics computed from the data, not looked up in a
# table: the Kleibergen-Paap rk statistic (and its Wald F) and the
# Montiel Olea-Pflueger effective F with its Patnaik critical values.
# Both are robust to heteroskedasticity, clustering and (MOP) serial
# correlation, where the Stock-Yogo (2005) tables assume iid errors.

# Residualise every column of `m` on the included exogenous regressors (and
# the constant): the Frisch-Waugh step both statistics start from.
#' @noRd
.ivw_partial <- function(m, W) {
  m <- as.matrix(m)
  qr.resid(qr(W), m)
}

#' @noRd
.ivw_design <- function(data, cols) {
  if (!length(cols)) return(matrix(numeric(0), nrow(data), 0L))
  mm <- stats::model.matrix(stats::as.formula(paste("~", paste(cols, collapse = " + "))), data)
  mm[, colnames(mm) != "(Intercept)", drop = FALSE]
}

#' @noRd
.ivw_prepare <- function(data, outcome, endogenous, instruments, exogenous, cluster) {
  cols <- unique(c(outcome, endogenous, instruments, exogenous, cluster))
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop("column(s) not in `data`: ", paste(miss, collapse = ", "), call. = FALSE)
  data <- data[stats::complete.cases(data[, cols, drop = FALSE]), , drop = FALSE]
  n <- nrow(data)
  W <- cbind(1, .ivw_design(data, exogenous))
  Z <- .ivw_design(data, instruments)
  K <- ncol(Z)
  if (K < 1L) stop("at least one excluded instrument is needed", call. = FALSE)
  if (n <= K + ncol(W)) stop("fewer observations than instruments and exogenous regressors", call. = FALSE)
  list(n = n, L = ncol(W) - 1L, K = K,
       y = if (length(outcome)) .ivw_partial(as.numeric(data[[outcome]]), W),
       X = .ivw_partial(as.matrix(data[, endogenous, drop = FALSE]), W),
       Z = .ivw_partial(Z, W),
       cl = if (length(cluster)) data[[cluster]])
}

# Long-run variance of the scores s_i (rows), divided by n: iid uses the
# Kronecker shortcut supplied by the caller; robust, cluster and HAC sum
# outer products of the scores (HAC with Bartlett weights 1 - j/(lag+1)).
#' @noRd
.ivw_meat <- function(S, vcov, cl = NULL, lag = 0L) {
  n <- nrow(S)
  if (vcov == "cluster") {
    G <- rowsum(S, cl, reorder = FALSE)
    return(crossprod(G) / n)
  }
  M <- crossprod(S) / n
  if (vcov == "hac" && lag > 0L) {
    for (j in seq_len(lag)) {
      Gj <- crossprod(S[(j + 1L):n, , drop = FALSE], S[seq_len(n - j), , drop = FALSE]) / n
      M <- M + (1 - j / (lag + 1)) * (Gj + t(Gj))
    }
  }
  M
}

#' Kleibergen-Paap rk statistic and rk Wald F for weak identification
#'
#' The Kleibergen and Paap (2006) rank test of the first-stage coefficient
#' matrix \eqn{\Pi} (instruments by endogenous regressors), computed natively:
#' after partialling out the included exogenous regressors and the constant,
#' \eqn{\hat\Theta = G \hat\Pi F^{-1}} with \eqn{G'G = Z'Z/n} and
#' \eqn{F'F = X'X/n} (Cholesky factors), its singular value decomposition gives
#' the projections \eqn{A_{q\perp}, B_{q\perp}} and
#' \eqn{rk = n\,\hat\lambda'\hat V_\lambda^{-1}\hat\lambda}, chi-square with
#' \eqn{(K-q)(k-q)} degrees of freedom under rank \eqn{q = k - 1}
#' (underidentification). The covariance of \eqn{vec(\hat\Pi)} is the
#' sandwich of the chosen \code{vcov}, so the statistic is valid under
#' heteroskedasticity or clustering, where Cragg-Donald is not. The rk Wald F
#' is \eqn{rk/n \times (n - L)/K} (\eqn{L} all instruments with the constant;
#' clustered: \eqn{rk/(n-1) (n-L)(G-1)/G / K}), the scaling ivreg2 reports.
#' With one endogenous regressor it is the robust Wald F of the first stage.
#'
#' Compare it with the conservative rule of thumb of 10 (Staiger and Stock
#' 1997) or, for one endogenous regressor, test it properly with
#' \code{\link{morie_iv_montiel_olea_pflueger}}; Stock-Yogo critical values
#' assume iid errors and do not apply to the robust statistic.
#'
#' @inheritParams morie_iv_params
#' @param vcov \code{"robust"} (default), \code{"iid"} or \code{"cluster"}.
#' @param cluster Cluster column, required when \code{vcov = "cluster"}.
#' @return A list with \code{statistic} (the rk Wald F), \code{chi2_statistic}
#'   (rk), \code{df}, \code{p_value} (of the underidentification test),
#'   \code{rule_of_thumb} (10), \code{weak} (\code{statistic < 10}),
#'   \code{name}, \code{vcov} and \code{details}.
#' @references Kleibergen, F. and Paap, R. (2006). Generalized reduced rank
#'   tests using the singular value decomposition. \emph{Journal of
#'   Econometrics} 133(1), 97-126. Staiger, D. and Stock, J. H. (1997).
#'   Instrumental variables regression with weak instruments.
#'   \emph{Econometrica} 65(3), 557-586.
#' @examples
#' set.seed(1)
#' n <- 300
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' out <- morie_iv_kleibergen_paap(df, "d", "z")
#' c(out$statistic, out$p_value)
#' @export
morie_iv_kleibergen_paap <- function(data, endogenous, instruments,
                                     exogenous = NULL,
                                     vcov = c("robust", "iid", "cluster"),
                                     cluster = NULL) {
  vcov <- match.arg(vcov)
  if (vcov == "cluster" && !length(cluster)) stop("vcov = \"cluster\" needs `cluster`", call. = FALSE)
  p <- .ivw_prepare(data, NULL, endogenous, instruments, exogenous,
                    if (vcov == "cluster") cluster)
  n <- p$n; K <- p$K; k <- ncol(p$X); X <- p$X; Z <- p$Z
  if (K < k) stop("fewer excluded instruments than endogenous regressors", call. = FALSE)
  Qzz <- crossprod(Z) / n
  Qxx <- crossprod(X) / n
  Pi <- solve(Qzz, crossprod(Z, X) / n)
  V <- X - Z %*% Pi
  Rz <- chol(Qzz)
  Rx <- chol(Qxx)
  Theta <- Rz %*% Pi %*% solve(Rx)
  S <- if (vcov == "iid") {
    kronecker(crossprod(V) / n, Qzz)
  } else {
    scores <- do.call(cbind, lapply(seq_len(k), function(j) V[, j] * Z))
    .ivw_meat(scores, vcov, p$cl)
  }
  Tm <- kronecker(t(solve(Rx)), t(solve(Rz)))
  Vtheta <- Tm %*% S %*% t(Tm)
  q <- k - 1L
  sv <- svd(Theta, nu = K, nv = k)
  u <- sv$u; v <- sv$v
  msqrt <- function(m) {
    e <- eigen(m, symmetric = TRUE)
    e$vectors %*% diag(sqrt(pmax(e$values, 0)), nrow(m)) %*% t(e$vectors)
  }
  idx_u <- (q + 1L):K
  idx_v <- (q + 1L):k
  u22 <- u[idx_u, idx_u, drop = FALSE]
  v22 <- v[idx_v, idx_v, drop = FALSE]
  aq <- u[, idx_u, drop = FALSE] %*% solve(u22) %*% msqrt(u22 %*% t(u22))
  bq <- msqrt(v22 %*% t(v22)) %*% solve(t(v22)) %*% t(v[, idx_v, drop = FALSE])
  B <- kronecker(bq, t(aq))
  lam <- B %*% as.vector(Theta)
  Vlam <- B %*% Vtheta %*% t(B)
  rk <- n * as.numeric(crossprod(lam, solve(Vlam, lam)))
  dfree <- (K - q) * (k - q)
  L_all <- K + p$L + 1L
  Fw <- if (vcov == "cluster") {
    G <- length(unique(p$cl))
    rk / (n - 1) * (n - L_all) * (G - 1) / G / K
  } else {
    rk / n * (n - L_all) / K
  }
  list(statistic = Fw,
       chi2_statistic = rk,
       df = dfree,
       p_value = stats::pchisq(rk, dfree, lower.tail = FALSE),
       rule_of_thumb = 10,
       weak = Fw < 10,
       name = "Kleibergen-Paap rk Wald F",
       vcov = vcov,
       details = list(n = n, K = K, k = k, singular_values = sv$d,
                      n_exogenous = p$L))
}

# Patnaik approximation of the effective-F critical value (MOP 2013, eq. 9 and
# appendix): the eigenvalues w of W2 (normalised to sum to one) give the
# effective degrees of freedom K_eff = 2(1+2x) / (2 sum w^2 + 4 x max w), and
# the critical value is the (1-alpha) quantile of chi2(K_eff, x K_eff) / K_eff.
#' @noRd
.ivw_patnaik <- function(W2, alpha, x) {
  w <- Re(eigen(W2, only.values = TRUE)$values)
  w <- w / sum(w)
  K_eff <- 2 * (1 + 2 * x) / (2 * sum(w^2) + 4 * x * max(w))
  c(cv = stats::qchisq(1 - alpha, df = K_eff, ncp = K_eff * x) / K_eff, K_eff = K_eff)
}

# Worst-case Nagar bias of TSLS at a given beta (MOP 2013, Theorem 1 bound,
# maximised analytically over the direction): the Bmaxfunction of weakivtest.
#' @noRd
.ivw_btsls_at <- function(beta, W1, W12, W2) {
  S12 <- W12 - beta * W2
  S1 <- W1 - 2 * beta * W12 + beta^2 * W2
  ev <- eigen((S12 + t(S12)) / 2, symmetric = TRUE, only.values = TRUE)$values
  base <- sum(diag(S12)) / sqrt(sum(diag(W2)) * sum(diag(S1)))
  max(abs(base * (1 - 2 * min(ev) / sum(diag(S12)))),
      abs(base * (1 - 2 * max(ev) / sum(diag(S12)))))
}

#' @noRd
.ivw_bliml_at <- function(beta, W1, W12, W2, Om) {
  S12 <- W12 - beta * W2
  S1 <- W1 - 2 * beta * W12 + beta^2 * W2
  sig12 <- Om[1, 2] - beta * Om[2, 2]
  sig1 <- Om[1, 1] - 2 * beta * Om[1, 2] + beta^2 * Om[2, 2]
  M <- 2 * S12 - sig12 / sig1 * S1
  ev <- eigen((M + t(M)) / 2, symmetric = TRUE, only.values = TRUE)$values
  base <- sum(diag(S12)) - sig12 / sig1 * sum(diag(S1))
  den <- sqrt(sum(diag(W2)) * sum(diag(S1)))
  max(abs((base - min(ev)) / den), abs((base - max(ev)) / den))
}

# The supremum over beta: widen [-b, b] until both ends are within eps of the
# beta -> +-infinity limit, scan 10,000 points, then polish the best point
# with a one-dimensional search between its grid neighbours (weakivtest uses
# Nelder-Mead from the best grid point).
#' @noRd
.ivw_sup_beta <- function(f, limit, eps = 1e-3, points = 10000L) {
  off <- function(b) max(abs(f(b) / limit - 1), abs(f(-b) / limit - 1))
  b <- 1
  while (off(b) > eps && b < 1e6) b <- b + 1
  grid <- seq(-b, b, length.out = points + 1L)
  vals <- vapply(grid, f, numeric(1))
  i <- which.max(vals)
  h <- 2 * b / points
  opt <- stats::optimize(f, c(grid[i] - h, grid[i] + h), maximum = TRUE)
  max(vals[i], opt$objective, limit)
}

#' Montiel Olea-Pflueger effective F test for weak instruments
#'
#' The weak-instrument test of Montiel Olea and Pflueger (2013) for one
#' endogenous regressor, robust to heteroskedasticity, clustering and serial
#' correlation. After partialling out the included exogenous regressors and
#' the constant, the instruments are orthonormalised (\eqn{Z'Z = nI}) and
#' \deqn{F_{eff} = \frac{Y_2' P_Z Y_2}{tr(\hat W_2)},}
#' where \eqn{\hat W_2} is the (robust, clustered or HAC) variance of
#' \eqn{n^{-1/2}\sum_i Z_i v_{2i}} from the first stage, with the
#' \eqn{n/(n-K-L-1)} degrees-of-freedom correction. Under homoskedasticity
#' \eqn{F_{eff}} is the ordinary first-stage F.
#'
#' The null hypothesis is that the Nagar bias exceeds a fraction \eqn{\tau}
#' of a worst-case benchmark. Critical values come from the Patnaik
#' approximation of the noncentral chi-square: \eqn{x = 1/\tau} for the
#' simplified (conservative) test, \eqn{x = B/\tau} with the estimated
#' worst-case bias bound \eqn{B} for the generalized TSLS and LIML tests.
#' Everything is computed from the data; no table is consulted.
#' Reject weak instruments when \eqn{F_{eff}} exceeds the critical value.
#'
#' @inheritParams morie_iv_params
#' @param outcome Outcome column (needed for the generalized TSLS/LIML bounds).
#' @param vcov \code{"robust"} (default), \code{"iid"}, \code{"cluster"} or
#'   \code{"hac"} (Bartlett kernel).
#' @param cluster Cluster column for \code{vcov = "cluster"}.
#' @param lag HAC truncation lag (default \eqn{\lfloor 4(n/100)^{2/9} \rfloor}).
#' @param alpha Test size (default 0.05).
#' @param tau Worst-case bias thresholds (default 5, 10, 20 and 30 percent).
#' @return A list with \code{F_eff}, \code{critical_values} (a data frame:
#'   \code{tau}, \code{simplified}, \code{tsls}, \code{liml} and the matching
#'   effective degrees of freedom), \code{B_tsls}, \code{B_liml}, \code{reject}
#'   (logical per \code{tau}, generalized TSLS), \code{n}, \code{K},
#'   \code{vcov} and \code{alpha}.
#' @references Montiel Olea, J. L. and Pflueger, C. (2013). A robust test for
#'   weak instruments. \emph{Journal of Business and Economic Statistics}
#'   31(3), 358-369. Pflueger, C. E. and Wang, S. (2015). A robust test for
#'   weak instruments in Stata. \emph{Stata Journal} 15(1), 216-225.
#' @examples
#' set.seed(2)
#' n <- 500
#' z1 <- rnorm(n); z2 <- rnorm(n); u <- rnorm(n)
#' d <- 0.3 * z1 + 0.2 * z2 + u * (1 + abs(z1)) / 2
#' y <- 0.5 * d + u + rnorm(n)
#' mop <- morie_iv_montiel_olea_pflueger(data.frame(y, d, z1, z2), "y", "d",
#'                                       c("z1", "z2"))
#' mop$F_eff
#' mop$critical_values
#' @export
morie_iv_montiel_olea_pflueger <- function(data, outcome, endogenous, instruments,
                                           exogenous = NULL,
                                           vcov = c("robust", "iid", "cluster", "hac"),
                                           cluster = NULL, lag = NULL, alpha = 0.05,
                                           tau = c(0.05, 0.10, 0.20, 0.30)) {
  vcov <- match.arg(vcov)
  if (length(endogenous) != 1L) {
    stop("the Montiel Olea-Pflueger test is for one endogenous regressor; ",
         "use morie_iv_kleibergen_paap() with several", call. = FALSE)
  }
  if (vcov == "cluster" && !length(cluster)) stop("vcov = \"cluster\" needs `cluster`", call. = FALSE)
  p <- .ivw_prepare(data, outcome, endogenous, instruments, exogenous,
                    if (vcov == "cluster") cluster)
  n <- p$n; K <- p$K; L <- p$L
  dof <- n - K - L - 1
  # orthonormal instruments, Z'Z = n I
  Q <- qr.Q(qr(p$Z)) * sqrt(n)
  x2 <- as.numeric(p$X)
  e1 <- as.numeric(qr.resid(qr(Q), p$y))   # reduced form of the outcome
  e2 <- as.numeric(qr.resid(qr(Q), x2))    # first stage
  Om <- crossprod(cbind(e1, e2)) / dof
  Wfull <- if (vcov == "iid") {
    kronecker(crossprod(cbind(e1, e2)) / n, crossprod(Q) / n) * n / dof
  } else {
    if (vcov != "hac") lag <- 0L else if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
    .ivw_meat(cbind(e1 * Q, e2 * Q), vcov, p$cl, as.integer(lag)) * n / dof
  }
  W1 <- Wfull[1:K, 1:K, drop = FALSE]
  W12 <- Wfull[1:K, K + (1:K), drop = FALSE]
  W2 <- Wfull[K + (1:K), K + (1:K), drop = FALSE]
  adj <- if (vcov == "cluster") {
    G <- length(unique(p$cl))
    (n / (n - 1)) * (G - 1) / G
  } else 1
  F_eff <- adj * sum(crossprod(Q, x2)^2) / n / sum(diag(W2))
  ev2 <- eigen(W2, symmetric = TRUE, only.values = TRUE)$values
  B_tsls <- .ivw_sup_beta(function(b) .ivw_btsls_at(b, W1, W12, W2),
                          limit = 1 - 2 * min(ev2) / sum(ev2))
  B_liml <- .ivw_sup_beta(function(b) .ivw_bliml_at(b, W1, W12, W2, Om),
                          limit = max(ev2) / sum(ev2))
  cvs <- do.call(rbind, lapply(tau, function(t) {
    s <- .ivw_patnaik(W2, alpha, 1 / t)
    g <- .ivw_patnaik(W2, alpha, B_tsls / t)
    l <- .ivw_patnaik(W2, alpha, B_liml / t)
    data.frame(tau = t, simplified = s[["cv"]], tsls = g[["cv"]], liml = l[["cv"]],
               K_eff_simplified = s[["K_eff"]], K_eff_tsls = g[["K_eff"]],
               K_eff_liml = l[["K_eff"]])
  }))
  list(F_eff = F_eff,
       critical_values = cvs,
       B_tsls = B_tsls,
       B_liml = B_liml,
       reject = stats::setNames(F_eff > cvs$tsls, paste0("tau_", 100 * tau)),
       n = n, K = K, vcov = vcov, alpha = alpha,
       name = "Montiel Olea-Pflueger effective F")
}
