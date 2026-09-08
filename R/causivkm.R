# SPDX-License-Identifier: AGPL-3.0-or-later
#' Moreira conditional likelihood-ratio weak-IV-robust test
#'
#' Source FETCHED: the reference implementation \code{CLR} in the CRAN
#' package \pkg{ivmodel} (file \code{R/CLR.r}), implementing Moreira,
#' M. J. (2003), Econometrica 71, 1027-1048, with the conditional
#' p-value integral of Andrews, Moreira and Stock (2007), Journal of
#' Econometrics 138, 46-81; Kleibergen, F. (2002), Econometrica 70,
#' 1781-1803, supplies the score decomposition.  With \code{P_Z} the
#' projection on the partialled-out instruments and
#' \code{sigmaHat = crossprod(M_Z \[Y D\]) / (n - k - L)},
#' \code{a0 = (beta0, 1)}, \code{b0 = (1, -beta0)}:
#' \code{QS = ||P_Z \[Y D\] b0||^2 / (b0' sigmaHat b0)},
#' \code{QT = ||P_Z \[Y D\] sigmaHat^-1 a0||^2 / (a0' sigmaHat^-1 a0)},
#' \code{QTS} the corresponding normalised cross term, and
#' \code{LR = (QS - QT + sqrt((QS+QT)^2 - 4(QS QT - QTS^2)))/2}.
#'
#' For \code{L = 1} the conditional p-value is \code{1 - pf(LR, 1, df2)}.
#' For \code{L >= 2} the package source integrates
#' \code{2 K int_0^1 pchisq((QT+LR)/(1 + QT x^2/LR), L) (1-x^2)^((L-3)/2) dx}
#' with \code{K = gamma(L/2)/(sqrt(pi) gamma((L-1)/2))}; the
#' substitution \code{x = sin(theta)} used here turns that into
#' \code{2 K int_0^{pi/2} pchisq((QT+LR)/(1 + QT sin^2(theta)/LR), L)
#' cos^(L-2)(theta) dtheta}, whose integrand is bounded and smooth for
#' every \code{L >= 2}, so the epsilon-regularised \code{L = 4} special
#' case in the package is not needed.  A fixed 4096-interval composite
#' Simpson rule is used, so the result is deterministic.
#'
#' @param y Numeric outcome of length n.
#' @param X Numeric endogenous regressor of length n.
#' @param Z Numeric n x L matrix of instruments.
#' @param beta0 Null value of the structural coefficient.  Default 0.
#' @param X_exog Optional n x q matrix of included exogenous covariates.
#' @param add_intercept Include an intercept.  Default TRUE.
#' @return list: statistic, p_value, QS, QT, QTS, beta0, n,
#'   n_instruments, df2, method.
#' @examples
#' set.seed(1)
#' Z <- matrix(rnorm(120), 60, 2)
#' D <- Z %*% c(1, 0.5) + rnorm(60)
#' Ivclr(1.5 * D + rnorm(60), D, Z, 1.5)$p_value
#' @export
Ivclr <- function(y, X, Z, beta0 = 0, X_exog = NULL, add_intercept = TRUE) {
  y <- as.numeric(y)
  n <- length(y)
  d <- as.numeric(X)
  Z <- as.matrix(Z)
  if (nrow(Z) != n) Z <- t(Z)
  C <- NULL
  if (add_intercept) C <- cbind(C, rep(1, n))
  if (!is.null(X_exog)) {
    Xe <- as.matrix(X_exog)
    if (nrow(Xe) != n) Xe <- t(Xe)
    C <- cbind(C, Xe)
  }
  k <- if (is.null(C)) 0L else ncol(C)
  L <- ncol(Z)
  df2 <- n - k - L
  if (L < 1 || df2 < 1) stop("need L >= 1 and n > k + L")
  po <- function(M) if (is.null(C)) M else M - C %*% qr.solve(C, M)
  YD <- po(cbind(y, d))
  Za <- po(Z)
  PZ <- Za %*% qr.solve(Za, YD)
  RZ <- YD - PZ
  sigma <- crossprod(RZ) / df2
  sinv <- solve(sigma)
  b0 <- c(1, -as.numeric(beta0))
  a0 <- c(as.numeric(beta0), 1)
  denomS <- as.numeric(t(b0) %*% sigma %*% b0)
  denomT <- as.numeric(t(a0) %*% sinv %*% a0)
  u <- as.numeric(PZ %*% b0)
  v <- as.numeric(PZ %*% (sinv %*% a0))
  QS <- sum(u * u) / denomS
  QT <- sum(v * v) / denomT
  QTS <- sum(u * v) / (sqrt(denomS) * sqrt(denomT))
  disc <- (QS + QT)^2 - 4 * (QS * QT - QTS^2)
  lr <- 0.5 * (QS - QT + sqrt(max(0, disc)))
  pval <- Morieclrp(lr, QT, L, df2)
  list(
    statistic = lr, p_value = pval, QS = QS, QT = QT, QTS = QTS,
    beta0 = as.numeric(beta0), n = n, n_instruments = L,
    df2 = as.integer(df2),
    method = "Moreira conditional LR weak-IV-robust test (Kleibergen 2002, Moreira 2003)"
  )
}

#' Conditional p-value for the CLR statistic
#'
#' Andrews, Moreira and Stock (2007) conditional p-value
#' \code{P(LR > m | Q_T = qT)}, evaluated with the \code{x = sin(theta)}
#' substitution and a fixed 4096-interval composite Simpson rule.  See
#' \code{\link{Ivclr}} for the full statement of the integral and its
#' source.
#'
#' @param m CLR statistic value.
#' @param qT Conditioning value of Q_T.
#' @param L Number of instruments.
#' @param df2 Denominator degrees of freedom, used only when L = 1.
#' @return Numeric p-value in \[0, 1\].
#' @examples
#' Morieclrp(2, 3, 2, 50)
#' @export
Morieclrp <- function(m, qT, L, df2) {
  if (L == 1) {
    return(stats::pf(m, 1, df2, lower.tail = FALSE))
  }
  if (m <= 0) {
    return(1)
  }
  K <- exp(lgamma(L / 2) - 0.5 * log(pi) - lgamma((L - 1) / 2))
  N <- 4096L
  h <- (pi / 2) / N
  th <- (0:N) * h
  s <- sin(th)
  arg <- (qT + m) / (1 + qT * s * s / m)
  val <- stats::pchisq(arg, L) * cos(th)^(L - 2)
  w <- rep(2, N + 1)
  w[seq(2, N, by = 2)] <- 4
  w[1] <- 1
  w[N + 1] <- 1
  integral <- sum(w * val) * h / 3
  min(1, max(0, 1 - 2 * K * integral))
}
