# HLM random-effects covariance (T matrix) estimation.
# Source: Raudenbush & Bryk (2002), Hierarchical Linear Models,
# 2nd ed., Sage, Ch. 3, Eqs. 3.28 (total = parameter + error
# dispersion) and 3.57 (multivariate reliability) (fetched-wave3/
# Hierarchical_Linear_Models_Applications_and_Data_Analysis_Methods.pdf).
# Mirrors Python morie.fn.hlmgr exactly (same PSD eigenvalue clip).

#' HLM T (tau) matrix by method of moments, with EB shrinkage
#'
#' T_hat = S - V_bar (sample covariance of per-group OLS coefficients
#' minus average sampling covariance), clipped to positive
#' semidefinite; per-group multivariate reliability
#' Lambda_j = T (T + V_j)^\{-1\} and empirical Bayes composites
#' beta*_j = Lambda_j beta_hat_j + (I - Lambda_j) gamma_hat.
#'
#' @param betas Matrix (J x q) of per-group OLS coefficients.
#' @param V Optional list of J sampling covariance matrices.
#' @return A list with elements \code{tau}, \code{gamma},
#'   \code{reliabilities}, \code{shrunken}, \code{s_total}, \code{J},
#'   \code{method}.
#' @references Raudenbush, S. W. and Bryk, A. S. (2002).
#'   Hierarchical Linear Models, 2nd ed. Sage, Ch. 3.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_hlmgr(V)
morie_hlmgr <- function(betas, V = NULL) {
  B <- as.matrix(betas)
  J <- nrow(B)
  if (J < 3) stop("need at least three groups")
  q <- ncol(B)
  Vs <- if (is.null(V)) {
    replicate(J, matrix(0, q, q), simplify = FALSE)
  } else {
    lapply(V, as.matrix)
  }
  if (length(Vs) != J) stop("need one V_j per group")
  gamma <- colMeans(B)
  S <- stats::cov(B)
  vbar <- Reduce(`+`, Vs) / J
  raw <- S - vbar
  ee <- eigen((raw + t(raw)) / 2, symmetric = TRUE)
  tau <- ee$vectors %*% diag(pmax(ee$values, 0), q) %*% t(ee$vectors)
  lams <- vector("list", J)
  shrunk <- matrix(0, J, q)
  for (j in seq_len(J)) {
    lam <- tau %*% solve(tau + Vs[[j]])
    lams[[j]] <- lam
    shrunk[j, ] <- as.numeric(lam %*% B[j, ] + (diag(q) - lam) %*% gamma)
  }
  list(tau = tau, gamma = gamma, reliabilities = lams,
       shrunken = shrunk, s_total = S, J = J,
       method = "HLM T matrix, MoM (R&B 2002 Eqs. 3.28, 3.57)")
}
