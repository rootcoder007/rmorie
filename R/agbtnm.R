# SPDX-License-Identifier: AGPL-3.0-or-later
#' Batch-normalisation running statistics and inference transform.
#'
#' runmean <- (1-momentum) runmean + momentum mean(x);
#' runvar  <- (1-momentum) runvar + momentum (m/(m-1)) biased_var(x);
#' y = gamma (x - runmean)/sqrt(runvar + eps) + beta.
#'
#' @param x Mini-batch of activations, length m >= 2.
#' @param runmean,runvar Running population moments.
#' @param momentum Weight given to the new batch, in \[0, 1\].
#' @param eps Numerical floor inside the square root.
#' @param gamma,beta Learned scale and shift.
#'
#' @return List with runmean, runvar, batchmean, batchvar, batchvarunb,
#'   normalized, trainnorm, m.
#' @references Ioffe and Szegedy (2015), arXiv:1502.03167, Sect. 3.1 and
#'   Algorithm 2.  Read from the ar5iv rendering; the same paper is in the
#'   local corpus.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Bnrunstat(V)
Bnrunstat <- function(x, runmean = 0, runvar = 1, momentum = 0.1,
                      eps = 1e-5, gamma = 1, beta = 0) {
  x <- .t1_vec(x); m <- length(x)
  if (m < 2) stop("need at least two activations to form a variance")
  momentum <- as.numeric(momentum)
  if (momentum < 0 || momentum > 1) stop("momentum must lie in [0, 1]")
  mu <- sum(x) / m
  vb <- sum((x - mu)^2) / m
  vu <- vb * m / (m - 1)
  rm <- (1 - momentum) * as.numeric(runmean) + momentum * mu
  rv <- (1 - momentum) * as.numeric(runvar) + momentum * vu
  g <- as.numeric(gamma); b <- as.numeric(beta); eps <- as.numeric(eps)
  .t1_result(runmean = rm, runvar = rv, batchmean = mu, batchvar = vb,
             batchvarunb = vu,
             normalized = g * (x - rm) / sqrt(rv + eps) + b,
             trainnorm = g * (x - mu) / sqrt(vb + eps) + b, m = m,
             method = "Batch-norm running statistics (Ioffe-Szegedy 2015 Sect. 3.1)")
}
