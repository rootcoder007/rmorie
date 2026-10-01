.pk_cov <- function(h, model) ifelse(h == 0, model[1], 0) + model[2] * exp(-h / model[3])

#' Probability kriging
#'
#' Ordinary cokriging of the threshold indicator with the uniform (rank)
#' transform (Sullivan 1984), with exponential direct and cross covariances
#' given as c(nugget, sill, range). Identical to the Python arm
#' \code{morie.fn.probkrig}.
#'
#' @param coords Data coordinates (two-column).
#' @param values Data values.
#' @param targets Prediction locations (two-column).
#' @param threshold Threshold z_k.
#' @param cov_i,cov_u,cov_iu Indicator, uniform and cross covariances.
#' @return A list with the estimates, clipped probabilities and weights.
#' @references Sullivan, J. (1984). Conditional recovery estimation through
#'   probability kriging: theory and practice. Geostatistics for Natural
#'   Resources Characterization, Reidel, 365-384.
#'
#'   Goovaerts, P. (1997). Geostatistics for Natural Resources Evaluation.
#'   Oxford University Press.
#' @examples
#' P <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1))
#' ProbabilityKriging(P, c(1, 3, 2, 4), rbind(c(0.5, 0.5)), 2.5,
#'                    c(0, 0.25, 1), c(0, 0.08, 1), c(0, 0.1, 1))$estimate
#' @export
ProbabilityKriging <- function(coords, values, targets, threshold, cov_i, cov_u, cov_iu) {
  P <- as.matrix(coords)
  Tg <- as.matrix(targets)
  n <- nrow(P)
  ind <- as.numeric(values <= threshold)
  rank <- numeric(n)
  rank[order(values, seq_len(n))] <- (seq_len(n) - 0.5) / n
  D <- as.matrix(stats::dist(P))
  K <- matrix(0, 2 * n + 2, 2 * n + 2)
  K[1:n, 1:n] <- .pk_cov(D, cov_i)
  K[n + 1:n, n + 1:n] <- .pk_cov(D, cov_u)
  K[1:n, n + 1:n] <- .pk_cov(D, cov_iu)
  K[n + 1:n, 1:n] <- .pk_cov(D, cov_iu)
  K[1:n, 2 * n + 1] <- 1
  K[2 * n + 1, 1:n] <- 1
  K[n + 1:n, 2 * n + 2] <- 1
  K[2 * n + 2, n + 1:n] <- 1
  est <- numeric(nrow(Tg))
  wts <- vector("list", nrow(Tg))
  for (q in seq_len(nrow(Tg))) {
    d <- sqrt(colSums((t(P) - Tg[q, ])^2))
    rhs <- c(ifelse(d > 0, .pk_cov(d, cov_i), cov_i[1] + cov_i[2]), ifelse(d > 0, .pk_cov(d, cov_iu), cov_iu[1] + cov_iu[2]), 1, 0)
    w <- solve(K, rhs)
    est[q] <- sum(w[1:n] * ind) + sum(w[n + 1:n] * rank)
    wts[[q]] <- w[1:(2 * n)]
  }
  list(estimate = est, probability = pmin(pmax(est, 0), 1), weights = wts, uniform = rank, indicator = ind)
}
