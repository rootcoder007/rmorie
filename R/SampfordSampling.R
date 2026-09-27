.sampford_cum <- function(w) {
  out <- numeric(length(w))
  acc <- 0
  tot <- 0
  for (t in w) tot <- tot + t
  for (k in seq_along(w)) {
    acc <- acc + w[k] / tot
    out[k] <- acc
  }
  out
}

.sampford_joint <- function(pik, n) {
  N <- length(pik)
  p <- pik / n
  lam <- p / (1 - n * p)
  L <- numeric(n)
  L[1] <- 1
  if (n >= 2) {
    for (i in 2:n) {
      acc <- 0
      for (r in 1:(i - 1)) acc <- acc + (-1)^(r - 1) * sum(lam^r) * L[i - r]
      L[i] <- acc / (i - 1)
    }
  }
  if (any(L < 0)) stop("joint inclusion probabilities cannot be computed for these pik", call. = FALSE)
  m <- seq_len(n)
  Kn <- 1 / sum((n + 1 - m) * L[m] / n^(n + 1 - m))
  P <- diag(pik, N)
  for (i in seq_len(N)) {
    for (j in seq_len(i - 1)) {
      L2 <- numeric(n - 1)
      L2[1] <- 1
      if (n > 2) L2[2] <- L[2] - (lam[i] + lam[j])
      if (n > 3) for (k in 3:(n - 1)) L2[k] <- L[k] - (lam[i] + lam[j]) * L2[k - 1] - lam[i] * lam[j] * L2[k - 2]
      t <- seq_len(n - 1)
      P[i, j] <- P[j, i] <- Kn * lam[i] * lam[j] * sum((t + 1 - n * (p[i] + p[j])) * L2[n - t] / n^(t - 1))
    }
  }
  P
}

#' Sampford's unequal-probability sampling without replacement
#'
#' With target inclusion probabilities \eqn{\pi_i} (\eqn{0 < \pi_i < 1},
#' summing to an integer n) the rejective procedure draws one unit with
#' probability \eqn{\pi_i / n} and n - 1 further units with replacement with
#' probabilities proportional to \eqn{\pi_i / (1 - \pi_i)}, accepting the
#' first attempt whose n units are distinct; the inclusion probabilities are
#' then exactly \eqn{\pi_i} (Sampford 1967). The joint inclusion
#' probabilities follow Sampford's formula through the elementary symmetric
#' functions of \eqn{\lambda_i = p_i / (1 - n p_i)}, \eqn{p_i = \pi_i / n}.
#' Units with \eqn{\pi_i} within 1e-6 of 0 or 1 are fixed out or in. Every
#' attempt uses n Philox uniforms from stream \code{attempt}, so the R and
#' Python arms draw the same sample.
#'
#' @param pik Inclusion probabilities summing to an integer n >= 2.
#' @param seed Philox key.
#' @param max_iter Maximum rejective attempts.
#' @return List with \code{sample} (0/1 indicator), \code{joint} (matrix of
#'   joint inclusion probabilities, \eqn{\pi_i} on the diagonal),
#'   \code{attempts} and \code{n}.
#' @references Sampford, M. R. (1967). On sampling without replacement with
#'   unequal probabilities of selection. Biometrika 54, 499-513.
#'
#'   Tille, Y. (2006). Sampling Algorithms. Springer, Sec. 7.6.
#' @examples
#' SampfordDesign(c(0.2, 0.4, 0.6, 0.8))$sample
#' @export
SampfordDesign <- function(pik, seed = 0, max_iter = 500L) {
  pik <- as.numeric(pik)
  tot <- 0
  for (t in pik) tot <- tot + t
  n <- round(tot)
  if (abs(tot - n) > 1e-8 || n < 2) stop("pik must sum to an integer n >= 2", call. = FALSE)
  eps <- 1e-6
  free <- which(pik > eps & pik < 1 - eps)
  s <- as.integer(pik >= 1 - eps)
  pb <- pik[free]
  nb <- round(sum(pb))
  c1 <- .sampford_cum(pb)
  c2 <- .sampford_cum(pb / (1 - pb))
  draw <- function(cum, u) {
    k <- which(u <= cum)
    if (length(k)) k[1] else length(cum)
  }
  attempts <- 0L
  ok <- FALSE
  while (attempts < max_iter && !ok) {
    u <- .morie_random_uniform(nb, seed = seed, stream = attempts)
    attempts <- attempts + 1L
    picks <- c(draw(c1, u[1]), vapply(u[-1], function(v) draw(c2, v), 0L))
    ok <- length(unique(picks)) == nb
  }
  if (!ok) stop("too many rejective attempts", call. = FALSE)
  s[free[picks]] <- 1L
  N <- length(pik)
  joint <- matrix(0, N, N)
  joint[free, free] <- if (nb >= 2) .sampford_joint(pb, nb) else diag(pb, length(pb))
  fixed <- setdiff(which(s == 1L), free)
  for (i in fixed) {
    v <- as.numeric(s)
    v[free] <- pik[free]
    joint[i, ] <- v
    joint[, i] <- v
  }
  list(sample = s, joint = joint, attempts = attempts, n = n)
}
