# morie.fn -- function file (rootcoder007/morie)
# RealNVP: exact-likelihood density estimation, and anomaly scoring.
#
# References
# Dinh, L., Sohl-Dickstein, J. & Bengio, S. (2017) "Density Estimation
# using Real NVP", ICLR, arXiv:1605.08803. The affine coupling layer,
# its Jacobian, and the multi-scale architecture.
# Dinh, L., Krueger, D. & Bengio, Y. (2015) "NICE: Non-linear
# Independent Components Estimation", ICLR Workshop, arXiv:1410.8516.
# The additive coupling RealNVP generalises.
# Rezende, D. J. & Mohamed, S. (2015) "Variational Inference with
# Normalizing Flows", ICML, PMLR 37, 1530-1538, arXiv:1505.05770.
# Papamakarios, G. et al. (2021) "Normalizing Flows for Probabilistic
# Modeling and Inference", JMLR 22(57), 1-64, arXiv:1912.02762.

.flow_an_EPS <- 1e-12
.LOG2PI <- log(2 * pi)

#' .flow_an_to_mat
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @return The value of \code{X}, as built in the body.
#' @export
.flow_an_to_mat <- function(X) {
  if (is.data.frame(X)) X <- as.matrix(X)
  X <- as.matrix(X)
  if (!is.numeric(X)) storage.mode(X) <- "numeric"
  X
}

#' .flow_an_quantile7
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param sorted_x See Usage.
#' @param q See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.flow_an_quantile7 <- function(sorted_x, q) {
  n <- length(sorted_x)
  pos <- q * (n + 1L)
  lo <- floor(pos)
  hi <- ceiling(pos)
  if (lo < 1L) lo <- 1L
  if (hi > n) hi <- n
  if (lo == hi) sorted_x[lo] else
    sorted_x[lo] + (pos - lo) * (sorted_x[hi] - sorted_x[lo])
}

#' .alternating_masks
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param d See Usage.
#' @param n_layers See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.alternating_masks <- function(d, n_layers) {
  if (d < 2L) stop(sprintf("flow_an: need at least 2 dimensions, got %d", d))
  out <- vector("list", n_layers)
  for (t in seq_len(n_layers)) {
    par <- (t - 1L) %% 2L
    out[[t]] <- vapply(seq_len(d) - 1L, function(i) if ((i %% 2L) == par) 1 else 0, numeric(1))
  }
  out
}

#' .st
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param mask See Usage.
#' @param Ws See Usage.
#' @param bs See Usage.
#' @param Wt See Usage.
#' @param bt See Usage.
#' @param scale_cap Defaults to \code{5}.
#' @return A list with \code{s}, \code{t}.
#' @export
.st <- function(x, mask, Ws, bs, Wt, bt, scale_cap = 5.0) {
  xin <- x * mask
  hs <- as.numeric(as.matrix(Ws) %*% xin + bs)
  ht <- as.numeric(as.matrix(Wt) %*% xin + bt)
  s <- scale_cap * tanh(hs) * (1 - mask)
  t <- ht * (1 - mask)
  list(s = s, t = t)
}

#' .coupling_forward
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param mask See Usage.
#' @param Ws See Usage.
#' @param bs See Usage.
#' @param Wt See Usage.
#' @param bt See Usage.
#' @param scale_cap Defaults to \code{5}.
#' @return A list with \code{y}, \code{logdet}.
#' @export
.coupling_forward <- function(x, mask, Ws, bs, Wt, bt, scale_cap = 5.0) {
  r <- .st(x, mask, Ws, bs, Wt, bt, scale_cap)
  y <- x * mask + (1 - mask) * (x * exp(r$s) + r$t)
  list(y = y, logdet = sum(r$s))
}

#' .coupling_inverse
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param y See Usage.
#' @param mask See Usage.
#' @param Ws See Usage.
#' @param bs See Usage.
#' @param Wt See Usage.
#' @param bt See Usage.
#' @param scale_cap Defaults to \code{5}.
#' @return A list with \code{x}, \code{logdet}.
#' @export
.coupling_inverse <- function(y, mask, Ws, bs, Wt, bt, scale_cap = 5.0) {
  r <- .st(y, mask, Ws, bs, Wt, bt, scale_cap)
  x <- y * mask + (1 - mask) * ((y - r$t) * exp(-r$s))
  list(x = x, logdet = -sum(r$s))
}

#' .flow_forward
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param layers See Usage.
#' @return A list with \code{z}, \code{logdet}.
#' @export
.flow_forward <- function(x, layers) {
  z <- as.numeric(x)
  logdet <- 0
  for (lyr in layers) {
    r <- .coupling_forward(z, lyr[[1L]], lyr[[2L]], lyr[[3L]],
                            lyr[[4L]], lyr[[5L]])
    z <- r$y; logdet <- logdet + r$logdet
  }
  list(z = z, logdet = logdet)
}

#' .flow_inverse
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param z See Usage.
#' @param layers See Usage.
#' @return A list with \code{x}, \code{logdet}.
#' @export
.flow_inverse <- function(z, layers) {
  x <- as.numeric(z)
  logdet <- 0
  for (lyr in rev(layers)) {
    r <- .coupling_inverse(x, lyr[[1L]], lyr[[2L]], lyr[[3L]],
                            lyr[[4L]], lyr[[5L]])
    x <- r$x; logdet <- logdet + r$logdet
  }
  list(x = x, logdet = logdet)
}

#' .log_prob
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param layers See Usage.
#' @return A list with \code{lp}, \code{z}, \code{logdet}.
#' @export
.log_prob <- function(x, layers) {
  r <- .flow_forward(x, layers)
  base <- -0.5 * sum(r$z * r$z) - 0.5 * length(r$z) * .LOG2PI
  list(lp = base + r$logdet, z = r$z, logdet = r$logdet)
}

#' .anomaly_score
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @param threshold_quantile Defaults to \code{0.95}.
#' @param reference Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{score}, \code{threshold}, \code{flag}, \code{n_flagged}, \code{n}, \code{quantile}, \code{self_referenced}, \code{log_likelihood}, \code{method}.
#' @export
.anomaly_score <- function(X, layers, threshold_quantile = 0.95, reference = NULL) {
  Xm <- .flow_an_to_mat(X)
  scores <- apply(Xm, 1, function(row) -.log_prob(row, layers)$lp)
  if (is.null(reference)) {
    ref <- scores
  } else {
    refm <- .flow_an_to_mat(reference)
    ref <- apply(refm, 1, function(row) -.log_prob(row, layers)$lp)
  }
  q <- as.numeric(threshold_quantile)
  if (q <= 0 || q >= 1) {
    stop(sprintf("flow_an: threshold_quantile must be in (0, 1), got %s", format(threshold_quantile)))
  }
  thr <- .flow_an_quantile7(sort(ref), q)
  flags <- as.numeric(scores > thr)
  list(
    estimate = scores, score = scores, threshold = thr, flag = flags,
    n_flagged = as.integer(sum(flags)), n = nrow(Xm), quantile = q,
    self_referenced = is.null(reference),
    log_likelihood = -scores,
    method = "RealNVP negative log-likelihood anomaly score, Dinh, Sohl-Dickstein & Bengio (2017)"
  )
}

#' morie_flow_an
#'
#' Part of the flow_an_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @param threshold_quantile Defaults to \code{0.95}.
#' @param reference Defaults to \code{NULL}.
#' @return The value of \code{.anomaly_score}.
#' @export
morie_flow_an <- function(X, layers, threshold_quantile = 0.95, reference = NULL) {
  .anomaly_score(X = X, layers = layers, threshold_quantile = threshold_quantile, reference = reference)
}
