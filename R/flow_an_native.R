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
#' A step of the flow_an_native implementation. Called by \code{.anomaly_score}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
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
#' A step of the flow_an_native implementation. Called by \code{.anomaly_score}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param sorted_x A vector; its length is taken and its elements indexed.
#' @param q Numeric; combined arithmetically in the body.
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
#' A step of the flow_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param d A count; the body uses it as \code{seq_len(...)}.
#' @param n_layers A count; the body uses it as \code{seq_len(...)}.
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
#' A step of the flow_an_native implementation. Called by \code{.coupling_forward}, \code{.coupling_inverse}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param mask Numeric; combined arithmetically in the body.
#' @param Ws A matrix; passed to \code{as.matrix}.
#' @param bs Numeric; combined arithmetically in the body.
#' @param Wt A matrix; passed to \code{as.matrix}.
#' @param bt Numeric; combined arithmetically in the body.
#' @param scale_cap Numeric; combined arithmetically in the body. Defaults to \code{5}.
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
#' A step of the flow_an_native implementation. Called by \code{.flow_forward}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param mask Numeric; combined arithmetically in the body.
#' @param Ws Passed to \code{.st}.
#' @param bs Passed to \code{.st}.
#' @param Wt Passed to \code{.st}.
#' @param bt Passed to \code{.st}.
#' @param scale_cap Passed to \code{.st}. Defaults to \code{5}.
#' @return A list with \code{y}, \code{logdet}.
#' @export
.coupling_forward <- function(x, mask, Ws, bs, Wt, bt, scale_cap = 5.0) {
  r <- .st(x, mask, Ws, bs, Wt, bt, scale_cap)
  y <- x * mask + (1 - mask) * (x * exp(r$s) + r$t)
  list(y = y, logdet = sum(r$s))
}

#' .coupling_inverse
#'
#' A step of the flow_an_native implementation. Called by \code{.flow_inverse}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @param mask Numeric; combined arithmetically in the body.
#' @param Ws Passed to \code{.st}.
#' @param bs Passed to \code{.st}.
#' @param Wt Passed to \code{.st}.
#' @param bt Passed to \code{.st}.
#' @param scale_cap Passed to \code{.st}. Defaults to \code{5}.
#' @return A list with \code{x}, \code{logdet}.
#' @export
.coupling_inverse <- function(y, mask, Ws, bs, Wt, bt, scale_cap = 5.0) {
  r <- .st(y, mask, Ws, bs, Wt, bt, scale_cap)
  x <- y * mask + (1 - mask) * ((y - r$t) * exp(-r$s))
  list(x = x, logdet = -sum(r$s))
}

#' .flow_forward
#'
#' A step of the flow_an_native implementation. Called by \code{.log_prob}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param layers See Usage.
#' @return A list with \code{z}, \code{logdet}.
#' @export
.flow_forward <- function(x, layers) {
  z <- as.numeric(x)
  logdet <- 0
  for (lyr in layers) {
    r <- .coupling_forward(z, lyr[[1L]], lyr[[2L]], lyr[[3L]],
                            lyr[[4L]], lyr[[5L]])
    z <- r$y
    logdet <- logdet + r$logdet
  }
  list(z = z, logdet = logdet)
}

#' .flow_inverse
#'
#' A step of the flow_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @param layers Numeric; passed to \code{rev}.
#' @return A list with \code{x}, \code{logdet}.
#' @export
.flow_inverse <- function(z, layers) {
  x <- as.numeric(z)
  logdet <- 0
  for (lyr in rev(layers)) {
    r <- .coupling_inverse(x, lyr[[1L]], lyr[[2L]], lyr[[3L]],
                            lyr[[4L]], lyr[[5L]])
    x <- r$x
    logdet <- logdet + r$logdet
  }
  list(x = x, logdet = logdet)
}

#' .log_prob
#'
#' A step of the flow_an_native implementation. Called by \code{.anomaly_score}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{.flow_forward}.
#' @param layers Passed to \code{.flow_forward}.
#' @return A list with \code{lp}, \code{z}, \code{logdet}.
#' @export
.log_prob <- function(x, layers) {
  r <- .flow_forward(x, layers)
  base <- -0.5 * sum(r$z * r$z) - 0.5 * length(r$z) * .LOG2PI
  list(lp = base + r$logdet, z = r$z, logdet = r$logdet)
}

#' .anomaly_score
#'
#' A step of the flow_an_native implementation. Called by \code{morie_flow_an}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X Passed to \code{.flow_an_to_mat}.
#' @param layers Passed to \code{.log_prob}.
#' @param threshold_quantile Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @param reference Optional; may be \code{NULL}. Passed to \code{.flow_an_to_mat}.
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
#' A step of the flow_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X Passed to \code{.anomaly_score}.
#' @param layers Passed to \code{.anomaly_score}.
#' @param threshold_quantile Passed to \code{.anomaly_score}. Defaults to \code{0.95}.
#' @param reference Passed to \code{.anomaly_score}.
#' @return The value of \code{.anomaly_score}.
#' @export
morie_flow_an <- function(X, layers, threshold_quantile = 0.95, reference = NULL) {
  .anomaly_score(X = X, layers = layers, threshold_quantile = threshold_quantile, reference = reference)
}
