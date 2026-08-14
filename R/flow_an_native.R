# RealNVP: exact-likelihood density estimation, and anomaly scoring.
#
# Sources: Dinh, L., Sohl-Dickstein, J. & Bengio, S. (2017) "Density
# Estimation using Real NVP", International Conference on Learning
# Representations, arXiv:1605.08803. The affine coupling layer, its
# Jacobian, and the multi-scale architecture. Dinh, L., Krueger, D. &
# Bengio, Y. (2015) "NICE: Non-linear Independent Components
# Estimation", ICLR Workshop, arXiv:1410.8516. The additive coupling
# RealNVP generalises. Rezende, D. J. & Mohamed, S. (2015)
# "Variational Inference with Normalizing Flows", Proceedings of the
# 32nd International Conference on Machine Learning, PMLR 37,
# 1530-1538, arXiv:1505.05770. The normalizing-flow framing.
# Papamakarios, G., Nalisnick, E., Rezende, D. J., Mohamed, S. &
# Lakshminarayanan, B. (2021) "Normalizing Flows for Probabilistic
# Modeling and Inference", Journal of Machine Learning Research
# 22(57), 1-64, arXiv:1912.02762. A review of the family.
#
# Native implementation mirroring Python morie.fn.flow_an exactly:
# the same coupling layer with mask, s and t, the same
# tanh-capped log-scale, the same exact log-determinant sum, the
# same standard-normal base log density, and the same
# RichResult-style payload as a named list.

.FLOW_AN_EPS <- 1e-12
.FLOW_AN_LOG2PI <- log(2 * pi)

alternating_masks <- function(d, n_layers) {
  if (d < 2L)
    stop("flow_an: need at least 2 dimensions, got ", d)
  out <- vector("list", as.integer(n_layers))
  for (t in seq_len(as.integer(n_layers))) {
    par <- (t - 1L) %% 2L
    out[[t]] <- vapply(seq_len(d) - 1L,
                       function(i) if ((i %% 2L) == par) 1 else 0,
                       numeric(1L))
  }
  out
}

.st <- function(x, mask, Ws, bs, Wt, bt, scale_cap = 5.0) {
  Ws <- as.matrix(Ws)
  Wt <- as.matrix(Wt)
  xin <- as.numeric(x) * mask
  hs <- as.numeric(Ws %*% xin) + as.numeric(bs)
  ht <- as.numeric(Wt %*% xin) + as.numeric(bt)
  s <- scale_cap * tanh(hs) * (1 - mask)
  t_out <- ht * (1 - mask)
  list(s = s, t = t_out)
}

coupling_forward <- function(x, mask, Ws, bs, Wt, bt,
                             scale_cap = 5.0) {
  st <- .st(x, mask, Ws, bs, Wt, bt, scale_cap)
  s <- st$s
  t_out <- st$t
  y <- mask * as.numeric(x) +
    (1 - mask) * (as.numeric(x) * exp(s) + t_out)
  list(y = y, logdet = sum(s))
}

coupling_inverse <- function(y, mask, Ws, bs, Wt, bt,
                             scale_cap = 5.0) {
  st <- .st(y, mask, Ws, bs, Wt, bt, scale_cap)
  s <- st$s
  t_out <- st$t
  x <- mask * as.numeric(y) +
    (1 - mask) * ((as.numeric(y) - t_out) * exp(-s))
  list(x = x, logdet = -sum(s))
}

flow_forward <- function(x, layers) {
  z <- as.numeric(x)
  logdet <- 0
  for (params in layers) {
    r <- coupling_forward(z, params$mask, params$Ws, params$bs,
                          params$Wt, params$bt)
    z <- r$y
    logdet <- logdet + r$logdet
  }
  list(z = z, logdet = logdet)
}

flow_inverse <- function(z, layers) {
  x <- as.numeric(z)
  logdet <- 0
  for (params in rev(layers)) {
    r <- coupling_inverse(x, params$mask, params$Ws, params$bs,
                          params$Wt, params$bt)
    x <- r$x
    logdet <- logdet + r$logdet
  }
  list(x = x, logdet = logdet)
}

log_prob <- function(x, layers) {
  r <- flow_forward(x, layers)
  z <- r$z
  logdet <- r$logdet
  base <- -0.5 * sum(z * z) - 0.5 * length(z) * .FLOW_AN_LOG2PI
  list(logp = base + logdet, z = z, logdet = logdet)
}

anomaly_score <- function(X, layers, threshold_quantile = 0.95,
                          reference = NULL) {
  Xm <- as.matrix(X)
  scores <- numeric(nrow(Xm))
  for (i in seq_len(nrow(Xm))) {
    scores[i] <- -log_prob(Xm[i, ], layers)$logp
  }
  if (is.null(reference)) {
    ref <- scores
  } else {
    Rm <- as.matrix(reference)
    ref <- numeric(nrow(Rm))
    for (i in seq_len(nrow(Rm))) {
      ref[i] <- -log_prob(Rm[i, ], layers)$logp
    }
  }
  q <- as.numeric(threshold_quantile)
  if (!(q > 0 && q < 1))
    stop("flow_an: threshold_quantile must be in (0, 1), got ", q)
  thr <- as.numeric(quantile(ref, q, type = 7))
  flags <- ifelse(scores > thr, 1, 0)
  list(estimate = scores, score = scores, threshold = thr,
       flag = flags, n_flagged = as.integer(sum(flags)),
       n = nrow(Xm), quantile = q,
       self_referenced = is.null(reference),
       log_likelihood = -scores,
       method = "RealNVP negative log-likelihood anomaly score, Dinh, Sohl-Dickstein & Bengio (2017)")
}

cheatsheet <- function() {
  paste("flow_an: coupling layer y1 = x1, y2 = x2*exp(s(x1)) +",
        "t(x1). Jacobian is TRIANGULAR so log|det| = sum(s), and s,",
        "t can be arbitrary nets because they are never",
        "differentiated for the determinant. log p(x) = log p_z(f(x))",
        "+ sum(s), exact. Masks must ALTERNATE or half the input",
        "is never transformed. Cap the log-scale or exp overflows.",
        sep = " ")
}

morie_flow_an <- function(X, layers, threshold_quantile = 0.95,
                          reference = NULL) {
  anomaly_score(X, layers, threshold_quantile, reference)
}

anomalyscore <- function(X, layers, threshold_quantile = 0.95,
                         reference = NULL) {
  anomaly_score(X, layers, threshold_quantile, reference)
}

normalizing_flow_anomaly <- function(X, layers,
                                     threshold_quantile = 0.95,
                                     reference = NULL) {
  anomaly_score(X, layers, threshold_quantile, reference)
}
