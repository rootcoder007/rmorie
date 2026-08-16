# tail2 batch, tranche 1 -- R mirror of the Python modules
#   morie/fn/kappac.py   Cohen (1960) kappa
#   morie/fn/kappaw.py   Cohen (1968) weighted kappa
#   morie/fn/cookd.py    Cook (1977) distance
#   morie/fn/pnaG.py     Corso et al. (2020) PNA, arXiv:2004.05718
#   morie/fn/rtsmpl.py   Cori et al. (2013) instantaneous R
#
# Byte-identical between r-package/morie/R and r-morie-oss/R.
#
# Sources actually consulted are named in each function; where a paper
# could not be obtained the docstring of the Python module says so in
# the same words.  Every routine below is closed form or runs a FIXED
# number of steps, because an early exit on one language arm and not the
# other silently breaks Py<->R parity.

#' .t2_lvl
#'
#' A step of the tail2_t01 implementation. Called by \code{KappaCoh}, \code{KappaWt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{c}.
#' @param b Passed to \code{c}.
#' @return A vector, from \code{sort}.
#' @export
.t2_lvl <- function(a, b) sort(unique(c(a, b)))

#' Cohen (1960) kappa for two raters
#' @param rater1,rater2 categorical ratings of the same n subjects
#' @return list(kappa, p_observed, p_expected, se, z, n, n_categories)
#' @export
KappaCoh <- function(rater1, rater2) {
  r1 <- rater1; r2 <- rater2
  n <- length(r1)
  if (n == 0L || length(r2) != n)
    stop("rater1 and rater2 must be non-empty and of equal length")
  lv <- .t2_lvl(r1, r2)
  k <- length(lv)
  i1 <- match(r1, lv); i2 <- match(r2, lv)
  tab <- matrix(0, k, k)
  for (i in seq_len(n)) tab[i1[i], i2[i]] <- tab[i1[i], i2[i]] + 1
  row <- rowSums(tab); col <- colSums(tab)
  po <- sum(diag(tab)) / n
  pe <- sum(row * col) / (n * n)
  kap <- if (pe < 1) (po - pe) / (1 - pe) else 0
  se <- if (pe < 1) sqrt(po * (1 - po) / n) / (1 - pe) else 0
  z <- if (se > 0) kap / se else 0
  list(kappa = kap, p_observed = po, p_expected = pe, se = se, z = z,
       n = n, n_categories = k,
       method = "Cohen (1960) kappa, two raters, nominal scale")
}

#' .t2_wmat
#'
#' A step of the tail2_t01 implementation. Called by \code{KappaWt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param weights A matrix; passed to \code{as.matrix}.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{w}, as built in the body.
#' @export
.t2_wmat <- function(weights, k) {
  if (is.character(weights)) {
    nm <- tolower(weights)
    idx <- outer(seq_len(k), seq_len(k), "-")
    if (substr(nm, 1L, 3L) == "lin") return(abs(idx))
    if (substr(nm, 1L, 4L) == "quad" || substr(nm, 1L, 2L) == "sq")
      return(idx^2)
    stop("weights must be 'linear', 'quadratic' or a k x k matrix")
  }
  w <- as.matrix(weights)
  if (nrow(w) != k || ncol(w) != k)
    stop("weight matrix must be k x k over the pooled category set")
  w
}

#' Cohen (1968) weighted kappa
#' @param rater1,rater2 ordered-category ratings of the same n subjects
#' @param weights "linear", "quadratic", or a k x k disagreement matrix
#' @return list(kappa, observed_disagreement, expected_disagreement, n,
#'   n_categories)
#' @export
KappaWt <- function(rater1, rater2, weights = "linear") {
  r1 <- rater1; r2 <- rater2
  n <- length(r1)
  if (n == 0L || length(r2) != n)
    stop("rater1 and rater2 must be non-empty and of equal length")
  lv <- .t2_lvl(r1, r2)
  k <- length(lv)
  w <- .t2_wmat(weights, k)
  i1 <- match(r1, lv); i2 <- match(r2, lv)
  p <- matrix(0, k, k)
  for (i in seq_len(n)) p[i1[i], i2[i]] <- p[i1[i], i2[i]] + 1 / n
  row <- rowSums(p); col <- colSums(p)
  qo <- sum(w * p)
  qe <- sum(w * outer(row, col))
  if (qe <= 0)
    stop("expected disagreement is zero; kappa_w is undefined")
  list(kappa = 1 - qo / qe, observed_disagreement = qo,
       expected_disagreement = qe, n = n, n_categories = k,
       method = "Cohen (1968) weighted kappa, 1 - sum(W O)/sum(W E)")
}

#' Cook (1977) distance for a linear model
#' @param y response, length n
#' @param X design matrix (n x p); no intercept is added
#' @return list(d, leverage, residual, std_residual, beta, sigma2, rss,
#'   max_d, argmax_d, threshold, p, n)
#' @export
CooksD <- function(y, X) {
  yv <- as.numeric(y)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  n <- length(yv)
  if (n == 0L || nrow(Xm) != n)
    stop("y and X must be non-empty and have the same length")
  p <- ncol(Xm)
  if (n <= p) stop("need more observations than parameters")
  xtx <- t(Xm) %*% Xm
  xtxi <- solve(xtx)
  beta <- as.numeric(xtxi %*% (t(Xm) %*% yv))
  fitted <- as.numeric(Xm %*% beta)
  resid <- yv - fitted
  rss <- sum(resid * resid)
  sigma2 <- rss / (n - p)
  lev <- rowSums((Xm %*% xtxi) * Xm)
  om <- 1 - lev
  d <- ifelse(om <= 0, Inf,
              if (sigma2 > 0) resid^2 / (p * sigma2) * lev / (om * om)
              else rep(0, n))
  stdres <- ifelse(om <= 0, Inf,
                   if (sigma2 > 0) resid / sqrt(sigma2 * om) else rep(0, n))
  mx <- max(d)
  list(d = d, leverage = lev, residual = resid, std_residual = stdres,
       beta = beta, sigma2 = sigma2, rss = rss,
       max_d = mx, argmax_d = which.max(d) - 1L,
       threshold = 4 / n, p = p, n = n,
       method = "Cook (1977) distance, D_i = e_i^2 h_ii / (p s^2 (1 - h_ii)^2)")
}

# ------------------------------------------------------------------
# Corso et al. (2020) Principal Neighbourhood Aggregation.
# eq. (2) max/min, eq. (3) sigma with the ReLU and eps, eq. (5) delta,
# eq. (6) S(d, alpha), eq. (7) scalers tensor aggregators.
.t2_pna_eps <- 1e-5

#' .t2_agg
#'
#' A step of the tail2_t01 implementation. Called by \code{PnaAgg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name One of \code{"max"}, \code{"mean"}, \code{"min"}, \code{"std"}.
#' @param vals A vector; its length is taken.
#' @return Nothing; this branch always raises.
#' @export
.t2_agg <- function(name, vals) {
  m <- length(vals)
  if (m == 0L) return(0)
  if (name == "mean") return(sum(vals) / m)
  if (name == "max") return(max(vals))
  if (name == "min") return(min(vals))
  if (name == "std") {
    mu <- sum(vals) / m
    mu2 <- sum(vals * vals) / m
    v <- mu2 - mu * mu
    if (v < 0) v <- 0
    return(sqrt(v + .t2_pna_eps))
  }
  stop(sprintf("unknown aggregator '%s'", name))
}

#' Principal Neighbourhood Aggregation (Corso et al. 2020, eq. 7)
#' @param A square adjacency matrix; a non-zero A[i, j] makes j a
#'   neighbour of i
#' @param X node features (n x f)
#' @param aggregators subset of c("mean", "std", "max", "min")
#' @param scalers subset of c("identity", "amplification", "attenuation")
#' @return list(out, aggregated, degree, delta, scale, n, n_features,
#'   n_columns)
#' @export
PnaAgg <- function(A, X,
                   aggregators = c("mean", "std", "max", "min"),
                   scalers = c("identity", "amplification", "attenuation")) {
  Am <- as.matrix(A); storage.mode(Am) <- "double"
  n <- nrow(Am)
  if (n == 0L) stop("A is empty")
  if (ncol(Am) != n) stop("A must be a square adjacency matrix")
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  if (nrow(Xm) != n) stop("X must have one row per node")
  f <- ncol(Xm)
  ok_a <- c("mean", "std", "max", "min")
  ok_s <- c("identity", "amplification", "attenuation")
  if (!all(aggregators %in% ok_a)) stop("unknown aggregator")
  if (!all(scalers %in% ok_s)) stop("unknown scaler")
  alpha <- c(identity = 0, amplification = 1, attenuation = -1)

  nb <- lapply(seq_len(n), function(i) which(Am[i, ] != 0))
  deg <- vapply(nb, length, integer(1))
  delta <- sum(log(deg + 1)) / n           # eq. (5)
  if (delta <= 0) stop("delta is zero: the graph has no edges")

  aggregated <- vector("list", n)
  for (i in seq_len(n)) {
    per <- vector("list", length(aggregators))
    for (ai in seq_along(aggregators))
      per[[ai]] <- vapply(seq_len(f), function(cc)
        .t2_agg(aggregators[ai], Xm[nb[[i]], cc]), numeric(1))
    aggregated[[i]] <- per
  }

  scale <- matrix(0, n, length(scalers))    # eq. (6)
  for (i in seq_len(n)) {
    base <- log(deg[i] + 1) / delta
    for (si in seq_along(scalers)) {
      al <- alpha[[scalers[si]]]
      scale[i, si] <- if (al == 0) 1
      else if (base <= 0) (if (al > 0) 0 else 1)
      else base^al
    }
  }

  out <- matrix(0, n, length(scalers) * length(aggregators) * f)  # eq. (7)
  for (i in seq_len(n)) {
    cpos <- 0L
    for (si in seq_along(scalers))
      for (ai in seq_along(aggregators))
        for (cc in seq_len(f)) {
          cpos <- cpos + 1L
          out[i, cpos] <- scale[i, si] * aggregated[[i]][[ai]][cc]
        }
  }

  list(out = out, aggregated = aggregated, degree = deg, delta = delta,
       scale = scale, aggregators = aggregators, scalers = scalers,
       n = n, n_features = f,
       n_columns = length(scalers) * length(aggregators) * f,
       method = "Corso et al. (2020) PNA, eq. (7) of arXiv:2004.05718")
}

#' Cori et al. (2013) instantaneous reproduction number
#' @param incidence daily incidence I_0 ... I_{T-1}
#' @param serial_interval discrete serial-interval distribution w_0, w_1, ...
#' @param window sliding window length tau, in days
#' @param a_prior,b_prior gamma prior shape and SCALE (EpiEstim default
#'   prior mean 5, sd 5 is a = 1, b = 5)
#' @return list(r_mean, r_std, a_posterior, b_posterior, lambda, t_start,
#'   t_end, n_windows, n)
#' @export
RtSi <- function(incidence, serial_interval, window = 7L,
                 a_prior = 1, b_prior = 5) {
  inc <- as.numeric(incidence)
  w <- as.numeric(serial_interval)
  Tn <- length(inc)
  tau <- as.integer(window)
  if (Tn == 0L) stop("incidence is empty")
  if (tau < 1L || tau > Tn)
    stop("window must lie in 1..length(incidence)")
  if (length(w) == 0L) stop("serial_interval is empty")

  # overall_infectivity: lambda[t] = sum_k w_k I_{t-k}; 0-based index 0
  # is undefined and reported as 0.
  lam <- numeric(Tn)
  for (t in seq_len(Tn - 1L)) {        # t here is the 0-based index
    acc <- 0
    for (k in 0:t) if (k < length(w)) acc <- acc + w[k + 1L] * inc[t - k + 1L]
    lam[t + 1L] <- acc
  }

  t_start <- integer(0); t_end <- integer(0)
  a_post <- numeric(0); b_post <- numeric(0)
  r_mean <- numeric(0); r_std <- numeric(0)
  for (endi in seq(tau, Tn - 1L)) {    # 0-based window end
    starti <- endi - tau + 1L
    a <- a_prior + sum(inc[(starti + 1L):(endi + 1L)])
    b <- 1 / (1 / b_prior + sum(lam[(starti + 1L):(endi + 1L)]))
    t_start <- c(t_start, starti); t_end <- c(t_end, endi)
    a_post <- c(a_post, a); b_post <- c(b_post, b)
    r_mean <- c(r_mean, a * b); r_std <- c(r_std, sqrt(a) * b)
  }

  list(r_mean = r_mean, r_std = r_std, a_posterior = a_post,
       b_posterior = b_post, lambda = lam, t_start = t_start,
       t_end = t_end, window = tau, a_prior = a_prior, b_prior = b_prior,
       n_windows = length(r_mean), n = Tn,
       method = "Cori et al. (2013) instantaneous R, gamma-Poisson conjugate posterior")
}
