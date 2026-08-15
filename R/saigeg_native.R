# morie.fn -- function file (rootcoder007/morie)
#
# SAIGE: a score test that survives extreme case-control imbalance.
#
# A biobank PheWAS runs tens of millions of variants against thousands of
# phenotypes, and most of those phenotypes are rare -- case:control of
# 1:100 or worse. Two things break there, and they break in opposite
# directions.
#
# **Relatedness inflates.** Population structure and cryptic relatedness
# make individuals non-independent, so a test that assumes independence
# finds signal that is family, not biology. Linear mixed models fix that
# for quantitative traits, but a binary trait is not Gaussian and an LMM
# applied to one has inflated type I error in its own right.
#
# **Imbalance breaks the asymptotics.** A logistic mixed-model score test
# -- GMMAT's approach -- handles the binary outcome properly, but it
# still assumes the score statistic is asymptotically Gaussian. With
# 1000 cases and 400,000 controls that assumption fails: the score
# distribution is skewed, and the Gaussian tail is far too thin. The test
# then reports p-values that are much too small, which at genome-wide
# scale means thousands of false positives.
#
# **The saddlepoint approximation is the fix, and the reason it works is
# worth stating.** The normal approximation keeps two moments. The
# saddlepoint approximation is built from the entire cumulant generating
# function K(t) = log E[e^{tS}], so it keeps all of them. For the
# score S = sum_i G_i (Y_i - mu_i) the CGF is available in closed form
# because the Y_i are independent Bernoulli given the fitted means:
#
# K(t) = sum_i [ log(1 - mu_i + mu_i e^{G_i t}) - G_i t mu_i ].
#
# Solve K'(t_hat) = s for the observed s, and Lugannani and Rice give
# the tail probability from t_hat, w = sign(t_hat) sqrt(2(t_hat s - K(t_hat)))
# and v = t_hat sqrt(K''(t_hat)):
#
# P(S > s) ~= 1 - Phi(w) + phi(w)(1/v - 1/w).
#
# That expression is exact in the Gaussian case and stays accurate far
# into the tail when the Gaussian one does not, which is precisely the
# regime a PheWAS lives in.
#
# **Where the skew comes from.** The score contribution of a variant is
# G_i(Y_i - mu_i). With balanced classes the positive and negative
# contributions offset symmetrically. With 1:100 imbalance almost every
# mu_i is near zero, so a case carrying the minor allele contributes a
# large positive term while a control contributes a small negative one --
# a long right tail that two moments cannot describe.
#
# **Cost matters as much as calibration.** A test that is correct but
# O(MN^2) cannot run on 400,000 samples. The variance-ratio trick used
# here is the paper's: estimate the ratio between the variance of the
# full mixed-model score and the variance of a score computed without
# the relatedness matrix, once, on a subset of variants; then reuse it.
# The expensive part is paid a fixed number of times rather than per
# variant.
#
# References
# ----------
# Zhou, W., Nielsen, J. B., Fritsche, L. G., Dey, R., Gabrielsen, M. E.,
# Wolford, B. N. et al. (2018) "Efficiently controlling for case-control
# imbalance and sample relatedness in large-scale genetic association
# studies", Nature Genetics, doi:10.1038/s41588-018-0184-y. The
# motivation (unbalanced case-control ratios in biobank PheWAS, the
# failure of LMM and of the Gaussian score test), the use of the
# saddlepoint approximation to calibrate the logistic mixed-model score
# test, and the computational strategy for large N. Volume and pages are
# not printed in the accepted-article file held locally.
#
# Dey, R., Schmidt, E. M., Abecasis, G. R. & Lee, S. (2017) "A fast and
# accurate algorithm to test for binary phenotypes and its application
# to PheWAS", The American Journal of Human Genetics 101(1), 37-49,
# doi:10.1016/j.ajhg.2017.05.014. The saddlepoint-approximation score
# test for unrelated samples that SAIGE extends to mixed models.
#
# Lugannani, R. & Rice, S. (1980) "Saddle point approximation for the
# distribution of the sum of independent random variables", Advances
# in Applied Probability 12(2), 475-490, doi:10.2307/1426607. The tail
# formula implemented in saddlepoint_pvalue.
#
# Chen, H., Wang, C., Conomos, M. P., Stilp, A. M., Li, Z., Sofer, T. et
# al. (2016) "Control for population structure and relatedness for
# binary traits in genetic association studies via logistic mixed
# models", The American Journal of Human Genetics 98(4), 653-666,
# doi:10.1016/j.ajhg.2016.02.012. GMMAT, the Gaussian-approximation
# predecessor whose calibration SAIGE repairs.

.saigeg_EPS <- 1e-12

# ---- core helpers (would live in _s03core in Python) ----

.saigeg_sigmoid <- function(x) {
  x <- as.numeric(x)
  out <- numeric(length(x))
  pos <- x >= 0
  out[pos] <- 1 / (1 + exp(-x[pos]))
  neg <- !pos
  out[neg] <- exp(x[neg]) / (1 + exp(x[neg]))
  out
}

.saigeg_pnorm <- function(x) {
  pnorm(x)
}

.saigeg_variance <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2) return(NA_real_)
  m <- mean(x)
  sum((x - m)^2) / (n - 1)
}

.saigeg_design <- function(X, n) {
  if (is.null(X)) {
    return(matrix(1, nrow=n, ncol=1))
  }
  if (is.list(X)) {
    if (length(X) == 0) {
      return(matrix(1, nrow=n, ncol=1))
    }
    if (all(lengths(X) == 0)) {
      return(matrix(1, nrow=n, ncol=1))
    }
    Xmat <- do.call(rbind, X)
    if (nrow(Xmat) != n) {
      stop(sprintf("saigeg: design rows %d != n %d", nrow(Xmat), n))
    }
    return(cbind(1, Xmat))
  }
  if (is.matrix(X)) {
    if (nrow(X) != n) {
      stop(sprintf("saigeg: design rows %d != n %d", nrow(X), n))
    }
    if (ncol(X) == 0) {
      return(matrix(1, nrow=n, ncol=1))
    }
    return(cbind(1, X))
  }
  if (is.numeric(X) && length(X) == n) {
    return(cbind(1, X))
  }
  stop("saigeg: invalid X for design matrix")
}

.saigeg_logit_irls <- function(D, y, ridge=1e-8) {
  n <- nrow(D)
  p <- ncol(D)
  beta <- rep(0, p)
  for (iter in 1:50) {
    eta <- as.numeric(D %*% beta)
    mu <- .saigeg_sigmoid(eta)
    mu <- pmin(pmax(mu, 1e-15), 1 - 1e-15)
    w <- mu * (1 - mu)
    z <- eta + (y - mu) / w
    XtWX <- crossprod(D, w * D)
    diag(XtWX) <- diag(XtWX) + ridge
    XtWz <- as.numeric(crossprod(D, w * z))
    beta_new <- solve(XtWX, XtWz)
    if (max(abs(beta_new - beta)) < 1e-10) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  beta
}

.saigeg_fit_null <- function(y, X, ridge=1e-8) {
  D <- .saigeg_design(X, length(y))
  beta <- .saigeg_logit_irls(D, y, ridge=ridge)
  eta <- as.numeric(D %*% beta)
  mu <- .saigeg_sigmoid(eta)
  list(mu=mu, beta=beta)
}

.saigeg_score_statistic <- function(y, G, mu) {
  yv <- as.numeric(y)
  gv <- as.numeric(G)
  mv <- as.numeric(mu)
  n <- length(yv)
  if (!(length(gv) == n && length(mv) == n)) {
    stop(sprintf("saigeg: y, G and mu must agree in length (%d, %d, %d)",
                 n, length(gv), length(mv)))
  }
  if (any(mv <= 0 | mv >= 1)) {
    stop("saigeg: fitted means must lie strictly in (0, 1)")
  }
  s <- sum(gv * (yv - mv))
  v <- sum((gv^2) * mv * (1 - mv))
  if (v <= .saigeg_EPS) {
    stop("saigeg: the score has zero variance -- the variant is monomorphic or every fitted mean is degenerate")
  }
  list(score=s, variance=v, n=n)
}

.saigeg_cgf <- function(t, G, mu, order=0) {
  gv <- as.numeric(G)
  mv <- as.numeric(mu)
  t_val <- as.numeric(t)
  gt <- gv * t_val
  gt <- pmin(pmax(gt, -500), 500)
  e <- exp(gt)
  d <- 1 - mv + mv * e
  if (any(d <= .saigeg_EPS)) {
    stop(sprintf("saigeg: the CGF diverged at t = %g", t_val))
  }
  if (order == 0) {
    sum(log(d) - gv * t_val * mv)
  } else if (order == 1) {
    sum(gv * (mv * e / d - mv))
  } else if (order == 2) {
    sum((gv^2) * mv * e * (1 - mv) / (d^2))
  } else {
    stop("saigeg: order must be 0, 1 or 2")
  }
}

.saigeg_solve_saddle <- function(s, G, mu, lo=-50, hi=50, tol=1e-11, iters=200) {
  fl <- .saigeg_cgf(lo, G, mu, 1) - s
  fh <- .saigeg_cgf(hi, G, mu, 1) - s
  if (fl > 0 || fh < 0) {
    stop(sprintf("saigeg: the observed score %g lies outside the range K'(t) can reach -- no saddlepoint exists", s))
  }
  for (i in seq_len(iters)) {
    mid <- 0.5 * (lo + hi)
    fm <- .saigeg_cgf(mid, G, mu, 1) - s
    if (abs(fm) < tol) {
      return(mid)
    }
    if (fm < 0) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  0.5 * (lo + hi)
}

.saigeg_saddlepoint_pvalue <- function(s, G, mu, two_sided=TRUE) {
  sv <- as.numeric(s)
  var0 <- .saigeg_cgf(0, G, mu, 2)
  if (var0 <= .saigeg_EPS) {
    stop("saigeg: the score has zero variance")
  }
  if (abs(sv) < 1e-6 * sqrt(var0)) {
    p <- 2 * (1 - .saigeg_pnorm(abs(sv) / sqrt(var0)))
    return(list(
      p_value=min(1, p),
      method="normal (at the mean, where the saddlepoint is unstable and the two agree)",
      t_hat=0
    ))
  }
  that <- .saigeg_solve_saddle(sv, G, mu)
  kt <- .saigeg_cgf(that, G, mu, 0)
  k2 <- .saigeg_cgf(that, G, mu, 2)
  if (k2 <= .saigeg_EPS) {
    stop("saigeg: K''(t) is non-positive at the saddlepoint")
  }
  inner <- 2 * (that * sv - kt)
  w <- if (that >= 0) sqrt(max(inner, 0)) else -sqrt(max(inner, 0))
  v <- that * sqrt(k2)
  if (abs(w) < 1e-9 || abs(v) < 1e-12) {
    p1 <- 1 - .saigeg_pnorm(abs(sv) / sqrt(var0))
  } else {
    phi <- exp(-0.5 * w * w) / sqrt(2 * pi)
    p1 <- 1 - .saigeg_pnorm(w) + phi * (1/v - 1/w)
  }
  p1 <- min(max(p1, 0), 1)
  p <- if (two_sided) 2 * min(p1, 1 - p1) else p1
  list(
    p_value=min(1, max(p, 0)),
    t_hat=that,
    w=w,
    v=v,
    K=kt,
    K2=k2,
    method="saddlepoint (Lugannani-Rice), all cumulants"
  )
}

.saigeg_normal_pvalue <- function(s, variance, two_sided=TRUE) {
  if (as.numeric(variance) <= 0) {
    stop("saigeg: the variance must be positive")
  }
  z <- as.numeric(s) / sqrt(as.numeric(variance))
  p <- if (two_sided) {
    2 * (1 - .saigeg_pnorm(abs(z)))
  } else {
    1 - .saigeg_pnorm(z)
  }
  list(
    p_value=min(1, max(p, 0)),
    z=z,
    method="normal approximation, first two moments"
  )
}

.saigeg_variance_ratio <- function(scores_full, scores_naive) {
  a <- as.numeric(scores_full)
  b <- as.numeric(scores_naive)
  if (length(a) != length(b) || length(a) < 2) {
    stop("saigeg: need at least 2 matched score pairs")
  }
  va <- .saigeg_variance(a)
  vb <- .saigeg_variance(b)
  if (vb <= .saigeg_EPS) {
    stop("saigeg: the naive scores have zero variance")
  }
  list(
    ratio=va / vb,
    var_full=va,
    var_naive=vb,
    n_variants=length(a)
  )
}

.saigeg_cheatsheet <- function() {
  "saigeg: SAIGE. Score S = sum G_i (Y_i - mu_i) from a logistic mixed model. Under 1:100 case-control imbalance S is right-skewed and the GAUSSIAN tail is far too thin, so p-values come out much too small. The saddlepoint approximation uses the whole CGF -- all cumulants -- via Lugannani-Rice, and stays calibrated in the tail. The variance ratio is estimated once and reused so the cost is not O(MN^2)."
}

# ---- main entry point ----

morie_saigeg <- function(y, G, X=NULL, mu=NULL, ratio=1.0, two_sided=TRUE) {
  yv <- as.numeric(y)
  bad <- which(yv != 0 & yv != 1)
  if (length(bad) > 0) {
    stop(sprintf("saigeg: the phenotype must be 0/1, got %g at index %d",
                 yv[bad[1]], bad[1]))
  }
  n_case <- as.integer(sum(yv))
  if (n_case == 0 || n_case == length(yv)) {
    stop(sprintf("saigeg: the phenotype has only one class (%d cases of %d)",
                 n_case, length(yv)))
  }
  if (is.null(mu)) {
    null_fit <- .saigeg_fit_null(yv, X)
    mu <- null_fit$mu
  }
  st <- .saigeg_score_statistic(yv, G, mu)
  var <- st$variance * as.numeric(ratio)
  nrm <- .saigeg_normal_pvalue(st$score, var, two_sided=two_sided)
  spa <- .saigeg_saddlepoint_pvalue(st$score, G, mu, two_sided=two_sided)
  list(
    estimate=spa$p_value,
    p_value=spa$p_value,
    p_normal=nrm$p_value,
    score=st$score,
    variance=var,
    z=nrm$z,
    case_control_ratio=n_case / (length(yv) - n_case),
    n_cases=n_case,
    n_controls=length(yv) - n_case,
    variance_ratio=as.numeric(ratio),
    saddlepoint=spa,
    method="logistic mixed-model score test with saddlepoint calibration; Zhou et al. (2018)",
    why="the Gaussian approximation keeps two moments and is anti-conservative under case-control imbalance; the saddlepoint keeps all of them"
  )
}
