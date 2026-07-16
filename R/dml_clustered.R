# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cluster-robust double machine learning for the ATE
#'
#' A general (non-OTIS) policy-evaluation entry point for double/debiased
#' machine learning when observations are clustered -- e.g. flights within an
#' airspace corridor, students within a school, patients within a hospital.
#' Standard DML standard errors assume independent observations and are
#' anti-conservative under within-cluster correlation of the treatment or the
#' errors. This cross-fits the AIPW (doubly-robust) score and computes a
#' cluster-robust variance from the per-cluster score sums (Liang & Zeger 1986,
#' one-way; Cameron, Gelbach & Miller 2011, up to two-way).
#'
#' Nuisances are cross-fitted (Chernozhukov et al. 2018): a ridge-logistic
#' propensity and per-arm ordinary-least-squares outcome regressions, so the
#' AIPW point estimate is Neyman-orthogonal. Only the SE is cluster-aware; the
#' point estimate is the usual AIPW ATE. Add own-implemented mixed-effects
#' propensities via \code{ps} if a corridor random effect is needed.
#'
#' @param data A data frame.
#' @param treatment Binary treatment column name (0/1 or two-valued).
#' @param outcome Numeric outcome column name.
#' @param covariates Character vector of confounder column names.
#' @param cluster Cluster column name (one-way) or length-2 character vector
#'   (two-way). \code{NULL} gives the i.i.d. (non-clustered) SE.
#' @param n_folds Cross-fitting folds (default 5).
#' @param seed Integer seed (default 123).
#' @param eps Propensity clip bound in \code{[eps, 1-eps]} (default 0.02).
#' @param ps Optional length-\code{nrow(data)} vector of externally supplied
#'   propensity scores (e.g. from a mixed-effects / cluster-level model); when
#'   given it replaces the cross-fitted propensity.
#' @return A list of class \code{morie_dml_clustered} with \code{ate},
#'   \code{se}, \code{ci95}, \code{z}, \code{pval}, \code{n}, \code{n_clusters},
#'   and \code{se_kind}.
#' @references
#' Chernozhukov V, et al. (2018). Double/debiased machine learning.
#'   \emph{The Econometrics Journal} 21(1), C1--C68. \doi{10.1111/ectj.12097}
#'
#' Cameron AC, Gelbach JB, Miller DL (2011). Robust inference with multiway
#'   clustering. \emph{JBES} 29(2), 238--249. \doi{10.1198/jbes.2010.07136}
#' @examples
#' set.seed(1)
#' G <- 40L; ng <- 10L; n <- G * ng
#' g <- rep(seq_len(G), each = ng)
#' u <- stats::rnorm(G)[g]                       # cluster effect
#' x <- stats::rnorm(n)
#' d <- stats::rbinom(n, 1, stats::plogis(0.5 * x + u))
#' y <- 2 * d + x + u + stats::rnorm(n)          # true ATE = 2
#' df <- data.frame(y = y, d = d, x = x, corridor = g)
#' morie_dml_clustered(df, "d", "y", "x", cluster = "corridor")$ate
#' @export
morie_dml_clustered <- function(data, treatment, outcome, covariates,
                                cluster = NULL, n_folds = 5L, seed = 123L,
                                eps = 0.02, ps = NULL) {
  cl_cols <- if (is.null(cluster)) character() else as.character(cluster)
  if (length(cl_cols) > 2L) {
    stop("`cluster` supports at most two-way clustering", call. = FALSE)
  }
  keep <- unique(c(treatment, outcome, covariates, cl_cols))
  miss <- setdiff(keep, names(data))
  if (length(miss)) stop("columns not found: ", paste(miss, collapse = ", "),
                         call. = FALSE)
  cc <- stats::complete.cases(data[, keep, drop = FALSE])
  data <- data[cc, , drop = FALSE]
  if (!is.null(ps)) ps <- ps[cc]

  d <- data[[treatment]]
  if (is.factor(d)) d <- as.integer(d) - 1L
  d <- as.numeric(d)
  uy <- sort(unique(d))
  if (!all(uy %in% c(0, 1))) {
    if (length(uy) != 2L) stop("`treatment` must be binary", call. = FALSE)
    d <- as.numeric(d == uy[2])
  }
  y <- as.numeric(data[[outcome]])
  X <- stats::model.matrix(
    stats::reformulate(covariates), data = data)   # includes intercept
  n <- length(y)
  p <- ncol(X)

  set.seed(seed)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  e_hat <- numeric(n)
  mu1 <- numeric(n)
  mu0 <- numeric(n)

  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    if (is.null(ps)) {
      e_hat[te] <- .dmlc_ps(X[tr, , drop = FALSE], d[tr],
                            X[te, , drop = FALSE], eps)
    }
    for (dv in c(1, 0)) {
      idx <- tr[d[tr] == dv]
      pred <- .dmlc_ols(X, idx, y, te, p)
      if (dv == 1) mu1[te] <- pred else mu0[te] <- pred
    }
  }
  if (!is.null(ps)) e_hat <- pmin(pmax(as.numeric(ps), eps), 1 - eps)

  # AIPW / doubly-robust influence function; ATE = its mean.
  psi <- (mu1 - mu0) + d * (y - mu1) / e_hat - (1 - d) * (y - mu0) / (1 - e_hat)
  ate <- mean(psi)
  infl <- psi - ate                       # influence function of the mean

  if (length(cl_cols) == 0L) {
    se <- stats::sd(psi) / sqrt(n)
    se_kind <- "iid"
    n_clusters <- NA_integer_
  } else {
    cls <- lapply(cl_cols, function(cn) as.character(data[[cn]]))
    se <- .dmlc_multiway_se(infl, cls, n)
    se_kind <- if (length(cl_cols) == 1L) "cluster-robust (1-way)"
               else "cluster-robust (2-way, CGM)"
    n_clusters <- length(unique(cls[[1]]))
  }
  z <- if (se > 0) ate / se else 0
  out <- list(
    ate = ate, se = se,
    ci95 = c(ate - 1.96 * se, ate + 1.96 * se),
    z = z, pval = 2 * stats::pnorm(-abs(z)),
    n = n, n_clusters = n_clusters, se_kind = se_kind
  )
  class(out) <- "morie_dml_clustered"
  out
}

#' @return \code{x}, invisibly.
#' @export
print.morie_dml_clustered <- function(x, ...) {
  cat(sprintf("Cluster-robust DML (AIPW)\n  ATE = %.4g  SE = %.4g [%s]\n",
              x$ate, x$se, x$se_kind))
  cat(sprintf("  95%% CI = [%.4g, %.4g]  z = %.3f  p = %.3g\n",
              x$ci95[1], x$ci95[2], x$z, x$pval))
  cat(sprintf("  n = %d%s\n", x$n,
              if (is.na(x$n_clusters)) "" else sprintf("  clusters = %d",
                                                       x$n_clusters)))
  invisible(x)
}

# Ridge-logistic propensity (tiny ridge for separation), predicted + clipped.
#' Internal helper: Dmlc Ps
#' @noRd
.dmlc_ps <- function(Xtr, dtr, Xte, eps) {
  fit <- tryCatch(
    stats::glm.fit(Xtr, dtr, family = stats::binomial()),
    warning = function(w) stats::glm.fit(Xtr, dtr, family = stats::binomial())
  )
  beta <- fit$coefficients
  beta[!is.finite(beta)] <- 0
  eta <- as.numeric(Xte %*% beta)
  pmin(pmax(stats::plogis(eta), eps), 1 - eps)
}

# Per-arm OLS outcome regression; robust to rank-deficiency and thin arms.
#' Internal helper: Dmlc Ols
#' @noRd
.dmlc_ols <- function(X, idx, y, te, p) {
  if (length(idx) < p + 2L) {
    return(rep(if (length(idx)) mean(y[idx]) else mean(y), length(te)))
  }
  Xm <- X[idx, , drop = FALSE]
  beta <- tryCatch(
    as.numeric(solve(crossprod(Xm), crossprod(Xm, y[idx]))),
    error = function(e) as.numeric(.morie_ginv(crossprod(Xm)) %*%
                                     crossprod(Xm, y[idx]))
  )
  as.numeric(X[te, , drop = FALSE] %*% beta)
}

# Liang-Zeger one-way cluster-robust SE of a mean, from the influence function.
#' Internal helper: Dmlc Cluster Se
#' @noRd
.dmlc_cluster_se <- function(infl, cluster, n) {
  grp <- tapply(infl, cluster, sum)
  sqrt(max(sum(grp^2, na.rm = TRUE) / (n^2), 0))
}

# Cameron-Gelbach-Miller up to two-way.
#' Internal helper: Dmlc Multiway Se
#' @noRd
.dmlc_multiway_se <- function(infl, clusters, n) {
  if (length(clusters) == 1L) return(.dmlc_cluster_se(infl, clusters[[1]], n))
  a <- clusters[[1]]
  b <- clusters[[2]]
  inter <- paste(a, b, sep = "|")
  va <- .dmlc_cluster_se(infl, a, n)^2
  vb <- .dmlc_cluster_se(infl, b, n)^2
  vab <- .dmlc_cluster_se(infl, inter, n)^2
  sqrt(max(va + vb - vab, 0))
}
