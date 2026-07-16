# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native permutation tests (feat/native-specializations, module 29).
# Replaces the coin package for the independence / Wilcoxon rank-sum /
# one-way (Fisher-Pitman) permutation tests. Built on the Strasser-Weber
# (1999) linear-statistic framework: a linear statistic T = sum_i g_i h_i'
# with exact conditional (permutation) expectation and covariance, so the
# asymptotic normal / chi-square reference distribution is obtained in
# closed form (no resampling). An exact two-sample permutation
# distribution is also provided by convolution.
#
# tests/cross validates statistics and p-values against coin to machine
# precision.
#
# Reference: Strasser, H., & Weber, C. (1999). On the asymptotic theory
# of permutation statistics. Mathematical Methods of Statistics, 8,
# 220-250.

# Strasser-Weber conditional moments of the linear statistic.
#   g : n x p transformation of the covariate (e.g. group indicators)
#   h : n x q influence (transformation) of the response
# Returns the vec'd statistic T (pq), its conditional mean mu and
# covariance Sigma, matching coin's `expectation()` / `covariance()`.
.morie_sw_moments <- function(g, h) {
  g <- as.matrix(g); h <- as.matrix(h)
  n <- nrow(g)
  Eh <- colMeans(h)                       # q
  hc <- sweep(h, 2L, Eh, "-")
  Vh <- crossprod(hc) / n                 # q x q  (1/n) sum (h-Eh)(h-Eh)'
  sg <- colSums(g)                        # p
  Sg <- crossprod(g)                      # p x p  sum g g'
  Tmat <- crossprod(g, h)                 # p x q  sum_i g_i h_i'
  Tvec <- as.vector(Tmat)                 # column-major vec
  mu <- as.vector(outer(sg, Eh))          # vec(sg Eh')
  Sigma <- (n / (n - 1)) * kronecker(Vh, Sg) -
    (1 / (n - 1)) * kronecker(Vh, outer(sg, sg))
  list(T = Tvec, mu = mu, Sigma = Sigma, n = n)
}

# Quadratic-form statistic c = (T-mu)' Sigma^+ (T-mu) ~ chi-square(df).
.morie_quad_stat <- function(m, tol = 1e-8) {
  d <- m$T - m$mu
  ev <- eigen(m$Sigma, symmetric = TRUE)
  pos <- ev$values > (tol * max(ev$values))
  df <- sum(pos)
  Spinv <- ev$vectors[, pos, drop = FALSE] %*%
    (t(ev$vectors[, pos, drop = FALSE]) / ev$values[pos])
  stat <- as.numeric(t(d) %*% Spinv %*% d)
  list(statistic = stat, df = df,
       p.value = stats::pchisq(stat, df, lower.tail = FALSE))
}

# Standardized (scalar / maximum) statistic and asymptotic p-value.
.morie_max_stat <- function(m, alternative = "two.sided") {
  d <- m$T - m$mu
  z <- d / sqrt(diag(m$Sigma))
  if (length(z) == 1L) {
    stat <- as.numeric(z)
    p <- switch(alternative,
      two.sided = 2 * stats::pnorm(-abs(stat)),
      greater   = stats::pnorm(stat, lower.tail = FALSE),
      less      = stats::pnorm(stat),
      stop("bad alternative"))
    return(list(statistic = stat, p.value = min(p, 1)))
  }
  # maximum-type: correlation of the standardized statistic
  R <- stats::cov2cor(m$Sigma)
  stat <- switch(alternative,
    two.sided = max(abs(z)),
    greater   = max(z),
    less      = min(z))
  list(statistic = stat, p.value = NA_real_, cor = R, z = z)
}

# Factor -> indicator design matching coin's f_trafo: k>2 gives k columns,
# k==2 gives a single column (indicator of the second level).
.morie_f_trafo <- function(f) {
  f <- as.factor(f)
  lv <- levels(f)
  if (length(lv) == 2L) {
    # coin's scalar linear statistic uses the FIRST level's indicator;
    # match its sign convention so one-sided p-values agree.
    matrix(as.numeric(f == lv[1L]), ncol = 1L,
           dimnames = list(NULL, lv[1L]))
  } else {
    m <- stats::model.matrix(~ f - 1)
    colnames(m) <- lv
    attr(m, "assign") <- NULL; attr(m, "contrasts") <- NULL
    m
  }
}

.morie_coin_parse <- function(formula, data) {
  mf <- stats::model.frame(formula, data = data)
  y <- mf[[1L]]
  x <- mf[[2L]]
  list(y = y, x = x)
}

#' Exact two-sample permutation distribution p-value for a rank/score sum
#'
#' For a two-group design the linear statistic is the sum of the response
#' scores in one group; its exact permutation distribution is the
#' convolution of choosing \code{n1} of the \code{n} scores. Ties are
#' handled because arbitrary real scores are convolved on a shifted grid.
#' @noRd
.morie_exact_twosample_p <- function(scores, n1, obs, mu,
                                     alternative = "two.sided") {
  n <- length(scores)
  # Distribution of the sum of a size-n1 subset via DP over counts.
  # Work on integer scores when possible for exactness (ranks are
  # half-integers at most -> scale by 2).
  sc2 <- scores * 2
  if (isTRUE(all.equal(sc2, round(sc2)))) {
    sc2 <- round(sc2); scale <- 2
  } else {
    return(NULL)  # non-rational scores: fall back to asymptotic
  }
  off <- -sum(pmin(sc2, 0))              # shift to keep indices >= 0
  maxsum <- sum(pmax(sc2, 0)) + off
  # dp[[k+1]] is a numeric vector over attainable shifted sums for size k
  dp <- vector("list", n1 + 1L)
  dp[[1L]] <- stats::setNames(1, as.character(off))  # size 0 -> sum 0 (shifted)
  tab <- new.env()
  # Use a matrix DP: counts[k+1, s+1] number of size-k subsets summing to s
  counts <- matrix(0, nrow = n1 + 1L, ncol = maxsum + 1L)
  counts[1L, off + 1L] <- 1
  for (v in sc2) {
    for (k in seq(min(n1, n), 1L)) {
      idx <- which(counts[k, ] > 0)
      if (!length(idx)) next
      counts[k + 1L, idx + v] <- counts[k + 1L, idx + v] + counts[k, idx]
    }
  }
  total <- choose(n, n1)
  sums_shifted <- which(counts[n1 + 1L, ] > 0) - 1L
  probs <- counts[n1 + 1L, sums_shifted + 1L] / total
  sums <- (sums_shifted - off) / scale     # back to original scale
  # obs is the observed group-2 score sum
  tol <- 1e-8
  p <- switch(alternative,
    greater   = sum(probs[sums >= obs - tol]),
    less      = sum(probs[sums <= obs + tol]),
    two.sided = {
      dev <- abs(sums - mu)
      sum(probs[dev >= abs(obs - mu) - tol])
    })
  min(p, 1)
}

#' Native general independence permutation test
#'
#' Reproduces \code{coin::independence_test} for a numeric response and
#' covariate (scalar linear statistic, asymptotic normal reference).
#'
#' @param formula \code{y ~ x}.
#' @param data A data frame.
#' @param alternative \code{"two.sided"} (default), \code{"greater"},
#'   \code{"less"}.
#' @param distribution \code{"asymptotic"} (only; kept for parity).
#' @return A list with \code{statistic} (standardized) and \code{p.value}.
#' @export
morie_indep_test <- function(formula, data,
                             alternative = "two.sided",
                             distribution = "asymptotic") {
  pd <- .morie_coin_parse(formula, data)
  g <- if (is.factor(pd$x) || is.character(pd$x)) .morie_f_trafo(pd$x) else
    matrix(as.numeric(pd$x), ncol = 1L)
  h <- matrix(as.numeric(pd$y), ncol = 1L)
  m <- .morie_sw_moments(g, h)
  res <- .morie_max_stat(m, alternative)
  list(statistic = res$statistic, p.value = res$p.value,
       method = "morie independence permutation test")
}

#' Native Wilcoxon rank-sum permutation test
#'
#' Reproduces \code{coin::wilcox_test(y ~ group)}: the response is
#' rank-transformed (midranks) and the standardized sum of ranks in the
#' second group is referred to the normal (asymptotic) or exact
#' permutation distribution.
#'
#' @param formula \code{y ~ group}, \code{group} a two-level factor.
#' @param data A data frame.
#' @param alternative \code{"two.sided"} / \code{"greater"} / \code{"less"}.
#' @param distribution \code{"asymptotic"} (default) or \code{"exact"}.
#' @return A list with \code{statistic} (standardized Z) and \code{p.value}.
#' @export
morie_wilcox_test <- function(formula, data,
                             alternative = "two.sided",
                             distribution = "asymptotic") {
  pd <- .morie_coin_parse(formula, data)
  f <- as.factor(pd$x)
  if (nlevels(f) != 2L) stop("wilcox_test needs a two-level group.")
  r <- rank(as.numeric(pd$y))               # midranks
  g <- .morie_f_trafo(f)                     # n x 1 indicator of level 2
  h <- matrix(r, ncol = 1L)
  m <- .morie_sw_moments(g, h)
  z <- (m$T - m$mu) / sqrt(diag(m$Sigma))
  stat <- as.numeric(z)
  if (identical(distribution, "exact")) {
    n1 <- sum(g[, 1L] == 1)
    p <- .morie_exact_twosample_p(r, n1, obs = m$T, mu = m$mu, alternative)
    if (is.null(p)) p <- switch(alternative,
      two.sided = 2 * stats::pnorm(-abs(stat)),
      greater = stats::pnorm(stat, lower.tail = FALSE),
      less = stats::pnorm(stat))
  } else {
    p <- switch(alternative,
      two.sided = 2 * stats::pnorm(-abs(stat)),
      greater = stats::pnorm(stat, lower.tail = FALSE),
      less = stats::pnorm(stat))
  }
  list(statistic = stat, p.value = min(p, 1),
       method = "morie Wilcoxon permutation test")
}

#' Native one-way permutation test (Fisher-Pitman)
#'
#' Reproduces \code{coin::oneway_test(y ~ group)}: the quadratic-form
#' permutation analogue of one-way ANOVA, referred to the chi-square
#' distribution (asymptotic).
#'
#' @param formula \code{y ~ group}, \code{group} a factor.
#' @param data A data frame.
#' @param distribution \code{"asymptotic"} (only; kept for parity).
#' @return A list with \code{statistic} (chi-square), \code{df} and
#'   \code{p.value}. For a two-level group the standardized scalar
#'   statistic and its normal p-value are returned instead, matching coin.
#' @export
morie_oneway_test <- function(formula, data, distribution = "asymptotic") {
  pd <- .morie_coin_parse(formula, data)
  f <- as.factor(pd$x)
  g <- .morie_f_trafo(f)
  h <- matrix(as.numeric(pd$y), ncol = 1L)
  m <- .morie_sw_moments(g, h)
  if (nlevels(f) == 2L) {
    z <- (m$T - m$mu) / sqrt(diag(m$Sigma))
    stat <- as.numeric(z)
    return(list(statistic = stat, df = 1L,
                p.value = 2 * stats::pnorm(-abs(stat)),
                method = "morie one-way permutation test"))
  }
  q <- .morie_quad_stat(m)
  list(statistic = q$statistic, df = q$df, p.value = q$p.value,
       method = "morie one-way permutation test")
}
