# SPDX-License-Identifier: AGPL-3.0-or-later
.hedd_golden_max <- function(f, a, b, tol = 1e-12) {
  g <- (sqrt(5) - 1) / 2
  c <- b - g * (b - a)
  d <- a + g * (b - a)
  while (b - a > tol) {
    if (f(c) > f(d)) {
      b <- d
      d <- c
      c <- b - g * (b - a)
    } else {
      a <- c
      c <- d
      d <- a + g * (b - a)
    }
  }
  (a + b) / 2
}

#' Quality levels and average outgoing quality of a sampling plan
#'
#' For the binomial single sampling plan (n, c), P_A(p) = pbinom(c, n, p); AQL
#' solves P_A = 1 - alpha and RQL solves P_A = beta (beta quantiles, Hedderich,
#' Sachs & Reynarowych 2023, eq 7.17), and AOQL maximises p P_A(p) (N - n) / N
#' (eq 7.18) by golden section.
#'
#' @param n,c Sample size and acceptance number.
#' @param alpha,beta_risk Producer's and consumer's risks.
#' @param lot_size Optional lot size N.
#' @return Named list: aql, rql, aoql, p_aoql.
#' @references Montgomery, D. C. (2013). Introduction to Statistical Quality
#'   Control, ch. 15.
#' @examples
#' accqlv(46, 1, lot_size = 1000)
#' @export
accqlv <- function(n, c, alpha = 0.05, beta_risk = 0.10, lot_size = NULL) {
  if (!(c >= 0 && c < n)) stop("need 0 <= c < n", call. = FALSE)
  f <- if (is.null(lot_size)) 1 else (lot_size - n) / lot_size
  aoq <- function(p) p * stats::pbinom(c, n, p) * f
  pm <- .hedd_golden_max(aoq, 0, 1)
  list(aql = stats::qbeta(alpha, c + 1, n - c), rql = stats::qbeta(1 - beta_risk, c + 1, n - c),
       aoql = aoq(pm), p_aoql = pm)
}

#' Smallest binomial single sampling plan for two risk points
#'
#' Searches (n, c) as AcceptanceSampling::find.plan: from c = 0, n = 1, raise n
#' while P_A(RQL) > beta, else raise c while P_A(AQL) < 1 - alpha (Hedderich,
#' Sachs & Reynarowych 2023, eq 7.17).
#'
#' @param aql,rql Quality levels, 0 < aql < rql < 1.
#' @param alpha,beta_risk Producer's and consumer's risks.
#' @param max_n Search limit.
#' @return Named list: n, c, p_accept_aql, p_accept_rql.
#' @references Kiermeier, A. (2008). Journal of Statistical Software 26(6).
#' @examples
#' accpln(0.01, 0.06)
#' @export
accpln <- function(aql, rql, alpha = 0.05, beta_risk = 0.10, max_n = 100000) {
  if (!(aql > 0 && aql < rql && rql < 1)) stop("need 0 < aql < rql < 1", call. = FALSE)
  n <- 1
  c <- 0
  repeat {
    if (stats::pbinom(c, n, rql) > beta_risk) {
      n <- n + 1
    } else if (stats::pbinom(c, n, aql) < 1 - alpha) {
      c <- c + 1
    } else {
      break
    }
    if (n > max_n) stop("no plan within max_n", call. = FALSE)
  }
  list(n = n, c = c, p_accept_aql = stats::pbinom(c, n, aql), p_accept_rql = stats::pbinom(c, n, rql))
}

#' Box-Cox lambda by the probability plot correlation
#'
#' Correlates the sorted transform (x^lambda - 1) / lambda (log x at 0) with
#' the qqnorm quantiles qnorm(ppoints(n)) for each candidate and returns the
#' maximiser (Hedderich, Sachs & Reynarowych 2023, eq 7.27 and its bcplot).
#'
#' @param x Positive data.
#' @param lambdas Candidates, default seq(-5, 2, by = 0.1).
#' @return Named list: lambda, cor, lambdas.
#' @references Box, G. E. P. & Cox, D. R. (1964). JRSS B 26, 211-252.
#' @examples
#' bcppcc(c(20, 22, 24, 21, 19, 30, 40, 23, 24, 25, 19))$lambda
#' @export
bcppcc <- function(x, lambdas = NULL) {
  x <- as.numeric(x)
  if (length(x) < 3 || min(x) <= 0) stop("need at least 3 positive values", call. = FALSE)
  lam <- if (is.null(lambdas)) -5 + (0:70) * 0.1 else as.numeric(lambdas)
  q <- stats::qnorm(stats::ppoints(length(x)))
  v <- vapply(lam, function(l) {
    stats::cor(q, sort(if (l == 0) log(x) else (x^l - 1) / l))
  }, numeric(1))
  list(lambda = lam[which.max(v)], cor = v, lambdas = lam)
}

#' Scored chi-square test of homogeneity
#'
#' (n - 1) (sum T_j^2 / n_j - T^2 / n) / (sum x_i^2 n_i. - T^2 / n) on k - 1 df
#' for k samples (columns) over ordered categories (rows) with scores x
#' (Hedderich, Sachs & Reynarowych 2023, eq 7.362); equals (n - 1) R^2 of the
#' one-way ANOVA of the scores.
#'
#' @param table r x k matrix of counts.
#' @param scores r category scores.
#' @return Named list: statistic, df, p_value.
#' @references Hedderich, J., Sachs, L. & Reynarowych, Z. (2023). Applied
#'   Statistics: Methods Using R. Springer.
#' @examples
#' schomg(matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE), c(1, 0, -1))
#' @export
schomg <- function(table, scores) {
  t <- as.matrix(table)
  storage.mode(t) <- "double"
  x <- as.numeric(scores)
  if (nrow(t) != length(x) || nrow(t) < 2 || ncol(t) < 2) {
    stop("need an r x k table (r, k >= 2) and r scores", call. = FALSE)
  }
  nj <- colSums(t)
  if (min(nj) <= 0) stop("every sample needs observations", call. = FALSE)
  tj <- colSums(x * t)
  n <- sum(nj)
  tt <- sum(tj)
  den <- sum(x^2 * rowSums(t)) - tt^2 / n
  if (den <= 0) stop("scores do not vary", call. = FALSE)
  s <- (n - 1) * (sum(tj^2 / nj) - tt^2 / n) / den
  list(statistic = s, df = ncol(t) - 1, p_value = stats::pchisq(s, ncol(t) - 1, lower.tail = FALSE))
}

#' Chi-square test with a Monte Carlo p-value
#'
#' Pearson's X^2 with p = (1 + #(X^2_b >= X^2)) / (B + 1) over B tables drawn
#' by r2dtable with the observed margins, as chisq.test(simulate.p.value =
#' TRUE) (Hedderich, Sachs & Reynarowych 2023, p. 739).
#'
#' @param table r x c matrix of counts.
#' @param B Number of simulated tables.
#' @param seed Optional seed; the caller's RNG state is restored.
#' @return Named list: statistic, p_value, B.
#' @references Hope, A. C. A. (1968). JRSS B 30, 582-598.
#' @examples
#' chisqmc(matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE), B = 500, seed = 1)
#' @export
chisqmc <- function(table, B = 2000, seed = NULL) {
  t <- as.matrix(table)
  storage.mode(t) <- "double"
  if (nrow(t) < 2 || ncol(t) < 2 || any(t < 0)) {
    stop("need an r x c table (r, c >= 2) of non-negative counts", call. = FALSE)
  }
  rs <- rowSums(t)
  cs <- colSums(t)
  if (min(rs) == 0 || min(cs) == 0) stop("a margin is zero", call. = FALSE)
  if (!is.null(seed)) {
    genv <- globalenv()
    old <- if (exists(".Random.seed", envir = genv, inherits = FALSE)) get(".Random.seed", envir = genv) else NULL
    on.exit(if (is.null(old)) rm(".Random.seed", envir = genv) else assign(".Random.seed", old, envir = genv))
    set.seed(seed)
  }
  e <- outer(rs, cs) / sum(t)
  x <- sum((t - e)^2 / e)
  sims <- vapply(stats::r2dtable(B, as.integer(rs), as.integer(cs)), function(s) sum((s - e)^2 / e), numeric(1))
  list(statistic = x, p_value = (1 + sum(sims >= (1 - 64 * .Machine$double.eps) * x)) / (B + 1), B = B)
}

#' Backward elimination by the partial F statistic
#'
#' Removes, one at a time, the regressor with the smallest F = (RSS(p - 1) -
#' RSS(p)) / (RSS(p) / (n - (p + 1))) while it is below f_out (Hedderich, Sachs
#' & Reynarowych 2023, eq 8.39; drop1(fit, test = "F")). The intercept stays.
#'
#' @param X n x p matrix of regressors.
#' @param y Response.
#' @param names Regressor names, default colnames(X) or x1..xp.
#' @param f_out Threshold.
#' @return Named list: selected, removed (data frame of name and F),
#'   coefficients, rss, last_f.
#' @references Draper, N. R. & Smith, H. (1998). Applied Regression Analysis,
#'   sec. 15.2.
#' @examples
#' i <- 1:30
#' X <- cbind(x1 = sin(i), x2 = cos(2 * i), x3 = log(i))
#' bkelim(X, 1 + 2 * X[, 1] - X[, 3] + 0.3 * cos(5 * i))$selected
#' @export
bkelim <- function(X, y, names = NULL, f_out = 4) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  p <- ncol(X)
  n <- nrow(X)
  if (is.null(names)) names <- if (is.null(colnames(X))) paste0("x", seq_len(p)) else colnames(X)
  if (n != length(y) || length(names) != p) stop("X must be n x p with n = length(y) and p names", call. = FALSE)
  fit <- function(cols) {
    q <- qr(cbind(1, X[, cols, drop = FALSE]))
    if (q$rank < length(cols) + 1) stop("design matrix is rank deficient", call. = FALSE)
    list(coef = unname(qr.coef(q, y)), rss = sum(qr.resid(q, y)^2))
  }
  active <- seq_len(p)
  rm_name <- character(0)
  rm_f <- numeric(0)
  last <- numeric(0)
  repeat {
    f0 <- fit(active)
    if (!length(active)) break
    df <- n - (length(active) + 1)
    if (df <= 0) stop("too few observations for the full model", call. = FALSE)
    fs <- vapply(active, function(j) (fit(setdiff(active, j))$rss - f0$rss) / (f0$rss / df), numeric(1))
    last <- stats::setNames(fs, names[active])
    k <- which.min(fs)
    if (fs[k] >= f_out) break
    rm_name <- c(rm_name, names[active[k]])
    rm_f <- c(rm_f, fs[k])
    active <- active[-k]
  }
  list(selected = names[active], removed = data.frame(name = rm_name, F = rm_f),
       coefficients = f0$coef, rss = f0$rss, last_f = last)
}

#' Hierarchical log-linear model for a three-way table
#'
#' Iterative proportional fitting of the margins in `margins` (1-based axis
#' vectors as loglin), e.g. list(1, 2, 3) for mutual independence (Hedderich,
#' Sachs & Reynarowych 2023, sec. 8.5.5, eq 8.100), with G^2, Pearson X^2
#' (eq 8.106), Pearson residuals and df = cells minus the parameters of every
#' axis subset inside a margin.
#'
#' @param table I x J x K array of counts.
#' @param margins List of axis vectors.
#' @param tol Convergence tolerance on the fitted counts.
#' @param max_iter Iteration limit.
#' @return Named list: fit, residuals, lrt, pearson, df, p_lrt, p_pearson.
#' @references Bishop, Y. M. M., Fienberg, S. E. & Holland, P. W. (1975).
#'   Discrete Multivariate Analysis, ch. 3.
#' @examples
#' ll3way(array(c(911, 538, 44, 456, 3, 43, 2, 279), c(2, 2, 2)), list(c(1, 2), c(1, 3), c(2, 3)))$lrt
#' @export
ll3way <- function(table, margins, tol = 1e-12, max_iter = 10000) {
  y <- array(as.numeric(table), dim(table))
  d <- dim(y)
  if (length(d) != 3 || any(y < 0)) stop("need a three-way array of non-negative counts", call. = FALSE)
  ms <- lapply(margins, function(m) sort(unique(as.integer(m))))
  if (!length(ms) || any(vapply(ms, function(m) !length(m) || any(!m %in% 1:3), logical(1)))) {
    stop("margins must be non-empty vectors of axes 1, 2, 3", call. = FALSE)
  }
  fit <- array(1, d)
  for (it in seq_len(max_iter)) {
    delta <- 0
    for (m in ms) {
      ratio <- apply(y, m, sum) / apply(fit, m, sum)
      ratio[!is.finite(ratio)] <- 0
      new <- sweep(fit, m, ratio, "*")
      delta <- max(delta, abs(new - fit))
      fit <- new
    }
    if (delta < tol) break
  }
  pos <- y > 0
  lrt <- 2 * sum(y[pos] * log(y[pos] / fit[pos]))
  fp <- fit > 0
  pearson <- sum((y[fp] - fit[fp])^2 / fit[fp])
  subsets <- unique(unlist(lapply(ms, function(m) {
    unlist(lapply(0:length(m), function(k) lapply(utils::combn(length(m), k, simplify = FALSE), function(i) m[i])),
           recursive = FALSE)
  }), recursive = FALSE))
  df <- prod(d) - sum(vapply(subsets, function(s) prod(d[s] - 1), numeric(1)))
  res <- array(0, d)
  res[fp] <- (y[fp] - fit[fp]) / sqrt(fit[fp])
  list(fit = fit, residuals = res, lrt = lrt, pearson = pearson, df = df,
       p_lrt = if (df > 0) stats::pchisq(lrt, df, lower.tail = FALSE) else 1,
       p_pearson = if (df > 0) stats::pchisq(pearson, df, lower.tail = FALSE) else 1)
}

#' Classify and solve a linear equation system
#'
#' Consistent iff rank(A, b) = rank(A); with full column rank the solution is
#' unique (A^-1 b when square) or, when inconsistent, the least-squares
#' (A'A)^-1 A'b, both by QR (Hedderich, Sachs & Reynarowych 2023, eqs
#' 2.47-2.50). A rank-deficient A gives x = NULL.
#'
#' @param A n x m matrix.
#' @param b Right-hand side.
#' @param tol Rank tolerance.
#' @return Named list: x, rank_A, rank_Ab, consistent, kind, rss.
#' @references Golub, G. H. & Van Loan, C. F. (2013). Matrix Computations,
#'   sec. 5.3.
#' @examples
#' linsys(matrix(c(2, 1, -1, -3, -1, 2, -2, 1, 2), 3, byrow = TRUE), c(8, -11, -3))$x
#' @export
linsys <- function(A, b, tol = 1e-7) {
  A <- as.matrix(A)
  storage.mode(A) <- "double"
  b <- as.numeric(b)
  if (nrow(A) != length(b)) stop("A must be n x m and b of length n", call. = FALSE)
  q <- qr(A, tol = tol)
  ra <- q$rank
  rab <- qr(cbind(A, b), tol = tol)$rank
  x <- NULL
  rss <- NULL
  if (ra == ncol(A)) {
    x <- unname(qr.coef(q, b))
    rss <- sum(qr.resid(q, b)^2)
    kind <- if (rab == ra) "unique" else "least-squares"
  } else {
    kind <- "rank-deficient"
  }
  list(x = x, rank_A = ra, rank_Ab = rab, consistent = ra == rab, kind = kind, rss = rss)
}
