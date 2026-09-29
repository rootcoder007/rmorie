# Coverage for phlpr .. pmiwd exports plus Pptest. Every expectation is
# recomputed in the test body.

cov_pp_series <- function() {
  e <- c(0.3, -0.5, 0.8, 0.1, -0.2, 0.6, -0.9, 0.4, 0.2, -0.3, 0.7, -0.1, 0.5, -0.6, 0.2,
         0.9, -0.4, 0.1, 0.3, -0.7, 0.6, 0.0, -0.2, 0.4, 0.8, -0.5, 0.1, 0.3, -0.1, 0.2)
  cumsum(e) + 0.05 * seq_along(e)
}

test_that("Pptest, Ppunit and Pptrend compute the Phillips-Perron statistic", {
  x <- cov_pp_series()
  nn <- length(x)
  yt <- x[-1]
  y1 <- x[-nn]
  n <- nn - 1
  tt <- seq_len(n) - n / 2
  fit <- stats::lm(yt ~ tt + y1)
  u <- stats::residuals(fit)
  lag <- max(1L, as.integer(trunc(4 * (n / 100)^0.25)))
  s2 <- mean(u^2)
  acf_l <- vapply(seq_len(lag), function(i) sum(u[(i + 1):n] * u[1:(n - i)]), 0)
  l2 <- s2 + 2 * sum((1 - seq_len(lag) / (lag + 1)) * acf_l) / n
  s <- seq_len(n)
  D <- n^2 * (n^2 - 1) * sum(y1^2) / 12 - n * sum(s * y1)^2 +
    n * (n + 1) * sum(s * y1) * sum(y1) - n * (n + 1) * (2 * n + 1) * sum(y1)^2 / 6
  rho <- unname(stats::coef(fit)[3])
  tstat <- (rho - 1) / summary(fit)$coefficients[3, 2]
  zt <- sqrt(s2) / sqrt(l2) * tstat - n^3 / (4 * sqrt(3) * sqrt(D) * sqrt(l2)) * (l2 - s2)
  za <- n * (rho - 1) - n^6 / (24 * D) * (l2 - s2)
  r <- Pptest(x)
  expect_equal(r$statistic, zt, tolerance = 1e-10)
  expect_equal(c(r$s2, r$lambda2, r$rho), c(s2, l2, rho), tolerance = 1e-10)
  expect_equal(Pptest(x, kind = "Z(alpha)")$statistic, za, tolerance = 1e-10)
  skip_if_not_installed("tseries")
  tp <- suppressWarnings(tseries::pp.test(x, type = "Z(t_alpha)"))
  expect_equal(r$statistic, unname(tp$statistic), tolerance = 1e-10)
  expect_equal(r$p_value, tp$p.value, tolerance = 1e-10)
  expect_equal(Ppunit(x)$statistic, r$statistic)
  expect_equal(Pptrend(x, lags = 3)$statistic, Pptest(x, lags = 3)$statistic)
  expect_error(Pptrend(x, trend = FALSE), "trend = FALSE is not available")
  expect_error(Pptest(1:5), "at least 6")
  expect_error(Pptest(x, kind = "Z(beta)"), "kind must be")
})

test_that("F81 substitution and Felsenstein pruning", {
  pi <- c(0.1, 0.2, 0.3, 0.4)
  P <- substitution_matrix(0.7, pi, u = 1.5)
  e <- exp(-1.5 * 0.7)
  expect_equal(P, e * diag(4) + (1 - e) * matrix(pi, 4, 4, byrow = TRUE), tolerance = 1e-12)
  # reversibility pi_i P_ij = pi_j P_ji
  expect_equal(pi * P, t(pi * P), tolerance = 1e-12)
  seqs <- list(a = "ACGT", b = "ACTT", c = "GCTA")
  tree <- list(list("a", 0.2, "b", 0.3), 0.1, "c", 0.4)
  idx <- function(ch) match(ch, c("A", "C", "G", "T"))
  site <- function(k) {
    ch <- vapply(seqs, function(s) substr(s, k, k), "")
    tot <- 0
    for (r in 1:4) for (m in 1:4) {
      tot <- tot + pi[r] * substitution_matrix(0.1, pi)[r, m] *
        substitution_matrix(0.2, pi)[m, idx(ch[["a"]])] * substitution_matrix(0.3, pi)[m, idx(ch[["b"]])] *
        substitution_matrix(0.4, pi)[r, idx(ch[["c"]])]
    }
    tot
  }
  L <- vapply(1:4, site, 0)
  expect_equal(vapply(1:4, function(k) site_likelihood(tree, seqs, k, pi), 0), L, tolerance = 1e-12)
  r <- morie_phylml(tree, seqs, pi)
  expect_equal(r$log_likelihood, sum(log(L)), tolerance = 1e-12)
  expect_equal(r$n_sites, 4L)
  # pulley principle: only the sum of the two root branches matters
  t1 <- list("a", 0.25, list("b", 0.1, "c", 0.3), 0.15)
  t2 <- list("a", 0.05, list("b", 0.1, "c", 0.3), 0.35)
  expect_equal(morie_phylml(t1, seqs, pi)$log_likelihood, morie_phylml(t2, seqs, pi)$log_likelihood,
               tolerance = 1e-12)
  # gaps are uninformative tips
  expect_equal(site_likelihood(list("a", 0.2, "b", 0.3), list(a = "-", b = "A"), 1, pi), pi[1],
               tolerance = 1e-12)
  expect_same_function(phylogenetic_ml, morie_phylml)
  expect_error(substitution_matrix(-1), "branch length")
  expect_error(morie_phylml(tree, list(a = "AC", b = "A", c = "AC")), "common length")
  expect_error(morie_phylml(tree, seqs, pi = c(0.5, 0.5, 0, 0.1)), "sum to 1")
  expect_error(site_likelihood(list("a", 0.1, "b", 0.1), list(a = "Z", b = "A"), 1), "unknown base")
})

test_that("optimise_branch recovers the Jukes-Cantor distance for two taxa", {
  seqs <- list(a = "ACGTACGTAC", b = "ACGTTCGAAC")
  p <- 2 / 10
  # F81 with uniform pi: P(differ) = 3/4 (1 - exp(-t)), so the MLE is
  t_hat <- -log(1 - 4 * p / 3)
  r <- optimise_branch(function(v) list("a", v, "b", 0), seqs)
  # a maximum is only located to about sqrt(machine eps) relative: the
  # log-likelihood is flat to second order there, whatever the bracket
  expect_equal(r$length, t_hat, tolerance = 1e-6)
  expect_false(r$at_bound)
  expect_equal(r$log_likelihood, morie_phylml(list("a", r$length, "b", 0), seqs)$log_likelihood,
               tolerance = 1e-12)
  expect_error(optimise_branch("x", seqs), "callable")
  expect_error(optimise_branch(function(v) v, seqs, lo = 2, hi = 1), "lo < hi")
})

test_that("Phylog is root-to-tip regression", {
  dates <- c(2001, 2003, 2004, 2007, 2010, 2012)
  div <- c(0.011, 0.016, 0.019, 0.027, 0.033, 0.040)
  r <- Phylog(dates, div)
  f <- stats::lm(div ~ dates)
  expect_equal(c(r$intercept, r$rate), unname(stats::coef(f)), tolerance = 1e-9)
  expect_equal(r$tmrca, -stats::coef(f)[[1]] / stats::coef(f)[[2]], tolerance = 1e-9)
  expect_equal(r$r_squared, summary(f)$r.squared, tolerance = 1e-10)
  expect_equal(r$residuals, unname(stats::residuals(f)), tolerance = 1e-10)
  expect_error(Phylog(dates[1:2], div[1:2]), "at least 3")
  expect_error(Phylog(rep(2000, 4), 1:4), "identical")
  expect_error(Phylog(dates, div[-1]), "equal length")
})

test_that("Phylotr applies the original Saitou-Nei joining", {
  # additive tree ((A:1, B:2):3, (C:4, D:5))
  D <- matrix(c(0, 3, 8, 9,
                3, 0, 9, 10,
                8, 9, 0, 9,
                9, 10, 9, 0), 4, byrow = TRUE)
  r <- Phylotr(D, labels = c("A", "B", "C", "D"))
  Sij <- function(i, j) {
    k <- setdiff(1:4, c(i, j))
    sum(D[i, k] + D[j, k]) / 4 + D[i, j] / 2 + D[k[1], k[2]] / 2
  }
  S <- outer(1:4, 1:4, Vectorize(function(i, j) if (i < j) Sij(i, j) else Inf))
  expect_equal(r$joins[[1]]$S, min(S), tolerance = 1e-12)
  expect_equal(c(r$joins[[1]]$a, r$joins[[1]]$b), c("A", "B"))
  expect_equal(c(r$joins[[1]]$La, r$joins[[1]]$Lb), c(1, 2), tolerance = 1e-12)
  expect_equal(r$s0, sum(D[upper.tri(D)]) / 3, tolerance = 1e-12)
  # the new node sits at the average distance (D_ik + D_jk) / 2
  dab <- c(mean(D[1:2, 3]), mean(D[1:2, 4]))
  expect_equal(r$final_lengths, c((9 + dab[1] - dab[2]) / 2, (9 + dab[2] - dab[1]) / 2,
                                  (dab[1] + dab[2] - 9) / 2), tolerance = 1e-12)
  expect_equal(r$final_labels, c("C", "D", "(A-B)"))
  expect_error(Phylotr(D[1:3, 1:3]), "at least 4")
  expect_error(Phylotr(D, labels = 1:3), "labels length")
})

test_that("morie_pibmd measures prior-data conflict and informativeness", {
  post <- c(1.2, 1.5, 0.9, 1.4, 1.1, 1.3, 1.0, 1.6, 1.25, 1.35)
  prior <- c(-1.0, 0.5, 2.1, -0.3, 1.0, 0.2, 1.7, -1.5, 0.8, 0.0, 2.5, -0.7)
  r <- morie_pibmd_prior_informativeness_bias_diagnostic(post, prior, n_grid = 256)
  mq <- mean(post)
  sq <- stats::var(post)
  mp <- mean(prior)
  sp <- stats::var(prior)
  kl <- function(m1, v1, m2, v2) 0.5 * log(v2 / v1) + (v1 + (m1 - m2)^2) / (2 * v2) - 0.5
  expect_equal(r$kl_divergence, kl(mq, sq, mp, sp), tolerance = 1e-12)
  expect_equal(r$kl_divergence_reverse, kl(mp, sp, mq, sq), tolerance = 1e-12)
  expect_equal(r$shrinkage, 1 - sq / sp, tolerance = 1e-12)
  expect_equal(r$bias_in_prior_sd, (mq - mp) / sqrt(sp), tolerance = 1e-12)
  f <- mean(prior <= mq)
  expect_equal(r$conflict_p_value_empirical, 2 * min(f, 1 - f), tolerance = 1e-12)
  u <- (seq_len(256) - 0.5) / 256
  w1 <- mean(abs(stats::quantile(post, u, type = 7, names = FALSE) -
                   stats::quantile(prior, u, type = 7, names = FALSE)))
  expect_equal(r$wasserstein_1, w1, tolerance = 1e-12)
  bw <- function(v) {
    a <- min(stats::sd(v), stats::IQR(v) / 1.34)
    0.9 * a * length(v)^-0.2
  }
  hq <- bw(post)
  hp <- bw(prior)
  lo <- min(min(post) - 4 * hq, min(prior) - 4 * hp)
  hi <- max(max(post) + 4 * hq, max(prior) + 4 * hp)
  dx <- (hi - lo) / 256
  xs <- lo + (seq_len(256) - 0.5) * dx
  kd <- function(s, h) vapply(xs, function(x) mean(stats::dnorm((x - s) / h)) / h, 0)
  a <- kd(post, hq)
  b <- kd(prior, hp)
  a <- a / (sum(a) * dx)
  b <- b / (sum(b) * dx)
  k <- a > 1e-300
  expect_equal(r$kl_divergence_kde, sum(a[k] * log(a[k] / b[k])) * dx, tolerance = 1e-10)
  m <- morie_pibmd(post, list(mean = 0, sd = 2))
  expect_equal(m$kl_divergence, kl(mq, sq, 0, 4), tolerance = 1e-12)
  expect_equal(m$conflict_p_value, 2 * stats::pnorm(-abs(mq / 2)), tolerance = 1e-12)
  expect_true(is.nan(m$wasserstein_1))
  expect_error(morie_pibmd(1, prior), "at least two posterior")
  expect_error(morie_pibmd(post, list(mean = 0)), "must give 'mean' and 'sd'")
  expect_error(morie_pibmd(post, list(mean = 0, sd = 0)), "must be positive")
  expect_error(morie_pibmd(post, 3), "at least two prior draws")
  expect_error(morie_pibmd(post, c(1, 1, 1)), "no spread")
})

test_that("Piepar is the population intervention effect of setting x", {
  x1 <- c(0.2, 1.1, 0.5, 1.8, 0.9, 1.4, 0.3, 1.6)
  x2 <- c(1, 0, 1, 1, 0, 0, 1, 0)
  y <- 1 + 0.7 * x1 - 0.4 * x2 + c(0.1, -0.2, 0.05, 0.15, -0.1, 0.2, -0.05, 0)
  xs <- c(0.5, 1.0, 2.0)
  r <- Piepar(y, cbind(x1, x2), xs)
  f <- stats::lm(y ~ x1 + x2)
  interv <- mean(vapply(xs, function(v) mean(stats::predict(f, data.frame(x1 = v, x2 = x2))), 0))
  expect_equal(r$intervened, interv, tolerance = 1e-10)
  expect_equal(r$estimate, interv - mean(y), tolerance = 1e-10)
  expect_equal(r$se, abs(mean(xs) - mean(x1)) * summary(f)$coefficients[2, 2], tolerance = 1e-10)
})

test_that("PiidInt, Pmedex and PmfVar evaluate their closed forms", {
  expect_equal(PiidInt(0.3, 4)$p_intersection, 0.3^4, tolerance = 1e-12)
  expect_error(PiidInt(1.5), "single value in")
  expect_error(PiidInt(0.5, -1), "integer >= 0")
  m <- Pmedex(0.3, 1.2)
  expect_equal(m$estimate, 0.25)
  expect_equal(m$implied_nde, 0.9, tolerance = 1e-12)
  expect_true(is.nan(Pmedex(1, 0)$estimate))
  v <- PmfVar(c(0, 1, 3), c(0.2, 0.5, 0.3))
  mu <- 0.5 + 0.9
  expect_equal(v$mean, mu, tolerance = 1e-12)
  expect_equal(v$variance, 0.2 * mu^2 + 0.5 * (1 - mu)^2 + 0.3 * (3 - mu)^2, tolerance = 1e-12)
  expect_equal(v$sd, sqrt(v$variance), tolerance = 1e-12)
  expect_error(PmfVar(1:2, c(0.5, 0.6)), "sum to 1")
  expect_error(PmfVar(1:2, 1), "equal-length")
})

test_that("morie_plncF is Planck's law with Wien and Stefan-Boltzmann limits", {
  h <- 6.62607015e-34
  cc <- 299792458
  kB <- 1.380649e-23
  lam <- c(3e-7, 5e-7, 1e-6, 1e-5)
  r <- morie_plncF(lam, 5800)
  expect_equal(r$estimate, 2 * h * cc^2 / lam^5 / expm1(h * cc / (lam * kB * 5800)), tolerance = 1e-12)
  # the peak of B(lam) located numerically
  pk <- stats::optimize(function(l) morie_plncF(l, 5800)$estimate, c(2e-7, 1e-6), maximum = TRUE,
                        tol = 1e-15)$maximum
  expect_equal(r$peak_wavelength, pk, tolerance = 1e-6)
  # pi * integral of B over wavelength is sigma T^4 (integrate over log-lambda)
  tot <- pi * stats::integrate(function(z) morie_plncF(exp(z), 5800)$estimate * exp(z),
                               log(5e-8), log(1e-3), rel.tol = 1e-10)$value
  expect_equal(r$total_power, tot, tolerance = 1e-7)
  expect_error(morie_plncF(lam, 0), "temperature")
  expect_error(morie_plncF(-1, 300), "wavelengths")
})

test_that("partial-linear GRF solves the local Robinson regression", {
  n <- 44
  X <- cbind(seq(0, 1, length.out = n), rep(c(0.1, 0.6, 0.3, 0.9), 11))
  W <- rep(c(1, 0, 0.5, 0.2), 11) + 0.1 * sin(1:n)
  # y exactly 2 W: without centering every neighbourhood returns tau = 2 and a zero score
  y <- 2 * W
  r <- morie_plrgrf(y, W, X, n_trees = 6, min_leaf = 3, seed = 2, center = FALSE,
                    at = X[c(5, 30), ])
  expect_equal(r$tau, c(2, 2), tolerance = 1e-10)
  expect_equal(r$se, c(0, 0), tolerance = 1e-10)
  expect_equal(r$ate, 2, tolerance = 1e-10)
  rf <- residual_forest(y + 0.3 * X[, 1], W, X, at = X[7, , drop = FALSE], n_trees = 6, min_leaf = 3,
                        seed = 1)
  fr <- grow_forest(X, y + 0.3 * X[, 1], n_trees = 6, min_leaf = 3, seed = 1)
  a <- forest_weights(fr$trees, X, X[7, , drop = FALSE])
  wb <- sum(a * W)
  yy <- y + 0.3 * X[, 1]
  yb <- sum(a * yy)
  expect_equal(rf$tau, sum(a * (W - wb) * (yy - yb)) / sum(a * (W - wb)^2), tolerance = 1e-12)
  lc <- local_centering(yy, W, X, n_folds = 2, n_trees = 5, min_leaf = 3, seed = 4)
  val <- which((seq_len(n) - 1) %% 2 == 0)
  tr <- setdiff(seq_len(n), val)
  f2 <- grow_forest(X[tr, ], yy[tr], n_trees = 5, min_leaf = 3, seed = 4)
  pred <- vapply(val, function(i) sum(forest_weights(f2$trees, X[tr, ], X[i, , drop = FALSE]) * yy[tr]), 0)
  expect_equal(lc$mh[val], pred, tolerance = 1e-12)
  expect_same_function(partial_linear_grf, morie_plrgrf)
  expect_error(morie_plrgrf(y[1:30], W[1:30], X[1:30, ]), "at least 40")
  expect_error(morie_plrgrf(y, W[-1], X), "outcomes but")
  expect_error(residual_forest(y, rep(1, n), X, n_trees = 3, min_leaf = 3), "no treatment variation")
})

test_that("morie_plsqs_pls_regression matches pls::plsr", {
  X <- cbind(c(1.2, 2.3, 3.1, 4.8, 5.0, 6.7, 7.2, 8.1),
             c(2.0, 1.5, 3.9, 3.1, 5.5, 4.2, 6.8, 6.1),
             c(0.3, -0.2, 0.9, 0.1, 1.4, 0.2, 1.1, 0.7))
  y <- c(2.1, 2.9, 4.2, 4.8, 6.3, 6.1, 8.0, 8.4)
  r <- morie_plsqs_pls_regression(X, y, n_components = 2)
  # full rank: PLS with p components is OLS
  r3 <- morie_plsqs_pls_regression(X, y, n_components = 3)
  expect_equal(r3$coefficients, unname(stats::coef(stats::lm(y ~ X))[-1]), tolerance = 1e-9)
  expect_equal(r$fitted, r$intercept + as.numeric(X %*% r$coefficients), tolerance = 1e-12)
  expect_equal(r$r_squared, 1 - sum(r$residuals^2) / sum((y - mean(y))^2), tolerance = 1e-12)
  skip_if_not_installed("pls")
  pm <- pls::plsr(y ~ X, ncomp = 2, method = "oscorespls")
  expect_equal(r$coefficients, as.numeric(stats::coef(pm, ncomp = 2)), tolerance = 1e-9)
  expect_error(morie_plsqs_pls_regression(X, y[-1]), "responses")
  expect_error(morie_plsqs_pls_regression(X, y, 0), "at least one component")
  expect_error(morie_plsqs_pls_regression(X, rep(1, 8)), "no covariance")
})

test_that("pmiwd is the Church-Hanks association ratio", {
  x <- c(1, 1, 2, 2, 3, 1, 2, 3)
  y <- c(5, 5, 6, 5, 6, 6, 6, 6, 9)
  r <- pmiwd(x, y, window = 3)
  y8 <- y[1:8]
  tab <- table(x, y8) / 8
  px <- rowSums(tab)
  py <- colSums(tab)
  keep <- which(tab > 0, arr.ind = TRUE)
  pmi <- log2(tab[keep] / (px[keep[, 1]] * py[keep[, 2]])) - log2(2)
  expect_equal(r$mi_bits, sum(tab[keep] * pmi), tolerance = 1e-12)
  expect_equal(sort(r$pmi), unname(sort(pmi)), tolerance = 1e-12)
  expect_equal(r$n, 8L)
  expect_equal(morie_pointwise_mutual_info(c(0, 0, 1, 1), c(0, 0, 1, 1))$estimate, 1, tolerance = 1e-12)
})
