# voom precision weights and moderated t (Law, Chen, Shi & Smyth 2014;
# Smyth 2004).
#
# Anchors outside the module: the special functions against base R's
# trigamma, psigamma and pnorm; the t survival function against pt; the
# weighted fit against lm(weights=); the empirical-Bayes posterior against
# the shrinkage formula recomputed from the returned prior; and the
# unweighted, unmoderated route against an ordinary linear model, which is
# exactly what it claims to reduce to.

test_that("trigamma matches base R", {
  for (x in c(0.05, 0.5, 1, 2.5, 7, 19.5, 20, 50, 1000)) {
    expect_equal(.limmav_trigamma(x), trigamma(x), tolerance = 1e-12)
  }
  # the recurrence trigamma(x) = trigamma(x+1) + 1/x^2 holds across the
  # threshold where the implementation switches to the asymptotic series
  for (x in c(0.3, 5, 19.5)) {
    expect_equal(.limmav_trigamma(x),
                 .limmav_trigamma(x + 1) + 1 / x^2, tolerance = 1e-10)
  }
  # trigamma is positive and decreasing
  v <- vapply(c(0.5, 1, 2, 4, 8), .limmav_trigamma, numeric(1))
  expect_true(all(v > 0))
  expect_true(all(diff(v) < 0))
  expect_error(.limmav_trigamma(0), "needs x > 0")
  expect_error(.limmav_trigamma(-1), "needs x > 0")
})

test_that("tetragamma matches base R psigamma", {
  # a short asymptotic series: accurate to about one part in a thousand,
  # which is all the Newton step in trigamma_inverse needs
  for (x in c(0.5, 1, 2.5, 7, 19.5, 20, 60)) {
    expect_equal(.limmav_tetragamma(x), psigamma(x, 2), tolerance = 5e-3)
  }
  # the third derivative of log-gamma is negative
  expect_true(all(vapply(c(0.5, 2, 10), .limmav_tetragamma, numeric(1)) < 0))
  # and it is the derivative of trigamma, checked by finite difference
  h <- 1e-5
  for (x in c(1.5, 6)) {
    fd <- (.limmav_trigamma(x + h) - .limmav_trigamma(x - h)) / (2 * h)
    expect_equal(.limmav_tetragamma(x), fd, tolerance = 5e-3)
  }
})

test_that("trigamma_inverse inverts trigamma", {
  for (y in c(1e-4, 0.01, 0.2, 1, 3, 50)) {
    x <- .limmav_trigamma_inverse(y)
    expect_equal(trigamma(x), y, tolerance = 1e-9)
  }
  # the two asymptotic shortcuts are exact in their regimes
  expect_equal(.limmav_trigamma_inverse(1e8), 1 / sqrt(1e8))
  expect_equal(.limmav_trigamma_inverse(1e-8), 1e8)
  # the inverse is decreasing
  expect_true(.limmav_trigamma_inverse(0.1) > .limmav_trigamma_inverse(10))
  expect_error(.limmav_trigamma_inverse(0), "needs x > 0")
  expect_error(.limmav_trigamma_inverse(-2), "needs x > 0")
})

test_that("erf matches the normal integral", {
  # erf(x) = 2 * pnorm(x * sqrt(2)) - 1. This is Abramowitz & Stegun 7.1.26,
  # whose absolute error is bounded by 1.5e-7, so the comparison has to be
  # absolute rather than relative.
  for (x in c(-3, -1, -0.25, 0, 0.25, 1, 3)) {
    expect_lt(abs(.limmav_erf(x) - (2 * pnorm(x * sqrt(2)) - 1)), 1.5e-7)
  }
  expect_lt(abs(.limmav_erf(0)), 1.5e-7)
  # odd symmetry and the correct limits
  expect_equal(.limmav_erf(1.3), -.limmav_erf(-1.3))
  expect_equal(.limmav_erf(10), 1, tolerance = 1e-9)
  expect_equal(.limmav_erf(-10), -1, tolerance = 1e-9)
})

test_that("the t tail routine is the two-sided p-value", {
  # Despite its name this returns P(|T| > |t|), the incomplete beta
  # I_x(df/2, 1/2), not the one-sided survival function. Both call sites
  # assign it straight to a p-value, and the infinite-df branch beside it
  # spells out the same two-sided form, so this is the intended quantity.
  for (df in c(1, 3, 10, 50)) {
    for (t in c(-6, -4, -1, 0, 0.5, 2, 6)) {
      expect_equal(.limmav_t_sf(t, df), 2 * pt(-abs(t), df),
                   tolerance = 1e-9)
    }
  }
  # a zero statistic is never evidence against the null
  expect_equal(.limmav_t_sf(0, 7), 1)
  # it is symmetric in t, decreasing in |t|, and a probability
  expect_equal(.limmav_t_sf(-1.7, 12), .limmav_t_sf(1.7, 12))
  s <- vapply(c(0, 1, 2, 3, 6), .limmav_t_sf, numeric(1), df = 8)
  expect_true(all(diff(s) < 0))
  expect_true(all(s >= 0 & s <= 1))
  # it agrees with the normal limit as the degrees of freedom grow
  expect_equal(.limmav_t_sf(2, 1e6), 2 * pnorm(-2), tolerance = 1e-5)
})

test_that("log counts per million follow their definition", {
  counts <- matrix(c(10, 20, 0, 5, 100, 3), nrow = 2, byrow = TRUE)
  got <- .limmav_log_cpm(counts)
  R <- colSums(counts)
  want <- log2(counts + 0.5) -
    matrix(log2(R + 1), nrow = 2, ncol = 3, byrow = TRUE) + log2(1e6)
  expect_equal(got$y, want)
  expect_equal(got$R, R)
  # explicit library sizes are used instead of the column sums
  ls <- c(1000, 2000, 3000)
  expect_equal(.limmav_log_cpm(counts, ls)$R, ls)
  expect_equal(.limmav_log_cpm(counts, ls)$y,
               log2(counts + 0.5) -
                 matrix(log2(ls + 1), nrow = 2, ncol = 3, byrow = TRUE) +
                 log2(1e6))
  # the prior count and the library offset are both honoured
  expect_equal(.limmav_log_cpm(counts, prior_count = 2, lib_offset = 0)$y,
               log2(counts + 2) -
                 matrix(log2(R), nrow = 2, ncol = 3, byrow = TRUE) + log2(1e6))
  # a zero count is finite because of the prior
  expect_true(all(is.finite(got$y)))
  expect_error(.limmav_log_cpm(matrix(0, 0, 3)), "non-empty")
  expect_error(.limmav_log_cpm(matrix(-1, 2, 2)), "non-negative")
  expect_error(.limmav_log_cpm(counts, c(1, 2)), "one library size per sample")
  expect_error(.limmav_log_cpm(counts, c(0, 1, 2)), "must be positive")
})

test_that("the weighted fit agrees with lm(weights=)", {
  set.seed(2)
  n <- 12
  X <- cbind(1, c(rep(0, 6), rep(1, 6)))
  y <- 3 + 2 * X[, 2] + rnorm(n, 0, 0.4)
  w <- runif(n, 0.5, 2)
  got <- .limmav_weighted_lm(y, X, w, c(0, 1))
  ref <- lm(y ~ X[, 2], weights = w)
  # the contrast estimate is the coefficient itself
  expect_equal(got$est, unname(coef(ref)[2]), tolerance = 1e-9)
  expect_equal(got$se, unname(sqrt(diag(vcov(ref)))[2]), tolerance = 1e-9)
  expect_equal(got$t, unname(summary(ref)$coefficients[2, 3]),
               tolerance = 1e-8)
  expect_equal(got$df, ref$df.residual)
  expect_equal(got$sd, summary(ref)$sigma, tolerance = 1e-9)
  # a contrast picking the intercept recovers that coefficient
  gi <- .limmav_weighted_lm(y, X, w, c(1, 0))
  expect_equal(gi$est, unname(coef(ref)[1]), tolerance = 1e-9)
  # unit weights reduce to ordinary least squares
  gu <- .limmav_weighted_lm(y, X, rep(1, n), c(0, 1))
  ru <- lm(y ~ X[, 2])
  expect_equal(gu$est, unname(coef(ru)[2]), tolerance = 1e-9)
  expect_equal(gu$se, unname(sqrt(diag(vcov(ru)))[2]), tolerance = 1e-9)
  # a contrast of zero has zero estimate and no signal
  gz <- .limmav_weighted_lm(y, X, w, c(0, 0))
  expect_equal(gz$est, 0)
  expect_equal(gz$t, 0)
})

test_that("empirical Bayes shrinks variances toward the fitted prior", {
  # the genes need genuinely different true variances: a common scale would
  # leave the observed spread of log-variances at its own sampling variance
  # and no prior would be estimable
  set.seed(5)
  G <- 60
  dfg <- 8
  s2 <- exp(rnorm(G, 0, 1)) * rchisq(G, dfg) / dfg
  eb <- .limmav_ebayes(s2, dfg)
  expect_true(is.finite(eb$d0))
  expect_true(eb$d0 > 0)
  expect_true(eb$s0_sq > 0)
  # the posterior is exactly the prior-weighted average the method defines
  expect_equal(eb$s2_post, (eb$d0 * eb$s0_sq + dfg * s2) / (eb$d0 + dfg))
  # each posterior variance lies between the raw estimate and the prior
  expect_true(all(pmin(s2, eb$s0_sq) - 1e-12 <= eb$s2_post))
  expect_true(all(eb$s2_post <= pmax(s2, eb$s0_sq) + 1e-12))
  # shrinkage strictly reduces the spread of the variance estimates
  expect_true(stats::sd(eb$s2_post) < stats::sd(s2))
  # per-gene degrees of freedom are accepted as a vector
  ebv <- .limmav_ebayes(s2, rep(dfg, G))
  expect_equal(ebv$s2_post, eb$s2_post)

  # when every gene shares one variance there is nothing to estimate, and the
  # method says so instead of inventing a prior
  flat <- .limmav_ebayes(rep(2, 40), 6)
  expect_true(flat$no_gene_variation)
  expect_equal(flat$d0, Inf)
  expect_equal(flat$s2_post, rep(flat$s0_sq, 40))
  expect_true(all(is.infinite(flat$df_total)))

  expect_error(.limmav_ebayes(numeric(0), 3), "no variances to moderate")
  expect_error(.limmav_ebayes(c(1, 2, 3), c(1, 2)),
               "one degrees-of-freedom value per gene")
  expect_error(.limmav_ebayes(c(0, 0), 5), "zero variance")
})

test_that("voom weights track the mean-variance trend", {
  set.seed(9)
  G <- 40
  m <- 6
  X <- cbind(1, c(rep(0, 3), rep(1, 3)))
  base <- rep(c(5, 50, 500, 5000), each = G / 4)
  counts <- matrix(rpois(G * m, base), nrow = G)
  v <- .limmav_voom_weights(counts, X)
  expect_equal(dim(v$weights), c(G, m))
  expect_equal(dim(v$log_cpm), c(G, m))
  expect_true(all(v$weights > 0))
  expect_true(all(is.finite(v$weights)))
  # the log-CPM matrix is what the helper produces
  expect_equal(v$log_cpm, .limmav_log_cpm(counts)$y)
  expect_equal(v$lib_sizes, colSums(counts))
  # the trend is evaluated on the fitted mean log-count, one point per gene
  expect_length(v$mean_log_count, G)
  expect_length(v$sqrt_sd, G)
  expect_true(all(v$sqrt_sd >= 0))
  # low-count genes are noisier, so they must receive the smaller weights
  lo <- mean(v$weights[1:(G / 4), ])
  hi <- mean(v$weights[(3 * G / 4 + 1):G, ])
  expect_true(lo < hi)
})

test_that("the unmoderated unweighted route is an ordinary linear model", {
  set.seed(11)
  G <- 25
  m <- 8
  grp <- c(rep(0, 4), rep(1, 4))
  counts <- matrix(rpois(G * m, 200), nrow = G)
  X <- cbind(1, grp)
  fit <- morie_limmav(counts, X, contrast = c(0, 1),
                      weights = FALSE, moderate = FALSE)
  y <- .limmav_log_cpm(counts)$y
  for (g in c(1, 7, G)) {
    ref <- lm(y[g, ] ~ grp)
    expect_equal(fit$estimate[g], unname(coef(ref)[2]), tolerance = 1e-8)
    expect_equal(fit$se[g], unname(sqrt(diag(vcov(ref)))[2]), tolerance = 1e-8)
    expect_equal(fit$t[g], unname(summary(ref)$coefficients[2, 3]),
                 tolerance = 1e-7)
    expect_equal(fit$pvalue[g], unname(summary(ref)$coefficients[2, 4]),
                 tolerance = 1e-7)
  }
  # the residual degrees of freedom are n - rank(X)
  expect_equal(fit$df, m - ncol(X))
  # two-sided p-values follow from the t statistics and their df
  expect_equal(fit$pvalue, 2 * pt(-abs(fit$t), fit$df), tolerance = 1e-9)
  # log fold change is the same quantity under its other name
  expect_equal(fit$log_fold_change, fit$estimate)
  expect_equal(fit$n_genes, G)
  expect_equal(fit$n_samples, m)
  expect_false(fit$weighted)
  expect_false(fit$moderated)
  expect_null(fit$d0)
  expect_match(fit$note, "moderate=False")
  expect_match(fit$method, "Law, Chen, Shi & Smyth")
  # the Benjamini-Hochberg column is the adjustment of the p-values
  expect_equal(fit$padj, p.adjust(fit$pvalue, "BH"))
})

test_that("moderation borrows prior degrees of freedom and shrinks variances", {
  # gene-specific overdispersion gives real variance heterogeneity, which is
  # what the empirical-Bayes step exists to exploit
  set.seed(2)
  G <- 90
  m <- 8
  grp <- c(rep(0, 4), rep(1, 4))
  disp <- rep(c(0.001, 0.05, 0.5), each = G / 3)
  counts <- t(vapply(seq_len(G),
                     function(g) rnbinom(m, mu = 300, size = 1 / disp[g]),
                     numeric(m)))
  X <- cbind(1, grp)
  mod <- morie_limmav(counts, X, contrast = c(0, 1), moderate = TRUE)
  un <- morie_limmav(counts, X, contrast = c(0, 1), moderate = FALSE)
  expect_true(mod$moderated)
  expect_true(is.finite(mod$d0))
  expect_true(mod$d0 > 0)
  expect_true(mod$s0_sq > 0)
  # the moderated test is run on d_g + d0 degrees of freedom
  expect_equal(mod$df_total, rep(un$df + mod$d0, G))
  expect_true(all(mod$df_total > un$df))
  # the posterior variance is exactly the prior-weighted average
  expect_equal(mod$s2_post,
               (mod$d0 * mod$s0_sq + un$df * mod$s2_gene) /
                 (mod$d0 + un$df))
  # each posterior variance lies between the raw estimate and the prior
  expect_true(all(pmin(mod$s2_gene, mod$s0_sq) - 1e-12 <= mod$s2_post))
  expect_true(all(mod$s2_post <= pmax(mod$s2_gene, mod$s0_sq) + 1e-12))
  # shrinkage tightens the spread of the variance estimates
  expect_true(stats::sd(mod$s2_post) < stats::sd(mod$s2_gene))
  # the point estimates are untouched: only the variance changes
  expect_equal(mod$estimate, un$estimate, tolerance = 1e-10)
  # p-values remain a monotone function of the absolute statistic
  expect_equal(order(mod$pvalue), order(-abs(mod$t)))
  expect_equal(mod$pvalue, 2 * pt(-abs(mod$t), mod$df_total),
               tolerance = 1e-9)
  expect_match(mod$note, "Smyth 2004")
})

test_that("homogeneous variances leave nothing to moderate", {
  # with one common variance the observed spread of the log-variances does
  # not exceed its own sampling variance, so no prior is estimable and the
  # method reports that rather than inventing one
  set.seed(1)
  G <- 60
  m <- 8
  grp <- c(rep(0, 4), rep(1, 4))
  counts <- matrix(rpois(G * m, 300), nrow = G)
  mod <- morie_limmav(counts, cbind(1, grp), contrast = c(0, 1),
                      moderate = TRUE)
  expect_equal(mod$d0, Inf)
  expect_true(all(is.infinite(mod$df_total)))
  # every gene is then tested against the single pooled prior variance
  expect_equal(mod$s2_post, rep(mod$s0_sq, G))
  # and the p-values fall back on the normal limit
  expect_true(all(mod$pvalue >= 0 & mod$pvalue <= 1))
  expect_equal(mod$pvalue, 2 * pnorm(-abs(mod$t)), tolerance = 1e-6)
})

test_that("a real group difference is detected among many null genes", {
  # the differential gene has to sit in a realistic library: with only a
  # handful of genes one twenty-fold gene dominates the totals and drags
  # every other gene's counts per million down with it
  set.seed(17)
  m <- 10
  grp <- c(rep(0, 5), rep(1, 5))
  counts <- rbind(
    c(rpois(5, 100), rpois(5, 2000)),
    matrix(rpois(200 * m, 500), nrow = 200)
  )
  fit <- morie_limmav(counts, cbind(1, grp), contrast = c(0, 1))
  expect_true(fit$pvalue[1] < 1e-4)
  expect_true(fit$estimate[1] > 2)
  # it is the most significant gene, and the only discovery
  expect_equal(which.min(fit$pvalue), 1L)
  expect_true(fit$padj[1] < 0.05)
  # Benjamini-Hochberg controls the false discovery rate, not the family-wise
  # error, so a stray null among 200 is within its design
  expect_true(sum(fit$padj < 0.05) <= 3L)
  # the null genes carry no fold change worth speaking of
  expect_true(max(abs(fit$estimate[-1])) < 1)
})

test_that("p-values are calibrated when nothing is differential", {
  # a test that reports too many discoveries on null data is worse than
  # useless, so this checks the null distribution itself rather than any
  # single gene
  set.seed(23)
  m <- 10
  grp <- c(rep(0, 5), rep(1, 5))
  counts <- matrix(rpois(400 * m, 500), nrow = 400)
  fit <- morie_limmav(counts, cbind(1, grp), contrast = c(0, 1))
  expect_true(mean(fit$pvalue < 0.05) < 0.09)
  expect_true(mean(fit$pvalue < 0.01) < 0.03)
  # multiplicity adjustment leaves no discovery at all
  expect_equal(sum(fit$padj < 0.05), 0L)
  # and the p-values are indistinguishable from uniform
  expect_true(stats::ks.test(fit$pvalue, "punif")$p.value > 0.01)
  # the fold changes are centred on zero
  expect_equal(mean(fit$estimate), 0, tolerance = 0.05)
})

test_that("the design may be given as a matrix, a list, or group labels", {
  set.seed(19)
  G <- 20
  counts <- matrix(rpois(G * 6, 400), nrow = G)
  X <- cbind(1, c(0, 0, 0, 1, 1, 1))
  a <- morie_limmav(counts, X, contrast = c(0, 1), moderate = FALSE)
  # a character vector of group labels builds the same two-column design
  b <- morie_limmav(counts, c("a", "a", "a", "b", "b", "b"),
                    contrast = c(0, 1), moderate = FALSE)
  expect_equal(b$estimate, a$estimate, tolerance = 1e-10)
  # a list of rows is read as a model matrix
  rows <- lapply(seq_len(6), function(i) X[i, ])
  cc <- morie_limmav(counts, rows, contrast = c(0, 1), moderate = FALSE)
  expect_equal(cc$estimate, a$estimate, tolerance = 1e-10)
  # a list of scalar labels is read as groups
  dd <- morie_limmav(counts, as.list(c("a", "a", "a", "b", "b", "b")),
                     contrast = c(0, 1), moderate = FALSE)
  expect_equal(dd$estimate, a$estimate, tolerance = 1e-10)
  # a single group cannot support a contrast
  expect_error(morie_limmav(counts, rep("a", 6)), "only one group")
  expect_error(morie_limmav(counts, as.list(rep("a", 6))), "only one group")
})

test_that("Benjamini-Hochberg adjustment matches p.adjust", {
  p <- c(0.001, 0.008, 0.039, 0.041, 0.042, 0.06, 0.074, 0.205, 0.212, 0.216)
  expect_equal(.limmav_benjamini_hochberg(p), p.adjust(p, "BH"))
  # the step-up procedure enforces monotonicity
  expect_true(all(diff(.limmav_benjamini_hochberg(sort(p))) >= -1e-12))
  expect_equal(.limmav_benjamini_hochberg(0.5), 0.5)
  # order is preserved, not sorted away
  sh <- c(0.2, 0.01, 0.5)
  expect_equal(.limmav_benjamini_hochberg(sh), p.adjust(sh, "BH"))
  expect_true(all(.limmav_benjamini_hochberg(p) >= p - 1e-12))
})
