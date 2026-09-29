# Coverage tests for R/survrsf_native.R (Ishwaran et al. 2008 random
# survival forests). Estimators are recomputed from their definitions
# and, where available, against the survival package.

rsf_data <- function() {
  i <- 1:40
  x1 <- (i * 37) %% 17 / 17
  x2 <- (i * 11) %% 13 / 13
  x3 <- (i * 5) %% 7 / 7
  t <- round(10 * exp(-1.5 * x1 + 0.3 * x2) * (1 + (i %% 5) / 10), 2)
  e <- as.integer((i %% 4) != 0)
  list(X = cbind(x1, x2, x3), t = t, e = e)
}

test_that("Nelson-Aalen estimator and conservation of events", {
  t <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  e <- c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1)
  na <- morie_survrsf_nelson_aalen(t, e)
  ut <- sort(unique(t[e == 1]))
  chf <- cumsum(vapply(ut, function(s) sum(t == s & e == 1) / sum(t >= s), 0))
  expect_equal(na$time, ut)
  expect_equal(na$chf, chf, tolerance = 1e-12)
  expect_equal(na$deaths, 7L)
  skip_if_not_installed("survival")
  sf <- survival::survfit(survival::Surv(t, e) ~ 1)
  expect_equal(na$chf, sf$cumhaz[sf$n.event > 0], tolerance = 1e-12)
  cc <- morie_survrsf_conservation_check(t, e)
  H <- vapply(t, function(s) max(c(0, chf[ut <= s])), 0)
  expect_equal(cc$sum_chf, sum(H), tolerance = 1e-12)
  expect_true(cc$conserved)
  expect_error(morie_survrsf_nelson_aalen(1:3, 1:2), "event indicators")
  expect_error(morie_survrsf_nelson_aalen(numeric(0), numeric(0)), "no observations")
})

test_that("log-rank statistic equals sqrt of survdiff's chi-square", {
  skip_if_not_installed("survival")
  d <- rsf_data()
  g <- as.integer(d$X[, 1] > 0.5)
  s <- morie_survrsf_logrank_statistic(d$t, d$e, g)
  sd <- survival::survdiff(survival::Surv(d$t, d$e) ~ g)
  expect_equal(s, sqrt(sd$chisq), tolerance = 1e-10)
  expect_equal(morie_survrsf_logrank_statistic(d$t, d$e, rep(0L, 40)), 0)
  expect_error(morie_survrsf_logrank_statistic(d$t, d$e, g[-1]), "same length")
})

test_that("log-rank scores and the two-sample score statistic", {
  t <- c(2, 5, 3, 8, 5, 1, 7)
  e <- c(1, 0, 1, 1, 1, 0, 1)
  N <- 7
  gam <- vapply(t, function(s) sum(t <= s), 0)
  a <- vapply(1:N, function(i) e[i] - sum((t <= t[i] & e == 1) / (N - gam + 1)), 0)
  expect_equal(morie_survrsf_logrank_scores(t, e), a, tolerance = 1e-12)
  grp <- c(0, 1, 0, 1, 1, 0, 0)
  m <- 4
  n <- 3
  Tm <- sum(a[grp == 0])
  ET <- m * sum(a) / N
  VT <- m * n / (N^2 * (N - 1)) * (N * sum(a^2) - sum(a)^2)
  expect_equal(morie_survrsf_logrank_score_statistic(t, e, grp), abs(Tm - ET) / sqrt(VT), tolerance = 1e-12)
  expect_equal(morie_survrsf_logrank_score_statistic(t, e, rep(1, 7)), 0)
  expect_error(morie_survrsf_logrank_scores(1:3, 1:2), "same length")
})

test_that("conservation residuals and the conservation-of-events split statistic", {
  t <- c(2, 5, 3, 8, 5, 1, 7, 4)
  e <- c(1, 0, 1, 1, 1, 0, 1, 1)
  na <- morie_survrsf_nelson_aalen(t, e)
  Hat <- function(s, na) if (any(na$time <= s)) max(na$chf[na$time <= s]) else 0
  o <- order(t)
  res <- cumsum(vapply(t[o], Hat, 0, na = na)) - cumsum(e[o])
  expect_equal(morie_survrsf_conservation_residuals(t, e), res, tolerance = 1e-12)
  expect_equal(morie_survrsf_conservation_residuals(numeric(0), numeric(0)), numeric(0))
  grp <- c(0, 0, 1, 1, 0, 1, 1, 0)
  tot <- 0
  for (g in 0:1) {
    idx <- which(grp == g)
    m <- morie_survrsf_conservation_residuals(t[idx], e[idx])
    tot <- tot + length(idx) * sum(abs(m[-length(m)]))
  }
  expect_equal(morie_survrsf_conserve_statistic(t, e, grp), 1 / (1 + tot / 8), tolerance = 1e-12)
  expect_equal(morie_survrsf_conserve_statistic(t, e, rep(1, 8)), 0)
})

test_that("best split maximises the rule's statistic over midpoints", {
  d <- rsf_data()
  bs <- morie_survrsf_best_split(d$X, d$t, d$e, features = c(0L, 1L), min_deaths = 3)
  best <- -Inf
  for (j in 1:2) {
    v <- sort(unique(d$X[, j]))
    for (cc in (v[-1] + v[-length(v)]) / 2) {
      gp <- as.integer(d$X[, j] > cc)
      if (sum(d$e[gp == 0]) < 3 || sum(d$e[gp == 1]) < 3) next
      s <- morie_survrsf_logrank_statistic(d$t, d$e, gp)
      if (s > best) {
        best <- s
        arg <- c(j - 1, cc)
      }
    }
  }
  expect_equal(bs$statistic, best, tolerance = 1e-12)
  expect_equal(c(bs$variable, bs$cut), arg, tolerance = 1e-12)
  expect_equal(sort(c(bs$left, bs$right)), 1:40)
  sc <- morie_survrsf_best_split(d$X, d$t, d$e, 0L, 3, rule = "logrankscore")
  gp <- as.integer(d$X[, 1] > sc$cut)
  expect_equal(sc$statistic, morie_survrsf_logrank_score_statistic(d$t, d$e, gp), tolerance = 1e-12)
  cs <- morie_survrsf_best_split(d$X, d$t, d$e, 0L, 3, rule = "conserve")
  expect_equal(cs$statistic, morie_survrsf_conserve_statistic(d$t, d$e, as.integer(d$X[, 1] > cs$cut)), tolerance = 1e-12)
  rr <- morie_survrsf_best_split(d$X, d$t, d$e, 0L, 3, rule = "logrankrandom")
  expect_true(rr$cut %in% ((sort(unique(d$X[, 1]))[-1] + sort(unique(d$X[, 1]))[-length(unique(d$X[, 1]))]) / 2))
  expect_null(morie_survrsf_best_split(d$X, d$t, d$e, 0L, min_deaths = 40))
  expect_error(morie_survrsf_best_split(d$X, d$t, d$e, 0L, rule = "gini"), "rule must be one of")
})

test_that("trees hold Nelson-Aalen leaves and route by their cuts", {
  d <- rsf_data()
  tr <- morie_survrsf_grow_tree(d$X, d$t, d$e, mtry = 3, min_deaths = 3, seed = 4)
  expect_equal(tr$mtry, 3L)
  leaves <- list()
  walk <- function(nd) if (nd$leaf) leaves[[length(leaves) + 1L]] <<- nd else {
    walk(nd$left)
    walk(nd$right)
  }
  walk(tr$root)
  expect_equal(sort(unlist(lapply(leaves, `[[`, "idx"))), 1:40)
  for (lf in leaves) {
    expect_equal(lf$na, morie_survrsf_nelson_aalen(d$t[lf$idx], d$e[lf$idx]))
    for (i in lf$idx) expect_identical(morie_survrsf_predict_tree(tr, d$X[i, ])$idx, lf$idx)
  }
  expect_false(tr$root$leaf)
  expect_error(morie_survrsf_grow_tree(d$X, numeric(0), numeric(0)), "no observations")
})

test_that("forest, OOB ensemble CHF, mortality, C-index and VIMP", {
  d <- rsf_data()
  f <- morie_survrsf_forest(d$X, d$t, d$e, n_trees = 6, min_deaths = 3, seed = 2)
  expect_equal(f$n_trees, length(f$trees))
  expect_equal(f$oob_fraction, sum(40 - lengths(f$inbag)) / (length(f$inbag) * 40), tolerance = 1e-12)
  expect_identical(morie_survrsf, morie_survrsf_forest)
  H <- function(nd, s) if (any(nd$na$time <= s)) max(nd$na$chf[nd$na$time <= s]) else 0
  ch <- morie_survrsf_ensemble_chf(f, d$X, t = 5)
  ref <- vapply(1:40, function(i) {
    b <- which(!vapply(f$inbag, function(u) i %in% u, TRUE))
    if (!length(b)) return(NaN)
    mean(vapply(b, function(k) H(morie_survrsf_predict_tree(f$trees[[k]], d$X[i, ]), 5), 0))
  }, 0)
  expect_equal(ch, ref, tolerance = 1e-12)
  chi <- morie_survrsf_ensemble_chf(f, d$X, t = 5, oob = FALSE)
  expect_false(anyNA(chi))
  mo <- morie_survrsf_mortality(f, d$X, oob = FALSE)
  ref_m <- vapply(1:40, function(i) mean(vapply(f$trees, function(tr) {
    nd <- morie_survrsf_predict_tree(tr, d$X[i, ])
    sum(vapply(d$t, function(s) H(nd, s), 0))
  }, 0)), 0)
  expect_equal(mo, ref_m, tolerance = 1e-12)
  ci <- morie_survrsf_c_index(d$t, d$e, mo)
  expect_equal(ci$prediction_error, 1 - ci$c_index)
  skip_if_not_installed("survival")
  # distinct event times: Harrell's C equals survival::concordance with
  # higher mortality predicting the shorter time
  sc <- survival::concordance(survival::Surv(d$t, d$e) ~ mo, reverse = TRUE)
  if (!anyDuplicated(d$t)) expect_equal(ci$c_index, sc$concordance, tolerance = 1e-12)
  v <- morie_survrsf_vimp(f, d$X, variables = c(0, 2))
  base <- morie_survrsf_c_index(d$t, d$e, morie_survrsf_mortality(f, d$X))$prediction_error
  expect_equal(v$baseline_error, base, tolerance = 1e-12)
  expect_equal(v$estimate, max(unlist(v$vimp)), tolerance = 1e-12)
  expect_error(morie_survrsf_forest(d$X, d$t, rep(0, 40), n_trees = 2), "no tree could be grown")
})

test_that("Harrell's C on a hand-checked set of pairs", {
  t <- c(1, 2, 3, 3, 5)
  e <- c(1, 0, 1, 1, 0)
  p <- c(9, 7, 4, 4, 1)
  r <- morie_survrsf_c_index(t, e, p)
  # permissible pairs: (1,2)(1,3)(1,4)(1,5)(3,4)(3,5)(4,5); (2,x) have the
  # shorter time censored. All concordant except the tie (3,4) with equal
  # predictions, which scores 1 under the both-dead tie rule.
  expect_equal(r$permissible, 7)
  expect_equal(r$c_index, 1)
  # a case without a prediction (NaN, e.g. never out of bag) is left out
  expect_equal(morie_survrsf_c_index(t, e, c(p[1:4], NaN))$permissible, 4)
  expect_error(morie_survrsf_c_index(c(1, 2), c(0, 0), c(1, 2)), "no permissible pairs")
  expect_error(morie_survrsf_c_index(1:3, c(1, 1, 1), 1:2), "same length")
})

test_that("rule registry and cheatsheet", {
  st <- morie_survrsf_rule_status()
  expect_setequal(st$available, morie_survrsf_SPLIT_RULES)
  expect_true(morie_survrsf_rule_status("conserve")$available)
  expect_error(morie_survrsf_rule_status("gini"), "rule must be one of")
  expect_match(morie_survrsf_cheatsheet(), "Nelson-Aalen")
})
