# Latent class analysis by EM with inverse-probability-weighted
# class-specific treatment effects (Lanza, Coffman & Xu 2013; Goodman 1974;
# Robins, Hernan & Brumback 2000).
#
# Anchors outside the module: EM's defining guarantee that the observed
# log-likelihood never decreases; the exact closed form of the one-class
# model; every reported summary recomputed from the returned quantities;
# a two-class design whose item probabilities and prevalences are known;
# and the module's own stated claim that the weighted and unweighted class
# effects coincide exactly when treatment is unrelated to the indicators.

# A well-separated two-class population: class 1 endorses every indicator
# with probability 0.9, class 2 with probability 0.1.
sim_lc <- function(n = 400, seed = 1, dep = FALSE) {
  set.seed(seed)
  cls <- rbinom(n, 1, 0.35)                  # 1 with prob 0.35
  Q <- 4
  H <- matrix(0, n, Q)
  for (q in seq_len(Q)) {
    H[, q] <- rbinom(n, 1, ifelse(cls == 1, 0.9, 0.1))
  }
  # treatment either independent of the indicators, or driven by them
  p <- if (dep) plogis(-1 + 1.5 * rowSums(H)) else rep(0.5, n)
  A <- rbinom(n, 1, p)
  # outcome: a class-specific effect of treatment
  y <- 1 + 2 * cls + ifelse(cls == 1, 3, 1) * A + rnorm(n, 0, 0.5)
  list(y = y, A = A, H = H, cls = cls)
}

test_that("the EM log-likelihood never decreases", {
  d <- sim_lc(300, 2)
  for (K in 1:4) {
    r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, K, max_iter = 200L)
    # the defining property of EM, which holds whether or not the run
    # reaches its tolerance
    expect_true(all(diff(r$loglik_path) >= -1e-9))
    expect_equal(r$loglik, tail(r$loglik_path, 1))
    expect_equal(length(r$loglik_path), r$iterations)
    expect_true(r$loglik < 0)
  }
  # the identified models reach their tolerance
  for (K in 1:2) {
    r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, K, max_iter = 200L)
    expect_true(r$converged)
    expect_true(r$iterations < 200L)
  }
  # the data were generated from two classes, so a third is redundant: the
  # likelihood creeps along a ridge instead of settling, and the run says so
  # rather than declaring a convergence it did not reach
  over <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 3L,
                                             max_iter = 200L)
  expect_false(over$converged)
  expect_equal(over$iterations, 200L)
  expect_true(all(diff(over$loglik_path) >= -1e-9))
  # more classes cannot fit worse
  lls <- vapply(1:3, function(K) {
    morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, K)$loglik
  }, numeric(1))
  expect_true(all(diff(lls) >= -1e-6))
  # but the information criterion prefers the two-class model that generated
  # the data
  bics <- vapply(1:3, function(K) {
    morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, K)$bic
  }, numeric(1))
  expect_equal(which.min(bics), 2L)
})

test_that("one class reduces to independent Bernoulli item means", {
  d <- sim_lc(200, 3)
  r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 1L)
  expect_equal(r$class_prevalence, 1)
  # with a single class every posterior is one, so the M-step returns the
  # plain column means
  expect_equal(as.numeric(r$item_probabilities), colMeans(d$H),
               tolerance = 1e-8)
  expect_equal(as.numeric(r$posterior), rep(1, nrow(d$H)))
  # and the log-likelihood is the saturated independent-Bernoulli one
  rho <- colMeans(d$H)
  want <- sum(vapply(seq_len(ncol(d$H)), function(q) {
    sum(ifelse(d$H[, q] > 0.5, log(rho[q]), log(1 - rho[q])))
  }, numeric(1)))
  expect_equal(r$loglik, want, tolerance = 1e-8)
  # one class has Q free item probabilities and no free prevalence
  expect_equal(r$n_parameters, ncol(d$H))
  # every subject is in class zero
  expect_equal(r$labels, rep(0, nrow(d$H)))
  expect_equal(r$entropy, 0, tolerance = 1e-9)
})

test_that("a two-class design recovers its item probabilities", {
  d <- sim_lc(1200, 5)
  r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L)
  expect_equal(dim(r$item_probabilities), c(2L, 4L))
  # classes come back in prevalence order, so the majority class is first
  expect_true(r$class_prevalence[1] >= r$class_prevalence[2])
  expect_equal(sum(r$class_prevalence), 1)
  # the majority class is the one endorsing few indicators (prevalence 0.65)
  expect_equal(r$class_prevalence[1], 0.65, tolerance = 0.06)
  expect_true(all(r$item_probabilities[1, ] < 0.25))
  expect_true(all(r$item_probabilities[2, ] > 0.75))
  # and the assignment agrees with the truth up to the label convention
  agree <- mean((r$labels == 1) == (d$cls == 1))
  expect_true(agree > 0.9)
  # posterior rows are distributions
  expect_equal(rowSums(r$posterior), rep(1, 1200), tolerance = 1e-10)
  expect_true(all(r$posterior >= 0 & r$posterior <= 1))
})

test_that("the reported summaries are recomputable from the output", {
  d <- sim_lc(400, 7)
  r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L)
  n <- 400
  # the aggregate effect is the prevalence-weighted class effect
  expect_equal(r$ate, sum(r$class_prevalence * r$class_ate))
  expect_equal(r$naive_ate, sum(r$class_prevalence * r$naive_class_ate))
  expect_equal(r$estimate, r$class_ate)
  # each class effect is the difference of its two weighted means
  expect_equal(r$class_ate, r$class_mean_treated - r$class_mean_control)
  expect_equal(r$naive_class_ate,
               r$naive_class_mean_treated - r$naive_class_mean_control)
  # the information criterion and the entropy follow their definitions
  expect_equal(r$n_parameters, 2L - 1L + 2L * 4L)
  expect_equal(r$bic, -2 * r$loglik + r$n_parameters * log(n))
  expect_equal(r$entropy, -sum(r$posterior * log(pmax(r$posterior, 1e-300))))
  # the effective sample size is Kish's
  expect_equal(r$effective_sample_size, sum(r$weights)^2 / sum(r$weights^2))
  expect_equal(r$weight_max, max(r$weights))
  expect_equal(r$weight_mean, mean(r$weights))
  # the weights are the stabilised inverse propensities
  marg <- mean(d$A)
  den <- ifelse(d$A > 0.5, r$propensity, 1 - r$propensity)
  nmr <- ifelse(d$A > 0.5, marg, 1 - marg)
  expect_equal(r$weights, nmr / den)
  expect_equal(r$K, 2L)
  expect_equal(r$n, 400L)
  expect_equal(r$Q, 4L)
  expect_true(r$stabilized)
  expect_equal(r$trim, 0)
  expect_match(r$method, "Lanza, Coffman & Xu")
})

test_that("weighting matters exactly when treatment depends on the indicators", {
  # the module's own claim: the weighted and unweighted class effects
  # coincide when assignment was unrelated to the indicators, and separate
  # when it was not
  ind <- sim_lc(1500, 11, dep = FALSE)
  ri <- morie_lcwphr_latent_class_weighted(ind$y, ind$A, ind$H, 2L)
  expect_equal(ri$class_ate, ri$naive_class_ate, tolerance = 0.25)

  dep <- sim_lc(1500, 11, dep = TRUE)
  rd <- morie_lcwphr_latent_class_weighted(dep$y, dep$A, dep$H, 2L)
  # under confounding the unweighted contrast is the more biased of the two
  truth <- c(1, 3)                      # effect in the low- and high-class
  ord <- order(rd$class_prevalence, decreasing = TRUE)
  err_w <- abs(rd$class_ate - truth)
  err_n <- abs(rd$naive_class_ate - truth)
  expect_true(sum(err_w) < sum(err_n))
  # and the propensity is no longer flat
  expect_true(stats::sd(rd$propensity) > 0.05)
  expect_true(stats::sd(ri$propensity) < 0.05)
})

test_that("the class-specific effects recover a class-specific truth", {
  d <- sim_lc(2000, 13)
  r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L)
  # class 1 of the simulation has effect 3, class 0 has effect 1; the
  # majority class is the low-endorsement one, so it comes first
  expect_equal(r$class_ate[1], 1, tolerance = 0.3)
  expect_equal(r$class_ate[2], 3, tolerance = 0.3)
  # the marginal effect lies between them
  expect_true(r$marginal_ate > 1 && r$marginal_ate < 3)
  # with assignment independent of everything, weighting the whole sample
  # or not makes little difference
  expect_equal(r$marginal_ate, r$unweighted_ate, tolerance = 0.2)
})

test_that("stabilisation and trimming change the weights as documented", {
  d <- sim_lc(600, 17, dep = TRUE)
  st <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L, stabilize = TRUE)
  un <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L, stabilize = FALSE)
  expect_true(st$stabilized)
  expect_false(un$stabilized)
  # stabilised weights average about one; unstabilised ones average about two
  expect_equal(st$weight_mean, 1, tolerance = 0.15)
  expect_true(un$weight_mean > st$weight_mean)
  # the unstabilised weights are the plain inverse propensities
  den <- ifelse(d$A > 0.5, un$propensity, 1 - un$propensity)
  expect_equal(un$weights, 1 / den)

  # trimming clamps the propensity into [trim, 1 - trim]
  tr <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L, trim = 0.2)
  expect_equal(tr$trim, 0.2)
  expect_true(all(tr$propensity >= 0.2 - 1e-12))
  expect_true(all(tr$propensity <= 0.8 + 1e-12))
  # so the extreme weights shrink and the effective sample size rises
  expect_true(tr$weight_max <= st$weight_max + 1e-9)
  expect_true(tr$effective_sample_size >= st$effective_sample_size - 1e-9)
})

test_that("the propensity model is a logistic fit on the indicators", {
  d <- sim_lc(500, 19, dep = TRUE)
  r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L)
  ref <- glm(d$A ~ d$H, family = binomial())
  # the fitted propensities agree with base R's logistic regression
  expect_equal(r$propensity, unname(fitted(ref)), tolerance = 1e-6)
  expect_equal(r$propensity_coefficients, unname(coef(ref)), tolerance = 1e-5)
  expect_length(r$propensity_coefficients, ncol(d$H) + 1L)
})

test_that("lcwphr rejects input it cannot use", {
  d <- sim_lc(80, 23)
  expect_error(morie_lcwphr_latent_class_weighted(numeric(0), numeric(0),
                                                  matrix(0, 0, 2), 2L),
               "no observations")
  expect_error(morie_lcwphr_latent_class_weighted(d$y[1:5], d$A, d$H, 2L),
               "must agree in length")
  # the indicators have to be binary
  Hb <- d$H
  Hb[1, 1] <- 0.5
  expect_error(morie_lcwphr_latent_class_weighted(d$y, d$A, Hb, 2L),
               "manifest indicators must be binary")
  # so does the treatment
  expect_error(morie_lcwphr_latent_class_weighted(d$y, d$A + 0.5, d$H, 2L),
               "treatment must be binary")
  # a single arm identifies no contrast
  expect_error(morie_lcwphr_latent_class_weighted(d$y, rep(1, 80), d$H, 2L),
               "both treatment arms must be occupied")
  expect_error(morie_lcwphr_latent_class_weighted(d$y, rep(0, 80), d$H, 2L),
               "both treatment arms must be occupied")
  expect_error(morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 0L),
               "K must be at least 1")
  expect_error(morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L,
                                                  trim = 0.5),
               "trim must be in")
  expect_error(morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 2L,
                                                  trim = -0.1),
               "trim must be in")
})

test_that("the initial partition does not collapse into one class", {
  # the operator precedence note in the source: (rank - 1L) * K %/% n would
  # be (rank - 1L) * (K %/% n) = 0 for K < n, putting every subject in one
  # class and leaving EM with nothing to separate. If that happened the
  # fitted classes would be degenerate.
  d <- sim_lc(300, 29)
  r <- morie_lcwphr_latent_class_weighted(d$y, d$A, d$H, 3L)
  expect_length(r$class_prevalence, 3L)
  # every class retains some mass, so the partition really did start split
  expect_true(all(r$class_prevalence > 0.01))
  expect_equal(sum(r$class_prevalence), 1)
  # and the classes are distinguishable in their item profiles
  expect_true(max(dist(r$item_probabilities)) > 0.2)
  expect_equal(sort(unique(r$labels)), c(0, 1, 2))
})
