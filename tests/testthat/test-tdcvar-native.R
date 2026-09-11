# Anchors for time-dependent covariate adjustment (Hernan & Robins 2020,
# ch. 20 and sec. 21.2).
#
# Two of the building blocks can be checked against base R exactly, and
# the IP weights have theoretical means that do not depend on the data
# generating process:
#
#   stabilized     E[ f(A|past) / f(A|past, L) ]  = 1
#   unstabilized   E[ 1 / f(A|past, L) ]          = 2^K  for binary A
#
# Those two identities are what caught the defect fixed alongside these
# tests: the weight was built from P(A = 1 | .) rather than from the
# probability of the treatment actually received, so every untreated
# subject carried the wrong weight and the stabilized weights averaged
# 1.67 instead of 1.

test_that("the weighted least squares fit agrees with lm(weights=)", {
  set.seed(1); n <- 200
  X <- cbind(1, rnorm(n), rnorm(n))
  y <- as.numeric(X %*% c(0.5, 1.2, -0.8)) + rnorm(n, 0, 0.5)
  w <- runif(n, 0.5, 2)
  fit <- .tdcvar_wls(X, y, w)
  ref <- lm(y ~ X[, 2] + X[, 3], weights = w)
  expect_equal(fit$coef, as.numeric(coef(ref)), tolerance = 1e-8)
  expect_equal(fit$se, as.numeric(summary(ref)$coefficients[, 2]),
               tolerance = 1e-6)
  expect_identical(dim(fit$vcov), c(3L, 3L))
  # unit weights are ordinary least squares
  expect_equal(.tdcvar_wls(X, y, rep(1, n))$coef,
               as.numeric(coef(lm(y ~ X[, 2] + X[, 3]))), tolerance = 1e-8)
  # scaling every weight by the same factor leaves the fit alone
  expect_equal(.tdcvar_wls(X, y, w * 7)$coef, fit$coef, tolerance = 1e-8)
})

test_that("the logistic fit agrees with glm(binomial)", {
  set.seed(2); n <- 300
  X <- cbind(1, rnorm(n), rnorm(n))
  y <- rbinom(n, 1, plogis(as.numeric(X %*% c(-0.3, 0.9, -0.5))))
  beta <- .tdcvar_logreg(X, y)
  g <- glm(y ~ X[, 2] + X[, 3], family = binomial())
  expect_equal(as.numeric(beta), as.numeric(coef(g)), tolerance = 1e-6)
  # the prediction is the logistic of the linear predictor
  expect_equal(as.numeric(.tdcvar_logreg_pred(X, beta)),
               as.numeric(plogis(X %*% beta)), tolerance = 1e-12)
  expect_equal(as.numeric(.tdcvar_logreg_pred(X, beta)),
               as.numeric(fitted(g)), tolerance = 1e-6)
  # every prediction is a probability
  expect_true(all(.tdcvar_logreg_pred(X, beta) > 0 &
                    .tdcvar_logreg_pred(X, beta) < 1))
})

test_that("the IP weight uses the probability of the treatment received", {
  # the regression. For a treated subject the received probability IS
  # P(A = 1); for an untreated one it is its complement, and using the
  # prediction directly gave those subjects weights more than twice what
  # they should be.
  set.seed(11); n <- 400
  L <- rnorm(n)
  A <- rbinom(n, 1, plogis(-0.2 + 1.5 * L))
  got <- .tdcvar_ip_weights_history(list(A), list(L), stabilize = TRUE)$weights
  p_num <- as.numeric(fitted(glm(A ~ 1, family = binomial())))
  p_den <- as.numeric(fitted(glm(A ~ L, family = binomial())))
  want <- ifelse(A == 1, p_num, 1 - p_num) / ifelse(A == 1, p_den, 1 - p_den)
  expect_equal(got, want, tolerance = 1e-4)
  # and NOT the ratio of P(A = 1), which is what it used to be
  expect_false(isTRUE(all.equal(got, p_num / p_den, tolerance = 1e-3)))
  # the treated subjects are the ones where the two agree
  expect_equal(got[A == 1], (p_num / p_den)[A == 1], tolerance = 1e-4)
})

test_that("stabilized weights average one and unstabilized average 2^K", {
  # an identity that holds whatever the confounding, so it is the
  # diagnostic Hernan and Robins recommend
  set.seed(21); n <- 4000
  for (K in 1:3) {
    A_hist <- list(); L_hist <- list()
    for (k in seq_len(K)) {
      Lk <- rnorm(n)
      L_hist[[k]] <- Lk
      A_hist[[k]] <- rbinom(n, 1, plogis(0.5 * Lk))
    }
    s <- .tdcvar_ip_weights_history(A_hist, L_hist, stabilize = TRUE)$weights
    u <- .tdcvar_ip_weights_history(A_hist, L_hist, stabilize = FALSE)$weights
    expect_equal(mean(s), 1, tolerance = 0.03)
    expect_equal(mean(u), 2^K, tolerance = 0.05 * 2^K)
    # a weight is positive, and there is one per subject
    expect_length(s, n)
    expect_true(all(s > 0))
    expect_true(all(u > 0))
  }
})

test_that("with no confounding the weights collapse to one", {
  set.seed(21); n <- 4000
  L <- rnorm(n)
  A <- rbinom(n, 1, 0.5)          # independent of L
  w <- .tdcvar_ip_weights_history(list(A), list(L))$weights
  expect_equal(mean(w), 1, tolerance = 0.01)
  # and they barely move, because there is nothing to reweight for
  expect_lt(sd(w), 0.05)
  expect_lt(max(abs(w - 1)), 0.15)
})

test_that("trimming clamps the weights at the quantiles asked for", {
  set.seed(22); n <- 2000
  L <- rnorm(n)
  A <- rbinom(n, 1, plogis(2.5 * L))     # strong confounding, heavy tail
  raw <- .tdcvar_ip_weights_history(list(A), list(L), stabilize = FALSE)$weights
  tr <- .tdcvar_ip_weights_history(list(A), list(L), stabilize = FALSE,
                                   trim = 0.05)$weights
  expect_equal(min(tr), as.numeric(quantile(raw, 0.05)), tolerance = 1e-9)
  expect_equal(max(tr), as.numeric(quantile(raw, 0.95)), tolerance = 1e-9)
  # trimming can only shrink the range
  expect_lte(max(tr), max(raw))
  expect_gte(min(tr), min(raw))
  expect_lt(max(tr), max(raw))
})

test_that("one weight is reported per period", {
  set.seed(23); n <- 300
  A_hist <- list(rbinom(n, 1, 0.5), rbinom(n, 1, 0.5), rbinom(n, 1, 0.5))
  L_hist <- list(rnorm(n), rnorm(n), rnorm(n))
  r <- .tdcvar_ip_weights_history(A_hist, L_hist)
  expect_length(r$per_time, 3L)
  # the periods are labelled from zero, and each carries a weight per subject
  expect_identical(vapply(r$per_time, function(p) p$time, numeric(1)), c(0, 1, 2))
  for (p in r$per_time) expect_length(p$weight, n)
  # the cumulative weight is the product of the per-period weights
  prod_w <- Reduce(`*`, lapply(r$per_time, function(p) p$weight))
  expect_equal(r$weights, prod_w, tolerance = 1e-10)
})

test_that("vector and matrix coercion accept what they document", {
  expect_equal(.tdcvar_vec(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(.tdcvar_vec(list(1, 2, 3)), c(1, 2, 3))
  m <- .tdcvar_mat(cbind(c(1, 2), c(3, 4)))
  expect_identical(dim(m), c(2L, 2L))
  # a bare vector becomes a single column
  expect_identical(dim(.tdcvar_mat(c(1, 2, 3))), c(3L, 1L))
})

test_that("the IP-weighted estimate recovers a known cumulative effect", {
  # The end-to-end anchor, and the measure of what the weight defect
  # cost: with the treatment probability used in place of the received
  # one, this estimate came out at 2.35 against a truth of 1.5 -- worse
  # than not weighting at all, which gives 2.05.
  set.seed(24); n <- 4000
  L1 <- rnorm(n)
  A1 <- rbinom(n, 1, plogis(1.2 * L1))
  L2 <- rnorm(n, 0.5 * A1 + 0.3 * L1)
  A2 <- rbinom(n, 1, plogis(1.2 * L2 - 0.2 * A1))
  y <- 2 + 1.5 * (A1 + A2) + 0.9 * L1 + rnorm(n, 0, 0.5)
  r <- morie_tdcvar(y, list(A1, A2), list(L1, L2))

  # the weighted estimate lands on the truth
  expect_equal(as.numeric(r$estimate), 1.5, tolerance = 0.1)
  expect_equal(as.numeric(r$msm), as.numeric(r$estimate), tolerance = 1e-12)
  # while leaving the confounding in place does not
  expect_gt(as.numeric(r$unadjusted), 1.9)
  # so weighting is closer to the truth than not weighting
  expect_lt(abs(as.numeric(r$estimate) - 1.5),
            abs(as.numeric(r$unadjusted) - 1.5))
  # the stabilized weights average one, which is what went wrong before
  expect_equal(as.numeric(r$mean_weight), 1, tolerance = 0.05)
  # the reported bookkeeping describes the design it was given
  expect_equal(r$n, n)
  expect_identical(r$n_times, 2L)
  expect_identical(r$contrast, "cumulative")
  expect_length(r$per_time, 2L)
  expect_equal(r$cumulative_exposure, A1 + A2)
  expect_length(r$weights, n)
  # the effective sample size is positive and no larger than the sample
  expect_gt(as.numeric(r$effective_sample_size), 0)
  expect_lte(as.numeric(r$effective_sample_size), n)
  expect_match(r$method, "Hernan|IP-weighted")
})

test_that("the estimator runs on a small design and reports finite numbers", {
  set.seed(25); n <- 600
  L1 <- rnorm(n)
  A1 <- rbinom(n, 1, plogis(0.8 * L1))
  L2 <- rnorm(n, 0.5 * A1 + 0.3 * L1)
  A2 <- rbinom(n, 1, plogis(0.8 * L2 - 0.2 * A1))
  y <- 2 + 1.5 * (A1 + A2) + 0.4 * L1 + rnorm(n, 0, 0.5)
  r <- morie_tdcvar(y, list(A1, A2), list(L1, L2))
  expect_true(all(is.finite(c(as.numeric(r$estimate), as.numeric(r$se),
                              as.numeric(r$unadjusted), as.numeric(r$adjusted)))))
  expect_gt(as.numeric(r$se), 0)
  expect_length(r$coef, 2L)
  expect_identical(dim(r$vcov), c(2L, 2L))
})
