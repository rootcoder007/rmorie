# Coverage for AFT survival boosting (Chen & Guestrin 2016; Barnwal et
# al. 2022). The Z densities are checked against stats' normal /
# logistic / (log-)Weibull forms and central differences, the AFT loss
# against the matching log-normal, log-logistic and Weibull likelihoods
# for every censoring type, the analytic derivatives against numeric
# ones, the first boosting round against eqs (5)-(7) by brute force, and
# the concordance against a direct pair count.

test_that("Z densities, distribution functions and their derivatives (Table 2)", {
  z <- c(-2.5, -0.3, 0, 0.7, 1.9)
  expect_equal(vapply(z, morie_surxgb_pdf, 1), stats::dnorm(z), tolerance = 1e-12)
  expect_equal(vapply(z, morie_surxgb_cdf, 1), stats::pnorm(z), tolerance = 1e-12)
  expect_equal(vapply(z, morie_surxgb_pdf, 1, dist = "logistic"), stats::dlogis(z), tolerance = 1e-12)
  expect_equal(vapply(z, morie_surxgb_cdf, 1, dist = "logistic"), stats::plogis(z), tolerance = 1e-12)
  # minimum-Gumbel Z: exp(Z) is standard exponential
  expect_equal(vapply(z, morie_surxgb_cdf, 1, dist = "extreme"), stats::pexp(exp(z)), tolerance = 1e-12)
  expect_equal(vapply(z, morie_surxgb_pdf, 1, dist = "extreme"), stats::dexp(exp(z)) * exp(z), tolerance = 1e-12)
  h <- 1e-5
  for (d in c("normal", "logistic", "extreme")) {
    num1 <- vapply(z, function(v) (morie_surxgb_pdf(v + h, d) - morie_surxgb_pdf(v - h, d)) / (2 * h), 1)
    num2 <- vapply(z, function(v) (morie_surxgb_dpdf(v + h, d) - morie_surxgb_dpdf(v - h, d)) / (2 * h), 1)
    # central differences: O(h^2) truncation, O(1e-16 / h) rounding
    expect_equal(vapply(z, morie_surxgb_dpdf, 1, dist = d), num1, tolerance = 1e-8)
    expect_equal(vapply(z, morie_surxgb_ddpdf, 1, dist = d), num2, tolerance = 1e-8)
  }
  expect_identical(morie_surxgb_cdf(Inf), 1)
  expect_identical(morie_surxgb_cdf(-Inf, "logistic"), 0)
  expect_error(morie_surxgb_pdf(0, "weibull"), "distribution must be one of")
})

test_that("the AFT loss is the censored log-normal / log-logistic / Weibull likelihood", {
  u <- 0.4
  s <- 0.8
  expect_equal(morie_surxgb_aft_loss(2, 2, u, s), -stats::dlnorm(2, u, s, log = TRUE), tolerance = 1e-12)
  expect_equal(morie_surxgb_aft_loss(2, Inf, u, s), -stats::plnorm(2, u, s, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
  expect_equal(morie_surxgb_aft_loss(0, 3, u, s), -stats::plnorm(3, u, s, log.p = TRUE), tolerance = 1e-12)
  expect_equal(morie_surxgb_aft_loss(1, 3, u, s), -log(stats::plnorm(3, u, s) - stats::plnorm(1, u, s)), tolerance = 1e-12)
  ll <- function(y) stats::dlogis(log(y), u, s) / y
  expect_equal(morie_surxgb_aft_loss(2, 2, u, s, "logistic"), -log(ll(2)), tolerance = 1e-12)
  expect_equal(morie_surxgb_aft_loss(1, 3, u, s, "logistic"), -log(stats::plogis(log(3), u, s) - stats::plogis(log(1), u, s)), tolerance = 1e-12)
  expect_equal(morie_surxgb_aft_loss(2, 2, u, s, "extreme"), -stats::dweibull(2, shape = 1 / s, scale = exp(u), log = TRUE), tolerance = 1e-12)
  expect_equal(morie_surxgb_aft_loss(2, Inf, u, s, "extreme"), -stats::pweibull(2, 1 / s, exp(u), lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
  expect_error(morie_surxgb_aft_loss(3, 1, u), "below the lower bound")
  expect_error(morie_surxgb_aft_loss(0, 0, u), "positive time")
  expect_error(morie_surxgb_aft_loss(1, 1, u, sigma = 0), "sigma must be positive")
})

test_that("analytic AFT gradients and hessians match the loss they come from", {
  cases <- list(c(2, 2), c(2, Inf), c(0, 3), c(1, 3))
  for (d in c("normal", "logistic", "extreme")) {
    for (cs in cases) {
      a <- morie_surxgb_aft_gradient_hessian(cs[1], cs[2], 0.3, 0.9, d)
      n <- morie_surxgb_aft_gradient_hessian(cs[1], cs[2], 0.3, 0.9, d, method = "numeric", eps = 1e-4)
      # the numeric second difference at eps = 1e-4 is good to ~1e-7
      expect_equal(a$gradient, n$gradient, tolerance = 1e-7)
      expect_equal(a$hessian, n$hessian, tolerance = 1e-5)
    }
  }
  o <- morie_surxgb_aft_gradient_hessian(2, 2, 0.3, 0.9)
  expect_equal(o$gradient, -(log(2) - 0.3) / 0.81, tolerance = 1e-12)
  expect_equal(o$hessian, 1 / 0.81, tolerance = 1e-12)
  expect_error(morie_surxgb_aft_gradient_hessian(2, 2, 0, method = "exact"), "analytic' or 'numeric")
})

test_that("leaf weights and split gains are eqs (5) and (7)", {
  expect_equal(morie_surxgb_leaf_weight(3, 5, 2), -3 / 7)
  expect_error(morie_surxgb_leaf_weight(1, -2, 1), "must be positive")
  gl <- 2
  hl <- 3
  gr <- -1
  hr <- 4
  expect_equal(morie_surxgb_split_gain(gl, hl, gr, hr, 1, 0.1),
               0.5 * (gl^2 / (hl + 1) + gr^2 / (hr + 1) - (gl + gr)^2 / (hl + hr + 1)) - 0.1, tolerance = 1e-12)
})

test_that("one boosting round takes the best eq (7) split and sets eq (5) leaves", {
  set.seed(8)
  n <- 30
  X <- cbind(stats::runif(n), stats::rnorm(n))
  tt <- exp(0.5 + 1.2 * (X[, 1] > 0.5) + 0.3 * stats::rnorm(n))
  yl <- tt
  yu <- ifelse(seq_len(n) %% 4 == 0, Inf, tt)
  fit <- morie_surxgb_boost(X, yl, yu, n_rounds = 1, eta = 0.3, max_depth = 1, min_child = 3, lam = 1)
  base <- mean(log(yl))
  expect_equal(fit$base_score, base, tolerance = 1e-12)
  gh <- lapply(seq_len(n), function(i) morie_surxgb_aft_gradient_hessian(yl[i], yu[i], base))
  g <- vapply(gh, function(z) z$gradient, 1)
  h <- vapply(gh, function(z) z$hessian, 1)
  best <- -Inf
  for (j in 1:2) {
    o <- order(X[, j])
    for (k in 3:(n - 3)) {
      L <- o[1:k]
      gain <- morie_surxgb_split_gain(sum(g[L]), sum(h[L]), sum(g) - sum(g[L]), sum(h) - sum(h[L]), 1, 0)
      if (gain > best) {
        best <- gain
        bj <- j
        bcut <- mean(sort(X[, j])[k:(k + 1)])
        bl <- L
      }
    }
  }
  tr <- fit$trees[[1]]
  expect_false(tr$leaf)
  expect_identical(tr$variable, bj)
  expect_equal(tr$cut, bcut, tolerance = 1e-12)
  expect_equal(tr$gain, best, tolerance = 1e-12)
  expect_equal(tr$left$weight, -sum(g[bl]) / (sum(h[bl]) + 1), tolerance = 1e-12)
  br <- setdiff(seq_len(n), bl)
  expect_equal(tr$right$weight, -sum(g[br]) / (sum(h[br]) + 1), tolerance = 1e-12)
  pred <- base + 0.3 * ifelse(X[, bj] > bcut, tr$right$weight, tr$left$weight)
  expect_equal(fit$prediction, pred, tolerance = 1e-12)
  expect_equal(morie_surxgb_predict(fit, X), pred, tolerance = 1e-12)
  expect_equal(fit$loss_history, mean(mapply(morie_surxgb_aft_loss, yl, yu, pred)), tolerance = 1e-12)
  stump <- morie_surxgb_boost(X, yl, yu, n_rounds = 1, eta = 1, max_depth = 0)
  expect_equal(stump$trees[[1]]$weight, -sum(g) / (sum(h) + 1), tolerance = 1e-12)
  f5 <- morie_surxgb(X, yl, yu, n_rounds = 5, eta = 0.3, max_depth = 2, min_child = 3)
  expect_lt(f5$loss_history[5], f5$loss_history[1])
  expect_identical(morie_surxgb_survival_xgboost, morie_surxgb_boost)
  ev <- as.integer(is.finite(yu))
  cc <- morie_surxgb_concordance(f5, X, yl, ev)
  p <- morie_surxgb_predict(f5, X)
  num <- 0
  den <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    a <- if (yl[i] < yl[j]) i else j
    b <- if (yl[i] < yl[j]) j else i
    if (!ev[a]) next
    den <- den + 1
    num <- num + if (p[a] < p[b]) 1 else if (p[a] == p[b]) 0.5 else 0
  }
  expect_equal(cc$c_index, num / den, tolerance = 1e-12)
  expect_error(morie_surxgb_boost(X, yl[-1], yu), "same length")
  expect_match(morie_surxgb_cheatsheet(), "Barnwal")
})
