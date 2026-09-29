# Coverage for pposc .. prgrl exports. Every expectation is recomputed in
# the test body.

test_that("Ppcheck is the posterior predictive tail probability of a statistic", {
  y <- c(2.1, 3.4, 1.8, 4.0, 2.9)
  R <- rbind(c(2.0, 3.0, 2.5, 3.5, 2.2), c(1.0, 4.2, 3.3, 2.8, 3.9),
             c(2.6, 2.4, 3.1, 4.4, 1.5), c(3.2, 3.8, 2.0, 2.9, 4.1))
  for (st in c("mean", "sd", "min", "max", "median")) {
    f <- switch(st, mean = mean, sd = stats::sd, min = min, max = max, median = stats::median)
    r <- Ppcheck(y, R, statistic = st)
    tr <- apply(R, 1, f)
    expect_equal(r$t_rep, tr, tolerance = 1e-12)
    expect_equal(r$p_value, mean(tr >= f(y)))
  }
  g <- function(z) sum(z > 3)
  rf <- Ppcheck(y, R, statistic = g)
  expect_equal(rf$p_value, mean(apply(R, 1, g) >= g(y)))
})

test_that("Ppmean summarises replicated datasets", {
  Y <- rbind(c(1, 2, 3), c(2, 3, 5), c(0, 4, 1), c(3, 3, 2))
  r <- Ppmean(Y)
  expect_equal(r$rep_mean, rowMeans(Y), tolerance = 1e-12)
  expect_equal(r$estimate, mean(Y), tolerance = 1e-12)
  expect_equal(r$sd, stats::sd(rowMeans(Y)), tolerance = 1e-12)
  q <- sort(as.numeric(t(Y)))
  expect_equal(c(r$ci_lower, r$ci_upper), c(q[floor(0.025 * 11) + 1], q[ceiling(0.975 * 11) + 1]))
  expect_error(Ppmean(Y[1, , drop = FALSE]), "two replicated")
})

test_that("Ppsamp and ppswrs give Hansen-Hurwitz quantities", {
  y <- c(10, 25, 7, 40, 18)
  x <- c(2, 5, 1, 8, 4)
  r <- Ppsamp(y, x, 2)
  p <- x / 20
  expect_equal(r$pi, 2 * p, tolerance = 1e-12)
  expect_equal(r$hh_variance, sum(p * (y / p - 100)^2) / 2, tolerance = 1e-12)
  expect_equal(r$srs_variance, sum((5 * y - 100)^2 / 5) / 2, tolerance = 1e-12)
  expect_equal(r$deff, r$hh_variance / r$srs_variance, tolerance = 1e-12)
  expect_error(Ppsamp(y, x, 3), "exceeds 1")
  expect_error(Ppsamp(y, -x, 1), "positive")
  expect_error(Ppsamp(y, x[-1], 1), "one entry per unit")
  expect_error(Ppsamp(y, x, 0), "at least 1")
  s <- ppswrs(c(10, 40, 18), c(0.1, 0.4, 0.2))
  z <- c(100, 100, 90)
  expect_equal(s$estimate, mean(z), tolerance = 1e-12)
  expect_equal(s$variance, stats::var(z) / 3, tolerance = 1e-12)
  # sizes normalised to probabilities
  expect_equal(ppswrs(c(10, 40), NULL, sizes = c(1, 3))$z, c(40, 160 / 3), tolerance = 1e-12)
  expect_true(is.na(ppswrs(5, 0.5)$variance))
  expect_same_function(morie_pps_with_replacement, ppswrs)
})

test_that("morie_pratt is two-level additive attention", {
  sm <- function(v) exp(v - max(v)) / sum(exp(v - max(v)))
  att <- function(H, W, b, u) sm(as.numeric(tanh(H %*% t(W) + matrix(b, nrow(H), length(b), byrow = TRUE)) %*% u))
  H1 <- rbind(c(0.2, -0.1), c(0.5, 0.3), c(-0.4, 0.8))
  H2 <- rbind(c(0.1, 0.9), c(-0.6, 0.2))
  Ww <- matrix(c(0.5, -0.3, 0.8, 0.1), 2)
  bw <- c(0.1, -0.2)
  uw <- c(1, -0.5)
  Ws <- matrix(c(-0.2, 0.7, 0.4, 0.3), 2)
  bs <- c(0, 0.1)
  us <- c(0.3, 0.9)
  Wc <- matrix(c(1, -1, 0.5, 0.2, -0.3, 0.8), 3)
  bc <- c(0.1, 0, -0.1)
  r <- morie_pratt(list(H1, H2), Ww, bw, uw, Ws, bs, us, Wc, bc)
  a1 <- att(H1, Ww, bw, uw)
  a2 <- att(H2, Ww, bw, uw)
  S <- rbind(as.numeric(a1 %*% H1), as.numeric(a2 %*% H2))
  as <- att(S, Ws, bs, us)
  dvec <- as.numeric(as %*% S)
  expect_equal(r$word_attention, list(a1, a2), tolerance = 1e-12)
  expect_equal(r$sentence_attention, as, tolerance = 1e-12)
  expect_equal(r$document_vector, dvec, tolerance = 1e-12)
  expect_equal(r$probabilities, sm(as.numeric(Wc %*% dvec) + bc), tolerance = 1e-12)
  e <- morie_pratt_attention_entropy(c(1, 1, 2))
  p <- c(0.25, 0.25, 0.5)
  expect_equal(e$entropy, -sum(p * log(p)), tolerance = 1e-12)
  expect_equal(e$concentration, 1 + sum(p * log(p)) / log(3), tolerance = 1e-12)
  expect_equal(morie_pratt_attention_entropy(5)$concentration, 1)
  expect_error(morie_pratt_attention_entropy(c(0, 0)), "no mass")
  expect_match(morie_pratt_cheatsheet(), "CONTEXT VECTOR")
  expect_error(morie_pratt(list(H1), Ww, bw, c(1, 2, 3), Ws, bs, us, Wc, bc), "context vector")
})

test_that("Prefixev evaluates Polish notation right to left", {
  r <- Prefixev(c("-", "*", "3", "+", "4", "2", "/", "8", "^", "2", "2"))
  expect_equal(r$value, 3 * (4 + 2) - 8 / 2^2)
  expect_equal(r$n_operators, 5)
  expect_equal(r$n_tokens, 11)
  expect_equal(Prefixev("7")$value, 7)
  expect_error(Prefixev(c("+", "1")), "fewer than two operands")
  expect_error(Prefixev(c("/", "1", "0")), "division by zero")
  expect_error(Prefixev(c("1", "2")), "malformed")
  expect_error(Prefixev(character(0)), "empty")
})

test_that("Prehay reproduces the multiple-mediator paths and percentile bootstrap", {
  x <- c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6, 1.4, -0.2, 0.9, -0.7)
  M <- cbind(0.5 * x + c(0.1, -0.2, 0.3, 0, 0.2, -0.1, 0.15, -0.05, 0.1, 0, -0.2, 0.1),
             -0.3 * x + c(0.2, 0.1, -0.1, 0.3, -0.2, 0.1, 0, 0.2, -0.1, 0.05, 0.1, -0.15))
  y <- 0.4 * x + 0.7 * M[, 1] + 0.5 * M[, 2] + c(0.1, 0, -0.1, 0.2, -0.05, 0.1, 0, -0.2, 0.15, 0, 0.05, -0.1)
  paths <- function(i) {
    a <- vapply(1:2, function(k) unname(stats::coef(stats::lm(M[i, k] ~ x[i]))[2]), 0)
    cb <- unname(stats::coef(stats::lm(y[i] ~ x[i] + M[i, ])))
    list(a = a, b = cb[3:4], cp = cb[2])
  }
  p0 <- paths(1:12)
  r <- Prehay(x, M, y, B = 40, alpha = 0.1, seed = 5)
  expect_equal(r$a, p0$a, tolerance = 1e-10)
  expect_equal(r$b, p0$b, tolerance = 1e-10)
  expect_equal(r$c_prime, p0$cp, tolerance = 1e-10)
  expect_equal(r$estimate, sum(p0$a * p0$b), tolerance = 1e-10)
  set.seed(5)
  bt <- vapply(1:40, function(b) {
    i <- sample.int(12, 12, replace = TRUE)
    p <- paths(i)
    sum(p$a * p$b)
  }, 0)
  st <- sort(bt)
  expect_equal(c(r$ci_lower, r$ci_upper), st[c(as.integer(40 * 0.05), as.integer(40 * 0.95) + 1)],
               tolerance = 1e-10)
  expect_equal(r$se, stats::sd(bt), tolerance = 1e-10)
  expect_error(Prehay(x, M, y, B = 1), "at least 2")
  expect_error(Prehay(x[-1], M, y), "matching first dimension")
})

test_that("curriculum_schedule and is_curriculum check the Bengio et al. conditions", {
  d <- c(0.9, 0.1, 0.5, 0.3, 0.7)
  cs <- curriculum_schedule(d, n_steps = 3)
  o <- order(d)
  for (s in 1:3) {
    take <- if (s == 3) 5 else max(1, round(s / 3 * 5))
    w <- numeric(5)
    w[o[seq_len(take)]] <- 1
    expect_equal(cs$weights[[s]], w)
    expect_equal(cs$dists[[s]], w / take)
  }
  expect_equal(cs$lambdas, (1:3) / 3)
  ic <- is_curriculum(cs$weights)
  ent <- vapply(cs$weights, function(w) log(sum(w)), 0)
  expect_equal(ic$entropies, ent, tolerance = 1e-12)
  expect_true(ic$is_curriculum && ic$final_step_is_p)
  # a weight that falls breaks monotonicity
  bad <- is_curriculum(list(c(1, 1, 0), c(1, 0, 1), c(1, 1, 1)))
  expect_false(bad$weights_monotone)
  expect_false(bad$is_curriculum)
  hf <- curriculum_schedule(d, 2, hard_first = TRUE)
  expect_equal(which(hf$weights[[1]] == 1), sort(order(-d)[seq_len(round(0.5 * 5))]))
  expect_error(curriculum_schedule(1), "two examples")
  expect_error(curriculum_schedule(d, 1), "two curriculum steps")
  expect_error(is_curriculum(list(c(1, 2))), "at least two steps")
  expect_error(is_curriculum(list(c(0.5, 2), c(1, 1))), "lie in \\[0, 1\\]")
})

test_that("prgrl and easy_only_fit replay the seeded perceptron experiments", {
  X <- rbind(c(1, 0.5), c(0.8, -0.3), c(-0.6, 0.9), c(-1.2, -0.4), c(0.3, 1.1), c(-0.2, -1.0))
  y <- c(1, 1, -1, -1, 1, -1)
  d <- c(0.2, 0.6, 0.1, 0.4, 0.9, 0.3)
  Xt <- rbind(c(0.9, 0.1), c(-0.8, 0.2), c(0.2, -0.9))
  yt <- c(1, -1, -1)
  lcg <- function(st) {
    hi <- st %/% 65536
    lo <- st %% 65536
    ((((1103515245 * hi) %% 2147483648) * 65536) %% 2147483648 + (1103515245 * lo) %% 2147483648 + 12345) %%
      2147483648
  }
  st <- 7
  rnd <- function() {
    st <<- lcg(st)
    st / 2^31
  }
  gauss <- function() {
    u <- max(rnd(), 1e-12)
    v <- rnd()
    sqrt(-2 * log(u)) * cos(2 * pi * v)
  }
  perc <- function(ord, w) {
    for (s in 0:29) {
      i <- ord[(s %% length(ord)) + 1]
      if (y[i] * sum(w * X[i, ]) <= 0) w <- w + y[i] * X[i, ]
    }
    w
  }
  err <- function(w) {
    s <- as.numeric(Xt %*% w)
    s[s == 0] <- -1
    mean(yt * s <= 0)
  }
  ce <- be <- numeric(3)
  for (r in 1:3) {
    w0 <- c(gauss(), gauss())
    sh <- 1:6
    for (i in 6:2) {
      j <- floor(rnd() * i) + 1
      sh[c(i, j)] <- sh[c(j, i)]
    }
    ce[r] <- err(perc(order(d), w0))
    be[r] <- err(perc(sh, w0))
  }
  res <- prgrl(X, y, d, X_test = Xt, y_test = yt, updates = 30, n_steps = 3, seed = 7, n_repeats = 3,
               order = "sorted")
  expect_equal(res$curriculum_errors, ce)
  expect_equal(res$baseline_errors, be)
  expect_equal(res$improvement, mean(be) - mean(ce), tolerance = 1e-12)
  expect_true(res$is_curriculum)
  expect_same_function(morie_prgrl, prgrl)
  st <- 3
  ee <- ae <- numeric(2)
  keep <- order(d)[1:3]
  for (r in 1:2) {
    w0 <- c(gauss(), gauss())
    ee[r] <- err(perc(keep, w0))
    sh <- 1:6
    for (i in 6:2) {
      j <- floor(rnd() * i) + 1
      sh[c(i, j)] <- sh[c(j, i)]
    }
    ae[r] <- err(perc(sh, w0))
  }
  ez <- easy_only_fit(X, y, d, Xt, yt, quantile = 0.5, updates = 30, seed = 3, n_repeats = 2)
  expect_equal(ez$easy_only_error, mean(ee), tolerance = 1e-12)
  expect_equal(ez$all_examples_error, mean(ae), tolerance = 1e-12)
  expect_equal(ez$n_kept, 3L)
  expect_error(prgrl(X, y * 2, d), "-1/\\+1")
  expect_error(prgrl(X, y, d, order = "random"), "sampled' or 'sorted")
  expect_error(prgrl(X, y, d[-1]), "one score per example")
  expect_error(easy_only_fit(X, y, d, Xt, yt, quantile = 0), "quantile must lie")
})
