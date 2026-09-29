# Coverage for the joint frailty and joint longitudinal-survival models,
# the mixed-outcome joint loss, Jensen-Shannon divergences and the
# jsonlite-compatible JSON router; recomputed with base R, lme4, survival
# and jsonlite in the test body.

test_that("Jntfr reports the gamma-frailty likelihood at its estimate", {
  set.seed(1)
  g <- 12
  w <- rgamma(g, 2, 2)
  cl <- rep(1:g, each = 2)
  tm <- runif(2 * g, 0.5, 2)
  ev <- rpois(2 * g, 0.8 * w[cl] * tm)
  te <- rbinom(2 * g, 1, pmin(0.9, 0.3 * w[cl]))
  r <- Jntfr(tm, ev, te, cl, sweeps = 2)
  N <- tapply(ev, cl, sum)
  A <- tapply(tm, cl, sum)
  dl <- tapply(te, cl, sum)
  k <- 1 / r$theta
  J <- diag(2 * (0:15) + k)
  for (i in 2:16) J[i, i - 1] <- J[i - 1, i] <- sqrt((i - 1) * (i - 1 + k - 1))
  e <- eigen(J, symmetric = TRUE)
  x <- e$values / k
  wq <- e$vectors[1, ]^2
  ll <- sum(vapply(1:g, function(i) log(sum(wq * exp(N[i] * log(r$lambda_r * x) - r$lambda_r * x * A[i] +
                                                  dl[i] * log(r$lambda_t * x^r$alpha) - r$lambda_t * x^r$alpha * A[i]))), 0))
  expect_equal(r$loglik, ll, tolerance = 1e-9)
  intg <- sum(vapply(1:g, function(i) log(integrate(function(u) exp(N[i] * log(r$lambda_r * u) - r$lambda_r * u * A[i] +
                                                                      dl[i] * log(r$lambda_t * u^r$alpha) - r$lambda_t * u^r$alpha * A[i]) *
                                                         dgamma(u, k, k), 0, Inf, rel.tol = 1e-12)$value), 0))
  # 16-node generalised Gauss-Laguerre against adaptive quadrature
  expect_equal(r$loglik, intg, tolerance = 1e-4)
  expect_equal(r$naive_lambda_r, sum(ev) / sum(tm), tolerance = 1e-12)
  expect_error(Jntfr(tm, ev, rep(0, 24), cl), "terminal event")
  expect_error(Jntfr(-tm, ev, te, cl), "positive")
})

test_that("Jntlmm feeds empirical-Bayes intercepts into a Cox model", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("survival")
  set.seed(2)
  g <- 25
  cl <- rep(1:g, each = 4)
  b <- rnorm(g)
  x <- rnorm(4 * g)
  y <- 1 + 0.5 * x + b[cl] + rnorm(4 * g, sd = 0.7)
  st <- rexp(g, exp(0.8 * b))
  ev <- rbinom(g, 1, 0.8)
  r <- Jntlmm(y, st[cl], ev[cl], x, NULL, cl)
  f <- lme4::lmer(y ~ x + (1 | cl), REML = FALSE)
  vc <- as.data.frame(lme4::VarCorr(f))$vcov
  # stage 1 is 200 EM iterations; they agree with lmer's ML fit to about 1e-6
  expect_equal(r$beta, unname(lme4::fixef(f)), tolerance = 1e-5)
  expect_equal(c(r$tau2, r$sigma2), vc, tolerance = 1e-5)
  expect_equal(r$b, unname(lme4::ranef(f)$cl[, 1]), tolerance = 1e-5)
  cx <- survival::coxph(survival::Surv(st, ev) ~ r$b, ties = "breslow",
                        control = survival::coxph.control(eps = 1e-12, toler.chol = 1e-15, iter.max = 100))
  expect_equal(r$eta, unname(coef(cx)), tolerance = 1e-8)
  expect_equal(r$se, unname(sqrt(diag(vcov(cx)))), tolerance = 1e-7)
  expect_equal(r$icc, r$tau2 / (r$tau2 + r$sigma2), tolerance = 1e-12)
  z <- rnorm(g)
  r2 <- Jntlmm(y, st[cl], ev[cl], x, z, cl)
  cx2 <- survival::coxph(survival::Surv(st, ev) ~ z + r2$b, ties = "breslow",
                         control = survival::coxph.control(eps = 1e-12, toler.chol = 1e-15, iter.max = 100))
  expect_equal(r2$gamma, unname(coef(cx2)), tolerance = 1e-8)
  expect_error(Jntlmm(y[1:8], st[1:2], ev[1:2], x[1:8], NULL, cl[1:8]), "different lengths")
  expect_error(Jntlmm(y, st[cl], rep(0, 100), x, NULL, cl), "no events")
})

test_that("Jntlo weights the per-type losses", {
  yd <- list(a = list("cont", c(1, 2, 3, 4)), b = list("binary", c(0, 1, 1, 0)),
             c = list("count", c(0, 2, 5, 1)))
  yh <- list(a = c(1.5, 2, 2.5, 4.5), b = c(0.2, 0.7, 0.9, 0.4), c = c(0.5, 2, 4, 1.5))
  la <- mean((yd$a[[2]] - yh$a)^2)
  lb <- -mean(yd$b[[2]] * log(yh$b) + (1 - yd$b[[2]]) * log(1 - yh$b))
  lc <- mean(yh$c - yd$c[[2]] * log(yh$c))
  r <- Jntlo(yd, yh, weights = c(1, 2, 0.5))
  expect_equal(unname(r$parts), c(la, lb, lc), tolerance = 1e-12)
  expect_equal(r$loss, la + 2 * lb + 0.5 * lc, tolerance = 1e-12)
  sp <- function(v) max(abs(median(v) - quantile(v, c(0.25, 0.75), names = FALSE)))
  d <- c(sp(yd$a[[2]]), sp(yd$b[[2]]), sp(yd$c[[2]]))
  rd <- Jntlo(yd, yh)
  expect_equal(unname(rd$weights), c(d[1], d[1] / d[2], d[1] / d[3]), tolerance = 1e-12)
  expect_equal(unname(Jntlo(yd, yh, weights = list(a = 1, b = 0, c = 0))$loss), la, tolerance = 1e-12)
  expect_error(Jntlo(list(a = list("gamma", 1)), list(a = 1)), "unknown outcome type")
  expect_error(Jntlo(yd, yh[1:2]), "no prediction")
})

test_that("Jsdiv and Jzdiff are the Jensen-Shannon divergence", {
  p <- c(0.1, 0.4, 0.5, 0)
  q <- c(0.3, 0.3, 0.2, 0.2)
  m <- (p + q) / 2
  kl <- function(a, b) sum(ifelse(a > 0, a * log(a / b), 0))
  js <- 0.5 * kl(p, m) + 0.5 * kl(q, m)
  r <- Jsdiv(p, q)
  expect_equal(r$estimate, js / log(2), tolerance = 1e-12)
  expect_equal(r$distance, sqrt(js / log(2)), tolerance = 1e-12)
  expect_equal(Jsdiv(2 * p, 3 * q, base = exp(1))$estimate, js, tolerance = 1e-12)
  expect_error(Jsdiv(p, q[-1]), "same length")
  expect_error(Jsdiv(-p, q), "non-negative")
  z <- Jzdiff(NULL, p, q)
  H <- function(v) -sum(v[v > 0] * log(v[v > 0]))
  expect_equal(z$estimate, H(m) - (H(p) + H(q)) / 2, tolerance = 1e-12)
  expect_equal(z$estimate, js, tolerance = 1e-12)
  expect_error(Jzdiff(1:3, p, q), "y and p")
})

test_that("morie_jsonlt routes agree with jsonlite", {
  skip_if_not_installed("jsonlite")
  x <- list(a = 1:3, b = "x", c = list(d = TRUE, e = NULL))
  tj <- morie_jsonlt(x, "to_json")$result
  expect_equal(as.character(tj), as.character(jsonlite::toJSON(x)))
  txt <- '{"a":[1,2,3],"b":{"c":"z"},"d":[{"u":1,"v":"p"},{"u":2,"v":"q"}]}'
  expect_equal(morie_jsonlt(txt, "from_json")$result, jsonlite::fromJSON(txt))
  expect_equal(morie_jsonlt_parse_json(txt), jsonlite::parse_json(txt))
  expect_equal(as.character(morie_jsonlt(txt, "minify")$result), as.character(jsonlite::minify(txt)))
  expect_equal(as.character(morie_jsonlt(txt, "prettify")$result), as.character(jsonlite::prettify(txt)))
  expect_true(morie_jsonlt(txt, "validate")$result)
  expect_false(morie_jsonlt("{bad", "validate")$result)
  raw <- charToRaw("hello json")
  b64 <- morie_jsonlt(raw, "base64_enc")$result
  expect_equal(b64, jsonlite::base64_enc(raw))
  expect_equal(morie_jsonlt(b64, "base64_dec")$result, raw)
  df <- data.frame(p = 1:2, q = c("a", "b"), stringsAsFactors = FALSE)
  ser <- morie_jsonlt(df, "serialize")$result
  expect_equal(morie_jsonlt(ser, "unserialize")$result, df)
  expect_error(morie_jsonlt(x, "yaml"), "expected one of")
})
