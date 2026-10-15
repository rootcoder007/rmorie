# R/lmm_native.R, the lines PR #77 added that no other test reached: the formula reader's other
# spellings (unary signs, a bar without brackets, bracketed grouping factors, || with interactions),
# nlme's random argument, every refusal, crossed factors with slopes, and the GLMM's responses and
# families, each fit against lme4 or nlme.

.lm <- function(name) get(name, envir = environment(morie_lmm))

.ll_equal <- function(f, m, tol = 1e-5) {
  expect_equal(as.numeric(f$logLik), as.numeric(stats::logLik(m)), tolerance = tol)
  expect_equal(unname(f$coefficients), unname(lme4::fixef(m)), tolerance = tol)
}

test_that("the formula reader takes unary signs, a bare bar and bracketed grouping factors as lme4 does", {
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
  .ll_equal(morie_lmm(Reaction ~ +Days + (1 | Subject), sleepstudy), m)
  .ll_equal(morie_lmm(Reaction ~ Days + (1 | (Subject)), sleepstudy), m)
  m0 <- lme4::lmer(Reaction ~ Days - 1 + (1 | Subject), sleepstudy)
  .ll_equal(morie_lmm(Reaction ~ Days - 1 + (1 | Subject), sleepstudy), m0)
  .ll_equal(morie_lmm(Reaction ~ -1 + Days + (1 | Subject), sleepstudy), m0)
  # a formula whose right side is one bar term, with no brackets
  mb <- lme4::lmer(Reaction ~ 1 + (Days | Subject), sleepstudy)
  fb <- morie_lmm(Reaction ~ Days | Subject, sleepstudy)
  expect_equal(as.numeric(fb$logLik), as.numeric(stats::logLik(lme4::lmer(Reaction ~ (Days | Subject), sleepstudy))),
               tolerance = 1e-5)
  sp <- .lm(".lmm_split")
  expect_identical(vapply(sp(quote(a - b + (1 | g))), function(t) t$sign, 1), c(1, -1, 1))
  expect_true(sp(quote(x | g))[[1]]$bar)
  gv <- .lm(".lmm_grp_vars")
  expect_identical(gv(quote((a:b))), c("a", "b"))
  expect_error(gv(quote(log(g))), "a grouping factor must be a variable, an interaction a:b or a nesting a/b; got 'log(g)'",
               fixed = TRUE)
  ge <- .lm(".lmm_grp_expand")
  expect_identical(ge(quote((a / b))), list("a", c("a", "b")))
})

test_that("|| splits interactions into uncorrelated terms, and refuses a term it cannot rebuild", {
  skip_if_not_installed("lme4")
  set.seed(3)
  d <- data.frame(g = factor(rep(1:15, each = 8)), x = stats::rnorm(120), z = stats::rnorm(120))
  d$y <- 1 + d$x + stats::rnorm(15)[d$g] + 0.4 * stats::rnorm(15)[d$g] * d$x * d$z + stats::rnorm(120, sd = 0.5)
  f <- morie_lmm(y ~ x + (x * z || g), d)
  m <- lme4::lmer(y ~ x + (1 | g) + (0 + x | g) + (0 + z | g) + (0 + x:z | g), d)
  expect_equal(as.numeric(f$logLik), as.numeric(stats::logLik(m)), tolerance = 1e-5)
  te <- .lm(".lmm_term_expr")
  expect_identical(te("x:z", quote(x * z)), quote(x:z))
  expect_error(te("I(x^2):z", quote(I(x^2) * z)), "cannot expand the term 'I(x^2):z'", fixed = TRUE)
})

test_that("nlme's random argument: a formula, a nested list, and the refusals", {
  skip_if_not_installed("nlme")
  data(Orthodont, package = "nlme")
  d <- as.data.frame(Orthodont)
  f <- morie_lmm(distance ~ age, d, random = ~ age | Subject)
  n <- nlme::lme(distance ~ age, d, random = ~ age | Subject, method = "REML")
  expect_equal(as.numeric(f$logLik), as.numeric(stats::logLik(n)), tolerance = 1e-5)
  d$grp <- factor(ifelse(as.integer(d$Subject) %% 2 == 0, "a", "b"))
  fl <- morie_lmm(distance ~ age, d, random = list(grp = ~ 1, Subject = ~ 1))
  nl <- nlme::lme(distance ~ age, d, random = list(grp = ~ 1, Subject = ~ 1), method = "REML")
  expect_equal(as.numeric(fl$logLik), as.numeric(stats::logLik(nl)), tolerance = 1e-5)
  rb <- .lm(".lmm_random_bars")
  expect_error(rb(~ age), "random must be a one-sided formula with a grouping factor")
  expect_error(rb(list(Subject = "age")), "each element of a random list must be a one-sided formula")
  expect_error(rb("age"), "random must be a one-sided formula (~ x | g) or a named list", fixed = TRUE)
  # no fixed effects at all: no nlme degrees of freedom to report
  f0 <- morie_lmm(distance ~ 0, d, random = ~ 1 | Subject)
  expect_null(.lm(".lmm_nlme_df")(.lm(".lmm_setup")(distance ~ 0, d, random = ~ 1 | Subject)))
  expect_true(is.finite(as.numeric(f0$logLik)))
})

test_that("the setup refuses what it cannot fit, in words", {
  set.seed(1)
  d <- data.frame(y = stats::rnorm(20), x = stats::rnorm(20), g = factor(rep(1:5, 4)))
  st <- .lm(".lmm_setup")
  expect_error(st(~ x + (1 | g), d), "formula must be a two-sided formula")
  expect_error(morie_lmm(y ~ x + (1 | g), d, weights = 1:3), "weights must have one value per row of data")
  expect_error(morie_lmm(y ~ x + (1 | g), d, offset = 1:3), "offset must have one value per row of data")
  expect_error(morie_lmm(y ~ x + (1 | g), d, weights = c(-1, rep(1, 19))), "weights must be non-negative")
  expect_error(morie_lmm(y ~ x + (1 | g), d[1, ]), "fewer than 2 complete observations")
  expect_error(morie_lmm(y ~ x + (1 | h), d), "grouping variable 'h' not found")
  expect_error(morie_lmm(g ~ x + (1 | g), d), "the response must be a numeric vector")
  expect_error(morie_lmm(y ~ x + (1 | g), d, start = c(1, 2, 3)), "start must have length 1")
  # a matrix for data, and a formula with no environment
  fm <- morie_lmm(y ~ x + (1 | g), d)
  dm <- d
  dm$g <- as.integer(dm$g)
  fmat <- morie_lmm(y ~ x + (1 | g), as.matrix(dm))
  expect_equal(fmat$coefficients, fm$coefficients, tolerance = 1e-8)
  fo <- y ~ x + (1 | g)
  environment(fo) <- NULL
  expect_equal(morie_lmm(fo, d)$coefficients, fm$coefficients)
  # a term with no columns is dropped; a rank-deficient fixed part drops columns, as lme4 does
  expect_equal(morie_lmm(y ~ x + (0 | g) + (1 | g), d)$coefficients, fm$coefficients, tolerance = 1e-8)
  d$x2 <- 2 * d$x
  expect_message(fr <- morie_lmm(y ~ x + x2 + (1 | g), d), "rank deficient so dropping 1 column\\(s\\): x2")
  expect_equal(unname(fr$coefficients), unname(fm$coefficients), tolerance = 1e-8)
  # the objective returns Inf, not an error, where the fit cannot be evaluated
  expect_identical(fm$devfun(NA_real_), Inf)
})

test_that("large crossed designs: a dense coupling is said past 5000 effects and refused past 2^28 entries", {
  n <- 12000
  d <- data.frame(y = stats::rnorm(n), g1 = factor(seq_len(n)), g2 = factor(rep(seq_len(6000), 2)))
  expect_warning(.lm(".lmm_setup")(y ~ (1 | g1) + (1 | g2), d), "random effects outside the largest grouping factor")
  n <- 20000
  d <- data.frame(y = stats::rnorm(n), g1 = factor(seq_len(n)), g2 = factor(rep(seq_len(15000), length.out = n)))
  expect_error(suppressWarnings(.lm(".lmm_setup")(y ~ (1 | g1) + (1 | g2), d)), "more than 2\\^28 entries")
})

test_that("crossed factors with a random slope in the smaller one match lme4", {
  skip_if_not_installed("lme4")
  set.seed(5)
  d <- expand.grid(a = factor(1:30), b = factor(1:6), rep = 1:2)
  d$x <- stats::rnorm(nrow(d))
  d$y <- 1 + 0.5 * d$x + stats::rnorm(30)[d$a] + (0.5 + stats::rnorm(6))[d$b] * d$x + stats::rnorm(nrow(d), sd = 0.4)
  f <- morie_lmm(y ~ x + (1 | a) + (x | b), d)
  m <- lme4::lmer(y ~ x + (1 | a) + (x | b), d)
  .ll_equal(f, m, tol = 1e-4)
})

test_that("the batched Cholesky refuses a block that is not positive definite", {
  A <- array(0, c(2L, 2L, 3L))
  A[, , 1] <- diag(2)
  A[, , 2] <- matrix(c(1, 2, 2, 1), 2)
  A[, , 3] <- diag(2)
  expect_error(.lm(".lmm_bchol")(A), "batched Cholesky: block not positive definite")
})

test_that("the convergence gradient skips coordinates on their bound", {
  fg <- .lm(".lmm_free_grad")
  f <- function(x) sum((x - 2)^2)
  expect_identical(fg(f, c(0), lower = 0), 0)
  expect_equal(fg(f, c(0, 1), lower = c(0, -Inf)), 2, tolerance = 1e-6)
})

test_that("GLMM families, responses and refusals", {
  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")
  f <- morie_glmm(cbind(incidence, size - incidence) ~ period + (1 | herd), cbpp, family = stats::binomial)
  # glmer stops its PIRLS on a looser change unless told otherwise (see test-lmm-native-parity.R)
  m <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), cbpp, family = stats::binomial,
                   control = lme4::glmerControl(tolPwrss = 1e-13))
  expect_equal(unname(f$coefficients), unname(lme4::fixef(m)), tolerance = 1e-5)
  set.seed(2)
  d <- data.frame(g = factor(rep(1:20, each = 10)), x = stats::rnorm(200))
  d$yb <- stats::rbinom(200, 1, stats::plogis(0.3 + d$x + stats::rnorm(20)[d$g]))
  d$yf <- factor(d$yb, labels = c("no", "yes"))
  expect_equal(morie_glmm(yf ~ x + (1 | g), d)$coefficients, morie_glmm(yb ~ x + (1 | g), d)$coefficients)
  expect_error(morie_glmm(yb ~ x + (1 | g), d, nAGQ = 2), "nAGQ must be 0 or 1")
  d$y2 <- d$yb * 2
  expect_error(morie_glmm(y2 ~ x + (1 | g), d), "binomial responses must lie in \\[0, 1\\]")
  d$yn <- -d$yb
  expect_error(morie_glmm(yn ~ x + (1 | g), d, family = "poisson"), "poisson responses must be non-negative")
  expect_error(morie_glmm(yb ~ x + (1 | g), d, family = stats::gaussian()), "family must be binomial or poisson")
})

test_that("print says when the optimiser stopped early and when the fit is singular", {
  set.seed(4)
  d <- data.frame(y = stats::rnorm(60), x = stats::rnorm(60), g = factor(rep(1:6, 10)))
  d$y <- d$y - stats::ave(d$y, d$g)                      # equal group means: the variance is on its bound
  f <- morie_lmm(y ~ (1 | g), d)
  out <- utils::capture.output(print(f))
  expect_true(isTRUE(f$convergence$singular))
  expect_true(any(grepl("boundary (singular) fit", out, fixed = TRUE)))
  f1 <- morie_lmm(y ~ x + (x | g), d, control = list(iter.max = 1L, eval.max = 2L))
  if (f1$convergence$code != 0) expect_true(any(grepl("Optimizer message:", utils::capture.output(print(f1)), fixed = TRUE)))
})

test_that("three crossed grouping factors couple the two smaller ones, as lme4 fits them", {
  skip_if_not_installed("lme4")
  set.seed(5)
  d <- expand.grid(a = factor(1:12), b = factor(1:5), c = factor(1:4))
  d$x <- stats::rnorm(nrow(d))
  d$y <- 1 + d$x + stats::rnorm(12)[d$a] + stats::rnorm(5)[d$b] + stats::rnorm(4)[d$c] + stats::rnorm(nrow(d), sd = 0.3)
  .ll_equal(morie_lmm(y ~ x + (1 | a) + (1 | b) + (1 | c), d), lme4::lmer(y ~ x + (1 | a) + (1 | b) + (1 | c), d), tol = 1e-4)
})

test_that("a separated binomial GLMM falls back on the RX covariance and says so", {
  set.seed(7)
  g <- data.frame(g = factor(rep(1:10, each = 12)), x = stats::rnorm(120))
  g$ys <- as.numeric(g$x > 0)
  w <- character()
  f <- withCallingHandlers(morie_glmm(ys ~ x + (1 | g), g),
                           warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("the finite-difference Hessian is not positive definite; using the RX-based covariance", w, fixed = TRUE)))
  expect_true(all(is.finite(f$coefficients)))
})
