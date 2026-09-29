# Coverage tests for R/causal_molak.R (Molak 2023, Causal Inference and
# Discovery in Python): d-separation, colliders, Markov equivalence,
# do-calculus rules, BIC scores, HSIC and the R-learner. d-separation is
# checked against dagitty when it is installed.

ml_dag <- rbind(c("A", "C"), c("B", "C"), c("C", "D"), c("A", "E"), c("E", "D"), c("D", "F"))

test_that("d-separation agrees with dagitty on every pair and small conditioning sets", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { A -> C; B -> C; C -> D; A -> E; E -> D; D -> F }")
  nodes <- LETTERS[1:6]
  sets <- list(character(0), "C", "D", c("C", "E"), "F", c("A", "D"))
  for (x in nodes) for (y in nodes) if (x < y) for (z in sets) {
    if (x %in% z || y %in% z) next
    expect_identical(morie_dseptest(ml_dag, x, y, z)$dseparated,
      dagitty::dseparated(g, x, y, z), info = paste(x, y, paste(z, collapse = ",")))
  }
  r <- morie_dseptest(ml_dag, "A", "B")
  expect_true(r$dseparated)
  expect_equal(r$nnodes, 6L)
  # a node absent from the graph is isolated, hence d-separated from all
  expect_true(morie_dseptest(ml_dag, "A", "Q", "Z")$dseparated)
})

test_that("faithfulness / Markov bookkeeping", {
  f <- morie_faithchk(ml_dag, "A", "B", indep = FALSE)
  expect_true(f$dseparated)
  expect_false(f$markov)
  expect_false(f$violation)
  v <- morie_faithchk(ml_dag, "A", "D", indep = TRUE)
  expect_false(v$dseparated)
  expect_true(v$violation)
  expect_false(v$faithful)
  expect_true(v$markov)
})

test_that("colliders, Markov equivalence and bow arcs", {
  c1 <- morie_collider(ml_dag, triple = c("A", "C", "B"))
  expect_true(c1$iscollider)
  expect_equal(c1$colliders, sort(c(paste("A", "C", "B", sep = "\r"), paste("C", "D", "E", sep = "\r"))))
  sh <- morie_collider(rbind(c("A", "C"), c("B", "C"), c("A", "B")), triple = c("A", "C", "B"))
  expect_true(sh$shielded)
  expect_equal(sh$ncolliders, 0L)
  chain <- rbind(c("X", "Y"), c("Y", "Z"))
  rev_chain <- rbind(c("Z", "Y"), c("Y", "X"))
  fork <- rbind(c("Y", "X"), c("Y", "Z"))
  vstr <- rbind(c("X", "Y"), c("Z", "Y"))
  expect_true(morie_mectest(chain, rev_chain)$equivalent)
  expect_true(morie_mectest(chain, fork)$equivalent)
  m <- morie_mectest(chain, vstr)
  expect_true(m$sameskeleton)
  expect_false(m$samecolliders)
  expect_false(m$equivalent)
  b <- morie_bowarc(rbind(c("X", "Y"), c("Z", "X")), rbind(c("Y", "X")), "X", "Y")
  expect_true(b$isbow)
  expect_false(b$bowfree)
  expect_false(b$identified)
  b2 <- morie_bowarc(rbind(c("X", "Y")), matrix(character(0), 0, 2), "X", "Y")
  expect_true(b2$identified)
  expect_false(morie_bowarc(rbind(c("X", "Y"), c("Y", "X")), matrix(character(0), 0, 2), "X", "Y")$acyclic)
})

test_that("do-calculus rules on the instrument graph Z -> X -> Y", {
  g <- rbind(c("Z", "X"), c("X", "Y"))
  r <- morie_docalc(g, "Y", "Z")
  # rule 1 in G: Y and Z connected through X; rule 2 in G with Z's
  # outgoing edge cut: separated; rule 3 cuts nothing into Z: as rule 1
  expect_equal(c(r$rule1, r$rule2, r$rule3), c(FALSE, TRUE, FALSE))
  rx <- morie_docalc(g, "Y", "Z", x = "X")
  expect_true(rx$rule1)
  expect_equal(rx$nrules, 3L)
  expect_error(morie_docalc(g, "Y", c("Z", "X")), "one z node")
  d <- morie_dointerv(rbind(c("Z", "X"), c("X", "Y"), c("W", "X")), "X")
  expect_equal(d$nremoved, 2L)
  expect_equal(d$edges, rbind(c("X", "Y")))
  expect_equal(d$nnodes, 4L)
})

test_that("Gaussian DAG BIC score is minus half the summed lm BIC", {
  n <- 25
  i <- 1:n
  a <- sin(i)
  b <- 0.5 * a + cos(3 * i) / 2
  cc <- b - 0.3 * a + sin(5 * i) / 3
  dat <- cbind(a, b, cc)
  s <- morie_bicdag(dat, rbind(c("a", "b"), c("b", "cc"), c("a", "cc")), names = c("a", "b", "cc"))
  fits <- list(lm(a ~ 1), lm(b ~ a), lm(cc ~ a + b))
  expect_equal(s$loglik, sum(vapply(fits, function(f) as.numeric(logLik(f)), 0)), tolerance = 1e-12)
  expect_equal(s$score, -sum(vapply(fits, BIC, 0)) / 2, tolerance = 1e-12)
  # parents + intercept + variance per node: 2 + 3 + 4
  expect_equal(s$k, 9L)
  s0 <- morie_bicdag(dat, list(), names = c("a", "b", "cc"))
  expect_equal(s0$k, 6L)
  expect_gt(s$score, s0$score)
  expect_error(morie_bicdag(dat, rbind(c("a", "q")), names = c("a", "b", "cc")), "no data column")
  expect_error(morie_bicdag(dat[1, , drop = FALSE], list()), "at least 2 rows")
})

test_that("HSIC statistic with median-heuristic Gaussian kernels", {
  a <- c(0.1, 0.9, -0.4, 1.3, 0.2, -1.1, 0.7, 0.5)
  b <- a^2 + c(0.05, -0.02, 0.01, 0.03, -0.04, 0.02, 0, -0.01)
  gram <- function(v) {
    d2 <- outer(v, v, "-")^2
    s <- sqrt(median(d2[upper.tri(d2)]) / 2)
    exp(-d2 / (2 * s^2))
  }
  H <- diag(8) - 1 / 8
  st <- sum(diag(gram(a) %*% H %*% gram(b) %*% H)) / 64
  r <- morie_hsicstat(a, b)
  expect_equal(r$hsic, st, tolerance = 1e-12)
  expect_equal(r$nhsic, 8 * st, tolerance = 1e-12)
  K <- exp(-outer(a, a, "-")^2 / 2)
  expect_equal(morie_hsicstat(a, b, sigma_a = 1, sigma_b = 1)$hsic,
    sum(diag(K %*% H %*% exp(-outer(b, b, "-")^2 / 2) %*% H)) / 64, tolerance = 1e-12)
  expect_error(morie_hsicstat(1:3, 1:3), "at least 4")
  expect_error(morie_hsicstat(1:5, 1:4), "same length")
})

test_that("R-learner: residual-on-residual regression", {
  n <- 12
  i <- 1:n
  x <- cos(i)
  e <- 0.3 + 0.4 * (i %% 2)
  t <- as.numeric(i %% 3 == 0 | i %% 2 == 1)
  m <- 0.5 + 0.2 * x
  y <- m + (t - e) * (1 + 0.5 * x) + sin(7 * i) / 10
  r <- morie_rlearn(y, t, m, e)
  expect_equal(r$tau, unname(coef(lm(I(y - m) ~ 0 + I(t - e)))), tolerance = 1e-12)
  rx <- morie_rlearn(y, t, m, e, x = cbind(x))
  ref <- unname(coef(lm(I(y - m) ~ 0 + I(t - e) + I((t - e) * x))))
  expect_equal(rx$tau, ref, tolerance = 1e-9)
  expect_equal(rx$ate, mean(ref[1] + ref[2] * x), tolerance = 1e-9)
  expect_error(morie_rlearn(y, t, m, t), "no variation")
  expect_error(morie_rlearn(y, t[-1], m, e), "same length")
})

test_that("separating sets, positivity, SUTVA and the causal ladder", {
  s <- morie_sepset(ml_dag, "A", "D")
  expect_true(s$found)
  expect_true(morie_dseptest(ml_dag, "A", "D", s$sepset)$dseparated)
  expect_equal(s$size, 2L)
  expect_false(morie_sepset(ml_dag, "A", "C")$found)
  expect_equal(morie_sepset(ml_dag, "A", "B")$size, 0L)
  p <- morie_poschk(c("t", "c", "t", "c", "t", "t"), c(1, 1, 1, 2, 2, 2))
  expect_equal(p$minprob, 1 / 3)
  expect_true(p$holds)
  expect_false(morie_poschk(c("t", "t", "c"), c(1, 1, 2))$holds)
  expect_error(morie_poschk(character(0), character(0)), "non-empty")
  M <- diag(3)
  M[1, 2] <- 0.2
  su <- morie_sutvachk(M)
  expect_equal(su$maxinterference, 0.2)
  expect_false(su$holds)
  expect_true(morie_sutvachk(diag(3))$holds)
  expect_false(morie_sutvachk(diag(3), versions = 2)$holds)
  expect_error(morie_sutvachk(matrix(0, 2, 3)), "square")
  expect_equal(morie_causrung(2)$name, "Intervention")
  expect_true(morie_causrung(3)$needsscm)
  expect_error(morie_causrung(4), "1, 2 or 3")
})
