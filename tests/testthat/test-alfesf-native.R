# Anchors for ESMFold/AlphaFold confidence decoding.
#
# This module was at 3.1% coverage with no test naming any of its
# functions, but unlike the others in that band it computes the right
# answers. The assertions below are the published closed forms, so they
# lock that in rather than discovering it again later:
#
#   d0     = 1.24 (N - 15)^(1/3) - 1.8, floored at 0.5
#            (Zhang & Skolnick 2004; Jumper et al. 2021 SI 1.9.7)
#   pLDDT  = sum over bins of softmax(logits)_b * centre_b
#            (Jumper et al. 2021 SI 1.9.6, a binned expectation)

test_that("d0 follows Zhang and Skolnick, with its floor", {
  for (n in c(30L, 50L, 100L, 200L, 500L)) {
    expect_equal(.alfesf_d0(n), 1.24 * (n - 15)^(1 / 3) - 1.8, tolerance = 1e-12)
  }
  # between 16 and 21 residues the formula is positive but under the floor
  for (n in 16:21) {
    expect_lt(1.24 * (n - 15)^(1 / 3) - 1.8, 0.5)
    expect_identical(.alfesf_d0(n), 0.5)
  }
  # at and below 15 residues the formula is undefined or negative
  expect_identical(.alfesf_d0(15L), 0.5)
  expect_identical(.alfesf_d0(1L), 0.5)
  expect_identical(.alfesf_d0(0L), 0.5)
  # the floor binds just above 15 too: 1.24 * 1^(1/3) - 1.8 is negative
  expect_identical(.alfesf_d0(16L), 0.5)
  # and d0 increases with chain length once the formula takes over
  d <- vapply(c(30L, 60L, 120L, 240L), .alfesf_d0, numeric(1))
  expect_true(all(diff(d) > 0))
})

test_that("the bin centres are the midpoints of their bins", {
  # pLDDT lives on 0..100, so nb bins of width 100/nb
  expect_equal(.alfesf_lddt_centres(5L), c(10, 30, 50, 70, 90))
  expect_equal(.alfesf_lddt_centres(2L), c(25, 75))
  expect_length(.alfesf_lddt_centres(50L), 50L)
  expect_equal(.alfesf_lddt_centres(50L)[1], 1)
  expect_equal(.alfesf_lddt_centres(50L)[50], 99)
  # PAE bins are a fixed width in angstroms
  expect_equal(.alfesf_pae_centres(4L, 0.5), c(0.25, 0.75, 1.25, 1.75))
  expect_equal(.alfesf_pae_centres(3L, 2), c(1, 3, 5))
})

test_that("the row softmax is a distribution and honours the temperature", {
  M <- matrix(c(1, 2, 3, 0, 0, 0), nrow = 2, byrow = TRUE)
  S <- .alfesf_softmax_rows(M, 1)
  expect_equal(rowSums(S), c(1, 1), tolerance = 1e-12)
  # an all-equal row is uniform
  expect_equal(S[2, ], rep(1 / 3, 3), tolerance = 1e-12)
  # the first row matches the closed form
  expect_equal(S[1, ], exp(c(1, 2, 3)) / sum(exp(c(1, 2, 3))), tolerance = 1e-12)
  # raising the temperature flattens it, lowering it sharpens
  hot <- .alfesf_softmax_rows(M, 5)[1, ]
  cold <- .alfesf_softmax_rows(M, 0.2)[1, ]
  expect_lt(max(hot), max(S[1, ]))
  expect_gt(max(cold), max(S[1, ]))
  # numerically safe on large logits: the max is subtracted first
  big <- matrix(c(1000, 1001, 999), nrow = 1)
  expect_false(anyNA(.alfesf_softmax_rows(big, 1)))
  expect_equal(sum(.alfesf_softmax_rows(big, 1)), 1, tolerance = 1e-12)
})

test_that("pLDDT is the binned expectation of the logits", {
  set.seed(2)
  nb <- 50L
  lg <- matrix(rnorm(6L * nb), 6L, nb)
  r <- morie_alfesf_esmfold_confidence(lddt_logits = lg)
  want <- as.numeric(.alfesf_softmax_rows(lg, 1) %*% .alfesf_lddt_centres(nb))
  expect_equal(as.numeric(r$plddt), want, tolerance = 1e-12)
  expect_equal(r$plddt_mean, mean(want), tolerance = 1e-12)
  expect_identical(r$n_lddt_bins, nb)
  expect_identical(r$estimate, r$plddt_mean)
  # a distribution concentrated on one bin returns that bin's centre
  one <- matrix(-1e3, 1L, nb); one[1, 40L] <- 1e3
  expect_equal(morie_alfesf_esmfold_confidence(lddt_logits = one)$plddt,
               .alfesf_lddt_centres(nb)[40], tolerance = 1e-9)
  # every pLDDT lies inside the 0..100 scale
  expect_true(all(r$plddt >= 0 & r$plddt <= 100))
})

test_that("the temperature reaches the decoded pLDDT", {
  set.seed(3)
  nb <- 50L
  lg <- matrix(rnorm(4L * nb), 4L, nb)
  for (temp in c(0.5, 1, 2.5)) {
    r <- morie_alfesf_esmfold_confidence(lddt_logits = lg, temperature = temp)
    want <- as.numeric(.alfesf_softmax_rows(lg, temp) %*% .alfesf_lddt_centres(nb))
    expect_equal(as.numeric(r$plddt), want, tolerance = 1e-12)
    expect_equal(r$temperature, temp)
  }
})

test_that("the PAE route reshapes to a square matrix and reports pTM", {
  set.seed(4)
  nres <- 6L; nb <- 8L
  pae_lg <- matrix(rnorm(nres * nres * nb), nres * nres, nb)
  r <- morie_alfesf_esmfold_confidence(pae_logits = pae_lg)
  expect_identical(dim(r$pae), c(nres, nres))
  expect_true(all(is.finite(r$pae)))
  # every entry is a positive expected error in angstroms
  expect_true(all(r$pae > 0))
  expect_identical(r$n_pae_bins, nb)
  # pTM is a score on (0, 1]
  expect_true(r$ptm > 0 && r$ptm <= 1)
  # six residues is below the d0 formula's range, so the floor applies
  expect_identical(r$d0, 0.5)
})

test_that("a fitted multinomial head reproduces the observations it saw", {
  set.seed(1)
  n <- 60L
  X <- cbind(1, matrix(rnorm(n * 2), n, 2))
  # a clean monotone relationship the head can learn
  lddt <- pmin(pmax(50 + 20 * X[, 2], 0), 100)
  r <- morie_alfesf_esmfold_confidence(features = X, lddt = lddt, iters = 200L)
  expect_match(r$route, "fitted a multinomial")
  expect_length(r$plddt, n)
  expect_false(is.null(r$weights))
  # the fitted head tracks the observations it was trained on
  expect_gt(cor(as.numeric(r$plddt), lddt), 0.8)
  # 50 bins over 3 features and 60 rows is overparameterised, so more
  # iterations need not raise the correlation with the marginal; that the
  # objective itself falls is asserted separately below
  few <- morie_alfesf_esmfold_confidence(features = X, lddt = lddt, iters = 5L)
  expect_gt(cor(as.numeric(few$plddt), lddt), 0.8)
})

test_that("a supplied head is applied as given", {
  set.seed(5)
  n <- 8L; p <- 3L; nb <- 50L
  X <- matrix(rnorm(n * p), n, p)
  W <- matrix(rnorm(p * nb), p, nb)
  b <- rnorm(nb)
  r <- morie_alfesf_esmfold_confidence(features = X, weights = list(W = W, b = b))
  expect_match(r$route, "supplied LDDT head")
  lg <- sweep(X %*% W, 2L, b, "+")
  want <- as.numeric(.alfesf_softmax_rows(lg, 1) %*% .alfesf_lddt_centres(nb))
  expect_equal(as.numeric(r$plddt), want, tolerance = 1e-10)
})

test_that("temperature scaling lowers the negative log-likelihood", {
  # Guo et al. (2017): a single scalar fitted on held-out logits
  set.seed(6)
  n <- 200L; k <- 5L
  L <- matrix(rnorm(n * k, 0, 4), n, k)      # deliberately overconfident
  y <- apply(L, 1L, which.max) - 1L
  y[1:40] <- sample(0:(k - 1L), 40L, TRUE)   # some label noise to calibrate away
  nll <- function(temp) {
    p <- .alfesf_softmax_rows(L, temp)
    -mean(log(pmax(p[cbind(seq_len(n), y + 1L)], 1e-12)))
  }
  t_hat <- .alfesf_fit_temperature(L, y, iters = 300L, lr = 0.5)
  expect_true(is.finite(t_hat) && t_hat > 0)
  expect_lt(nll(t_hat), nll(1))
  # and it is close to the numerical minimiser over a grid
  grid <- seq(0.2, 8, by = 0.05)
  best <- grid[which.min(vapply(grid, nll, numeric(1)))]
  expect_equal(t_hat, best, tolerance = 0.25)
})

test_that("the multinomial fit drives its own loss down", {
  set.seed(7)
  n <- 80L; p <- 3L; nb <- 10L
  X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))
  y <- pmin(as.integer((X[, 2] - min(X[, 2])) / (diff(range(X[, 2])) + 1e-9) * nb),
            nb - 1L)
  nll <- function(fit) {
    lg <- sweep(X %*% fit$W, 2L, fit$b, "+")
    p <- .alfesf_softmax_rows(lg, 1)
    -mean(log(pmax(p[cbind(seq_len(n), y + 1L)], 1e-12)))
  }
  a <- .alfesf_fit_multinomial(X, y, nb, l2 = 1e-3, iters = 5L, lr = 0.5)
  b <- .alfesf_fit_multinomial(X, y, nb, l2 = 1e-3, iters = 300L, lr = 0.5)
  expect_identical(dim(a$W), c(p, nb))
  expect_length(a$b, nb)
  expect_lt(nll(b), nll(a))
})

test_that("row coercion accepts the shapes it documents", {
  m <- matrix(1:6, nrow = 2)
  expect_equal(.alfesf_rows(m, "x"), m, ignore_attr = FALSE)
  expect_equal(.alfesf_rows(as.data.frame(m), "x"), m, ignore_attr = TRUE)
  expect_equal(.alfesf_rows(list(c(1, 2, 3), c(4, 5, 6)), "x"),
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))
})

test_that("the inputs are validated", {
  X <- matrix(rnorm(20), 10, 2)
  expect_error(morie_alfesf_esmfold_confidence(features = X, lddt = rnorm(3)),
               "feature rows but")
  expect_error(
    morie_alfesf_esmfold_confidence(features = X, lddt = c(rep(50, 9), 150)),
    "must be on 0\\.\\.100")
  expect_error(
    morie_alfesf_esmfold_confidence(features = X,
                                    weights = list(W = matrix(0, 5, 50),
                                                   b = rep(0, 50))),
    "rows but the features")
  expect_error(
    morie_alfesf_esmfold_confidence(features = X,
                                    weights = list(W = matrix(0, 2, 50),
                                                   b = rep(0, 3))),
    "bias length")
})
