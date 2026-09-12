# Pattern classification and decomposition (Rangayyan, Biomedical Signal
# Analysis, Ch 9).
#
# Anchors outside the module: base R's %*% for the matrix product and
# eigen() for the Jacobi spectrum; perfect reconstruction for the
# short-time Fourier transform; exact sparse recovery for orthogonal
# matching pursuit; exact factorisation for the nonnegative updates; and
# the definitions written longhand for the confusion counts and scores.

test_that("the compensated matrix product agrees with %*%", {
  set.seed(4)
  for (d in list(c(3, 4, 5), c(1, 1, 1), c(6, 2, 3))) {
    A <- matrix(rnorm(d[1] * d[2]), d[1], d[2])
    B <- matrix(rnorm(d[2] * d[3]), d[2], d[3])
    expect_equal(.morie_bx_mm(A, B), A %*% B)
  }
  # multiplying by the identity changes nothing
  A <- matrix(rnorm(9), 3)
  expect_equal(.morie_bx_mm(A, diag(3)), A)
  expect_equal(.morie_bx_mm(diag(3), A), A)
  expect_error(.morie_bx_mm(matrix(0, 2, 3), matrix(0, 4, 2)),
               "inner matrix dimensions do not agree")
})

test_that("cyclic Jacobi recovers the spectrum that eigen() reports", {
  set.seed(6)
  for (n in c(2, 3, 5, 8)) {
    M <- matrix(rnorm(n * n), n)
    S <- crossprod(M) + diag(n)
    got <- .morie_bx_jacobi(S)
    ref <- eigen(S, symmetric = TRUE)
    # eigenvalues, largest first
    expect_equal(got$values, ref$values, tolerance = 1e-8)
    expect_true(all(diff(got$values) <= 1e-12))
    # the vectors are orthonormal and satisfy the eigen equation
    expect_equal(crossprod(got$vectors), diag(n), tolerance = 1e-8)
    for (k in seq_len(n)) {
      expect_equal(as.numeric(S %*% got$vectors[, k]),
                   got$values[k] * got$vectors[, k], tolerance = 1e-7)
    }
  }
  # a diagonal matrix is already diagonal: no rotation is needed
  D <- diag(c(3, 1, 2))
  jd <- .morie_bx_jacobi(D)
  expect_equal(jd$values, c(3, 2, 1))
  # repeated eigenvalues are all reported, which is what power iteration
  # with deflation would lose
  I3 <- diag(3)
  expect_equal(.morie_bx_jacobi(I3)$values, rep(1, 3))
  # a one-by-one matrix is a degenerate but legal input
  expect_equal(.morie_bx_jacobi(matrix(7, 1, 1))$values, 7)
})

test_that("the row-major fill draws in order", {
  vals <- c(1, 2, 3, 4, 5, 6)
  i <- 0L
  u <- function() {
    i <<- i + 1L
    vals[i]
  }
  m <- .morie_bx_fill(2, 3, u, identity)
  # filled row by row, not column by column
  expect_equal(m, matrix(c(1, 2, 3, 4, 5, 6), 2, 3, byrow = TRUE))
  # the transform is applied to every draw
  i <- 0L
  expect_equal(.morie_bx_fill(2, 3, u, function(z) z * 10),
               matrix(c(1, 2, 3, 4, 5, 6) * 10, 2, 3, byrow = TRUE))
  i <- 0L
  expect_equal(dim(.morie_bx_fill(3, 2, u, identity)), c(3L, 2L))
})

test_that("confusion counts and scores follow their definitions", {
  true <- c(1, 1, 1, 0, 0, 0, 0, 1)
  pred <- c(1, 1, 0, 0, 0, 1, 0, 1)
  cm <- .morie_bx_confusion(true, pred)
  expect_equal(cm$tp, 3)
  expect_equal(cm$tn, 3)
  expect_equal(cm$fp, 1)
  expect_equal(cm$fn, 1)
  # the four cells partition the sample
  expect_equal(cm$tp + cm$tn + cm$fp + cm$fn, length(true))

  sc <- .morie_bx_scores(cm$tp, cm$tn, cm$fp, cm$fn)
  expect_equal(sc$sensitivity, 3 / 4)
  expect_equal(sc$specificity, 3 / 4)
  expect_equal(sc$accuracy, 6 / 8)
  # a perfect classifier scores one everywhere
  p <- .morie_bx_scores(5, 5, 0, 0)
  expect_equal(p$sensitivity, 1)
  expect_equal(p$specificity, 1)
  expect_equal(p$accuracy, 1)
  # an absent class gives no rate rather than dividing by zero
  expect_true(is.nan(.morie_bx_scores(0, 5, 0, 0)$sensitivity))
  expect_true(is.nan(.morie_bx_scores(5, 0, 0, 0)$specificity))
  expect_true(is.nan(.morie_bx_scores(0, 0, 0, 0)$accuracy))
})

test_that("the nonnegative factorisation descends and can be exact", {
  # a matrix that is exactly rank two and nonnegative must be recoverable
  set.seed(8)
  W0 <- matrix(runif(6 * 2, 0.5, 1.5), 6, 2)
  H0 <- matrix(runif(2 * 5, 0.5, 1.5), 2, 5)
  V <- W0 %*% H0
  fit <- .morie_bx_nmfmu(V, 2L, 4000L, 1e-14, 1L, "ls")
  expect_true(all(fit$W >= 0))
  expect_true(all(fit$H >= 0))
  expect_equal(dim(fit$W), c(6L, 2L))
  expect_equal(dim(fit$H), c(2L, 5L))
  # the reported error is the Frobenius norm of the residual
  expect_equal(fit$error, sqrt(sum((V - fit$W %*% fit$H)^2)),
               tolerance = 1e-8)
  # and it gets close, since an exact factorisation exists
  expect_true(fit$error < 0.01 * sqrt(sum(V^2)))

  # the Lee-Seung updates never increase the least-squares error
  errs <- vapply(c(2L, 5L, 20L, 80L),
                 function(it) .morie_bx_nmfmu(V, 2L, it, 0, 1L, "ls")$error,
                 numeric(1))
  expect_true(all(diff(errs) <= 1e-9))
  # the divergence cost also runs and returns a usable factorisation
  kl <- .morie_bx_nmfmu(V, 2L, 200L, 1e-12, 1L, "kld")
  expect_true(all(kl$W >= 0) && all(kl$H >= 0))
  expect_true(is.finite(kl$error))
  # a fixed seed is reproducible
  expect_equal(.morie_bx_nmfmu(V, 2L, 50L, 0, 3L, "ls")$W,
               .morie_bx_nmfmu(V, 2L, 50L, 0, 3L, "ls")$W)
  expect_error(.morie_bx_nmfmu(matrix(-1, 3, 3), 1L, 10L, 0, 1L, "ls"),
               "nonnegative matrix")
  expect_error(.morie_bx_nmfmu(V, 0L, 10L, 0, 1L, "ls"), "rank r must satisfy")
  expect_error(.morie_bx_nmfmu(V, 9L, 10L, 0, 1L, "ls"), "rank r must satisfy")
})

test_that("matching pursuit recovers an exactly sparse signal", {
  n <- 24
  # an orthonormal dictionary makes the greedy choice provably right
  set.seed(10)
  D <- qr.Q(qr(matrix(rnorm(n * n), n)))
  D <- t(D)                       # atoms in rows, as the routine expects
  idx <- c(3L, 11L, 19L)
  w <- c(2, -3, 1.5)
  x <- as.numeric(t(D[idx, , drop = FALSE]) %*% w)
  got <- .morie_bx_omp(x, D, 3L, 1e-10)
  expect_setequal(got$support, idx)
  expect_equal(got$coefficients[idx], w, tolerance = 1e-8)
  # every other coefficient is exactly zero
  expect_equal(got$coefficients[-idx], rep(0, n - 3))
  # and the residual vanishes
  expect_true(max(abs(got$residual)) < 1e-8)

  # the residual norm is non-increasing as atoms are added
  rs <- vapply(1:4, function(k) sqrt(sum(.morie_bx_omp(x, D, k, 0)$residual^2)),
               numeric(1))
  expect_true(all(diff(rs) <= 1e-9))
  # the support never exceeds the requested sparsity
  expect_length(.morie_bx_omp(x, D, 2L, 0)$support, 2L)
  # a residual tolerance reached early stops the search before the sparsity
  # budget is spent
  expect_length(.morie_bx_omp(x, D, 10L, 1e-6)$support, 3L)
  expect_true(length(.morie_bx_omp(x, D, 10L, 0)$support) > 3L)
  expect_error(.morie_bx_omp(x, D[, 1:5], 2L, 0), "same length as x")
  expect_error(.morie_bx_omp(x, rbind(rep(0, n), D[1, ]), 2L, 0),
               "nonzero norm")
  expect_error(.morie_bx_omp(x, D, 0L, 0), "positive integer")
})

test_that("the Gabor dictionary is unit-norm and reproducible", {
  n <- 32
  g <- .morie_bx_gabor(n, 12L)
  expect_equal(dim(g$atoms), c(12L, n))
  expect_length(g$params, 12L)
  # every atom is normalised, which is what makes the pursuit correlations
  # comparable across scales
  expect_equal(apply(g$atoms, 1, function(a) sqrt(sum(a^2))), rep(1, 12),
               tolerance = 1e-10)
  # the parameters describe each atom
  expect_named(g$params[[1]], c("scale", "translation", "frequency"))
  expect_true(all(vapply(g$params, function(p) p$scale > 0, logical(1))))
  # the grid is fixed, so two builds agree exactly
  expect_equal(.morie_bx_gabor(n, 5L)$atoms, .morie_bx_gabor(n, 5L)$atoms)
  # a larger request is a superset of a smaller one
  expect_equal(.morie_bx_gabor(n, 3L)$atoms, g$atoms[1:3, , drop = FALSE])
  expect_error(.morie_bx_gabor(2, 4L), "n >= 4 samples")
  expect_error(.morie_bx_gabor(16, 0L), "at least one atom")
})

test_that("the short-time transform reconstructs the signal it analysed", {
  set.seed(12)
  x <- sin(2 * pi * 5 * (0:127) / 64) + 0.1 * rnorm(128)
  for (cfg in list(c(32, 16), c(32, 8), c(64, 16))) {
    nwin <- cfg[1]
    hop <- cfg[2]
    s <- .morie_bx_stft(x, nwin, hop)
    expect_named(s, c("re", "im", "mag", "win"))
    expect_equal(dim(s$re), dim(s$im))
    # one row per frame, one column per non-negative frequency
    expect_equal(ncol(s$re), nwin %/% 2 + 1)
    expect_equal(nrow(s$re), length(seq(0, length(x) - nwin, by = hop)))
    # the magnitude is the modulus of the complex coefficients
    expect_equal(s$mag, sqrt(s$re^2 + s$im^2))
    expect_length(s$win, nwin)
    # weighted overlap-add inverts the analysis. The Hann window is zero at
    # its first tap, so sample one is covered by no frame at all and comes
    # back as zero; every sample that has support is recovered exactly.
    y <- .morie_bx_istft(s$re, s$im, nwin, hop, s$win, length(x))
    expect_equal(s$win[1], 0)
    expect_equal(y[1], 0)
    expect_equal(y[-1], x[-1], tolerance = 1e-8)
  }
  # a constant signal comes back constant on its supported samples
  cst <- rep(2, 64)
  sc <- .morie_bx_stft(cst, 16, 8)
  rec <- .morie_bx_istft(sc$re, sc$im, 16, 8, sc$win, 64)
  expect_equal(rec[-1], cst[-1], tolerance = 1e-8)
  expect_error(.morie_bx_stft(x, 2, 1), "4 <= nwin <= len")
  expect_error(.morie_bx_stft(x, 999, 1), "4 <= nwin <= len")
})

test_that("channel selection ranks by deviation from the mid-point", {
  set.seed(14)
  nch <- 5
  # three channels carry structure, two are near-constant noise
  trials <- rbind(
    sin(2 * pi * 3 * (1:64) / 64),
    cos(2 * pi * 7 * (1:64) / 64),
    sin(2 * pi * 11 * (1:64) / 64),
    rnorm(64, 0, 0.01),
    rnorm(64, 0, 0.01)
  )
  got <- .morie_bx_chsel(trials, 2L, 3L, 300L, 1e-10, 1L)
  expect_equal(dim(got$C), c(nch, nch))
  expect_equal(dim(got$W), c(nch, 3L))
  expect_length(got$rmsd, nch)
  # the normalised rows lie in the unit interval by construction
  expect_true(all(got$normalized >= 0 & got$normalized <= 1))
  # the ranking is a permutation, reported zero-based
  expect_setequal(got$ranking, 0:(nch - 1))
  expect_length(got$selected, 2L)
  expect_true(all(got$selected %in% 0:(nch - 1)))
  # the selected set is the head of the ranking, in ascending order
  expect_equal(got$selected, sort(got$ranking[1:2]))
  # the root-mean-square deviation is recomputable from the normalised rows
  expect_equal(got$rmsd,
               apply(got$normalized, 1, function(r) sqrt(mean((r - 0.5)^2))))
  expect_error(.morie_bx_chsel(trials[1, , drop = FALSE], 1L, 3L, 10L, 0, 1L),
               "at least two EEG channels")
  expect_error(.morie_bx_chsel(trials, 0L, 3L, 10L, 0, 1L), "nselect must")
  expect_error(.morie_bx_chsel(trials, 9L, 3L, 10L, 0, 1L), "nselect must")
  # rank two is refused with the reason spelled out: it ranks nothing
  expect_error(.morie_bx_chsel(trials, 2L, 2L, 10L, 0, 1L),
               "rank must be at least 3")
})
