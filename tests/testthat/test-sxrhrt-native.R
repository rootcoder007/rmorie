# Bivariate REML for sex-specific heritability, treating the sexes as two
# traits measured on disjoint individuals (Yang et al. 2011 GCTA; Lee et
# al. 2012).
#
# Anchors outside the module: base R's chol, solve and determinant for the
# linear algebra; the restricted log-likelihood written out longhand with
# base R matrix operations; lm-style generalised least squares for the
# fixed effects; and closed-form maxima for the grid search. The fitted
# optimum is checked to beat its own starting point rather than being
# taken on trust.
#
# The likelihood is evaluated by a dense Cholesky in interpreted R, so the
# grid search is expensive; the end-to-end fits here are deliberately small.

test_that("the Cholesky factor agrees with base R", {
  set.seed(3)
  for (n in c(1, 2, 5, 12)) {
    A <- crossprod(matrix(rnorm(n * n), n)) + n * diag(n)
    L <- .sxrhrt_chol(A)
    expect_equal(dim(L), c(n, n))
    expect_true(all(abs(L[upper.tri(L)]) < 1e-14))
    # a small ridge is added to the diagonal, so the agreement is close
    # rather than exact
    expect_equal(L %*% t(L), A, tolerance = 1e-8)
    expect_equal(L, t(chol(A)), tolerance = 1e-8)
    # the log determinant follows from the diagonal
    expect_equal(.sxrhrt_logdet(L),
                 as.numeric(determinant(A, logarithm = TRUE)$modulus),
                 tolerance = 1e-8)
  }
  # an indefinite matrix is reported as unusable rather than returning NaN
  expect_null(.sxrhrt_chol(matrix(c(1, 5, 5, 1), 2)))
  expect_null(.sxrhrt_chol(matrix(c(-1, 0, 0, -1), 2)))
})

test_that("the triangular solve agrees with base R", {
  set.seed(5)
  for (n in c(1, 2, 4, 9)) {
    A <- crossprod(matrix(rnorm(n * n), n)) + n * diag(n)
    L <- .sxrhrt_chol(A)
    b <- rnorm(n)
    # forward and back substitution together solve the full system
    expect_equal(.sxrhrt_solve(L, b), as.numeric(solve(A, b)),
                 tolerance = 1e-7)
  }
  # the last row has no terms above the diagonal
  expect_equal(.sxrhrt_solve(.sxrhrt_chol(matrix(4, 1)), 8), 2,
               tolerance = 1e-7)
})

test_that("the grid search finds known maxima", {
  # a smooth unimodal function on a bracket containing its peak
  expect_equal(.sxrhrt_gridmax(function(x) -(x - 1.234)^2, -5, 5), 1.234,
               tolerance = 1e-6)
  expect_equal(.sxrhrt_gridmax(sin, 0, pi), pi / 2, tolerance = 1e-6)
  expect_equal(.sxrhrt_gridmax(function(x) -abs(x + 2), -5, 5), -2,
               tolerance = 1e-6)
  # a monotone function is maximised at the relevant endpoint
  expect_equal(.sxrhrt_gridmax(function(x) x, 0, 1), 1, tolerance = 1e-9)
  expect_equal(.sxrhrt_gridmax(function(x) -x, 0, 1), 0, tolerance = 1e-9)
  # the result always lies inside the bracket
  for (f in list(sin, cos, function(x) x^3)) {
    v <- .sxrhrt_gridmax(f, -2, 3)
    expect_true(v >= -2 && v <= 3)
  }
  # a coarser grid still brackets the peak
  expect_equal(.sxrhrt_gridmax(function(x) -(x - 1)^2, -5, 5, points = 21L,
                               stages = 2L), 1, tolerance = 0.05)
})

test_that("rows are coerced from whatever shape they arrive in", {
  m <- matrix(1:6, 2, 3)
  expect_equal(.sxrhrt_rows(m), matrix(as.double(1:6), 2, 3))
  expect_equal(.sxrhrt_rows(as.data.frame(m)), matrix(as.double(1:6), 2, 3),
               ignore_attr = TRUE)
  # a list of rows becomes a matrix, one row per element
  expect_equal(.sxrhrt_rows(list(c(1, 2), c(3, 4))),
               matrix(c(1, 3, 2, 4), 2, 2))
  # a bare vector becomes a single column
  expect_equal(.sxrhrt_rows(c(1, 2, 3)), matrix(c(1, 2, 3), 3, 1))
  expect_equal(storage.mode(.sxrhrt_rows(m)), "double")
})

test_that("the restricted log-likelihood matches a longhand computation", {
  set.seed(7)
  n <- 30
  male <- rep(c(TRUE, FALSE), each = n / 2)
  Z <- matrix(rnorm(n * 80), n)
  K <- tcrossprod(scale(Z)) / 80
  K <- K / mean(diag(K))
  y <- rnorm(n, 2, 1)
  X <- matrix(1, n, 1)

  ref <- function(theta) {
    cv <- theta[3] * sqrt(theta[1] * theta[2])
    B <- outer(male, male, function(a, b) {
      ifelse(a & b, theta[1], ifelse(!a & !b, theta[2], cv))
    })
    V <- B * K + diag(ifelse(male, theta[4], theta[5]))
    Vi <- solve(V)
    XtViX <- t(X) %*% Vi %*% X
    bt <- solve(XtViX, t(X) %*% Vi %*% y)
    r <- y - X %*% bt
    list(
      ll = -0.5 * (as.numeric(determinant(V, logarithm = TRUE)$modulus) +
                   as.numeric(determinant(XtViX, logarithm = TRUE)$modulus) +
                   as.numeric(t(r) %*% Vi %*% r)),
      beta = as.numeric(bt)
    )
  }
  for (th in list(c(0.5, 0.5, 0, 0.5, 0.5), c(1, 2, 0.6, 0.8, 1.2),
                  c(0.3, 0.9, -0.4, 1, 0.7))) {
    got <- .sxrhrt_reml(th, y, X, K, male)
    want <- ref(th)
    expect_equal(got$ll, want$ll, tolerance = 1e-8)
    # the fixed effect is the generalised least squares estimate
    expect_equal(got$beta, want$beta, tolerance = 1e-8)
  }
  # the parameter space is guarded: variances positive, correlation interior
  expect_null(.sxrhrt_reml(c(-1, 1, 0, 1, 1), y, X, K, male))
  expect_null(.sxrhrt_reml(c(1, 0, 0, 1, 1), y, X, K, male))
  expect_null(.sxrhrt_reml(c(1, 1, 0, -1, 1), y, X, K, male))
  expect_null(.sxrhrt_reml(c(1, 1, 1, 1, 1), y, X, K, male))
  expect_null(.sxrhrt_reml(c(1, 1, -1.5, 1, 1), y, X, K, male))
})

test_that("a small fit reports a coherent variance decomposition", {
  set.seed(11)
  n <- 16
  male <- rep(c(TRUE, FALSE), each = n / 2)
  Z <- matrix(rnorm(n * 40), n)
  K <- tcrossprod(scale(Z)) / 40
  K <- K / mean(diag(K))
  y <- rnorm(n, 1, 1)
  r <- morie_sxrhrt_sex_specific_h2(y, ifelse(male, 1, 0), K,
                                    max_cycles = 2L)
  # heritabilities are proportions of their own sex's total variance
  expect_equal(r$h2_male,
               r$sigma2_g_male / (r$sigma2_g_male + r$sigma2_e_male))
  expect_equal(r$h2_female,
               r$sigma2_g_female / (r$sigma2_g_female + r$sigma2_e_female))
  expect_true(r$h2_male >= 0 && r$h2_male <= 1)
  expect_true(r$h2_female >= 0 && r$h2_female <= 1)
  expect_equal(r$estimate, c(r$h2_male, r$h2_female))
  # the genetic correlation is bounded away from the admissibility edge
  expect_true(abs(r$rg) < 1)
  # the cross-sex covariance follows from the correlation and the variances
  expect_equal(r$sigma2_g_cross,
               r$rg * sqrt(r$sigma2_g_male * r$sigma2_g_female))
  # the coordinate ascent never moves downhill, and ends where it says
  expect_true(all(diff(r$reml_path) >= -1e-9))
  expect_equal(r$reml_loglik, tail(r$reml_path, 1), tolerance = 1e-8)
  # counts are reported as given
  expect_equal(r$n, 16L)
  expect_equal(r$n_male, 8L)
  expect_equal(r$n_female, 8L)
  expect_equal(r$p, 1L)
  expect_match(r$method, "bivariate REML")
  # the likelihood-ratio statistics are non-negative by construction
  expect_true(r$lrt_rg_equals_one >= 0)
  expect_true(r$lrt_equal_h2 >= 0)
  # the two p-values use different null geometries: rg = 1 sits on the
  # boundary and takes the half-and-half mixture, equal heritability is an
  # interior one-degree-of-freedom test
  expect_equal(r$p_rg_equals_one, 1 - pnorm(sqrt(r$lrt_rg_equals_one)))
  expect_equal(r$p_equal_h2, 2 * (1 - pnorm(sqrt(r$lrt_equal_h2))))
  expect_true(r$p_rg_equals_one >= 0 && r$p_rg_equals_one <= 1)
  expect_true(r$p_equal_h2 >= 0 && r$p_equal_h2 <= 1)
  # the cross-sex block of the relatedness matrix is the only thing that
  # identifies rg, so it is reported as the diagnostic to read first
  expect_equal(r$max_cross_sex_relatedness,
               max(abs(K[outer(male, male, "!=")])))
  expect_match(r$note, "max_cross_sex_relatedness")
})

test_that("the fitted optimum is no worse than its starting point", {
  set.seed(13)
  n <- 16
  male <- rep(c(TRUE, FALSE), each = n / 2)
  Z <- matrix(rnorm(n * 40), n)
  K <- tcrossprod(scale(Z)) / 40
  K <- K / mean(diag(K))
  y <- rnorm(n, 0, 1)
  r <- morie_sxrhrt_sex_specific_h2(y, ifelse(male, 1, 0), K,
                                    max_cycles = 2L)
  # the search starts from half the phenotypic variance in each component
  expect_true(r$reml_loglik >= r$reml_path[1] - 1e-9)
  expect_true(length(r$reml_path) >= 2)
  expect_equal(r$cycles, 2L)
  expect_false(r$converged)
})

test_that("sxrhrt refuses input that identifies nothing", {
  n <- 12
  K <- diag(n)
  y <- rnorm(n)
  sex <- rep(c(1, 0), each = n / 2)
  expect_error(morie_sxrhrt_sex_specific_h2(numeric(0), numeric(0),
                                            matrix(0, 0, 0)),
               "no observations")
  expect_error(morie_sxrhrt_sex_specific_h2(y[1:5], sex, K),
               "phenotypes but .* sex labels")
  expect_error(morie_sxrhrt_sex_specific_h2(y, sex, diag(5)),
               "K must be .* by")
  # a single member of one sex cannot support a variance
  lop <- c(1, rep(0, n - 1))
  expect_error(morie_sxrhrt_sex_specific_h2(y, lop, K),
               "a variance cannot be estimated")
  # an asymmetric relatedness matrix is not a genomic relationship matrix
  Kb <- K
  Kb[1, 2] <- 0.5
  expect_error(morie_sxrhrt_sex_specific_h2(y, sex, Kb), "not symmetric")
  # the male label is configurable
  expect_error(morie_sxrhrt_sex_specific_h2(y, rep(3, n), K, male_label = 3),
               "a variance cannot be estimated")
})
