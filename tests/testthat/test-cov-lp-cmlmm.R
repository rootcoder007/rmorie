# Coverage for clpopt_native.R and cmlmer_native.R: the two-phase
# simplex against lpSolve and LP duality, and the compressed mixed model
# against hclust average linkage, the REML criterion evaluated directly
# and per-marker GLS t-tests from solve().

test_that("standard_form appends slacks and flips negative rows", {
  sf <- standard_form(c(1, 2), A_ub = rbind(c(1, 1), c(-1, 2)), b_ub = c(4, -1),
                      A_eq = rbind(c(1, -1)), b_eq = 0.5, upper = list(NULL, 3))
  expect_equal(sf$A, rbind(c(1, 1, 1, 0, 0), c(1, -2, 0, -1, 0), c(0, 1, 0, 0, 1),
                           c(1, -1, 0, 0, 0)))
  expect_equal(sf$b, c(4, 1, 3, 0.5))
  expect_equal(sf$c, c(1, 2, 0, 0, 0))
  expect_equal(sf$row_kinds, c("ub", "ub", "ub", "eq"))
  expect_error(standard_form(numeric(0)), "no variables")
  expect_error(standard_form(1:2, A_ub = list(1:3), b_ub = 1), "differing length")
  expect_error(standard_form(1:2, A_ub = rbind(1:2), b_ub = 1:2), "b_ub has 2")
  expect_error(standard_form(1:2, A_eq = rbind(1:2), b_eq = 1:2), "b_eq has 2")
  expect_error(standard_form(1:2, upper = list(1, 1, 1)), "variable 2 of 2")
  expect_error(standard_form(1:2), "no constraints")
})

test_that("morie_clpopt solves linear programs", {
  A <- rbind(c(1, 1), c(1, 3))
  r <- morie_clpopt(c(3, 2), A_ub = A, b_ub = c(4, 7), upper = list(3, NULL), maximise = TRUE)
  expect_equal(r$status, "optimal")
  expect_equal(r$x, c(3, 1), tolerance = 1e-12)
  expect_equal(r$fun, 11, tolerance = 1e-12)
  expect_false(r$multiple_optima)
  sf <- standard_form(-c(3, 2), A_ub = A, b_ub = c(4, 7), upper = list(3, NULL))
  expect_equal(sum(sf$b * r$duals), r$fun, tolerance = 1e-12)
  expect_equal(r$slack, c(4, 7, 3) - c(as.numeric(A %*% c(3, 1)), 3), tolerance = 1e-12)
  expect_equal(morie_clpopt(c(3, 2), A_ub = A, b_ub = c(4, 7), upper = list(3, NULL),
                            maximise = TRUE, rule = "dantzig")$fun, 11, tolerance = 1e-12)
  m <- morie_clpopt(c(1, 2, 3), A_ub = rbind(c(-1, 1, 0)), b_ub = 1, A_eq = rbind(c(1, 1, 1)),
                    b_eq = 5, upper = list(2, NULL, NULL))
  expect_equal(m$x, c(2, 3, 0), tolerance = 1e-12)
  sfm <- standard_form(c(1, 2, 3), A_ub = rbind(c(-1, 1, 0)), b_ub = 1, A_eq = rbind(c(1, 1, 1)),
                       b_eq = 5, upper = list(2, NULL, NULL))
  expect_equal(sum(sfm$b * m$duals), m$fun, tolerance = 1e-12)
  expect_true(all(sfm$c - as.numeric(crossprod(sfm$A, m$duals)) > -1e-12))
  skip_if_not_installed("lpSolve")
  lp <- lpSolve::lp("min", c(1, 2, 3), rbind(c(-1, 1, 0), c(1, 1, 1), c(1, 0, 0)),
                    c("<=", "=", "<="), c(1, 5, 2))
  expect_equal(m$fun, lp$objval, tolerance = 1e-12)
})

test_that("morie_clpopt reports alternate optima, infeasibility and unboundedness", {
  a <- morie_clpopt(c(1, 1), A_ub = rbind(c(1, 1)), b_ub = 2, maximise = TRUE)
  expect_equal(a$fun, 2, tolerance = 1e-12)
  expect_true(a$multiple_optima)
  expect_equal(unlist(a$alternate_entering), setdiff(0:1, unlist(a$basis)))
  inf <- morie_clpopt(1, A_ub = rbind(1, -1), b_ub = c(1, -2))
  expect_equal(inf$status, "infeasible")
  unb <- morie_clpopt(c(1, 0), A_ub = rbind(c(-1, 1)), b_ub = 1, maximise = TRUE)
  expect_equal(unb$status, "unbounded")
  lim <- morie_clpopt(c(3, 2), A_ub = rbind(c(1, 1), c(1, 3)), b_ub = c(4, 7), maximise = TRUE,
                      max_iter = 1)
  expect_equal(lim$status, "iteration_limit")
  expect_error(morie_clpopt(1, A_ub = rbind(1), b_ub = 1, rule = "steepest"), "rule must be one of")
})

test_that("morie_cmlmer_compressed_lmm is REML plus GLS marker tests", {
  Mk <- rbind(c(0, 1, 2, 1, 0), c(1, 1, 2, 0, 0), c(2, 0, 1, 1, 1), c(0, 2, 2, 1, 0),
              c(1, 0, 0, 2, 2), c(2, 1, 0, 2, 1), c(0, 1, 1, 0, 2), c(1, 2, 1, 1, 1))
  Zs <- scale(Mk)
  K <- tcrossprod(Zs) / 5 + diag(0.5, 8)
  y <- c(1.2, 0.8, 2.1, 1.0, 2.5, 2.9, 1.1, 1.6)
  M <- cbind(c(0, 1, 2, 0, 2, 2, 1, 1), c(1, 1, 0, 1, 2, 0, 2, 1))
  reml <- function(delta, V0, X = matrix(1, 8, 1)) {
    V <- V0 + diag(delta, 8)
    Vi <- solve(V)
    XtVX <- t(X) %*% Vi %*% X
    b <- solve(XtVX, t(X) %*% Vi %*% y)
    r <- y - X %*% b
    s2 <- as.numeric(t(r) %*% Vi %*% r) / 7
    -0.5 * (7 * log(s2) + determinant(V)$modulus + determinant(XtVX)$modulus + 7)
  }
  r <- morie_cmlmer_compressed_lmm(y, M, K)
  expect_equal(r$reml_loglik, as.numeric(reml(r$delta, K)), tolerance = 1e-9)
  expect_gte(r$reml_loglik, max(r$reml_profile[, 2]) - 1e-12)
  expect_equal(r$reml_profile[, 2], vapply(r$reml_profile[, 1], function(t) as.numeric(reml(exp(t), K)), 0),
               tolerance = 1e-9)
  Vi <- solve(K + diag(r$delta, 8))
  for (j in 1:2) {
    Xj <- cbind(1, M[, j])
    A <- t(Xj) %*% Vi %*% Xj
    b <- solve(A, t(Xj) %*% Vi %*% y)
    res <- y - Xj %*% b
    se <- sqrt(as.numeric(t(res) %*% Vi %*% res) / 6 * solve(A)[2, 2])
    expect_equal(c(r$beta[j], r$se[j]), c(b[2], se), tolerance = 1e-9)
    expect_equal(r$p_value[j], 2 * pnorm(-abs(b[2] / se)), tolerance = 1e-9)
  }
  expect_equal(r$group_kinship, K, tolerance = 1e-12)
  expect_equal(r$h2, r$sigma2_g / (r$sigma2_g + r$sigma2_e), tolerance = 1e-12)
  c3 <- morie_cmlmer_compressed_lmm(y, M, K, clusters = 3, compare_levels = c(2, 8))
  hc <- cutree(hclust(as.dist(1 - K), method = "average"), k = 3)
  grp <- split(1:8, hc)
  grp <- grp[order(vapply(grp, min, 0))]
  lab <- integer(8)
  for (g in seq_along(grp)) lab[grp[[g]]] <- g - 1L
  expect_equal(c3$group, lab)
  Kg <- outer(seq_along(grp), seq_along(grp), Vectorize(function(a, b) mean(K[grp[[a]], grp[[b]]])))
  expect_equal(c3$group_kinship, Kg, tolerance = 1e-12)
  expect_equal(c3$reml_loglik, as.numeric(reml(c3$delta, Kg[lab + 1, lab + 1])), tolerance = 1e-9)
  grid <- -10 + 20 * (0:40) / 40
  expect_equal(c3$level_loglik[2, ], c(8, max(vapply(grid, function(t) as.numeric(reml(exp(t), K)), 0))),
               tolerance = 1e-9)
  expect_same_function(morie_cmlmer, morie_cmlmer_compressed_lmm)
  expect_equal(morie_cmlmer_compressed_lmm(y, NULL, K)$n_markers, 0L)
  expect_error(morie_cmlmer_compressed_lmm(numeric(0), M, K), "no observations")
  expect_error(morie_cmlmer_compressed_lmm(y, M, K[1:7, 1:7]), "8 by 8")
  expect_error(morie_cmlmer_compressed_lmm(y, M, K + upper.tri(K)), "not symmetric")
  expect_error(morie_cmlmer_compressed_lmm(y, M[1:7, ], K), "marker rows")
  expect_error(morie_cmlmer_compressed_lmm(y[1:2], NULL, K[1:2, 1:2]), "too few observations")
  expect_error(morie_cmlmer_compressed_lmm(y, M, K, clusters = 9), "between 1 and 8")
  expect_error(morie_cmlmer_compressed_lmm(y, M, K, compare_levels = 0), "outside 1..8")
})
