# MVSML chapter 5: R tests + 10-digit Python parity anchors.

X6 <- matrix(1, nrow = 6, ncol = 1)
Z6 <- matrix(c(1,0, 1,0, 1,0, 0,1, 0,1, 0,1), nrow = 6, byrow = TRUE)
Y6 <- c(5.0, 5.2, 4.8, 6.4, 6.6, 6.2)
D2 <- diag(0.5, 2)

test_that("eq (5.1) BLUE/BLUP match the Python anchors", {
  f <- morie_mvsml_blue_blup_v(X6, Z6, Y6, D2)
  expect_equal(f$blue[1], 5.7, tolerance = 1e-9)
  expect_equal(f$blup, c(-0.42, 0.42), tolerance = 1e-9)
  V <- morie_mvsml_lmm_v(Z6, D2)
  expect_equal(V[1, 1], 1.5, tolerance = 1e-12)
  expect_equal(V[1, 2], 0.5, tolerance = 1e-12)
  expect_equal(V[1, 4], 0, tolerance = 1e-12)
})

test_that("eq (5.2) likelihood and REML match the anchors", {
  expect_equal(morie_mvsml_lmm_loglik(X6, Z6, Y6, D2)$loglik,
               -7.097921931, tolerance = 1e-8)
  expect_equal(morie_mvsml_reml_loglik(X6, Z6, Y6, D2)$loglik,
               -2.022025101, tolerance = 1e-8)
  # REML = ML + n/2 log(2 pi) - 1/2 log|X'V^-1X|  (book p.146)
  V <- morie_mvsml_lmm_v(Z6, D2)
  A <- t(X6) %*% solve(V) %*% X6
  expect_equal(morie_mvsml_reml_loglik(X6, Z6, Y6, D2)$loglik,
               morie_mvsml_lmm_loglik(X6, Z6, Y6, D2)$loglik +
                 0.5 * 6 * log(2 * pi) -
                 0.5 * determinant(A, logarithm = TRUE)$modulus[1],
               tolerance = 1e-9)
})

test_that("the EM algorithm matches the Python anchors", {
  r <- morie_mvsml_em_lmm(X6, Z6, Y6, n_iter = 800L)
  expect_equal(r$beta[1], 5.7, tolerance = 1e-9)
  expect_equal(r$sigma2, 0.0320080483, tolerance = 1e-7)
  expect_equal(r$D[1, 1], 0.4846720288, tolerance = 1e-7)
})

test_that("eq (5.3) GBLUP matches the Python anchors", {
  Z <- matrix(c(1,0,0, 1,0,0, 0,1,0, 0,1,0, 0,0,1, 0,0,1),
              nrow = 6, byrow = TRUE)
  y <- c(5.1, 4.9, 6.0, 6.2, 5.5, 5.7)
  G <- matrix(c(1,.5,0, .5,1,0, 0,0,1), nrow = 3, byrow = TRUE)
  f <- morie_mvsml_gblup_model(y, Z, G, 0.5)
  expect_equal(f$mu, 5.569230769, tolerance = 1e-8)
  expect_equal(f$b, c(-0.1948717949, 0.1717948718, 0.01538461538),
               tolerance = 1e-8)
})

test_that("eq (5.5) reduces to univariate GBLUP when diagonal", {
  Y <- matrix(c(5.0, 2.0, 6.0, 3.0, 5.5, 2.4), nrow = 3,
              byrow = TRUE)
  Z <- diag(3)
  G <- matrix(c(1,.2,0, .2,1,0, 0,0,1), nrow = 3, byrow = TRUE)
  r <- morie_mvsml_multitrait(Y, Z, G, diag(c(0.4, 0.9)),
                              diag(c(1, 2)))
  for (t in 1:2) {
    s2g <- c(0.4, 0.9)[t]; s2e <- c(1, 2)[t]
    uni <- morie_mvsml_gblup_model(Y[, t], Z, G, s2g, s2e)
    expect_equal(r$mu[t], uni$mu, tolerance = 1e-8)      # p.153
    got <- vapply(r$b_by_line, function(v) v[t], 0)
    expect_equal(unname(got), uni$b, tolerance = 1e-8)
  }
})

test_that("eq (5.6) reduces to univariate G x E when all diagonal", {
  Y <- matrix(c(5.0,2.0, 6.0,3.0, 5.4,2.2, 6.6,3.4), nrow = 4,
              byrow = TRUE)
  Z_L <- matrix(c(1,0, 0,1, 1,0, 0,1), nrow = 4, byrow = TRUE)
  Z_EL <- diag(4)
  G <- diag(2)
  r <- morie_mvsml_gxe_multitrait(Y, Z_L, Z_EL, G,
                                  diag(c(0.4, 0.9)), diag(2),
                                  diag(c(0.2, 0.5)), diag(c(1, 2)))
  for (t in 1:2) {
    s2g <- c(0.4, 0.9)[t]; s2ge <- c(0.2, 0.5)[t]
    s2e <- c(1, 2)[t]
    uni <- morie_mvsml_gxe_blup(Y[, t], NULL, Z_L, Z_EL, G, s2g,
                                diag(s2ge, 2), s2e)
    expect_equal(r$b_lines[t], uni$b_lines[1], tolerance = 1e-7)
    expect_equal(r$b_lines[2 + t], uni$b_lines[2],
                 tolerance = 1e-7)                        # p.155
  }
})

test_that("kron matches the definition", {
  K <- morie_mvsml_kron(matrix(c(1,2,3,4), 2, byrow = TRUE),
                        matrix(c(0,5,6,7), 2, byrow = TRUE))
  expect_equal(as.numeric(K[1, ]), c(0, 5, 0, 10))
  expect_equal(as.numeric(K[4, ]), c(18, 21, 24, 28))
})
