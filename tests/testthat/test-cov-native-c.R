# Coverage for abundance_text_voting_native.R (ESS), bnp_ghosal.R
# (Polya-tree bits), bricklayer.R (status check), burkov_lm_native.R,
# cqtmpl_native.R, crkbsg_native.R, entheo_dmt.R (offline branches) and
# frfgrf_native.R: autocorrelations against acf, composite interval
# mapping replayed as a two-point mixture EM, cokriging against the
# block system solved by solve(), and the forest audit against its own
# grown trees.

test_that("morie_ess_autocorrelation uses Geyer's positive pairs", {
  x <- c(0.3, 0.8, 1.1, 0.9, 0.2, -0.4, -0.6, 0.1, 0.7, 1.3, 0.9, 0.4, -0.2, -0.5, 0.3, 0.6)
  r <- morie_ess_autocorrelation(x)
  rho <- as.numeric(acf(x, lag.max = 8, plot = FALSE)$acf)
  expect_equal(r$rho, rho, tolerance = 1e-12)
  tot <- 0
  used <- 0
  k <- 1
  while (k + 1 <= 8 && rho[k + 1] + rho[k + 2] > 0) {
    tot <- tot + rho[k + 1] + rho[k + 2]
    used <- k + 1
    k <- k + 2
  }
  expect_equal(r$tau_int, 1 + 2 * tot, tolerance = 1e-12)
  expect_equal(r$ess, 16 / (1 + 2 * tot), tolerance = 1e-12)
  expect_equal(r$n_lags_used, used)
  expect_length(morie_ess_autocorrelation(x, max_lag = 3)$rho, 4L)
  expect_equal(morie_ess_autocorrelation(c(1, 2, NA))$ess, 2)
  expect_equal(morie_ess_autocorrelation(rep(1, 6))$tau_int, 1)
})

test_that("morie_gh_pt_bits and morie_burkov_linear_vector", {
  expect_equal(morie_gh_pt_bits(0.625, 4), c(1L, 0L, 1L, 0L))
  expect_equal(morie_gh_pt_bits(0.3, 6), as.integer(floor(0.3 * 2^(1:6)) %% 2))
  expect_equal(morie_gh_pt_bits(-1, 3), c(0L, 0L, 0L))
  expect_equal(morie_gh_pt_bits(2, 5), rep(1L, 5))
  b <- morie_burkov_linear_vector(c(1, -2, 0.5), c(3, 1, 4), 0.25)
  expect_equal(c(b$estimate, b$dot), c(3 - 2 + 2 + 0.25, 3))
  expect_error(morie_burkov_linear_vector(1:2, 1), "same length")
})

test_that("morie_bricklayer check mode reports without installing", {
  skip_on_cran()
  out <- NULL
  expect_output(out <- morie_bricklayer(check = TRUE), "morie family status")
  expect_named(out, c("morie", "rmorie", "rmoriedata", "rmoriebricklayer", "rmorie-cli"))
  expect_true(out[["rmorie"]])
  expect_equal(out[["rmoriedata"]], requireNamespace("rmoriedata", quietly = TRUE))
})

test_that("morie_entheo_clone_dmt_imaging stops before any network step", {
  root <- file.path(tempdir(), "dmt_present")
  dir.create(root, showWarnings = FALSE)
  expect_message(got <- morie_entheo_clone_dmt_imaging(root = root), "already present")
  expect_equal(got, root)
  fresh <- file.path(tempdir(), "dmt_fresh_never_cloned")
  unlink(fresh, recursive = TRUE)
  expect_error(morie_entheo_clone_dmt_imaging(root = fresh, branch = "-bad"),
               "invalid branch|git not found")
  expect_false(dir.exists(fresh))
  unlink(root, recursive = TRUE)
})

test_that("morie_cqtmpl is EM for the flanking-marker mixture", {
  y <- c(2.1, 3.5, 2.4, 3.9, 1.8, 3.2, 2.6, 4.1, 2.0, 3.7)
  left <- c(0, 1, 0, 1, 0, 1, 1, 1, 0, 0)
  right <- c(0, 1, 0, 1, 0, 0, 1, 1, 1, 0)
  cf <- c(0.5, -0.2, 0.1, 0.3, -0.4, 0.2, 0, 0.6, -0.1, 0.2)
  gp <- function(l, r) {
    q <- sapply(0:1, function(g) (if (g == l) 0.9 else 0.1) * (if (g == r) 0.85 else 0.15))
    q / sum(q)
  }
  P <- t(mapply(gp, left, right))
  run <- function(cof) {
    Z <- if (is.null(cof)) matrix(0, 10, 0) else cbind(cof)
    beta <- c(mean(y), 0.1 * (max(y) - min(y) + 1e-12), rep(0, ncol(Z)))
    s2 <- mean((y - mean(y))^2)
    hist <- numeric(0)
    repeat {
      mu0 <- beta[1] + Z %*% beta[-(1:2)]
      d0 <- P[, 1] * exp(-(y - mu0)^2 / (2 * s2))
      d1 <- P[, 2] * exp(-(y - mu0 - beta[2])^2 / (2 * s2))
      post <- as.numeric(d1 / (d0 + d1))
      hist <- c(hist, sum(log((d0 + d1) / sqrt(2 * pi * s2))))
      k <- length(hist)
      if ((k > 1 && abs(hist[k] - hist[k - 1]) < 1e-10) || k == 200) break
      Xs <- rbind(cbind(1, 0, Z), cbind(1, 1, Z))
      f <- lm.wfit(Xs, c(y, y), c(1 - post, post))
      beta <- unname(f$coefficients)
      s2 <- sum(c(1 - post, post) * f$residuals^2) / 10
    }
    X0 <- cbind(1, Z)
    s0 <- mean(lm.fit(X0, y)$residuals^2)
    list(hist = hist, beta = beta, s2 = s2, post = post,
         lod = (hist[length(hist)] + 5 * (log(2 * pi * s0) + 1)) / log(10))
  }
  e <- run(NULL)
  r <- morie_cqtmpl(y, left, right, 0.1, 0.15)
  expect_equal(r$loglik_history, e$hist, tolerance = 1e-9)
  expect_equal(c(r$b0, r$b), e$beta, tolerance = 1e-9)
  expect_equal(r$posterior, e$post, tolerance = 1e-9)
  expect_equal(r$lod, e$lod, tolerance = 1e-9)
  expect_true(all(diff(r$loglik_history) > -1e-10))
  ec <- run(cf)
  rc <- morie_cqtmpl(y, left, right, 0.1, 0.15, cofactors = list(cf))
  expect_equal(c(rc$b0, rc$b, rc$cofactor_coefficients), ec$beta, tolerance = 1e-9)
  expect_equal(rc$lod, ec$lod, tolerance = 1e-9)
  expect_error(morie_cqtmpl(y, left[-1], right, 0.1, 0.1), "same length")
  expect_error(morie_cqtmpl(y, left, right, 0.1, 0.1, cofactors = list(1:3)), "10 entries")
  expect_error(morie_cqtmpl(y, left, right, 0.7, 0.1), "\\[0, 0.5\\]")
})

test_that("morie_crkbsg_cokriging solves the two-constraint system", {
  C1 <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1.5))
  C2 <- rbind(c(0.5, 0.5), c(1.5, 0.2), c(0.2, 1.4))
  y <- c(1.2, 0.7, 1.9, 1.1)
  z <- c(0.4, 0.1, 0.8)
  tg <- rbind(c(0.6, 0.6), c(2, 2))
  par <- list(model = "spherical", range = 2.5, b11 = 1.2, b22 = 0.9, b12 = 0.6,
              nugget11 = 0.1, nugget22 = 0.05, nugget12 = 0.02)
  rho <- function(h) ifelse(h >= 2.5, 0, 1 - 1.5 * h / 2.5 + 0.5 * (h / 2.5)^3)
  cv <- function(A, B, b, nug) {
    H <- outer(seq_len(nrow(A)), seq_len(nrow(B)),
               Vectorize(function(i, j) sqrt(sum((A[i, ] - B[j, ])^2))))
    b * rho(H) + nug * (H == 0)
  }
  K <- rbind(cbind(cv(C1, C1, 1.2, 0.1), cv(C1, C2, 0.6, 0.02), 1, 0),
             cbind(cv(C2, C1, 0.6, 0.02), cv(C2, C2, 0.9, 0.05), 0, 1),
             c(rep(1, 4), rep(0, 5)), c(rep(0, 4), rep(1, 3), 0, 0))
  r <- morie_crkbsg_cokriging(C1, y, z, tg, par, coords_z = C2)
  for (k in 1:2) {
    c0 <- c(cv(C1, tg[k, , drop = FALSE], 1.2, 0.1), cv(C2, tg[k, , drop = FALSE], 0.6, 0.02))
    sol <- solve(K, c(c0, 1, 0))
    expect_equal(r$prediction[k], sum(sol[1:7] * c(y, z)), tolerance = 1e-10)
    expect_equal(r$variance[k], 1.3 - sum(sol[1:7] * c0) - sol[8], tolerance = 1e-10)
    Kk <- rbind(cbind(cv(C1, C1, 1.2, 0.1), 1), c(rep(1, 4), 0))
    sk <- solve(Kk, c(c0[1:4], 1))
    expect_equal(r$kriging_prediction[k], sum(sk[1:4] * y), tolerance = 1e-10)
    expect_equal(r$kriging_variance[k], 1.3 - sum(sk[1:4] * c0[1:4]) - sk[5], tolerance = 1e-10)
  }
  expect_true(all(r$variance_reduction >= -1e-12))
  ind <- morie_crkbsg_cokriging(C1, y, z, tg, list(b12 = 0), coords_z = C2)
  expect_equal(ind$prediction, ind$kriging_prediction, tolerance = 1e-10)
  expect_equal(ind$variance_reduction, c(0, 0), tolerance = 1e-10)
  gs <- morie_crkbsg_cokriging(C1, y, y + 0.1, c(0.3, 0.3), list(model = "gaussian", b12 = 0.5))
  expect_equal(gs$n_secondary, 4L)
  expect_error(morie_crkbsg_cokriging(C1[0, ], numeric(0), z, tg), "no primary")
  expect_error(morie_crkbsg_cokriging(C1, y[-1], z, tg, coords_z = C2), "4 primary locations but 3")
  expect_error(morie_crkbsg_cokriging(C1, y, z[-1], tg, coords_z = C2), "3 secondary locations but 2")
  expect_error(morie_crkbsg_cokriging(C1, y, z, tg, coords_z = cbind(C2, 0)), "same dimension")
  expect_error(morie_crkbsg_cokriging(C1, y, z, tg, list(sill = 1), coords_z = C2), "unknown cross_variogram key")
  expect_error(morie_crkbsg_cokriging(C1, y, z, tg, list(range = 0), coords_z = C2), "range must be positive")
  expect_error(morie_crkbsg_cokriging(C1, y, z, tg, list(b12 = 2), coords_z = C2), "not positive semidefinite")
  expect_error(morie_crkbsg_cokriging(C1, y, z, tg, list(nugget11 = -1), coords_z = C2), "nugget matrix")
  expect_error(morie_crkbsg_cokriging(C1, y, z, c(1, 2, 3), coords_z = C2), "dimension 2")
  expect_error(morie_crkbsg_cokriging(C1, y, z, tg, list(model = "cubic"), coords_z = C2), "model must be")
})

test_that("morie_frfgrf audits the forest it grows", {
  n <- 60
  X <- cbind(((1:n) * 0.6180339887) %% 1, ((1:n) * 0.4142135624) %% 1)
  y <- 2 * X[, 1] + sin(3 * X[, 2]) + 0.1 * cos(1:n)
  r <- morie_frfgrf(y, X, n_trees = 4, min_leaf = 5, seed = 2)
  gf <- grow_forest(X, y, n_trees = 4, min_leaf = 5, subsample_frac = 0.5, seed = 2)
  walk <- function(nd, acc) {
    if (isTRUE(nd$leaf)) return(acc)
    acc[nd$feature] <- acc[nd$feature] + 1
    walk(nd$right, walk(nd$left, acc))
  }
  cnt <- Reduce(function(a, t) walk(t, a), gf$trees, c(0, 0))
  expect_equal(r$split_counts, cnt)
  expect_equal(r$split_share, cnt / max(sum(cnt), 1), tolerance = 1e-12)
  nI <- function(nd) if (isTRUE(nd$leaf)) nd$n_I else nI(nd$left) + nI(nd$right)
  worst <- function(nd) {
    if (isTRUE(nd$leaf)) return(1)
    a <- nI(nd$left)
    b <- nI(nd$right)
    min(if (a + b > 0) min(a, b) / (a + b) else 1, worst(nd$left), worst(nd$right))
  }
  expect_equal(r$regularity, min(vapply(gf$trees, worst, 0)), tolerance = 1e-12)
  bmin <- 1 - 1 / (1 + (2 / 0.5) * log(1 / 0.05) / log(1 / 0.95))
  expect_equal(r$beta_min, bmin, tolerance = 1e-12)
  expect_equal(r$beta, log(gf$s) / log(n), tolerance = 1e-12)
  expect_equal(r$checks$random_split_floor, min(r$split_share) >= 0.2 * 0.5 / 2)
  expect_equal(r$checks$alpha_regular, r$regularity >= 0.025)
  expect_equal(r$honesty$honest, r$honesty$splits_stable_under_I_permutation &&
                 r$honesty$splits_move_under_J_permutation)
  expect_equal(r$passes, all(unlist(r$checks)))
  expect_equal(r$failed, names(r$checks)[!unlist(r$checks)])
  expect_error(morie_frfgrf(y, X[-1, ]), "59 covariate rows for 60")
  expect_error(morie_frfgrf(y, X[, 0]), "no features")
  expect_error(morie_frfgrf(y[1:30], X[1:30, ]), "at least 40")
  expect_error(morie_frfgrf(y, X, alpha = 0.6, n_trees = 1), "alpha must be in")
})
