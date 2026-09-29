# Coverage for dsp_waveform.R (complex demodulation, minimum phase,
# Parzen density), farmlmm_native.R, flow_an_native.R and fmFM_native.R:
# demodulation of a known carrier, the minimum-phase magnitude and
# energy-delay properties, the FarmCPU loop against lm fits, RealNVP
# log-densities against a numerical Jacobian, and factorization machines
# against the pairwise double sum and a replayed SGD.

test_that("morie_dsp_complex_demodulation recovers amplitude and phase", {
  t <- 0:399
  x <- 2 * cos(2 * pi * 0.1 * t + 0.5)
  r <- morie_dsp_complex_demodulation(x, fc = 0.1)
  mid <- 60:340
  # the 2 fc image is attenuated by the 8th-order zero-phase low-pass
  expect_equal(r$envelope[mid], rep(2, length(mid)), tolerance = 1e-3)
  expect_equal(((r$phase[mid] + pi) %% (2 * pi)) - pi, rep(0.5, length(mid)), tolerance = 1e-3)
  am <- (1 + 0.5 * cos(2 * pi * 0.004 * t)) * cos(2 * pi * 0.1 * t)
  ra <- morie_dsp_complex_demodulation(am, fc = 0.1)
  expect_equal(ra$envelope[mid], (1 + 0.5 * cos(2 * pi * 0.004 * t))[mid], tolerance = 1e-2)
})

test_that("morie_dsp_min_phase keeps the magnitude and front-loads energy", {
  for (x in list(c(0.2, 1, -0.5, 0.8, 0.3, -0.1, 0.4), c(0.2, 1, -0.5, 0.8, 0.3, -0.1, 0.4, 0.6),
                 c(0.3, 1, 0.2))) {
    m <- morie_dsp_min_phase(x)
    expect_equal(Mod(fft(m)), Mod(fft(x)) + 1e-10, tolerance = 1e-9)
    expect_true(all(cumsum(m^2) >= cumsum(x^2) - 1e-9))
  }
})

test_that("morie_dsp_parzen_pdf is a Gaussian kernel density on a grid", {
  x <- c(0.3, 1.2, -0.5, 2.2, 0.9, 0.9, -1.1)
  bw <- 1.06 * sd(x) * 7^(-0.2)
  r <- morie_dsp_parzen_pdf(x, n_points = 50)
  g <- seq(min(x) - 3 * bw, max(x) + 3 * bw, length.out = 50)
  expect_equal(r$grid, g, tolerance = 1e-12)
  expect_equal(r$density, colMeans(outer(x, g, function(a, b) dnorm(b, a, bw))), tolerance = 1e-12)
  expect_equal(morie_dsp_parzen_pdf(x, bandwidth = -1, n_points = 5)$grid,
               seq(min(x) - 0.3, max(x) + 0.3, length.out = 5), tolerance = 1e-12)
})

test_that("morie_farmlmm iterates the fixed-effect scan to a stable set", {
  n <- 40
  G <- outer(1:n, 1:6, function(i, j) floor(3 * ((i * 0.6180339887 * sqrt(j + 1) + j / 7) %% 1)))
  y <- 1 + 1.5 * G[, 2] - 0.8 * G[, 5] + sin(1:n)
  scan <- function(sel) {
    t(vapply(1:6, function(j) {
      cols <- c(j, setdiff(sel + 1, j))
      X <- G[, cols, drop = FALSE]
      f <- lm(y ~ X)
      dof <- max(n - length(cols) - 1, 1)
      s2 <- sum(resid(f)^2) / dof
      se <- sqrt(s2 / sum((X[, 1] - mean(X[, 1]))^2))
      b <- unname(coef(f)[2])
      c(b, 2 * pnorm(-abs(b / se)))
    }, c(0, 0)))
  }
  thr <- 0.05 / 6
  sel <- integer(0)
  hist <- list()
  conv <- FALSE
  repeat {
    s <- scan(sel)
    new <- sort(which(s[, 2] < thr) - 1L)
    hist[[length(hist) + 1]] <- new
    if (identical(new, sel)) {
      conv <- TRUE
      break
    }
    if (length(hist) >= 3 && identical(new, hist[[length(hist) - 2]])) break
    sel <- new
    if (length(hist) == 10) break
  }
  r <- morie_farmlmm(y, G)
  # the kernel adds a 1e-8 ridge to X'X
  expect_equal(r$p, s[, 2], tolerance = 1e-6)
  expect_equal(r$selected, sel)
  expect_equal(r$history, hist)
  expect_equal(r$converged, conv)
  expect_true(all(c(1L, 4L) %in% r$selected))
  one <- morie_farmlmm(y, G, max_iter = 1, threshold = 0.5)
  expect_equal(one$iterations, 1L)
  expect_error(morie_farmlmm(y[-1], G), "39 phenotypes but 40 genotypes")
})

test_that("morie_flow_an scores by the RealNVP negative log-likelihood", {
  L1 <- list(c(1, 0, 1), rbind(c(0.2, 0, 0.1), c(0.3, -0.2, 0.4), c(0, 0.5, 0.1)), c(0.1, -0.1, 0),
             rbind(c(0.5, 0.1, 0), c(-0.3, 0.2, 0.2), c(0.1, 0, 0.3)), c(0, 0.2, -0.1))
  L2 <- list(c(0, 1, 0), diag(0.3, 3), c(0, 0.1, 0.2), matrix(0.1, 3, 3), c(0.05, 0, 0))
  layers <- list(L1, L2)
  fwd <- function(x) {
    for (l in layers) {
      m <- l[[1]]
      s <- 5 * tanh(as.numeric(l[[2]] %*% (x * m) + l[[3]])) * (1 - m)
      tt <- as.numeric(l[[4]] %*% (x * m) + l[[5]]) * (1 - m)
      x <- x * m + (1 - m) * (x * exp(s) + tt)
    }
    x
  }
  X <- rbind(c(0.5, -1, 0.2), c(1.5, 0.3, -0.7), c(-0.2, 0.1, 0.9), c(3, -2, 1), c(0, 0, 0))
  nll <- apply(X, 1, function(x) {
    J <- sapply(1:3, function(k) {
      h <- 1e-6
      e <- replace(numeric(3), k, h)
      (fwd(x + e) - fwd(x - e)) / (2 * h)
    })
    -(sum(dnorm(fwd(x), log = TRUE)) + log(abs(det(J))))
  })
  r <- morie_flow_an(X, layers, threshold_quantile = 0.6)
  # central-difference Jacobian with h = 1e-6
  expect_equal(r$score, nll, tolerance = 1e-8)
  expect_equal(r$threshold, unname(quantile(r$score, 0.6, type = 7)), tolerance = 1e-12)
  expect_equal(r$flag, as.numeric(r$score > r$threshold))
  ref <- morie_flow_an(X[1:2, ], layers, 0.5, reference = X)
  expect_equal(ref$threshold, unname(quantile(r$score, 0.5, type = 7)), tolerance = 1e-12)
  expect_false(ref$self_referenced)
  expect_error(morie_flow_an(X, layers, threshold_quantile = 1), "threshold_quantile")
})

test_that("factorization machines match the pairwise double sum", {
  x <- c(1, 0, 2, 0.5)
  w0 <- 0.3
  w <- c(0.1, -0.2, 0.4, 0.05)
  Vm <- rbind(c(0.2, -0.1), c(0.5, 0.3), c(-0.4, 0.2), c(0.1, 0.1))
  G <- Vm %*% t(Vm)
  ref <- w0 + sum(w * x) + sum((G * outer(x, x))[upper.tri(G)])
  expect_equal(predict_naive(x, w0, w, lapply(1:4, function(i) Vm[i, ])), ref, tolerance = 1e-12)
  expect_equal(predict_naive(x, w0, w, Vm), ref, tolerance = 1e-12)
  expect_equal(design_mf(1, 2, 3, 4), c(0, 1, 0, 0, 0, 1, 0))

  X <- rbind(design_mf(0, 0, 2, 2), design_mf(1, 1, 2, 2), design_mf(0, 1, 2, 2))
  y <- c(1, 2, 0.5)
  fit <- fit_fm(X, y, k_dim = 2, iters = 3, alpha = 0.1, lam = 0.01, seed = 4)
  V <- withr::with_seed(4, replicate(2, runif(4) - 0.5)) * 0.1
  b0 <- 0
  b <- numeric(4)
  pr <- function(x) b0 + sum(b * x) + 0.5 * sum((x %*% V)^2 - (x^2) %*% (V^2))
  mse <- numeric(3)
  for (it in 1:3) {
    for (r in 1:3) {
      xr <- X[r, ]
      e <- pr(xr) - y[r]
      b0 <- b0 - 0.1 * e
      for (i in which(xr != 0)) {
        b[i] <- b[i] - 0.1 * (e * xr[i] + 0.01 * b[i])
        for (f in 1:2) {
          g <- xr[i] * sum(V[, f] * xr) - V[i, f] * xr[i]^2
          V[i, f] <- V[i, f] - 0.1 * (e * g + 0.01 * V[i, f])
        }
      }
    }
    mse[it] <- mean((apply(X, 1, pr) - y)^2)
  }
  expect_equal(fit$V, V, tolerance = 1e-12)
  expect_equal(c(fit$w0, fit$w), c(b0, b), tolerance = 1e-12)
  expect_equal(fit$mse_history, mse, tolerance = 1e-12)
  expect_error(fit_fm(X, y[-1]), "3 rows but 2 targets")
  expect_error(fit_fm(X[0, ], numeric(0)), "no data")
  expect_error(fit_fm(X, y, k_dim = 0), "at least 1")
})
