# Coverage for PLINK IBD, the ICC family, item-based CF, ideal fourths,
# IDW, iHS, implicit-feedback ALS, the information bottleneck, WAIC/LOO,
# influence functions, Fisher information, Deep InfoMax, Informer
# ProbSparse attention, the A2AJ cache, the CIHI table list and the
# extras installer; recomputed in the test body.

test_that("morie_p_ibs_given_ibd tends to the HWE IBS/IBD table", {
  T <- 2e7
  X <- 0.3 * T
  P <- morie_p_ibs_given_ibd(X, T - X, T)
  p <- 0.3
  q <- 0.7
  expect_equal(P[1, ], c(2 * p^2 * q^2, 4 * p^3 * q + 4 * p * q^3, p^4 + q^4 + 4 * p^2 * q^2), tolerance = 1e-6)
  expect_equal(P[2, ], c(0, 2 * p^2 * q + 2 * p * q^2, p^3 + q^3 + p^2 * q + p * q^2), tolerance = 1e-6)
  expect_equal(P[3, ], c(0, 0, 1))
  s <- morie_p_ibs_given_ibd(4, 6, 10)
  expect_equal(s[1, 1], 2 * 0.16 * 0.36 * (3 / 4) * (5 / 6) * (10 / 9) * (10 / 8) * (10 / 7), tolerance = 1e-12)
  expect_error(morie_p_ibs_given_ibd(1, 1, 2), "T = X \\+ Y >= 4")
})

test_that("morie_ibdmtx solves the PLINK method-of-moments system per pair", {
  set.seed(3)
  G <- matrix(sample(0:2, 6 * 40, TRUE, prob = c(0.3, 0.45, 0.25)), 6)
  G[2, ] <- G[1, ]
  r <- morie_ibdmtx(G)
  tabs <- lapply(1:40, function(j) {
    Y <- sum(G[, j])
    X <- 12 - Y
    if (X == 0 || Y == 0) NULL else morie_p_ibs_given_ibd(X, Y, 12)
  })
  pair <- function(i, k) {
    ibs <- 2 - abs(G[i, ] - G[k, ])
    ok <- !vapply(tabs, is.null, TRUE)
    No <- tabulate(ibs[ok] + 1, 3)
    Ne <- Reduce(`+`, tabs[ok])
    z0 <- No[1] / Ne[1, 1]
    z1 <- (No[2] - z0 * Ne[1, 2]) / Ne[2, 2]
    z2 <- (No[3] - z0 * Ne[1, 3] - z1 * Ne[2, 3]) / Ne[3, 3]
    c(z0, z1, z2)
  }
  # PLINK's clamping of the moment solution to the probability simplex
  clamp <- function(z) {
    if (z[1] > 1) z <- c(1, 0, 0) else if (z[1] < 0) {
      z[1] <- 0
      if (z[2] + z[3] > 0) z[2:3] <- z[2:3] / sum(z[2:3])
    }
    if (z[2] < 0) {
      z[2] <- 0
      if (z[1] + z[3] > 0) z[c(1, 3)] <- z[c(1, 3)] / sum(z[c(1, 3)])
    }
    if (z[3] < 0) {
      z[3] <- 0
      if (z[1] + z[2] > 0) z[1:2] <- z[1:2] / sum(z[1:2])
    }
    pih <- 0.5 * z[2] + z[3]
    if (pih^2 <= z[3]) z <- c((1 - pih)^2, 2 * pih * (1 - pih), pih^2)
    c(z, pih)
  }
  for (pr in list(c(3, 5), c(1, 4), c(2, 6))) {
    z <- clamp(pair(pr[1], pr[2]))
    expect_equal(c(r$Z0[pr[1], pr[2]], r$Z1[pr[1], pr[2]], r$Z2[pr[1], pr[2]], r$estimate[pr[1], pr[2]]), z,
                 tolerance = 1e-12)
  }
  expect_equal(r$estimate[1, 2], 1, tolerance = 1e-12)
  expect_equal(r$Z2[1, 2], 1, tolerance = 1e-12)
  expect_equal(r$estimate, t(r$estimate))
  expect_equal(r$n_snps_used, sum(!vapply(tabs, is.null, TRUE)))
  expect_error(morie_ibdmtx(G[1, , drop = FALSE]), "at least 2")
})

test_that("the ICC functions match the Shrout-Fleiss mean squares", {
  X <- matrix(c(9, 2, 5, 8,
                6, 1, 3, 2,
                8, 4, 6, 8,
                7, 1, 2, 6,
                10, 5, 6, 9,
                6, 2, 4, 7), 6, byrow = TRUE)
  n <- 6
  k <- 4
  gm <- mean(X)
  bms <- k * sum((rowMeans(X) - gm)^2) / (n - 1)
  jms <- n * sum((colMeans(X) - gm)^2) / (k - 1)
  ems <- sum((X - outer(rowMeans(X), colMeans(X), "+") + gm)^2) / ((n - 1) * (k - 1))
  wms <- sum((X - rowMeans(X))^2) / (n * (k - 1))
  icc21 <- (bms - ems) / (bms + (k - 1) * ems + k * (jms - ems) / n)
  icc31 <- (bms - ems) / (bms + (k - 1) * ems)
  y <- as.vector(t(X))
  subj <- rep(1:6, each = 4)
  rat <- rep(1:4, 6)
  o <- c(3, 1, 4, 2, 5, 6, 8, 7, 12, 9, 10, 11, 16, 13, 14, 15, 17, 20, 18, 19, 22, 21, 24, 23)
  expect_equal(Icc1(y, subj)$estimate, (bms - wms) / (bms + (k - 1) * wms), tolerance = 1e-12)
  expect_equal(Icc2(y[o], subj[o], rat[o])$estimate, icc21, tolerance = 1e-12)
  expect_equal(Icc3(y[o], subj[o], rat[o])$estimate, icc31, tolerance = 1e-12)
  expect_equal(Icc2(y, subj, rat)$jms, jms, tolerance = 1e-12)
  expect_equal(IccA(y[o], subj[o], rat[o])$estimate, icc21, tolerance = 1e-12)
  expect_equal(IccC(y, subj, rat)$estimate, icc31, tolerance = 1e-12)
  r <- Icc12c(X, "2,k")
  expect_equal(r$icc2k, (bms - ems) / (bms + (jms - ems) / n), tolerance = 1e-12)
  expect_equal(r$icc1k, (bms - wms) / bms, tolerance = 1e-12)
  expect_equal(r$estimate, r$icc2k)
  expect_equal(Icc12c(X, "ICC(3,4)")$model, "3k")
  expect_equal(Icc12c(X, "3,1")$estimate, icc31, tolerance = 1e-12)
  skip_if_not_installed("psych")
  ps <- suppressMessages(psych::ICC(X, lmer = FALSE))$results$ICC
  expect_equal(c(r$icc11, r$icc21, r$icc31, r$icc1k, r$icc2k, r$icc3k), ps, tolerance = 1e-9)
  expect_error(Icc12c(X, "4,1"), "six Shrout-Fleiss")
  expect_error(Icc1(y[-1], subj[-1]), "balanced")
  expect_error(Icc2(y, subj, rat[-1]), "one entry per rating")
  expect_error(Icc3(y, subj, rep(1:2, 12)), "number of raters")
})

test_that("Itemcf predicts with adjusted-cosine neighbours", {
  R <- rbind(c(5, 3, NA, 1),
             c(4, NA, 4, 1),
             c(1, 1, 5, 5),
             c(2, 1, 4, NA),
             c(NA, 5, 1, 2))
  um <- rowMeans(R, na.rm = TRUE)
  sim <- function(p, q) {
    ok <- !is.na(R[, p]) & !is.na(R[, q])
    a <- R[ok, p] - um[ok]
    b <- R[ok, q] - um[ok]
    sum(a * b) / sqrt(sum(a^2) * sum(b^2))
  }
  r <- Itemcf(R, u = 0, i = 2, k_nn = 2)
  cand <- c(1, 2, 4)
  s <- vapply(cand, function(j) sim(3, j), 0)
  take <- order(-abs(s), cand)[1:2]
  expect_equal(r$prediction, sum(s[take] * R[1, cand[take]]) / sum(abs(s[take])), tolerance = 1e-12)
  expect_equal(r$neighbours, cand[take] - 1L)
  c0 <- Itemcf(R, u = 0, i = 2, k_nn = 3, similarity = "cosine")
  cs <- vapply(cand, function(j) {
    ok <- !is.na(R[, 3]) & !is.na(R[, j])
    sum(R[ok, 3] * R[ok, j]) / sqrt(sum(R[ok, 3]^2) * sum(R[ok, j]^2))
  }, 0)
  expect_equal(c0$prediction, sum(cs * R[1, cand]) / sum(abs(cs)), tolerance = 1e-12)
})

test_that("Idealf, Idw and IdwPowerSearch", {
  x <- c(3.1, 1.2, 5.5, 2.2, 4.8, 3.9, 0.7, 6.1, 2.9)
  r <- Idealf(x)
  s <- sort(x)
  g <- 9 / 4 + 5 / 12
  j <- floor(g)
  h <- g - j
  expect_equal(r$q1, (1 - h) * s[j] + h * s[j + 1], tolerance = 1e-12)
  expect_equal(r$q2, (1 - h) * s[9 - j + 1] + h * s[9 - j], tolerance = 1e-12)
  expect_error(Idealf(1:2), "at least 3")

  P <- rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 1))
  z <- c(1, 2, 3, 4)
  S <- rbind(c(0.5, 0.5), c(1, 0))
  d <- sqrt(colSums((t(P) - S[1, ])^2))
  w <- 1 / d^1.5
  id <- Idw(P, z, S, power = 1.5)
  expect_equal(id$pred, c(sum(w * z) / sum(w), 2), tolerance = 1e-12)
  expect_equal(id$ess[1], sum(w)^2 / sum(w^2), tolerance = 1e-12)
  expect_equal(id$exact, c(FALSE, TRUE))

  set.seed(4)
  C <- cbind(runif(10), runif(10))
  zz <- rnorm(10)
  ps <- c(1, 2, 3)
  cv <- vapply(ps, function(p) {
    pr <- vapply(1:10, function(i) {
      dd <- sqrt(colSums((t(C[-i, ]) - C[i, ])^2))
      sum(dd^-p * zz[-i]) / sum(dd^-p)
    }, 0)
    sqrt(mean((zz - pr)^2))
  }, 0)
  ip <- IdwPowerSearch(zz, C, powers = ps)
  expect_equal(ip$rmse, cv, tolerance = 1e-12)
  expect_equal(ip$best, ps[which.min(cv)])
})

test_that("morie_ihstst integrates the allele-wise EHH curves", {
  H <- rbind(c(0, 1, 0, 1, 1),
             c(0, 1, 0, 1, 0),
             c(1, 1, 0, 1, 1),
             c(1, 0, 0, 0, 1),
             c(0, 0, 1, 1, 0),
             c(1, 1, 1, 0, 0),
             c(1, 1, 0, 1, 1),
             c(0, 1, 1, 1, 1))
  pos <- c(0, 1.5, 3, 4, 7)
  ehh <- function(car) vapply(1:5, function(j) {
    lo <- min(j, 3)
    hi <- max(j, 3)
    keys <- apply(H[car, lo:hi, drop = FALSE], 1, paste, collapse = "")
    tb <- table(keys)
    sum(tb * (tb - 1)) / (length(car) * (length(car) - 1))
  }, 0)
  e0 <- ehh(which(H[, 3] == 0))
  e1 <- ehh(which(H[, 3] == 1))
  ihh <- function(e) {
    a <- 0
    for (side in list(4:5, 2:1)) {
      pp <- pos[3]
      pe <- e[3]
      for (j in side) {
        a <- a + abs(pos[j] - pp) * 0.5 * (e[j] + pe)
        pp <- pos[j]
        pe <- e[j]
        if (e[j] < 0.05) break
      }
    }
    a
  }
  r <- morie_ihstst(H, core = 2, positions = pos)
  expect_equal(c(r$ihh_a, r$ihh_d), c(ihh(e0), ihh(e1)), tolerance = 1e-12)
  expect_equal(r$ihs_unstandardized, log(ihh(e0) / ihh(e1)), tolerance = 1e-12)
  expect_equal(r$daf, 3 / 8)
  st <- morie_ihstst(H, core = 2, positions = pos, standardize = c(0.1, 2))
  expect_equal(st$estimate, (r$ihs_unstandardized - 0.1) / 2, tolerance = 1e-12)
  expect_error(morie_ihstst(H, 2, pos, standardize = c(0, 0)), "sd must be positive")
})

test_that("morie_impFB alternates exact weighted ridge solves", {
  R <- rbind(c(3, 0, 1, 0), c(0, 2, 0, 5), c(1, 1, 0, 0))
  r <- morie_impFB(R, f = 2, alpha = 10, lam = 0.5, iters = 1, seed = 4)
  e <- .ghc_rng(4)
  X <- matrix(.ghc_unif(e, 6) * 0.1, 3, 2, byrow = TRUE)
  Y <- matrix(.ghc_unif(e, 8) * 0.1, 4, 2, byrow = TRUE)
  P <- (R > 0) * 1
  C <- 1 + 10 * R
  for (u in 1:3) X[u, ] <- solve(t(Y) %*% (C[u, ] * Y) + diag(0.5, 2), t(Y) %*% (C[u, ] * P[u, ]))
  for (i in 1:4) Y[i, ] <- solve(t(X) %*% (C[, i] * X) + diag(0.5, 2), t(X) %*% (C[, i] * P[, i]))
  expect_equal(r$X, X, tolerance = 1e-12)
  expect_equal(r$Y, Y, tolerance = 1e-12)
  expect_equal(r$final_cost, sum(C * (P - X %*% t(Y))^2) + 0.5 * (sum(X^2) + sum(Y^2)), tolerance = 1e-12)
  s <- morie_impFB(R, f = 2, alpha = 10, lam = 0.5, iters = 1, seed = 4, fast = FALSE)
  expect_equal(s$Y, Y, tolerance = 1e-12)
  expect_error(morie_impFB(-R), "negative")
  expect_error(morie_impFB(R, f = 0), "at least 1")
})

test_that("Infobtl converges to the information-bottleneck fixed point", {
  J <- matrix(c(0.2, 0.05, 0.05, 0.1, 0.25, 0.05, 0.05, 0.05, 0.2), 3, byrow = TRUE)
  r <- Infobtl(pxy = J, beta = 3, T = 2, iters = 2000, tol = 1e-15)
  px <- rowSums(J)
  pyx <- J / px
  Q <- r$p_t_x
  pt <- colSums(px * Q)
  pyt <- t(Q * px) %*% pyx / pt
  expect_equal(r$p_t, pt, tolerance = 1e-10)
  kl <- sapply(1:2, function(t) rowSums(pyx * log(pyx / matrix(pyt[t, ], 3, 3, byrow = TRUE))))
  Qn <- t(apply(log(matrix(pt, 3, 2, byrow = TRUE)) - 3 * kl, 1, function(v) exp(v - max(v)) / sum(exp(v - max(v)))))
  expect_equal(Q, Qn, tolerance = 1e-9)
  ixt <- sum(px * Q * log(Q / matrix(pt, 3, 2, byrow = TRUE)))
  py <- colSums(J)
  ity <- sum(pt * pyt * log(pyt / matrix(py, 2, 3, byrow = TRUE)))
  expect_equal(r$ixt, ixt, tolerance = 1e-9)
  expect_equal(r$ity, ity, tolerance = 1e-9)
  expect_equal(r$lagrangian, ixt - 3 * ity, tolerance = 1e-9)
  d <- Infobtl(c("a", "b", "a", "c"), c(1, 2, 1, 1), beta = 2, T = 2)
  expect_equal(sum(d$p_t), 1, tolerance = 1e-12)
})

test_that("Infcrt gives WAIC (checked against loo) and the raw IS-LOO for small S", {
  set.seed(5)
  L <- matrix(rnorm(20 * 6, -1, 0.3), 20)
  r <- Infcrt(L)
  lppd <- sum(log(colMeans(exp(L))))
  pw <- sum(apply(L, 2, var))
  expect_equal(r$estimate, -2 * (lppd - pw), tolerance = 1e-12)
  elpd <- sum(log(20 / colSums(exp(-L))))
  expect_equal(r$elpd_loo, elpd, tolerance = 1e-12)
  expect_equal(r$p_loo, lppd - elpd, tolerance = 1e-12)
  expect_true(is.nan(r$k_max))
  skip_if_not_installed("loo")
  w <- suppressWarnings(loo::waic(L))
  expect_equal(r$estimate, unname(w$estimates["waic", "Estimate"]), tolerance = 1e-9)
})

test_that("Infcrv extrapolates the Gateaux derivative; Infgnt is the Fisher metric", {
  v <- c(1.5, 2.2, 0.4, 3.3, 2.8)
  expect_equal(Infcrv("mean", v, 4)$estimate, 4 - mean(v), tolerance = 1e-12)
  m <- mean(v)
  s2 <- mean((v - m)^2)
  expect_equal(Infcrv("var", v, 4)$estimate, (4 - m)^2 - s2, tolerance = 1e-9)
  expect_equal(Infcrv(function(v, w) sum(w * v^2) / sum(w), v, 1)$estimate, 1 - mean(v^2), tolerance = 1e-9)
  expect_true(is.finite(Infcrv("median", v, 10)$estimate))
  expect_error(Infcrv("mode", v, 1), "estimator must be")
  expect_error(Infcrv("mean", v, 1:2), "single point")

  lp <- function(x, th) x * log(th) + (1 - x) * log(1 - th)
  g <- Infgnt(lp, 0.3, c(0, 1))
  # central differences at h = 1e-5 carry an O(h^2) error
  expect_equal(g$estimate, 1 / (0.3 * 0.7), tolerance = 1e-8)
  expect_equal(g$total_mass, 1, tolerance = 1e-12)
  lpn <- function(x, th) dnorm(x, th[1], exp(th[2]), log = TRUE)
  gn <- Infgnt(lpn, c(0.5, log(2)), seq(-15, 16, by = 0.01), discrete = FALSE)
  expect_equal(gn$metric, diag(c(1 / 4, 2)), tolerance = 1e-6)
  expect_error(Infgnt(lp, 0.3, 1), "two points")
  expect_error(Infgnt(1, 0.3, c(0, 1)), "callable")
})

test_that("morie_infmax is the local Deep InfoMax JSD / DV objective", {
  G <- list(c(1, 0), c(0, 1), c(1, 1))
  Fm <- list(list(c(1, 0.5), c(0.2, 0)), list(c(0, 1), c(0.3, 0.9)), list(c(1, 1), c(0.5, 0.5)))
  cr <- function(g, f) sum(g * f)
  r <- morie_infmax(G, Fm, cr)
  J <- M <- numeric(0)
  for (i in 1:3) for (l in 1:2) {
    J <- c(J, cr(G[[i]], Fm[[i]][[l]]))
    for (j in setdiff(1:3, i)) M <- c(M, cr(G[[i]], Fm[[j]][[l]]))
  }
  sp <- function(z) log1p(exp(z))
  expect_equal(r$estimate, mean(-sp(-J)) - mean(sp(M)), tolerance = 1e-12)
  expect_equal(c(r$n_positive, r$n_negative), c(6L, 12L))
  d <- morie_infmax(G, Fm, cr, estimator = "dv")
  expect_equal(d$estimate, mean(J) - log(mean(exp(M))), tolerance = 1e-12)
  expect_error(morie_infmax(G[1], Fm[1], cr), "at least 2")
  expect_error(morie_infmax(G, list(Fm[[1]], Fm[[2]], Fm[[3]][1]), cr), "differing numbers")
})

test_that("morie_infmer keeps full attention for the top-u queries and the mean for the rest", {
  set.seed(6)
  q <- matrix(rnorm(24), 6)
  k <- matrix(rnorm(20), 5)
  v <- matrix(rnorm(15), 5)
  r <- morie_infmer(q, k, v, c = 1)
  S <- q %*% t(k) / 2
  msc <- apply(S, 1, max) - rowMeans(S)
  u <- floor(log(6))
  sel <- sort(order(msc, decreasing = TRUE)[1:u])
  A <- exp(S - apply(S, 1, max))
  A <- A / rowSums(A)
  out <- matrix(colMeans(v), 6, 3, byrow = TRUE)
  out[sel, ] <- (A %*% v)[sel, ]
  expect_equal(r$sparsity_scores, msc, tolerance = 1e-12)
  expect_equal(r$selected_queries, sel)
  expect_equal(r$output, out, tolerance = 1e-12)
  expect_equal(r$complexity$probsparse_flops, 6 * log(5) + log(6) * 5, tolerance = 1e-12)
  expect_match(morie_infmer_cheatsheet(), "ProbSparse")
  expect_error(morie_infmer(q, k[, 1:3], v), "feature dimension")
})

test_that("the A2AJ loader reads a cached parquet, the CIHI list and ensure_extras", {
  dir <- tempfile("a2aj")
  dir.create(file.path(dir, "cases"), recursive = TRUE)
  df <- data.frame(citation = c("2020 SCC 1", "2021 SCC 2"), year = c(2020L, 2021L),
                   stringsAsFactors = FALSE)
  dest <- file.path(dir, "cases", "SCC.parquet")
  morie_write_parquet(df, dest)
  expect_equal(morie_ingest_a2aj_download("SCC", cache_dir = dir), dest)
  back <- morie_ingest_a2aj_load("SCC", engine = "native", cache_dir = dir)
  expect_equal(back$citation, df$citation)
  expect_equal(as.integer(back$year), df$year)
  one <- morie_ingest_a2aj_load("SCC", columns = "year", engine = "native", cache_dir = dir)
  expect_equal(names(one), "year")
  expect_error(morie_ingest_a2aj_download("scc", cache_dir = dir), "upper-case")
  unlink(dir, recursive = TRUE)

  tb <- morie_datasets_cihi_data_tables()
  expect_true(all(c("title", "url") %in% names(tb)))
  expect_gt(nrow(tb), 0)

  expect_true(morie_ensure_extras("stats"))
  expect_error(morie_ensure_extras("surelyNotAPackageZz9", ask = FALSE), "morie_install_extras")
  expect_error(morie_ensure_extras(1), "is.character")
})
