# Coverage for farsig.R, flowmm.R, flsh2_native.R, forsnp_native.R,
# frtaxd_native.R and felsen_native.R: the Farrington threshold against
# quasi-Poisson glm fits (hatvalues, predict se.fit), max-flow
# against enumeration of every s-t cut, tiled attention against plain
# softmax attention, the NRC II match probability, gridded Shannon
# diversity and the Felsenstein pruning likelihood against a direct sum
# over ancestral states.

test_that("Farsig recomputes the Farrington threshold", {
  per <- 10
  n <- 34
  y <- round(20 + 6 * sin(2 * pi * (0:(n - 1)) / per) + rep_len(c(3, -2, 4, 0, -1, 5, -3, 2, 1, -4), n))
  y[n] <- 45
  t0 <- n - 1
  idx <- sort(unique(unlist(lapply(1:3, function(j) (t0 - j * per) + (-2:2)))))
  idx <- idx[idx >= 0 & idx < t0]
  yb <- y[idx + 1]
  tt <- idx - t0
  ref <- function(trend, reweight) {
    fitw <- function(om) {
      # IRLS reaches machine precision here and reports "not converged" at epsilon 1e-15 while the
      # coefficients are stable to 1e-12; the reference compares at 1e-9
      ctl <- glm.control(epsilon = 1e-15, maxit = 200)
      f <- suppressWarnings(if (trend) glm(yb ~ tt, family = quasipoisson, weights = om, control = ctl) else
        glm(yb ~ 1, family = quasipoisson, weights = om, control = ctl))
      raw <- sum(om * (yb - fitted(f))^2 / fitted(f)) / f$df.residual
      list(f = f, raw = raw, phi = max(raw, 1))
    }
    ft <- fitw(rep(1, length(yb)))
    if (reweight) {
      mu <- fitted(ft$f)
      s <- 1.5 * (yb^(2 / 3) * mu^(-1 / 6) - sqrt(mu)) / sqrt(ft$phi * (1 - hatvalues(ft$f)))
      gam <- length(yb) / sum(ifelse(s > 1, s^-2, 1))
      ft <- fitw(ifelse(s > 1, gam * s^-2, gam))
    }
    pr <- predict(ft$f, newdata = data.frame(tt = 0), type = "response", se.fit = TRUE,
                  dispersion = ft$raw)
    mu0 <- unname(pr$fit)
    tau <- ft$phi + unname(pr$se.fit)^2 / mu0
    U <- (mu0^(2 / 3) + qnorm(1 - 0.005 / 2) * sqrt(4 / 9 * mu0^(1 / 3) * tau))^1.5
    c(mu0, U, ft$phi)
  }
  r <- Farsig(y, baseline_years = 3, reference_window = 2, period = per)
  e <- ref(TRUE, TRUE)
  expect_equal(c(r$expected, r$threshold, r$phi), e, tolerance = 1e-9)
  expect_equal(r$score, (45 - e[1]) / (e[2] - e[1]), tolerance = 1e-9)
  expect_equal(r$alarm, as.numeric(45 > e[2]))
  expect_equal(r$nbaseline, length(idx))
  r2 <- Farsig(y, 3, 2, per, reweight = FALSE, trend = FALSE)
  expect_equal(c(r2$expected, r2$threshold, r2$phi), ref(FALSE, FALSE), tolerance = 1e-9)
  expect_equal(r2$trend_coef, 0)
  expect_error(Farsig(numeric(0)), "no observations")
  expect_error(Farsig(-y), "non-negative")
  expect_error(Farsig(y, baseline_years = 0), "baseline_years")
  expect_error(Farsig(y, reference_window = -1), "reference_window")
  expect_error(Farsig(y, period = 0), "period")
  expect_error(Farsig(y, alpha = 1), "alpha")
  expect_error(Farsig(y[1:5], 1, 0, 3), "not enough baseline")
})

test_that("Flowmm matches the minimum s-t cut", {
  C <- rbind(c(0, 3, 2, 0, 0), c(0, 0, 1, 3, 0), c(0, 0, 0, 1, 2), c(0, 0, 0, 0, 2),
             c(0, 0, 0, 0, 0))
  subsets <- as.matrix(expand.grid(0:1, 0:1, 0:1))
  cuts <- apply(subsets, 1, function(b) {
    S <- c(1, which(b == 1) + 1)
    sum(C[S, setdiff(1:5, S), drop = FALSE])
  })
  r <- Flowmm(C, 0, 4)
  expect_equal(r$max_flow, min(cuts))
  expect_equal(r$min_cut, min(cuts))
  expect_equal(sum(C[r$source_side + 1, setdiff(1:5, r$source_side + 1)]), min(cuts))
  expect_equal(Flowmm(C, 4, 0)$max_flow, 0)
  expect_error(Flowmm(matrix(0, 0, 0), 0, 1), "no vertices")
  expect_error(Flowmm(C[, 1:4], 0, 1), "square")
  expect_error(Flowmm(-C, 0, 1), "non-negative")
  expect_error(Flowmm(C, 0, 5), "valid vertex")
  expect_error(Flowmm(C, 1, 1), "must differ")
})

test_that("Flsh2 is exact tiled softmax attention", {
  Q <- rbind(c(1, 0.5), c(-0.3, 0.8), c(0.2, 0.2))
  K <- rbind(c(0.4, -1), c(1, 1), c(0, 0.5))
  V <- rbind(c(1, 2), c(0, -1), c(3, 0.5))
  S <- Q %*% t(K) / sqrt(2)
  sm <- function(S) exp(S - apply(S, 1, max)) / rowSums(exp(S - apply(S, 1, max)))
  r <- Flsh2(Q, K, V, block_size = 2)
  expect_equal(r$output, sm(S) %*% V, tolerance = 1e-12)
  Sc <- S
  Sc[upper.tri(Sc)] <- -Inf
  cz <- Flsh2(Q, K, V, block_size = 1, causal = TRUE)
  expect_equal(cz$output, sm(Sc) %*% V, tolerance = 1e-12)
})

test_that("morie_forsnp applies the NRC II 4.10 match probability", {
  gt <- list(c("12", "12"), c("8", "10"))
  fr <- list(list("12" = 0.2, "13" = 0.3), list("8" = 0.1, "10" = 0.25))
  r <- morie_forsnp(gt, fr, theta = 0.03)
  th <- 0.03
  hom <- (2 * th + (1 - th) * 0.2) * (3 * th + (1 - th) * 0.2) / ((1 + th) * (1 + 2 * th))
  het <- 2 * (th + (1 - th) * 0.1) * (th + (1 - th) * 0.25) / ((1 + th) * (1 + 2 * th))
  expect_equal(r$locus_rmp, c(hom, het), tolerance = 1e-12)
  expect_equal(r$lr, 1 / (hom * het), tolerance = 1e-12)
  expect_equal(morie_forsnp(gt, fr)$locus_rmp, c(0.04, 2 * 0.1 * 0.25), tolerance = 1e-12)
  expect_error(morie_forsnp(gt, fr[1]), "paired")
  expect_error(morie_forsnp(gt, fr, theta = 1), "theta")
  expect_error(morie_forsnp(list(c("9", "12")), fr[1]), "frequency missing")
  expect_error(morie_forsnp(list(c("12", "13")), list(list("12" = 0, "13" = 0))), "zero match")
})

test_that("Frtaxd grids Shannon diversity", {
  xy <- rbind(c(0, 0), c(0.1, 0.2), c(0.9, 0.1), c(1, 1), c(0.6, 0.7), c(0.4, 0.9), c(0.2, 0.1))
  sp <- c("a", "b", "a", "c", "c", "a", "a")
  r <- Frtaxd(xy, sp, grid = 2)
  ix <- pmin(floor(xy[, 1] * 2), 1)
  iy <- pmin(floor(xy[, 2] * 2), 1)
  H <- function(v) {
    p <- table(v) / length(v)
    -sum(p * log(p))
  }
  cnt <- matrix(0, 2, 2)
  Hm <- matrix(NaN, 2, 2)
  for (a in 0:1) for (b in 0:1) {
    v <- sp[ix == a & iy == b]
    cnt[a + 1, b + 1] <- length(v)
    if (length(v)) Hm[a + 1, b + 1] <- H(v)
  }
  expect_equal(r$counts, cnt, ignore_attr = TRUE)
  expect_equal(r$H, Hm, tolerance = 1e-12)
  expect_equal(r$H_overall, H(sp), tolerance = 1e-12)
  expect_equal(r$J_overall, H(sp) / log(3), tolerance = 1e-12)
  one <- Frtaxd(rbind(c(1, 1), c(1, 1)), c("x", "x"), grid = 1)
  expect_true(is.nan(one$J_overall) && one$S_overall == 1L)
  expect_same_function(forest_taxon_diversity, Frtaxd)
  expect_error(Frtaxd(xy[, 1, drop = FALSE], sp), "\\(n, 2\\)")
  expect_error(Frtaxd(xy, sp[-1]), "equal length")
  expect_error(Frtaxd(xy, sp, grid = 0), "positive integer")
})

test_that("morie_felsen sums over ancestral states", {
  tree <- list(list("a", 0.2), list(list(list("b", 0.1), list("c", 0.3)), 0.15))
  sites <- list(list(a = "A", b = "A", c = "G"), list(a = "T", b = "C", c = "C"))
  pi <- c(0.3, 0.2, 0.2, 0.3)
  P <- function(t) {
    e <- exp(-t)
    outer(rep(1, 4), pi) * (1 - e) + diag(e, 4)
  }
  lik <- function(s) {
    ia <- match(s$a, c("A", "C", "G", "T"))
    ib <- match(s$b, c("A", "C", "G", "T"))
    ic <- match(s$c, c("A", "C", "G", "T"))
    tot <- 0
    for (r in 1:4) for (m in 1:4) {
      tot <- tot + pi[r] * P(0.2)[r, ia] * P(0.15)[r, m] * P(0.1)[m, ib] * P(0.3)[m, ic]
    }
    tot
  }
  f <- morie_felsen(tree, sites, pi)
  L <- vapply(sites, lik, 0)
  expect_equal(f$site_likelihoods, L, tolerance = 1e-12)
  expect_equal(f$loglik, sum(log(L)), tolerance = 1e-12)
  expect_equal(morie_felsen(list(list("a", 0)), list(list(a = "G")))$loglik, log(0.25))
  expect_error(morie_felsen(tree, sites, pi = rep(0.3, 4)), "sum to 1")
  expect_error(morie_felsen(tree, list()), "at least one site")
})
