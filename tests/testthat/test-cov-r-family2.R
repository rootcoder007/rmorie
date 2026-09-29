# Coverage for rdfzzy .. reffec exports. Every expectation is recomputed in
# the test body.

cov_rd_side <- function(r, y, h, side) {
  w <- pmax(0, 1 - abs(r) / h)
  k <- if (side == "right") r >= 0 & r <= h else r >= -h & r < 0
  # zero-weight boundary points are dropped: sandwich counts them in the
  # meat but not in the bread
  f <- stats::lm(y ~ r, weights = w, subset = k & w > 0)
  V <- sandwich::vcovHC(f, type = "HC0")
  list(a = unname(stats::coef(f)[1]), b = unname(stats::coef(f)[2]), va = V[1, 1], vb = V[2, 2])
}

test_that("Rdksrn, Rdfzzy and Rdkkin are triangular local-linear RD estimators", {
  skip_if_not_installed("sandwich")
  x <- seq(-1, 1, length.out = 31)
  e <- sin(5 * seq_along(x)) * 0.15
  D <- as.numeric(x >= 0.05 | x < -0.7)
  y <- 0.5 + 0.9 * x + ifelse(x >= 0, 1.2 + 0.6 * x, 0) + e
  R <- cov_rd_side(x, y, 0.8, "right")
  L <- cov_rd_side(x, y, 0.8, "left")
  s <- Rdksrn(y, x, bandwidth = 0.8)
  expect_equal(s$tau, R$a - L$a, tolerance = 1e-10)
  expect_equal(s$se, sqrt(R$va + L$va), tolerance = 1e-10)
  expect_equal(c(s$slope_right, s$slope_left), c(R$b, L$b), tolerance = 1e-10)
  dR <- cov_rd_side(x, D, 0.8, "right")
  dL <- cov_rd_side(x, D, 0.8, "left")
  f <- Rdfzzy(y, x, D, bandwidth = 0.8)
  num <- R$a - L$a
  den <- dR$a - dL$a
  expect_equal(f$tau, num / den, tolerance = 1e-10)
  expect_equal(f$se, sqrt((R$va + L$va) / den^2 + num^2 * (dR$va + dL$va) / den^4), tolerance = 1e-10)
  k <- Rdkkin(y, x, bandwidth = 0.8)
  expect_equal(k$tau, R$b - L$b, tolerance = 1e-10)
  expect_equal(k$se, sqrt(R$vb + L$vb), tolerance = 1e-10)
  kd <- Rdkkin(y, x, D = x * (x >= 0) + 0.2 * x, bandwidth = 0.8)
  expect_equal(kd$first_stage, 1, tolerance = 1e-10)
  expect_equal(kd$tau, R$b - L$b, tolerance = 1e-10)
  expect_error(Rdksrn(y, x, bandwidth = 0), "bandwidth must be positive")
  expect_error(Rdksrn(y, x, bandwidth = 0.01), "at least two points")
  expect_error(Rdfzzy(y, x, rep(1, 31), bandwidth = 0.8), "indistinguishable from zero")
  expect_error(Rdkkin(y, x, D = rep(1, 31), bandwidth = 0.8), "no kink")
  expect_error(Rdfzzy(y, x, D[-1]), "one entry per observation")
})

test_that("RDKit path fingerprints enumerate every connected bond subset", {
  count_sub <- function(A, maxpath, branched) {
    ij <- which(upper.tri(A) & A != 0, arr.ind = TRUE)
    nb <- nrow(ij)
    tot <- 0
    for (m in 1:min(maxpath, nb)) {
      for (S in utils::combn(nb, m, simplify = FALSE)) {
        e <- ij[S, , drop = FALSE]
        at <- unique(as.vector(e))
        comp <- stats::setNames(seq_along(at), at)
        for (r in seq_len(nrow(e))) {
          a <- comp[as.character(e[r, 1])]
          b <- comp[as.character(e[r, 2])]
          comp[comp == b] <- a
        }
        if (length(unique(comp)) != 1) next
        if (!branched && (any(tabulate(as.vector(e)) > 2) || m != length(at) - 1)) next
        tot <- tot + 1
      }
    }
    tot
  }
  ring <- matrix(0, 6, 6)
  for (i in 1:6) ring[i, i %% 6 + 1] <- ring[i %% 6 + 1, i] <- 1
  arom <- rep(1, 6)
  r <- morie_rdkfp(ring, rep(6, 6), aromatic = arom)
  r2 <- Rdkfp(ring, rep(6, 6), aromatic = arom)
  expect_equal(r$nsubgraph, count_sub(ring, 7, TRUE))
  # by symmetry every arc of a given length hashes alike: one feature per size
  expect_equal(r$nfeature, 6L)
  expect_equal(r2$features, r$features)
  expect_equal(morie_rdkfp(ring, rep(6, 6), branched = FALSE)$nsubgraph, count_sub(ring, 7, FALSE))
  # 2-methylbutane with a double bond: C1-C2(=C5)-C3-C4
  M <- matrix(0, 5, 5)
  M[1, 2] <- M[2, 1] <- 1
  M[2, 3] <- M[3, 2] <- 1
  M[3, 4] <- M[4, 3] <- 1
  M[2, 5] <- M[5, 2] <- 2
  for (br in c(TRUE, FALSE)) for (mp in c(2, 4)) {
    a <- morie_rdkfp(M, c(6, 6, 6, 6, 6), maxpath = mp, branched = br)
    b <- Rdkfp(M, c(6, 6, 6, 6, 6), maxpath = mp, branched = br)
    expect_equal(a$nsubgraph, count_sub(M, mp, br))
    expect_equal(b$features, a$features)
    expect_equal(sum(a$count), a$nsubgraph)
    expect_equal(which(a$bits == 1) - 1, sort(unique(a$features %% 2048)))
  }
  # ignoring bond order merges the double bond with the single ones
  expect_lte(morie_rdkfp(M, rep(6, 5), use_bond_order = FALSE)$nfeature, morie_rdkfp(M, rep(6, 5))$nfeature)
  expect_error(morie_rdkfp(M, rep(6, 4)), "one entry per atom")
  expect_error(Rdkfp(M, rep(6, 5), minpath = 0), "at least 1")
  expect_error(Rdkfp(M, rep(6, 5), minpath = 3, maxpath = 2), "at least minpath")
  expect_error(morie_rdkfp(M, rep(6, 5), nbits = 0), "positive")
})

test_that("Renyi DP of Gaussian and sampled Gaussian mechanisms", {
  expect_equal(morie_rdpc(4, 2, 1.5)$epsilon_rdp, 4 * 1.5^2 / 8, tolerance = 1e-12)
  expect_equal(Rdpc(4, 2, 1.5)$estimate, 4 * 1.5^2 / 8, tolerance = 1e-12)
  expect_error(Rdpc(1, 2), "alpha must exceed 1")
  expect_error(morie_rdpc(2, 0), "sigma must be positive")
  expect_error(morie_rdpc(2, 1, 0), "sensitivity must be positive")
  A <- function(a, q, s) sum(choose(a, 0:a) * (1 - q)^(a - 0:a) * q^(0:a) * exp((0:a) * (0:a - 1) / (2 * s^2)))
  expect_equal(morie_rdp_sampled_gaussian(5, 0.1, 1.2), log(A(5, 0.1, 1.2)) / 4, tolerance = 1e-12)
  # q = 1 is the plain Gaussian mechanism: alpha / (2 sigma^2)
  expect_equal(morie_rdp_sampled_gaussian(6, 1, 1.5), 6 / (2 * 1.5^2), tolerance = 1e-12)
  expect_equal(morie_rdp_compose(3, 0.2, 1, steps = 10), 10 * log(A(3, 0.2, 1)) / 2, tolerance = 1e-12)
  r <- morie_rdpcomp(0.05, 1.1, alpha = 2:10, steps = 100, delta = 1e-5)
  curve <- vapply(2:10, function(a) 100 * log(A(a, 0.05, 1.1)) / (a - 1), 0)
  eps <- curve + log(1e5) / (1:9)
  expect_equal(r$rdp_epsilons, curve, tolerance = 1e-12)
  expect_equal(r$epsilon, min(eps), tolerance = 1e-12)
  expect_equal(r$best_alpha, (2:10)[which.min(eps)])
  expect_equal(morie_rdpcomp(0.05, 1.1, alpha = 2:4)$estimate, min(curve[1:3]) / 100, tolerance = 1e-12)
  expect_error(morie_rdp_sampled_gaussian(2.5, 0.1, 1), "must be an integer")
  expect_error(morie_rdp_sampled_gaussian(1, 0.1, 1), "exceed 1")
  expect_error(morie_rdp_sampled_gaussian(2, 0, 1), "q must lie")
  expect_error(morie_rdp_compose(2, 0.1, 1, steps = 0), "at least 1")
  expect_error(morie_rdpcomp(0.1, 1, alpha = 2, delta = 1), "strictly in")
})

test_that("morie_rdrobu reports the three CCT intervals of one fit", {
  x <- seq(-1, 1, length.out = 61)
  y <- 0.5 + 0.9 * x + 0.4 * x^2 + ifelse(x >= 0, 1, 0) + sin(3 * seq_along(x)) * 0.2
  fit <- morie_causrddc(y, x, h = 0.5, b = 0.7)
  r <- morie_rdrobu(y, x, h = 0.5, b = 0.7)
  expect_equal(r$intervals$robust, fit$ci_robust)
  expect_equal(r$intervals$conventional, fit$ci_conventional)
  expect_equal(r$widths$robust, diff(fit$ci_robust), tolerance = 1e-12)
  expect_equal(r$correction_factor, fit$se_robust / fit$se_conventional, tolerance = 1e-12)
  expect_equal(r$bias_estimate, fit$estimate - fit$bias_corrected, tolerance = 1e-12)
  z <- stats::qnorm(0.975)
  expect_equal(r$widths$conventional, 2 * z * fit$se_conventional, tolerance = 1e-10)
  cct <- morie_calonico_cattaneo_titiunik(y, x, 0, h = 0.5, b = 0.7)
  expect_equal(cct$intervals, r$intervals)
})

test_that("recapTOA balances absorbed solar and outgoing radiation", {
  r <- recapTOA(1361, 0.3, 238.175)
  expect_equal(r$absorbed, 0.7 * 1361 / 4, tolerance = 1e-12)
  expect_equal(r$estimate, 0.7 * 1361 / 4 - 238.175, tolerance = 1e-12)
  expect_equal(r$teq, (0.7 * 1361 / 4 / 5.670374419e-8)^0.25, tolerance = 1e-12)
  v <- recapTOA(c(1361, 1300), c(0.3, 0.25, 0.35))
  ab <- (1 - c(0.3, 0.25, 0.35)) * c(1361, 1300, 1361) / 4
  expect_equal(v$absorbed, mean(ab), tolerance = 1e-12)
  expect_equal(v$net, rep(0, 3))
  expect_same_function(morie_toa_radiation_balance, recapTOA)
})

test_that("recurrent-event Cox models match survival::coxph", {
  skip_if_not_installed("survival")
  id <- rep(1:6, each = 3)
  start <- rep(c(0, 2, 5), 6) + rep(c(0, 0.5, 0.2, 0.1, 0.3, 0.4), each = 3)
  stop <- start + c(2, 3, 2.5, 1.8, 2.2, 3.1, 2.4, 1.5, 2.8, 2.1, 3.3, 1.9, 1.6, 2.7, 2.2, 2.9, 1.7, 2.6)
  event <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1)
  x <- rep(c(0.5, -0.3, 1.1, -0.8, 0.2, 0.9), each = 3)
  z <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0)
  X <- cbind(x, z)
  occ <- rep(1:3, 6)
  S <- survival::Surv
  ag <- Agrec(start, stop, event, X)
  c1 <- survival::coxph(S(start, stop, event) ~ x + z, ties = "breslow",
                        control = survival::coxph.control(eps = 1e-10, iter.max = 100))
  expect_equal(unname(ag$estimate), unname(stats::coef(c1)), tolerance = 1e-8)
  expect_equal(ag$loglik, c1$loglik[2], tolerance = 1e-10)
  gap <- stop - start
  pw <- Pwpgt(start, stop, event, X, occ)
  c2 <- survival::coxph(S(gap, event) ~ x + z + strata(occ), ties = "breslow",
                        control = survival::coxph.control(eps = 1e-10, iter.max = 100))
  expect_equal(unname(pw$estimate), unname(stats::coef(c2)), tolerance = 1e-8)
  expect_equal(unname(pw$se), unname(sqrt(diag(c2$var))), tolerance = 1e-8)
  wl <- Wlwmm(stop, event, X, occ)
  c3 <- survival::coxph(S(stop, event) ~ x + z + strata(occ), ties = "breslow",
                        control = survival::coxph.control(eps = 1e-10, iter.max = 100))
  expect_equal(unname(wl$estimate), unname(stats::coef(c3)), tolerance = 1e-8)
  expect_error(Agrec(start, start, event, X), "stop > start")
  expect_error(Agrec(start, stop, event * 2, X), "0 or 1")
  expect_error(Agrec(start, stop, rep(0, 18), X), "no events")
})

test_that("Survtdc is the truncated time-dependent concordance", {
  tt <- c(2, 5, 3, 8, 6, 1, 7)
  ev <- c(1, 0, 1, 1, 0, 1, 1)
  mk <- c(0.9, 0.2, 0.7, 0.3, 0.4, 0.95, 0.3)
  r <- Survtdc(tt, ev, mk, 6)
  num <- 0
  den <- 0
  for (i in 1:7) for (j in 1:7) {
    if (i != j && ev[i] == 1 && tt[i] <= 6 && tt[i] < tt[j]) {
      den <- den + 1
      num <- num + (mk[i] > mk[j]) + 0.5 * (mk[i] == mk[j])
    }
  }
  expect_equal(r$estimate, num / den, tolerance = 1e-12)
  expect_equal(r$comparable, den)
  expect_error(Survtdc(tt, ev, mk, 0.5), "no comparable pairs")
  expect_error(Survtdc(tt, ev[-1], mk, 6), "equal length")
})

test_that("Reffec scales R0 by the susceptible fraction", {
  r <- Reffec(2.5, 300, 1000)
  expect_equal(r$Rt, 0.75, tolerance = 1e-12)
  expect_equal(r$growing, 0)
  expect_equal(r$herd_immunity_threshold, 0.6, tolerance = 1e-12)
  expect_true(is.na(Reffec(0, 10, 100)$herd_immunity_threshold))
  expect_error(Reffec(-1, 1, 2), "non-negative")
  expect_error(Reffec(1, 3, 2), "\\[0, N\\]")
  expect_error(Reffec(1, 1, 0), "positive")
})
