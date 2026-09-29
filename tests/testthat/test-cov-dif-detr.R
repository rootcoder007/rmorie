# Coverage for defdtr.R, detrbb.R, dgi.R, dif1pl.R, difsib.R, emdtsm.R,
# emkfst.R and esmoeg.R: deformable sampling by direct bilinear
# interpolation, Hungarian matching against enumeration, Deep Graph
# Infomax on its replayed initialisation, Mantel-Haenszel DIF against
# mantelhaen.test, SIBTEST from its cell means, EMD sifting against
# splinefun, the state-space EM step against the joint-Gaussian
# posterior, and target rotation through the polar decomposition.

test_that("Defdtr samples the map bilinearly at the offset points", {
  Fm <- rbind(c(1, 2, 3, 4), c(5, 6, 7, 8), c(9, 10, 11, 12))
  bil <- function(y, x) {
    y <- min(max(y, 0), 2)
    x <- min(max(x, 0), 3)
    y0 <- floor(y)
    x0 <- floor(x)
    y1 <- min(y0 + 1, 2)
    x1 <- min(x0 + 1, 3)
    dy <- y - y0
    dx <- x - x0
    sum(Fm[c(y0, y0, y1, y1) + 1, ][cbind(1:4, c(x0, x1, x0, x1) + 1)] *
          c((1 - dy) * (1 - dx), (1 - dy) * dx, dy * (1 - dx), dy * dx))
  }
  qs <- rbind(c(0.5, 0.5), c(0, 1))
  off <- c(0.3, -0.2, 1.4, 0.6, -5, 0, 0.25, 0.75)
  w <- c(1, 3, 2, 2)
  r <- Defdtr(Fm, qs, K = 2, offsets = off, weights = w)
  O <- aperm(array(off, c(2, 2, 2)), c(3, 2, 1))
  ref <- rbind(c(1, 1.5), c(2, 0))
  smp <- matrix(0, 2, 2)
  for (q in 1:2) for (k in 1:2) smp[q, k] <- bil(ref[q, 1] + O[q, k, 1], ref[q, 2] + O[q, k, 2])
  W <- matrix(w, 2, 2, byrow = TRUE)
  expect_equal(r$ref_pixels, ref)
  expect_equal(r$samples, smp, tolerance = 1e-12)
  expect_equal(r$out, rowSums(W * smp) / rowSums(W), tolerance = 1e-12)
  z <- .ghc_norm(.ghc_rng(5), 8L)
  rd <- Defdtr(Fm, qs, K = 2, seed = 5)
  expect_equal(rd$out, Defdtr(Fm, qs, K = 2, offsets = z)$out, tolerance = 1e-12)
  expect_equal(Defdtr(Fm, rbind(c(0, 0)), K = 1, offsets = c(0, 0))$out, 1)
  expect_equal(Defdtr(Fm, qs, K = 2, offsets = off, weights = c(1, -1, 0, 0))$out[1], 0)
  expect_error(Defdtr(matrix(0, 0, 2), qs), "no rows")
  expect_error(Defdtr(Fm, cbind(qs, 0)), "Q x 2")
  expect_error(Defdtr(Fm, qs, K = 0), "at least 1")
  expect_error(Defdtr(Fm, qs, K = 2, offsets = 1:3), "Q x K x 2")
  expect_error(Defdtr(Fm, qs, K = 2, weights = 1:3), "Q x K values")
})

test_that("Detrbb finds the minimum-cost matching", {
  P <- rbind(c(0.5, 0.5, 0.2, 0.2), c(0.2, 0.3, 0.1, 0.2), c(0.8, 0.7, 0.3, 0.3),
             c(0.4, 0.6, 0.5, 0.4))
  Tg <- rbind(c(0.25, 0.3, 0.1, 0.2), c(0.75, 0.75, 0.3, 0.2), c(0.5, 0.5, 0.4, 0.4))
  corner <- function(b) c(b[1] - b[3] / 2, b[2] - b[4] / 2, b[1] + b[3] / 2, b[2] + b[4] / 2)
  giou <- function(a, b) {
    a <- corner(a)
    b <- corner(b)
    inter <- max(0, min(a[3], b[3]) - max(a[1], b[1])) * max(0, min(a[4], b[4]) - max(a[2], b[2]))
    un <- prod(a[3:4] - a[1:2]) + prod(b[3:4] - b[1:2]) - inter
    hull <- (max(a[3], b[3]) - min(a[1], b[1])) * (max(a[4], b[4]) - min(a[2], b[2]))
    inter / un - (hull - un) / hull
  }
  C <- outer(1:3, 1:4, Vectorize(function(g, q) {
    5 * sum(abs(Tg[g, ] - P[q, ])) + 2 * (1 - giou(Tg[g, ], P[q, ]))
  }))
  perms <- as.matrix(expand.grid(1:4, 1:4, 1:4))
  perms <- perms[apply(perms, 1, function(p) length(unique(p)) == 3), ]
  tot <- apply(perms, 1, function(p) sum(C[cbind(1:3, p)]))
  r <- Detrbb(P, Tg)
  expect_equal(r$cost, min(tot), tolerance = 1e-12)
  expect_equal(r$assignment, as.integer(perms[which.min(tot), ]) - 1L)
  expect_equal(r$unmatched, setdiff(0:3, r$assignment))
  expect_equal(Detrbb(P, Tg, n_objects = 3)$cost, r$cost)
  expect_error(Detrbb(P[0, ], Tg), "both boxes and targets")
  expect_error(Detrbb(P[, 1:3], Tg), "four columns")
  expect_error(Detrbb(P, Tg, n_objects = 2), "n_objects disagrees")
  expect_error(Detrbb(P[1:2, ], Tg), "more ground-truth")
})

test_that("Dgi scores real against corrupted node summaries", {
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
  X <- rbind(c(1, 0, 0.5), c(0, 1, 0.2), c(0.3, 0.3, 1), c(1, 1, 0))
  sig <- function(z) 1 / (1 + exp(-z))
  enc <- rbind(c(0.5, -0.2), c(0.1, 0.4), c(-0.3, 0.8))
  prop <- function(Xs, W) sig((A %*% Xs / rowSums(A)) %*% W)
  perm <- unique(((0:3) * 7 + 3) %% 4)
  perm <- c(perm, setdiff(0:3, perm))
  H <- prop(X, enc)
  Hc <- prop(X[perm + 1, ], enc)
  s <- sig(colMeans(H))
  r <- Dgi(A, X, encoder = enc)
  pos <- sig(as.numeric(H %*% s))
  neg <- sig(as.numeric(Hc %*% s))
  expect_equal(r$h, H, tolerance = 1e-12)
  expect_equal(r$pos_score, pos, tolerance = 1e-12)
  expect_equal(r$loss, -(sum(log(pos)) + sum(log(1 - neg))) / 8, tolerance = 1e-12)
  d <- Dgi(A, X, seed = 2)
  Wd <- matrix(.ghc_norm(.ghc_rng(2), 9L), 3, 3, byrow = TRUE) / sqrt(3)
  expect_equal(d$h, prop(X, Wd), tolerance = 1e-12)
  # the default bilinear map is zero, so every score is 1/2
  expect_equal(d$loss, log(2), tolerance = 1e-12)
  iso <- Dgi(diag(0, 4), X, encoder = enc)
  expect_equal(iso$h, matrix(0.5, 4, 2))
  expect_error(Dgi(matrix(0, 0, 0), X), "no rows")
  expect_error(Dgi(A[, 1:3], X), "square")
  expect_error(Dgi(A, X[1:3, ]), "one row per node")
  expect_error(Dgi(A, X, encoder = enc[1:2, ]), "one row per feature")
})

test_that("Difmh is the Mantel-Haenszel DIF test", {
  y <- c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1)
  g <- rep(c("R", "F"), 11)
  k <- c(rep(1, 8), rep(2, 8), rep(3, 4), 4, 4)
  r <- Difmh(y, g, item = k)
  keep <- k != 4
  tab <- table(factor(g[keep], c("R", "F")), factor(y[keep], c(1, 0)), k[keep])
  mh <- mantelhaen.test(tab, correct = TRUE)
  expect_equal(r$statistic, unname(mh$statistic), tolerance = 1e-12)
  expect_equal(r$alpha_MH, unname(mh$estimate), tolerance = 1e-12)
  expect_equal(r$delta_MH, -2.35 * log(unname(mh$estimate)), tolerance = 1e-12)
  expect_equal(r$n_strata, 3L)
  un <- Difmh(y, g, item = k, correct = FALSE)
  expect_equal(un$statistic, unname(mantelhaen.test(tab, correct = FALSE)$statistic), tolerance = 1e-12)
  flip <- Difmh(y, g, item = k, reference = "F")
  expect_equal(flip$alpha_MH, 1 / r$alpha_MH, tolerance = 1e-12)
  one <- Difmh(y, g)
  expect_equal(one$n_strata, 1L)
  expect_error(Difmh(y, g[-1]), "same length")
  expect_error(Difmh(y + 1, g), "0/1")
  expect_error(Difmh(y, rep("a", 22)), "exactly 2")
  expect_error(Difmh(y, g, reference = "Z"), "not one of")
  expect_error(Difmh(y, g, item = k[-1]), "item must be")
  expect_error(Difmh(c(1, 1, 0, 0), c("a", "a", "b", "b"), item = 1:4), "no stratum")
})

test_that("Difsib weights the matched-cell mean differences", {
  s <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1)
  g <- rep(c("r", "f"), 10)
  mt <- c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4)
  cell <- function(v, lv, grp, f) f(v[mt == lv & g == grp])
  st <- data.frame(k = 1:4)
  st$yr <- sapply(1:4, function(l) cell(s, l, "r", mean))
  st$yf <- sapply(1:4, function(l) cell(s, l, "f", mean))
  st$vr <- sapply(1:4, function(l) cell(s, l, "r", var))
  st$vf <- sapply(1:4, function(l) cell(s, l, "f", var))
  st$nr <- sapply(1:4, function(l) sum(mt == l & g == "r"))
  st$nf <- sapply(1:4, function(l) sum(mt == l & g == "f"))
  kp <- st$vr > 0 & st$vf > 0
  p <- (st$nr + st$nf)[kp] / sum((st$nr + st$nf)[kp])
  beta <- sum(p * (st$yr - st$yf)[kp])
  sig <- sqrt(sum(p^2 * (st$vf / st$nf + st$vr / st$nr)[kp]))
  r <- Difsib(s, g, matching = mt)
  expect_equal(r$levels, st$k[kp])
  expect_equal(r$beta, beta, tolerance = 1e-12)
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  expect_equal(r$p_value, pchisq((beta / sig)^2, 1, lower.tail = FALSE), tolerance = 1e-12)
  # with a matching score equal to the level, the regression correction is zero
  expect_equal(Difsib(s, g, matching = mt, correction = TRUE)$beta, beta, tolerance = 1e-12)
  expect_equal(Difsib(0, g, studied = s, matching = mt, reference = "f")$beta, -beta,
               tolerance = 1e-12)
  expect_error(Difsib(s, g[-1], matching = mt), "same length as the item")
  expect_error(Difsib(s, g), "matching score is required")
  expect_error(Difsib(s, g, matching = mt[-1]), "matching must be")
  expect_error(Difsib(s, rep("a", 20), matching = mt), "exactly 2")
  expect_error(Difsib(s, g, matching = mt, reference = "q"), "not one of")
  expect_error(Difsib(rep(1, 20), g, matching = mt), "non-zero within-cell")
})

test_that("Emdtsm sifts with natural cubic envelopes", {
  y <- sin(seq(0, 6 * pi, length.out = 40)) + 0.3 * seq(0, 1, length.out = 40) +
    0.4 * sin(seq(0, 30, length.out = 40))
  x <- 0:39
  ext <- function(v) {
    i <- 2:39
    list(hi = i[v[i] > v[i - 1] & v[i] >= v[i + 1]], lo = i[v[i] < v[i - 1] & v[i] <= v[i + 1]])
  }
  e <- ext(y)
  up <- splinefun(x[c(1, e$hi, 40)], y[c(1, e$hi, 40)], method = "natural")(x)
  dn <- splinefun(x[c(1, e$lo, 40)], y[c(1, e$lo, 40)], method = "natural")(x)
  one <- Emdtsm(y, max_imf = 1, max_sift = 1)
  expect_equal(one$imfs, y - 0.5 * (up + dn), tolerance = 1e-10)
  expect_equal(one$residual, 0.5 * (up + dn), tolerance = 1e-10)
  full <- Emdtsm(y)
  expect_lt(full$completeness, 1e-12)
  expect_equal(colSums(rbind(matrix(full$imfs, ncol = 40, byrow = TRUE), full$residual)), y,
               tolerance = 1e-12)
  expect_equal(Emdtsm(1:10)$n_imf, 0L)
  expect_error(Emdtsm(numeric(0)), "no observations")
  expect_error(Emdtsm(y, max_imf = 0), "positive")
  expect_error(Emdtsm(y, sd_tol = 0), "positive")
})

test_that("Emkfst takes EM steps from the smoothed moments", {
  y <- c(0.5, 1.1, 0.7, 1.6, 1.2, 0.4, -0.2, 0.3, 0.9, 1.4)
  n <- 10
  kf_ll <- function(phi, Q, R) {
    x <- 0
    P <- 1
    ll <- 0
    for (t in 1:n) {
      xp <- phi * x
      Pp <- phi^2 * P + Q
      S <- Pp + R
      ll <- ll + dnorm(y[t], xp, sqrt(S), log = TRUE)
      x <- xp + Pp / S * (y[t] - xp)
      P <- (1 - Pp / S) * Pp
    }
    ll
  }
  mstep <- function(phi, Q, R) {
    # prior of x_0..x_n: x_0 ~ N(0, 1), x_t = phi x_{t-1} + w_t
    Lm <- diag(n + 1)
    for (t in 1:n) Lm[t + 1, t] <- -phi
    Pinv <- diag(c(1, rep(1 / Q, n)))
    prec <- t(Lm) %*% Pinv %*% Lm
    Hm <- cbind(0, diag(n))
    Sp <- solve(prec + crossprod(Hm) / R)
    mu <- as.numeric(Sp %*% t(Hm) %*% y / R)
    E2 <- Sp + outer(mu, mu)
    S11 <- sum(diag(E2)[2:(n + 1)])
    S00 <- sum(diag(E2)[1:n])
    S10 <- sum(E2[cbind(2:(n + 1), 1:n)])
    ph <- S10 / S00
    c(ph, (S11 - ph * S10) / n, sum((y - mu[-1])^2 + diag(Sp)[-1]) / n)
  }
  r0 <- Emkfst(y, init = c(0.8, 0.3, 0.2), max_iter = 0)
  expect_equal(r0$loglik, kf_ll(0.8, 0.3, 0.2), tolerance = 1e-12)
  r1 <- Emkfst(y, init = c(0.8, 0.3, 0.2), max_iter = 1)
  expect_equal(c(r1$phi, r1$Q, r1$R), mstep(0.8, 0.3, 0.2), tolerance = 1e-9)
  expect_equal(r1$loglik, kf_ll(r1$phi, r1$Q, r1$R), tolerance = 1e-12)
  r <- Emkfst(y, max_iter = 20)
  expect_true(all(diff(r$loglik_path) > -1e-10))
  v <- var(y)
  expect_equal(Emkfst(y, max_iter = 0)$loglik, kf_ll(0.9, v / 2, v / 2), tolerance = 1e-12)
  expect_error(Emkfst(numeric(0)), "no observations")
  expect_error(Emkfst(y, max_iter = -1), "non-negative")
  expect_error(Emkfst(y, init = 1:2), "\\(phi, Q, R\\)")
  expect_error(Emkfst(y, init = c(0.5, 1, 0)), "R positive")
})

test_that("Esmoeg iterates Procrustes rotations towards the target", {
  L <- rbind(c(0.7, 0.3), c(0.6, 0.4), c(0.2, 0.8), c(0.3, 0.7), c(0.5, 0.5))
  polar <- function(M) {
    s <- svd(M)
    s$u %*% t(s$v)
  }
  Hf <- rbind(c(0.8, 0), c(0.7, 0), c(0, 0.8), c(0, 0.7), c(0.4, 0.4))
  full <- Esmoeg(L, Hf)
  Tt <- polar(t(L) %*% Hf)
  expect_equal(matrix(full$rotation, 2, 2, byrow = TRUE), Tt, tolerance = 1e-10)
  expect_equal(full$rms, sqrt(mean((L %*% Tt - Hf)^2)), tolerance = 1e-10)
  Hp <- Hf
  Hp[5, ] <- NA
  pr <- Esmoeg(L, Hp)
  Tp <- matrix(pr$rotation, 2, 2, byrow = TRUE)
  Rot <- L %*% Tp
  Hfill <- Hp
  Hfill[5, ] <- Rot[5, ]
  expect_equal(crossprod(Tp), diag(2), tolerance = 1e-10)
  expect_equal(Tp, polar(t(L) %*% Hfill), tolerance = 1e-9)
  expect_equal(pr$n_specified, 8L)
  expect_error(Esmoeg(matrix(0, 0, 2), Hf), "no rows")
  expect_error(Esmoeg(L, Hf[1:4, ]), "same shape")
  expect_error(Esmoeg(L[, 0], Hf[, 0]), "no columns")
  expect_error(Esmoeg(L, Hf * NA), "specifies no elements")
  expect_error(Esmoeg(cbind(L[, 1], 0), cbind(Hf[, 1], 0)), "degenerate")
})
