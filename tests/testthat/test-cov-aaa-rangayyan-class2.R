# Coverage for the Rangayyan chapter 9-10 decomposition, dictionary,
# separation, network and application routines. Deterministic parts are
# recomputed with base R linear algebra (solve, eigen, qr, stats::fft),
# the Numerical Recipes LCG that seeds the iterative fits is regenerated in
# the test, and iterative fits are checked against the optimality or
# reconstruction identities they must satisfy.

.lcg <- function(seed) {
  st <- seed %% 4294967296
  function() {
    st <<- (1664525 * st + 1013904223) %% 4294967296
    (st + 0.5) / 4294967296
  }
}
.sig <- function(b) 1 / (1 + exp(-b))

.omp_ref <- function(x, D, k) {
  r <- x
  sup <- integer(0)
  nrm <- sqrt(rowSums(D^2))
  coef <- numeric(nrow(D))
  for (s in seq_len(k)) {
    sc <- abs(as.numeric(D %*% r)) / nrm
    sc[sup] <- -1
    sup <- c(sup, which.max(sc))
    A <- t(D[sup, , drop = FALSE])
    w <- solve(crossprod(A) + diag(1e-10, length(sup)), crossprod(A, x))
    coef <- numeric(nrow(D))
    coef[sup] <- w
    r <- x - as.numeric(A %*% w)
  }
  list(coef = coef, sup = sup, r = r)
}

test_that("MlpBp: one epoch of eqs (10.79)-(10.85) from the seeded weights", {
  X <- rbind(c(0, 0), c(0, 1), c(1, 0), c(1, 1), c(0.5, 0.2))
  y <- c(0, 1, 1, 0, 1)
  r <- MlpBp(X, y, hidden = 3, eta = 0.4, alpha = 0.7, maxiter = 1, tol = 0)
  u <- .lcg(1)
  W1 <- matrix(0, 2, 3)
  for (i in 1:2) for (j in 1:3) W1[i, j] <- u() - 0.5
  T1 <- vapply(1:3, function(i) u() - 0.5, 1)
  W2 <- matrix(vapply(1:3, function(i) u() - 0.5, 1), 3, 1)
  T2 <- u() - 0.5
  dW1 <- 0 * W1
  dT1 <- 0 * T1
  dW2 <- 0 * W2
  dT2 <- 0
  tot <- 0
  for (s in 1:5) {
    xh <- .sig(as.numeric(X[s, ] %*% W1) - T1)
    yo <- .sig(sum(W2 * xh) - T2)
    dk <- yo * (1 - yo) * (y[s] - yo)
    tot <- tot + (y[s] - yo)^2
    dW2 <- 0.4 * outer(xh, dk) + 0.7 * dW2
    W2 <- W2 + dW2
    dT2 <- -0.4 * dk + 0.7 * dT2
    T2 <- T2 + dT2
    bp <- xh * (1 - xh) * as.numeric(W2 * dk)
    dW1 <- 0.4 * outer(X[s, ], bp) + 0.7 * dW1
    W1 <- W1 + dW1
    dT1 <- -0.4 * bp + 0.7 * dT1
    T1 <- T1 + dT1
  }
  expect_equal(r$weights$input_hidden, W1, tolerance = 1e-12)
  expect_equal(r$weights$hidden_output, W2, tolerance = 1e-12)
  expect_equal(r$offsets$output, T2, tolerance = 1e-12)
  expect_equal(r$mse, tot / 5, tolerance = 1e-12)
  out <- .sig(as.numeric(.sig(sweep(X %*% W1, 2, T1)) %*% W2) - T2)
  expect_equal(as.numeric(r$outputs), out, tolerance = 1e-12)
  expect_equal(r$predictions, ifelse(out >= 0.5, 1L, 0L))
  m3 <- MlpBp(rbind(X, c(2, 2)), c(0, 1, 2, 0, 1, 2), hidden = 2, maxiter = 3)
  f <- .sig(sweep(.sig(sweep(rbind(X, c(2, 2)) %*% m3$weights$input_hidden, 2, m3$offsets$hidden)) %*% m3$weights$hidden_output, 2, m3$offsets$output))
  expect_equal(m3$outputs, f, tolerance = 1e-12)
  expect_equal(m3$predictions, (0:2)[apply(f, 1, which.max)])
  expect_error(MlpBp(X, rep(1, 5)), "two distinct classes")
  expect_error(MlpBp(X, y, eta = 0), "0 < eta <= 10")
})

test_that("Bbb applies the conjunctive bundle-branch-block rules", {
  lc <- list(qrsneg_v1v2 = TRUE, qsdur80_v1v2 = TRUE, noq_two_of_i_v5_v6 = TRUE, rdur60_two_of_i_avl_v5_v6 = TRUE)
  expect_identical(Bbb(110, lc)$blocktype, "incomplete left bundle-branch block")
  expect_false(Bbb(104, lc)$left)
  rc <- list(sdur40_two_of_i_avl_v4_v5_v6 = TRUE, rprime_v1v2 = TRUE)
  expect_identical(Bbb(95, rc)$blocktype, "incomplete right bundle-branch block")
  expect_identical(Bbb(110, c(lc, rc))$blocktype, "criteria met for both left and right incomplete block")
  expect_identical(Bbb(130)$blocktype, "QRS wider than 120 ms, complete block not excluded")
  expect_identical(Bbb(105)$blocktype, "QRS wider than normal, block criteria not met")
  expect_identical(Bbb(90)$blocktype, "no bundle-branch block by these criteria")
  expect_error(Bbb(-5), "positive, finite")
  expect_error(Bbb(100, criteria = 1), "list of boolean")
})

test_that("PvcBayes posteriors are the standardised Gaussian class densities", {
  set.seed(2)
  F <- rbind(cbind(stats::rnorm(8, 1, 0.3), stats::rnorm(8, 2, 0.5)), cbind(stats::rnorm(8, 2, 0.4), stats::rnorm(8, 3.2, 0.6)))
  y <- rep(0:1, each = 8)
  r <- PvcBayes(F, y, priors = c(3, 1), query = rbind(c(1, 2), c(2.2, 3.5)))
  sc <- apply(F, 2, stats::sd)
  Z <- sweep(F, 2, sc, "/")
  dens <- function(z, cls) {
    R <- Z[y == cls, ]
    C <- stats::cov(R) + diag(1e-9, 2)
    d <- z - colMeans(R)
    exp(-0.5 * sum(d * solve(C, d))) / sqrt((2 * pi)^2 * det(C))
  }
  P <- t(apply(Z, 1, function(z) {
    v <- c(0.75 * dens(z, 0), 0.25 * dens(z, 1))
    v / sum(v)
  }))
  expect_equal(r$posterior, P, tolerance = 1e-9)
  expect_equal(r$predictions, ifelse(P[, 1] >= P[, 2], 0, 1))
  q <- sweep(rbind(c(1, 2), c(2.2, 3.5)), 2, sc, "/")
  qp <- t(apply(q, 1, function(z) c(0.75 * dens(z, 0), 0.25 * dens(z, 1))))
  expect_equal(r$queryclass, ifelse(qp[, 1] >= qp[, 2], 0, 1))
  expect_equal(r$accuracy, mean(r$predictions == y), tolerance = 1e-12)
  expect_error(PvcBayes(F, rep(0:2, length.out = 16)), "exactly two classes")
  expect_error(PvcBayes(F[c(1:2, 9:10), ], c(0, 0, 1, 1)), "more rows than features")
})

test_that("BciChSel and NmfChSel score channels by eqs (9.94)-(9.96)", {
  set.seed(5)
  X <- matrix(stats::rnorm(6 * 30), 6, 30)
  X[2, ] <- X[1, ] + 0.1 * X[2, ]
  r <- NmfChSel(X, 2, rank = 3, maxiter = 60)
  expect_equal(r$covariance, stats::cov(t(X)), tolerance = 1e-12)
  W <- r$W
  expect_true(all(W >= 0))
  Wn <- t(apply(W, 1, function(v) (v - min(v)) / (max(v) - min(v))))
  rm <- sqrt(rowMeans((Wn - 0.5)^2))
  expect_equal(r$rmsd, rm, tolerance = 1e-12)
  expect_equal(r$ranking, order(-rm) - 1L)
  expect_equal(r$selected, sort(order(-rm)[1:2]) - 1L)
  V <- r$covariance - min(r$covariance)
  expect_equal(r$error, sqrt(sum((V - W %*% r$H)^2)), tolerance = 1e-10)
  b <- BciChSel(X, 2, rank = 3, maxiter = 60)
  expect_equal(b$selected, r$selected)
  expect_equal(b$weighted, X[r$selected + 1, ] * rm[r$selected + 1], tolerance = 1e-12)
  expect_error(NmfChSel(X, 2, rank = 2), "rank must be at least 3")
  expect_error(BciChSel(X, 7), "nselect must satisfy")
})

test_that("BPursuit and SparseCode: soft thresholding in an orthonormal basis, KKT otherwise", {
  x <- c(1.5, -0.2, 0.7, -2, 0.05, 0.9)
  Q <- qr.Q(qr(matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 4, 1, 0, 2, 1, 1, 0, 5, 2, 1, 0, 1, 2, 3, 1, 1, 2, 0, 1, 1, 6, 1, 1, 2, 1, 0, 1, 7), 6)))
  D <- t(Q)
  lam <- 0.3
  r <- BPursuit(x, D, lam = lam, tol = 1e-14)
  cf <- as.numeric(D %*% x)
  expect_equal(r$alpha, sign(cf) * pmax(abs(cf) - lam, 0), tolerance = 1e-12)
  expect_equal(r$objective, 0.5 * sum(r$residual^2) + lam * sum(abs(r$alpha)), tolerance = 1e-12)
  expect_equal(r$reconstruction + r$residual, x, tolerance = 1e-12)
  set.seed(3)
  D2 <- matrix(stats::rnorm(5 * 6), 5, 6)
  r2 <- BPursuit(x, D2, lam = 0.2, maxiter = 20000, tol = 1e-13)
  g <- as.numeric(D2 %*% r2$residual)
  act <- r2$alpha != 0
  # stationarity of the lasso: the correlation equals lam * sign on the
  # support and is bounded by lam off it (ISTA stopped at a 1e-13 step)
  expect_equal(g[act], 0.2 * sign(r2$alpha[act]), tolerance = 1e-9)
  expect_true(all(abs(g[!act]) <= 0.2 + 1e-9))
  s1 <- SparseCode(x, D2, lam = 0.2, maxiter = 20000, tol = 1e-13)
  expect_equal(s1$alpha, r2$alpha, tolerance = 1e-12)
  expect_identical(s1$mode, "lasso")
  s2 <- SparseCode(x, D2, sparsity = 2)
  expect_equal(s2$alpha, OmpFit(x, D2, sparsity = 2)$coefficients, tolerance = 1e-12)
  expect_equal(s2$energyratio, 1 - sum(s2$residual^2) / sum(x^2), tolerance = 1e-12)
  expect_error(SparseCode(x, D2), "exactly one of")
  expect_error(BPursuit(x, D2, lam = -1), "nonnegative")
})

test_that("OmpFit and DictCode agree with a least-squares OMP", {
  set.seed(8)
  D <- matrix(stats::rnorm(7 * 10), 7, 10)
  x <- 2 * D[3, ] - 1.2 * D[6, ] + 0.05 * stats::rnorm(10)
  ref <- .omp_ref(x, D, 3)
  o <- OmpFit(x, D, sparsity = 3)
  expect_equal(o$coefficients, ref$coef, tolerance = 1e-9)
  expect_equal(o$support, ref$sup - 1L)
  expect_equal(o$residual, ref$r, tolerance = 1e-9)
  expect_equal(o$error, sqrt(sum(ref$r^2)), tolerance = 1e-9)
  Y <- rbind(x, D[1, ] + D[2, ])
  dc <- DictCode(Y, D, sparsity = 2)
  for (i in 1:2) {
    ri <- .omp_ref(Y[i, ], D, 2)
    expect_equal(dc$coefficients[[i]], ri$coef, tolerance = 1e-9)
    expect_equal(dc$reconstruction[[i]], as.numeric(ri$coef %*% D), tolerance = 1e-9)
  }
  expect_equal(dc$error, sqrt(sum(unlist(dc$residual)^2)), tolerance = 1e-12)
  expect_error(DictCode(Y, D, sparsity = 8), "sparsity must satisfy")
  expect_error(OmpFit(rep(0, 10), D), "zero energy")
})

test_that("MPursuit: greedy projections, energy identity and the Gabor default", {
  set.seed(9)
  D <- matrix(stats::rnorm(6 * 12), 6, 12)
  x <- stats::rnorm(12)
  r <- MPursuit(x, dictionary = D, natoms = 4)
  Dn <- D / sqrt(rowSums(D^2))
  res <- x
  used <- integer(0)
  cf <- numeric(0)
  for (s in 1:4) {
    v <- abs(as.numeric(Dn %*% res))
    v[used] <- -1
    j <- which.max(v)
    a <- sum(Dn[j, ] * res)
    res <- res - a * Dn[j, ]
    used <- c(used, j)
    cf <- c(cf, a)
  }
  expect_equal(r$indices, used - 1L)
  expect_equal(r$coefficients, cf, tolerance = 1e-12)
  expect_equal(r$residual, res, tolerance = 1e-12)
  expect_equal(sum(x^2), sum(cf^2) + sum(res^2), tolerance = 1e-12)
  g <- MPursuit(sin(2 * pi * (0:31) / 8), natoms = 5)
  expect_equal(g$reconstruction + g$residual, sin(2 * pi * (0:31) / 8), tolerance = 1e-12)
  expect_equal(rowSums(g$atoms^2), rep(1, nrow(g$atoms)), tolerance = 1e-12)
  expect_error(MPursuit(rep(0, 8), dictionary = diag(8)), "zero energy")
  expect_error(MPursuit(x, dictionary = D[, 1:5]), "same length")
})

test_that("CadPipe: stratified folds and the standardised nearest-prototype rule", {
  set.seed(12)
  F <- rbind(matrix(stats::rnorm(20, 0), 10), matrix(stats::rnorm(20, 1.5), 10))
  y <- rep(0:1, each = 10)
  r <- CadPipe(F, y, k = 3)
  folds <- lapply(1:3, function(f) c(which(y == 0)[(seq_len(10) - 1) %% 3 + 1 == f], which(y == 1)[(seq_len(10) - 1) %% 3 + 1 == f]))
  expect_equal(r$folds, lapply(folds, function(f) sort(f) - 1L))
  pred <- integer(20)
  for (f in folds) {
    tr <- setdiff(1:20, f)
    sc <- apply(F[tr, ], 2, stats::sd)
    p0 <- colMeans(sweep(F[tr[y[tr] == 0], ], 2, sc, "/"))
    p1 <- colMeans(sweep(F[tr[y[tr] == 1], ], 2, sc, "/"))
    for (i in f) pred[i] <- as.integer(sum((F[i, ] / sc - p1)^2) < sum((F[i, ] / sc - p0)^2))
  }
  expect_equal(r$predictions, pred)
  expect_equal(r$accuracy, mean(pred == y), tolerance = 1e-12)
  expect_equal(r$weightedaccuracy, mean(pred[y == 1] == 1) * 0.5 + mean(pred[y == 0] == 0) * 0.5, tolerance = 1e-12)
  expect_error(CadPipe(F, rep(1, 20)), "both 0")
  expect_error(CadPipe(F, y, k = 21), "k must satisfy")
})

test_that("CnnSig: valid convolution, rectifier, max-pool and softmax", {
  x <- c(0.5, -1, 2, 0.3, 1.1, -0.4, 0.8, 1.5)
  K <- rbind(c(1, -1, 0.5), c(0.2, 0.3, 0.1))
  dense <- matrix(c(0.1, -0.2, 0.3, 0.4, -0.1, 0.2, 0.05, 0.1, -0.3, 0.2, 0.1, 0), 2, 6)
  r <- CnnSig(x, K, bias = c(0.1, -0.2), pool = 2, dense = dense)
  maps <- lapply(1:2, function(k) pmax(0, vapply(1:6, function(i) sum(K[k, ] * x[i:(i + 2)]) + c(0.1, -0.2)[k], 1)))
  expect_equal(r$maps, maps, tolerance = 1e-12)
  pooled <- lapply(maps, function(m) c(max(m[1:2]), max(m[3:4]), max(m[5:6])))
  expect_equal(r$features, unlist(pooled), tolerance = 1e-12)
  z <- as.numeric(dense %*% unlist(pooled))
  expect_equal(r$scores, exp(z) / sum(exp(z)), tolerance = 1e-12)
  expect_identical(r$predicted, which.max(z) - 1L)
  expect_error(CnnSig(x, K, dense = dense[, 1:5]), "dense rows must match")
  expect_error(CnnSig(x[1:2], K), "no longer than the signal")
})

test_that("FecgNmf factorises the Hann-STFT magnitude and picks rows by peak count", {
  fs <- 64
  i <- 0:255
  x <- sin(2 * pi * 1.3 * i / fs)^15 + 0.3 * sin(2 * pi * 2.4 * i / fs)^31 + 0.02 * cos(i)
  r <- FecgNmf(x, fs, nwin = 32, hop = 16, rank = 3, maxiter = 40)
  w <- 0.5 - 0.5 * cos(2 * pi * (0:31) / 32)
  st <- seq(0, 256 - 32, by = 16)
  V <- vapply(st, function(s) Mod(stats::fft(x[s + 1:32] * w))[1:17], numeric(17))
  expect_true(all(r$W >= 0) && all(r$H >= 0))
  expect_equal(r$error, sqrt(sum((V - r$W %*% r$H)^2)), tolerance = 1e-9)
  pc <- function(row, tau) {
    nr <- abs(row) / max(abs(row))
    k <- 2:(length(nr) - 1)
    sum(nr[k] > tau & nr[k] >= nr[k - 1] & nr[k] > nr[k + 1])
  }
  cnt <- apply(r$H, 1, pc, tau = 0.45)
  ord <- order(cnt)
  expect_identical(c(r$maternalrow, r$fetalrow), ord[1:2] - 1L)
  expect_equal(unname(unlist(r$peaks$per_row)), cnt)
  expect_length(r$fetal, 256L)
  rk <- FecgNmf(x, fs, nwin = 32, rank = 3, lam = 0.1, maxiter = 20)
  expect_true(all(rk$H >= 0))
  expect_error(FecgNmf(x, fs, taum = 0), "taum and tauf")
  expect_error(FecgNmf(x, 0), "positive sampling rate")
})

test_that("PvcLinDf: eq (10.131) and the trained perpendicular bisector", {
  rr <- c(0.7, 0.4, 0.65, 0.5)
  ff <- c(1.5, 2.9, 1.7, 2.2)
  r <- PvcLinDf(rr, ff)
  d <- rr - 5.56 * ff + 11.44
  expect_equal(r$discriminant, d, tolerance = 1e-12)
  expect_equal(r$labels, as.integer(d <= 0))
  tr <- list(c(0.7, 0.68, 0.45, 0.4), c(1.5, 1.6, 2.8, 3), c(0, 0, 1, 1))
  t2 <- PvcLinDf(rr, ff, train = tr)
  p0 <- c(0.69, 1.55)
  p1 <- c(0.425, 2.9)
  dd <- p1 - p0
  cst <- sum(dd * (p0 + p1) / 2)
  expect_equal(t2$discriminant, -dd[1] * rr - dd[2] * ff + cst, tolerance = 1e-12)
  expect_equal(t2$labels, as.integer(-dd[1] * rr - dd[2] * ff + cst <= 0))
  expect_error(PvcLinDf(rr, ff[-1]), "same length")
  expect_error(PvcLinDf(rr, ff, train = list(1, 2)), "triple")
})

test_that("EegBands: fractional DFT power in the Section 1.2.6 bands", {
  fs <- 128
  i <- 0:255
  x <- sin(2 * pi * 10 * i / fs) + 0.5 * sin(2 * pi * 6 * i / fs) + 0.2 * sin(2 * pi * 20 * i / fs) + 0.1
  r <- EegBands(x, fs)
  xc <- x - mean(x)
  ps <- Mod(stats::fft(xc))[1:129]^2
  f <- (0:128) * fs / 256
  tot <- sum(ps)
  expect_equal(r$fraction$alpha, sum(ps[f >= 8 & f <= 13]) / tot, tolerance = 1e-12)
  expect_equal(r$fraction$theta, sum(ps[f >= 4 & f < 8]) / tot, tolerance = 1e-12)
  expect_equal(r$fraction$beta, sum(ps[f > 13 & f <= 64]) / tot, tolerance = 1e-12)
  expect_equal(r$power$delta, sum(ps[f >= 0.5 & f < 4]), tolerance = 1e-9)
  expect_identical(r$dominant, "alpha")
  u <- EegBands(x, fs, bands = list(lo = c(0, 9), top = c(15, 64)))
  expect_equal(u$fraction$lo, sum(ps[f < 9]) / tot, tolerance = 1e-12)
  expect_equal(u$fraction$top, sum(ps[f >= 15]) / tot, tolerance = 1e-12)
  expect_error(EegBands(x[1:7], fs), "eight samples")
  expect_error(EegBands(x, fs, bands = list(b = c(5, 2))), "0 <= f1 < f2")
})

test_that("SeizDict features are projections plus reconstruction error, classified by centroid", {
  set.seed(14)
  S <- rbind(matrix(stats::rnorm(3 * 16, sd = 0.3), 3) + rep(sin(2 * pi * (0:15) / 8), each = 3),
             matrix(stats::rnorm(3 * 16, sd = 0.3), 3) + rep(sign(sin(2 * pi * (0:15) / 4)), each = 3))
  y <- rep(0:1, each = 3)
  r <- SeizDict(S, y, iterations = 3, test = S[c(1, 6), ])
  D <- r$dictionary
  expect_equal(rowSums(D^2), rep(1, nrow(D)), tolerance = 1e-12)
  feats <- t(apply(S, 1, function(s) {
    co <- as.numeric(D %*% s)
    c(co, sqrt(sum((s - as.numeric(co %*% D))^2)))
  }))
  expect_equal(r$error, feats[, ncol(feats)], tolerance = 1e-12)
  expect_equal(do.call(rbind, r$coefficients), feats[, -ncol(feats)], tolerance = 1e-12)
  cen <- rbind(colMeans(feats[y == 0, ]), colMeans(feats[y == 1, ]))
  pr <- apply(feats, 1, function(v) which.min(rowSums(sweep(cen, 2, v)^2)) - 1)
  expect_equal(r$predictions, pr)
  expect_equal(r$testclass, pr[c(1, 6)])
  expect_equal(r$isseizure, pr == 1)
  expect_error(SeizDict(S, rep(0, 6)), "two classes")
})

test_that("IcaFix whitens, decorrelates and separates two non-Gaussian sources", {
  tt <- 0:399
  s1 <- sin(2 * pi * tt / 37)
  s2 <- ((tt %% 23) / 23) - 0.5
  Y <- rbind(0.8 * s1 + 0.6 * s2 + 1, 0.3 * s1 - 0.9 * s2)
  r <- IcaFix(Y, maxiter = 300, tol = 1e-12)
  Yc <- Y - rowMeans(Y)
  C <- Yc %*% t(Yc) / 400
  expect_equal(r$whitening %*% C %*% t(r$whitening), diag(2), tolerance = 1e-9)
  expect_equal(r$sources %*% t(r$sources) / 400, diag(2), tolerance = 1e-9)
  expect_equal(r$sources, r$unmixing %*% Yc, tolerance = 1e-9)
  W <- r$unmixing
  expect_equal(r$mixing, t(W) %*% solve(W %*% t(W)), tolerance = 1e-9)
  cm <- abs(stats::cor(t(r$sources), cbind(s1, s2)))
  expect_gt(min(apply(cm, 1, max)), 0.99)
  ic <- IcaClean(Y, drop = 0, maxiter = 300)
  S <- ic$components
  Sk <- S
  Sk[1, ] <- 0
  expect_equal(ic$clean, ic$mixing %*% Sk + rowMeans(Y), tolerance = 1e-12)
  kx <- apply(S, 1, function(v) mean((v - mean(v))^4) / mean((v - mean(v))^2)^2 - 3)
  expect_equal(ic$kurtosis, kx, tolerance = 1e-12)
  expect_equal(ic$removedpower, sum(S[1, ]^2) / sum(S^2), tolerance = 1e-12)
  expect_equal(IcaClean(Y, kurtosis = 1.2, maxiter = 300)$artifacts, which(abs(kx) > 1.2) - 1L)
  expect_error(IcaClean(Y, drop = 2), "drop indices")
  expect_error(IcaFix(Y, ncomp = 3), "ncomp must satisfy")
  expect_error(IcaFix(rbind(1:10, 2 * (1:10))), "rank deficient")
})

test_that("Infomax natural-gradient ICA separates super-Gaussian sources", {
  u <- .lcg(3)
  lap <- function(n) vapply(seq_len(n), function(i) {
    a <- u()
    if (a < 0.5) log(2 * a) else -log(2 * (1 - a))
  }, 1)
  s1 <- lap(500)
  s2 <- lap(500)
  Y <- rbind(s1 + 0.5 * s2, 0.4 * s1 - s2)
  r <- Infomax(Y, eta = 0.1, maxiter = 2000, tol = 1e-10)
  Yc <- Y - rowMeans(Y)
  expect_equal(r$sources, r$unmixing %*% Yc, tolerance = 1e-9)
  expect_lte(r$change, 1e-10)
  cm <- abs(stats::cor(t(r$sources), cbind(s1, s2)))
  expect_gt(min(apply(cm, 1, max)), 0.98)
  expect_error(Infomax(Y, eta = 2), "eta must lie")
})

test_that("VagClass: variance of segment means and the 90/10 duration rule", {
  S <- rbind(c(1, 2, 3), c(2, 2, 2), c(0, 1, 5), c(4, 4, 4))
  d <- c(2, 3, 1, 4)
  r <- VagClass(S, durations = d, segclass = c(0, 0, 0, 0))
  expect_equal(r$varmeans, stats::var(rowMeans(S)), tolerance = 1e-12)
  expect_identical(r$decision, "normal")
  expect_identical(VagClass(S, d, segclass = c(1, 1, 1, 1))$decision, "abnormal")
  mid <- VagClass(S, d, segclass = c(1, 0, 1, 0))
  expect_equal(mid$abnormalfraction, 3 / 10, tolerance = 1e-12)
  expect_identical(mid$decision, "undecided, four-group classifier required")
  s2 <- VagClass(S, d, segclass = c(1, 0, 1, 0), arthro = c(0, 0, 1, 0))
  expect_identical(c(s2$decision, s2$stage), c("normal", "2"))
  expect_identical(VagClass(S, d, segclass = c(1, 0, 1, 0), arthro = c(1, 0, 1, 0))$decision, "abnormal")
  expect_error(VagClass(S, segclass = c(0, 2, 0, 0)), "0 \\(normal\\) or 1")
  expect_error(VagClass(S, durations = c(1, 1)), "one entry per segment")
})

test_that("KsvdFit and NmfMu reconstruction identities", {
  set.seed(21)
  D0 <- matrix(stats::rnorm(4 * 8), 4, 8)
  Y <- t(vapply(1:6, function(i) as.numeric(stats::rnorm(1) * D0[(i %% 4) + 1, ] + stats::rnorm(1) * D0[((i + 1) %% 4) + 1, ]), numeric(8)))
  k <- KsvdFit(Y, natoms = 4, sparsity = 2, maxiter = 5)
  expect_equal(rowSums(k$dictionary^2), rep(1, 4), tolerance = 1e-9)
  expect_equal(k$error, sqrt(sum((Y - k$coefficients %*% k$dictionary)^2)), tolerance = 1e-9)
  expect_true(all(rowSums(k$coefficients != 0) <= 2))
  expect_error(KsvdFit(Y, 4, 5), "sparsity must satisfy")
  V <- matrix(c(1, 2, 3, 4, 2, 4, 6, 8.5, 1, 1, 2, 2, 3, 0.5, 1, 2), 4)
  for (cost in c("ls", "kld")) {
    n <- NmfMu(V, 2, maxiter = 50, cost = cost)
    expect_true(all(n$W >= 0) && all(n$H >= 0))
    expect_equal(n$error, sqrt(sum((V - n$W %*% n$H)^2)), tolerance = 1e-12)
    expect_equal(Reduce(`+`, n$submatrices), n$W %*% n$H, tolerance = 1e-12)
  }
  expect_error(NmfMu(V - 5, 2), "nonnegative")
  expect_error(NmfMu(cbind(V, 0), 2, cost = "kld"), "divergence cost is undefined")
  expect_error(NmfMu(V, 5), "rank r must satisfy")
})

test_that("Lstm recurrence and ridge readout with supplied gate weights", {
  H <- 2
  mk <- function(s) matrix(round(sin(seq_len(6) * s), 3), 2, 3)
  wts <- list(i = mk(1.1), f = mk(0.7), o = mk(1.9), g = mk(2.3), bias = rbind(c(0, 0.1), c(1, 1), c(0, -0.1), c(0.05, 0)))
  seqs <- list(c(0.5, -0.2, 1), c(1, 1), c(-0.3, 0.4, 0.2, -1), c(2), c(0.1, 0.1, 0.1))
  r <- Lstm(seqs, labels = c(0, 1, 0, 1, 0), hidden = H, weights = wts, ridge = 1e-6)
  hs <- t(vapply(seqs, function(s) {
    h <- c(0, 0)
    cc <- c(0, 0)
    for (xt in s) {
      z <- c(h, xt)
      gi <- .sig(as.numeric(wts$i %*% z) + wts$bias[1, ])
      gf <- .sig(as.numeric(wts$f %*% z) + wts$bias[2, ])
      go <- .sig(as.numeric(wts$o %*% z) + wts$bias[3, ])
      gg <- tanh(as.numeric(wts$g %*% z) + wts$bias[4, ])
      cc <- gf * cc + gi * gg
      h <- go * tanh(cc)
    }
    h
  }, numeric(2)))
  expect_equal(r$hidden, hs, tolerance = 1e-12)
  A <- cbind(hs, 1)
  rd <- t(vapply(0:1, function(k) as.numeric(solve(crossprod(A) + diag(1e-6, 3), crossprod(A, as.numeric(c(0, 1, 0, 1, 0) == k)))), numeric(3)))
  expect_equal(r$readout, rd, tolerance = 1e-9)
  expect_equal(r$predictions, (0:1)[apply(A %*% t(rd), 1, which.max)])
  expect_error(Lstm(seqs, hidden = 2, weights = list(i = mk(1))), "missing gate")
  expect_error(Lstm(list()), "non-empty list")
})

test_that("BmiDec is the Kalman recursion of eqs (8.95)-(8.99)", {
  C <- rbind(c(1, 0.5), c(0.2, 1), c(0.7, -0.3))
  A <- rbind(c(0.9, 0.1), c(0, 0.95))
  Y <- rbind(c(0.5, 0.2, 0.1), c(0.8, 0.1, 0.4), c(0.3, 0.6, -0.2), c(1, 0.9, 0.3))
  r <- BmiDec(Y, C, a = A, procnoise = 0.01, obsnoise = 0.1, p0 = 0.5)
  x <- c(0, 0)
  P <- diag(0.5, 2)
  for (t in 1:4) {
    S <- C %*% P %*% t(C) + diag(0.1, 3)
    K <- A %*% P %*% t(C) %*% solve(S)
    z <- Y[t, ] - as.numeric(C %*% x)
    expect_equal(r$states[[t]], x, tolerance = 1e-12)
    expect_equal(r$innovations[[t]], z, tolerance = 1e-12)
    expect_equal(r$gain[[t]], K, tolerance = 1e-10)
    x <- as.numeric(A %*% x + K %*% z)
    # eq (8.98): phi(n) = phi(n, n-1) - a(n, n+1) K C phi(n, n-1)
    P <- A %*% (P - solve(A) %*% K %*% C %*% P) %*% t(A) + diag(0.01, 2)
  }
  expect_error(BmiDec(Y[, 1:2], C), "3 entries")
  expect_error(BmiDec(Y, C, p0 = 0), "p0 must be positive")
})

test_that("PcaSig and MixCmp: covariance eigenbasis and reconstruction errors", {
  set.seed(31)
  X <- rbind(stats::rnorm(40), stats::rnorm(40), stats::rnorm(40))
  X[3, ] <- X[1, ] + 0.2 * X[3, ]
  p <- PcaSig(X, ncomp = 2)
  e <- eigen(stats::cov(t(X)), symmetric = TRUE)
  expect_equal(p$eigenvalues, e$values, tolerance = 1e-10)
  # cyclic Jacobi stops once the squared off-diagonal mass is <= 1e-12, so
  # eigenvectors are exact to about sqrt(1e-12) / (eigenvalue gap)
  expect_equal(abs(p$eigenvectors), abs(e$vectors), tolerance = 1e-7)
  Yc <- X - rowMeans(X)
  expect_equal(abs(p$components), abs(t(e$vectors[, 1:2]) %*% Yc), tolerance = 1e-7)
  expect_equal(p$mse, e$values[3], tolerance = 1e-10)
  expect_equal(p$varexplained, e$values[1:2] / sum(e$values), tolerance = 1e-10)
  m <- MixCmp(X, ncomp = 2, maxiter = 50)
  rp <- p$eigenvectors[, 1:2] %*% p$components + rowMeans(X)
  expect_equal(m$error$pca, sqrt(sum((X - rp)^2)) / sqrt(sum(X^2)), tolerance = 1e-12)
  expect_identical(m$best, names(m$error)[which.min(unlist(m$error))])
  expect_error(PcaSig(X, ncomp = 4), "ncomp must satisfy")
  expect_error(MixCmp(0 * X), "zero energy")
})

test_that("Rbfn: Gaussian design with the log(2) half-response width, ridge solve", {
  X <- cbind(c(0, 1, 2, 3, 4, 5), c(1, 0, 1, 0, 1, 0))
  y <- c(0.1, 0.9, 0.2, 1.1, 0.3, 0.8)
  cs <- X[c(2, 5), ]
  r <- Rbfn(X, y, spread = 1.5, centers = cs, query = rbind(c(2.5, 0.5)))
  phi <- function(a, c) exp(-log(2) * sum((a - c)^2) / 1.5^2)
  A <- cbind(t(apply(X, 1, function(a) c(phi(a, cs[1, ]), phi(a, cs[2, ])))), 1)
  w <- as.numeric(solve(crossprod(A) + diag(1e-8, 3), crossprod(A, y)))
  expect_equal(c(r$weights, r$bias), w, tolerance = 1e-9)
  expect_equal(r$predictions, as.numeric(A %*% w), tolerance = 1e-9)
  expect_equal(r$queryvalues, sum(w[1:2] * c(phi(c(2.5, 0.5), cs[1, ]), phi(c(2.5, 0.5), cs[2, ]))) + w[3], tolerance = 1e-9)
  expect_equal(phi(c(1.5, 0), c(0, 0)), 0.5, tolerance = 1e-12)
  g <- Rbfn(X, y, ncenters = 3)
  expect_identical(nrow(g$centers), 3L)
  expect_true(all(apply(g$centers, 1, function(cc) any(apply(X, 1, function(xx) all(xx == cc))))))
  expect_equal(g$mse, mean((y - g$predictions)^2), tolerance = 1e-12)
  expect_error(Rbfn(X, y, spread = 0), "spread must be positive")
  expect_error(Rbfn(X, y, ncenters = 9), "ncenters must satisfy")
})

test_that("Ahi scores sub-threshold airflow episodes of at least minsec", {
  fs <- 4
  tt <- (0:(4 * 300 - 1)) / fs
  air <- sin(2 * pi * tt / 4)
  air[tt >= 60 & tt < 75] <- 0.02 * air[tt >= 60 & tt < 75]
  air[tt >= 150 & tt < 162] <- 0.3 * air[tt >= 150 & tt < 162]
  air[tt >= 200 & tt < 205] <- 0
  r <- Ahi(air, fs, envsec = 1)
  w <- 4
  env <- vapply(seq_along(air), function(i) max(abs(air[max(1, i - w):min(length(air), i + w)])), 1)
  base <- sort(env)[as.integer(0.75 * (length(air) - 1)) + 1]
  expect_equal(r$baseline, base, tolerance = 1e-12)
  low <- env < 0.5 * base
  runs <- rle(low)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  ev <- which(runs$values & runs$lengths >= 40)
  expect_identical(length(r$events), length(ev))
  kinds <- vapply(ev, function(k) if (min(env[starts[k]:ends[k]]) < 0.1 * base) "apnea" else "hypopnea", "")
  expect_identical(vapply(r$events, function(e) e$kind, ""), kinds)
  expect_equal(r$ahi, length(ev) / (length(air) / fs / 3600), tolerance = 1e-12)
  idx <- length(ev) / (length(air) / fs / 3600)
  expect_identical(r$severity, if (idx < 5) "normal" else if (idx < 15) "mild" else if (idx < 30) "moderate" else "severe")
  ox <- rep(97, length(air))
  ox[tt >= 62 & tt < 76] <- 93
  r2 <- Ahi(air, fs, spo2 = ox, desat = 3, hours = 1)
  expect_identical(length(r2$events), 1L)
  expect_equal(r2$ahi, 1, tolerance = 1e-12)
  expect_identical(r2$severity, "normal")
  expect_error(Ahi(air, fs, apneafrac = 0.6), "0 < apneafrac < hypofrac")
  expect_error(Ahi(air, fs, spo2 = ox[-1]), "same length")
})

test_that("VagTfd is the cross-term-free MP Wigner sum with its EP/ESP/FP/FSP features", {
  x <- sin(2 * pi * 3 * (0:47) / 48) * exp(-((0:47) - 24)^2 / 200)
  r <- VagTfd(x, fs = 100, natoms = 3, nfreq = 8, ntime = 6, lag = 4)
  mp <- MPursuit(x, natoms = 3)
  tid <- as.integer(round((0:5) * 47 / 5))
  tfd <- matrix(0, 6, 8)
  for (a in seq_len(nrow(mp$atoms))) {
    g <- mp$atoms[a, ]
    for (ti in 1:6) {
      m <- -4:4
      p <- tid[ti] + m
      q <- tid[ti] - m
      ok <- p >= 0 & p < 48 & q >= 0 & q < 48
      pr <- ifelse(ok, g[pmin(pmax(p, 0), 47) + 1] * g[pmin(pmax(q, 0), 47) + 1], 0)
      for (fi in 1:8) tfd[ti, fi] <- tfd[ti, fi] + mp$coefficients[a]^2 * 2 * sum(pr * cos(-2 * pi * (fi - 1) / 16 * 2 * m))
    }
  }
  expect_equal(r$tfd, tfd, tolerance = 1e-12)
  fr <- (0:7) * 100 / 16
  expect_equal(r$ep, rowMeans(tfd), tolerance = 1e-12)
  expect_equal(r$esp, sqrt(rowMeans((tfd - rowMeans(tfd))^2)), tolerance = 1e-12)
  pos <- pmax(tfd, 0)
  f1 <- as.numeric(pos %*% fr) / rowSums(pos)
  expect_equal(r$fp, f1, tolerance = 1e-12)
  expect_equal(r$times, tid / 100, tolerance = 1e-12)
  expect_error(VagTfd(x, 100, ntime = 49), "ntime must satisfy")
  expect_error(VagTfd(x, 100, nfreq = 1), "nfreq >= 2")
})
