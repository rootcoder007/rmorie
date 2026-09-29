# Coverage for the causal-inference files (caus*.R, cssant/boryis,
# causal_forest_honest.R): estimates are rebuilt with lm/anova/integrate,
# checked against did / synthdid where they implement the same estimand,
# and solvers are checked at their optimality conditions.

cz_x <- c(0.3, -1.2, 0.8, 1.5, -0.4, 0.9, -0.7, 1.1, 0.2, -1.5, 0.6, -0.1, 1.3, -0.9,
          0.45, -0.3, 1.7, -1.1, 0.05, 0.75)
cz_z <- c(1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1)
cz_d <- c(1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1)
cz_y <- 1 + 1.5 * cz_d + 0.7 * cz_x + c(0.2, -0.3, 0.1, 0.4, -0.2, 0.3, -0.1, 0.25, -0.35,
  0.15, -0.05, 0.1, -0.2, 0.3, 0.12, -0.18, 0.22, -0.08, 0.05, -0.15)

test_that("Causbckd is stratified backdoor adjustment", {
  s <- rep(c("a", "b"), 10)
  r <- Causbckd(cz_y, cz_d, s)
  eff <- vapply(c("a", "b"), function(k) mean(cz_y[s == k & cz_d == 1]) -
                  mean(cz_y[s == k & cz_d == 0]), 0)
  v <- vapply(c("a", "b"), function(k) var(cz_y[s == k & cz_d == 1]) / sum(s == k & cz_d == 1) +
                var(cz_y[s == k & cz_d == 0]) / sum(s == k & cz_d == 0), 0)
  expect_equal(r$estimate, mean(eff), tolerance = 1e-12)
  expect_equal(r$se, sqrt(sum(0.25 * v)), tolerance = 1e-12)
  expect_error(Causbckd(cz_y, cz_d[-1], s), "equal length")
  expect_error(Causbckd(cz_y, cz_d + 1, s), "binary")
  expect_error(Causbckd(cz_y, cz_d, ifelse(cz_d == 1, "t", "c")), "positivity")
})

test_that("Causdr2 is cross-fitted partialling-out DML", {
  X <- cbind(cz_x, cz_x^2)
  r <- Causdr2(cz_y, cz_d, X, K = 2L, seed = 3L)
  folds <- integer(20)
  withr::with_seed(3L, {
    perm <- sample.int(20)
  })
  folds[perm] <- (0:19) %% 2
  lh <- mh <- numeric(20)
  for (k in 0:1) {
    tr <- folds != k
    lh[!tr] <- as.numeric(cbind(1, X[!tr, ]) %*% coef(lm(cz_y[tr] ~ X[tr, ])))
    mh[!tr] <- as.numeric(cbind(1, X[!tr, ]) %*% coef(lm(cz_d[tr] ~ X[tr, ])))
  }
  v <- cz_d - mh
  th <- sum(v * (cz_y - lh)) / sum(v^2)
  psi <- (cz_y - lh - th * v) * v
  expect_equal(r$folds, folds + 1L)
  expect_equal(r$estimate, th, tolerance = 1e-10)
  expect_equal(r$se, sqrt(mean(psi^2) / (sum(v^2) / 20)^2 / 20), tolerance = 1e-10)
  one <- Causdr2(cz_y, cz_d, X, K = 1L)
  vv <- resid(lm(cz_d ~ X))
  expect_equal(one$estimate, sum(vv * resid(lm(cz_y ~ X))) / sum(vv^2), tolerance = 1e-10)
  expect_error(Causdr2(cz_y, cz_d, X[-1, ]), "matching")
  expect_error(Causdr2(cz_y, cz_d, X, K = 0L), "1..n")
  expect_error(Causdr2(cz_y, cz_d, cbind(1, X)), "constant")
})

test_that("Causgsw, Caustrnsp and Causipw", {
  ss <- c(0.2, 0.35, 0.5, 0.4, 0.3)
  st <- c(0.1, 0.25, 0.3, 0.15, 0.2, 0.45)
  g <- Causgsw(ss, st)
  expect_equal(g$estimate, (mean(qlogis(ss)) - mean(qlogis(st))) / sd(qlogis(st)),
               tolerance = 1e-12)
  expect_error(Causgsw(ss, 0.5), "need at least")
  expect_error(Causgsw(c(0, 0.5), st), "strictly")
  expect_error(Causgsw(ss, c(0.2, 0.2)), "constant")
  sv <- 0.2 + 0.03 * (1:20)
  tr <- Caustrnsp(cz_y, cz_d, sv, pr_w0 = 2)
  w <- (1 - sv) / sv / 2
  expect_equal(tr$estimate, weighted.mean(cz_y[cz_d == 1], w[cz_d == 1]) -
                 weighted.mean(cz_y[cz_d == 0], w[cz_d == 0]), tolerance = 1e-12)
  gz <- Caustrnsp(cz_y, cz_d, sv, mode = "generalize")
  expect_equal(gz$weights, 1 / sv, tolerance = 1e-12)
  expect_error(Caustrnsp(cz_y, cz_d[-1], sv), "equal length")
  expect_error(Caustrnsp(cz_y, cz_d + 1, sv), "binary")
  expect_error(Caustrnsp(cz_y, cz_d, sv + 1), "strictly")
  expect_error(Caustrnsp(cz_y, cz_d, sv, mode = "x"), "transport or generalize")
  expect_error(Caustrnsp(cz_y, cz_d, sv, pr_w0 = 0), "positive")
  expect_error(Caustrnsp(cz_y, rep(1, 20), sv), "both treatment arms")
  ps <- plogis(0.4 * cz_x + 0.1 * (1:20) / 20 - 0.2)
  ip <- Causipw(cz_d, cz_y, ps, alpha = 0.3)
  k <- ps >= 0.3 & ps <= 0.7
  expect_equal(ip$estimate, weighted.mean(cz_y[k & cz_d == 1], 1 / ps[k & cz_d == 1]) -
                 weighted.mean(cz_y[k & cz_d == 0], 1 / (1 - ps[k & cz_d == 0])), tolerance = 1e-12)
  # Crump et al. (2009): no trimming when max 1/(e(1-e)) <= 2 mean
  kk <- 1 / (ps * (1 - ps))
  if (max(kk) <= 2 * mean(kk)) expect_equal(Causipw(cz_d, cz_y, ps, alpha = NULL)$alpha, 0)
  ext <- c(ps[1:18], 0.01, 0.99)
  ke <- sort(1 / (ext * (1 - ext)))
  gam <- NA
  for (j in 1:20) if (ke[j] <= 2 * sum(ke[1:j]) / j) gam <- 2 * sum(ke[1:j]) / j
  expect_equal(Causipw(cz_d, cz_y, ext, alpha = NULL)$alpha, 0.5 - sqrt(0.25 - 1 / gam),
               tolerance = 1e-12)
  expect_error(Causipw(cz_d, cz_y[-1], ps), "equal length")
  expect_error(Causipw(cz_d + 1, cz_y, ps), "binary")
  expect_error(Causipw(cz_d, cz_y, ps + 1), "strictly")
  expect_error(Causipw(cz_d, cz_y, ps, alpha = 0.5), "0 <= alpha < 0.5")
  expect_error(Causipw(cz_d, cz_y, ps, alpha = 0.49), "entire treatment arm")
})

test_that("weak-IV tests: first-stage F, Anderson-Rubin and CLR", {
  Z <- cbind(cz_z, cz_x^2)
  w <- cz_x
  fs <- Ivfstage(cz_d, Z, X_exog = w)
  a <- anova(lm(cz_d ~ w), lm(cz_d ~ w + Z))
  expect_equal(fs$statistic, a$F[2], tolerance = 1e-10)
  expect_equal(fs$p_value, a$`Pr(>F)`[2], tolerance = 1e-10)
  f0 <- Ivfstage(cz_d, cz_z, add_intercept = FALSE)
  a0 <- anova(lm(cz_d ~ 0), lm(cz_d ~ 0 + cz_z))
  expect_equal(f0$statistic, a0$F[2], tolerance = 1e-10)
  expect_error(Ivfstage(cz_d[1:2], cz_z[1:2]), "n > k \\+ L")
  ar <- Ivartest(cz_y, cz_d, Z, beta0 = 1.2, X_exog = w)
  tmp <- cz_y - 1.2 * cz_d
  aa <- anova(lm(tmp ~ w), lm(tmp ~ w + Z))
  expect_equal(ar$statistic, aa$F[2], tolerance = 1e-10)
  expect_error(Ivartest(cz_y[1:2], cz_d[1:2], cz_z[1:2]), "n > k \\+ L")
  cl <- Ivclr(cz_y, cz_d, Z, beta0 = 1.2, X_exog = w)
  C <- cbind(1, w)
  po <- function(M) M - C %*% qr.solve(C, M)
  YD <- po(cbind(cz_y, cz_d))
  Za <- po(Z)
  PZ <- Za %*% solve(crossprod(Za), crossprod(Za, YD))
  S <- crossprod(YD - PZ) / (20 - 2 - 2)
  b0 <- c(1, -1.2)
  a0v <- c(1.2, 1)
  qs <- sum((PZ %*% b0)^2) / drop(t(b0) %*% S %*% b0)
  qt <- sum((PZ %*% solve(S, a0v))^2) / drop(t(a0v) %*% solve(S, a0v))
  qts <- sum((PZ %*% b0) * (PZ %*% solve(S, a0v))) /
    sqrt(drop(t(b0) %*% S %*% b0) * drop(t(a0v) %*% solve(S, a0v)))
  lr <- 0.5 * (qs - qt + sqrt((qs + qt)^2 - 4 * (qs * qt - qts^2)))
  expect_equal(c(cl$QS, cl$QT, cl$statistic), c(qs, qt, lr), tolerance = 1e-9)
  K <- gamma(1) / (sqrt(pi) * gamma(0.5))
  pint <- stats::integrate(function(t) pchisq((qt + lr) / (1 + qt * sin(t)^2 / lr), 2) *
                             cos(t)^0, 0, pi / 2, rel.tol = 1e-12)$value
  expect_equal(cl$p_value, 1 - 2 * K * pint, tolerance = 1e-9)
  expect_equal(Morieclrp(2.5, 1.3, 1, 15), pf(2.5, 1, 15, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(Morieclrp(0, 1, 3, 15), 1)
  p3 <- stats::integrate(function(t) pchisq((1.3 + 2.5) / (1 + 1.3 * sin(t)^2 / 2.5), 3) *
                           cos(t), 0, pi / 2, rel.tol = 1e-12)$value
  K3 <- gamma(1.5) / (sqrt(pi) * gamma(1))
  expect_equal(Morieclrp(2.5, 1.3, 3, 15), 1 - 2 * K3 * p3, tolerance = 1e-9)
  expect_equal(Ivclr(cz_y, cz_d, cz_z)$p_value,
               pf(Ivclr(cz_y, cz_d, cz_z)$statistic, 1, 18, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(Ivclr(cz_y[1:2], cz_d[1:2], cz_z[1:2]), "n > k \\+ L")
})

test_that("Causmnde is the four-way mediation decomposition", {
  M <- 0.5 + 0.8 * cz_d + 0.3 * cz_x + c(0.1, -0.2, 0.15, -0.05)[(1:20) %% 4 + 1]
  Y <- cz_y + 0.6 * M + 0.4 * cz_d * M
  r <- Causmnde(cz_d, M, Y, Cc = cz_x, a = 1, astar = 0, m = 0.2)
  th <- coef(lm(Y ~ cz_d + M + I(cz_d * M) + cz_x))
  be <- coef(lm(M ~ cz_d + cz_x))
  bc <- be[1] + be[3] * mean(cz_x)
  cde <- th[2] + th[4] * 0.2
  intref <- th[4] * (bc - 0.2)
  intmed <- th[4] * be[2]
  pie <- th[3] * be[2]
  expect_equal(unname(c(r$cde, r$intref, r$intmed, r$PIE)),
               unname(c(cde, intref, intmed, pie)), tolerance = 1e-9)
  expect_equal(unname(r$NDE), unname(cde + intref), tolerance = 1e-9)
  expect_equal(unname(r$estimate), unname(cde + intref + intmed + pie), tolerance = 1e-9)
})

rd_x <- c(-0.95, -0.81, -0.66, -0.52, -0.41, -0.33, -0.25, -0.18, -0.11, -0.05,
          0.02, 0.09, 0.15, 0.22, 0.31, 0.38, 0.47, 0.58, 0.71, 0.86, -0.72, 0.64,
          -0.28, 0.27, -0.07, 0.11)
rd_y <- 1 + 0.8 * rd_x + 0.9 * rd_x^2 + 0.5 * (rd_x >= 0) +
  0.05 * sin(7 * seq_along(rd_x))
rd_t <- as.numeric(rd_x >= 0)
rd_t[c(3, 12)] <- 1 - rd_t[c(3, 12)]

rdd_side <- function(d, y, w) {
  f <- lm(y ~ d, weights = w)
  X <- cbind(1, d)
  A <- solve(crossprod(X * w, X))
  meat <- crossprod(X * (w * resid(f)))
  list(a = unname(coef(f)[1]), v = (A %*% meat %*% A)[1, 1])
}

test_that("morie_causrdd, morie_causrddf: local-linear sharp and fuzzy RDD", {
  h <- 0.6
  r <- morie_causrdd(rd_x, rd_y, h = h)
  w <- pmax(1 - abs(rd_x) / h, 0)
  l <- rd_x < 0 & w > 0
  rr <- rd_x >= 0 & w > 0
  L <- rdd_side(rd_x[l], rd_y[l], w[l])
  R <- rdd_side(rd_x[rr], rd_y[rr], w[rr])
  expect_equal(r$estimate, R$a - L$a, tolerance = 1e-10)
  expect_equal(r$se, sqrt(L$v + R$v), tolerance = 1e-10)
  u <- morie_causrdd(rd_x, rd_y, h = h, kernel = "uniform")
  lu <- rd_x < 0 & abs(rd_x) <= h
  ru <- rd_x >= 0 & abs(rd_x) <= h
  expect_equal(u$estimate, unname(coef(lm(rd_y[ru] ~ rd_x[ru]))[1] - coef(lm(rd_y[lu] ~ rd_x[lu]))[1]),
               tolerance = 1e-10)
  expect_error(morie_causrdd(rd_x, rd_y, h = -1), "positive")
  expect_error(morie_causrdd(rd_x, rd_y, h = 0.03), "fewer than 3")
  expect_error(morie_causrdd(rd_x, rd_y, h = 1, kernel = "x"), "kernel must")
  fz <- morie_causrddf(rd_x, rd_y, rd_t, h = h, h_treat = 0.8)
  ft <- morie_causrdd(rd_x, rd_t, h = 0.8)
  expect_equal(fz$estimate, r$estimate / ft$estimate, tolerance = 1e-12)
  expect_equal(fz$se, sqrt((r$se^2 + fz$estimate^2 * ft$se^2) / ft$estimate^2), tolerance = 1e-12)
  expect_error(morie_causrddf(rd_x, rd_y, rep(1, 26), h = h, h_treat = h), "first-stage")
})

test_that("morie_causrddh follows the Imbens-Kalyanaraman algorithm", {
  r <- morie_causrddh(rd_x, rd_y)
  n <- 26
  h1 <- 1.84 * sd(rd_x) * n^-0.2
  il <- rd_x >= -h1 & rd_x < 0
  ir <- rd_x >= 0 & rd_x <= h1
  f <- (sum(il) + sum(ir)) / (2 * n * h1)
  s2 <- ((sum(il) - 1) * var(rd_y[il]) + (sum(ir) - 1) * var(rd_y[ir])) / (sum(il) + sum(ir))
  expect_equal(c(r$h1, r$f_hat, r$sigma2), c(h1, f, s2), tolerance = 1e-12)
  kp <- rd_x >= median(rd_x[rd_x < 0]) & rd_x <= median(rd_x[rd_x >= 0])
  dk <- rd_x[kp]
  m3 <- 6 * unname(coef(lm(rd_y[kp] ~ I(dk >= 0) + dk + I(dk^2) + I(dk^3)))[5])
  expect_equal(r$m3, m3, tolerance = 1e-9)
  base <- (s2 / (f * max(m3^2, 0.01)))^(1 / 7)
  h2r <- 3.56 * base * sum(rd_x >= 0)^(-1 / 7)
  h2l <- 3.56 * base * sum(rd_x < 0)^(-1 / 7)
  sr <- rd_x >= 0 & rd_x <= h2r
  sl <- rd_x < 0 & rd_x >= -h2l
  m2r <- 2 * unname(coef(lm(rd_y[sr] ~ rd_x[sr] + I(rd_x[sr]^2)))[3])
  m2l <- 2 * unname(coef(lm(rd_y[sl] ~ rd_x[sl] + I(rd_x[sl]^2)))[3])
  n2r <- max(sum(sr & rd_x <= median(rd_x[rd_x >= 0])), 1)
  n2l <- max(sum(sl & rd_x >= median(rd_x[rd_x < 0])), 1)
  rreg <- 720 * s2 / (n2r * h2r^4) + 720 * s2 / (n2l * h2l^4)
  expect_equal(r$estimate, 3.4375 * (2 * s2 / (f * ((m2r - m2l)^2 + rreg)))^0.2 * n^-0.2,
               tolerance = 1e-9)
  expect_error(morie_causrddh(1:5, 1:5), "at least 10")
  expect_error(morie_causrddh(c(rd_x, 5), c(rd_y, 1), cutoff = 0.9), "fewer than 3")
})

test_that("morie_causrddc_rd_bandwidth is the CCT MSE-optimal plug-in", {
  r <- morie_causrddc_rd_bandwidth(rd_x, rd_y, nu = 0, p = 1)
  dr <- function(side) {
    s <- if (side > 0) rd_x >= 0 else rd_x < 0
    f <- lm(rd_y[s] ~ rd_x[s] + I(rd_x[s]^2) + I(rd_x[s]^3))
    c(unname(coef(f)[3]) * 2, sum(resid(f)^2) / (sum(s) - 4))
  }
  p <- dr(1)
  m <- dr(-1)
  expect_equal(c(r$mu_plus, r$mu_minus), c(p[1], m[1]), tolerance = 1e-9)
  G <- outer(0:1, 0:1, function(i, j) 1 / ((i + j + 1) * (i + j + 2)))
  th <- 1 / ((0:1 + 3) * (0:1 + 4))
  P <- outer(0:1, 0:1, function(i, j) 2 / ((i + j + 1) * (i + j + 2) * (i + j + 3)))
  ge <- solve(G, c(1, 0))
  B <- (p[1] - m[1]) / 2 * sum(ge * th)
  hs <- 1.06 * sd(rd_x) * 26^-0.2
  fd <- sum(pmax(0.75 * (1 - (rd_x / hs)^2), 0)) / (26 * hs)
  V <- (p[2] + m[2]) * sum(ge * (P %*% ge)) / fd
  expect_equal(c(r$B, r$f), c(B, fd), tolerance = 1e-9)
  expect_equal(r$V, V, tolerance = 1e-9)
  C <- (V / (4 * B^2))^(1 / 5)
  expect_equal(r$h_unclamped, C * 26^(-1 / 5), tolerance = 1e-9)
  expect_equal(r$h, min(C * 26^(-1 / 5), 0.95), tolerance = 1e-9)
  expect_type(morie_causrddc_cheatsheet(), "character")
  expect_match(morie_causrddc_cheatsheet(), "Calonico")
  expect_error(morie_causrddc_rd_bandwidth(rd_x, rd_y[-1]), "same length")
  expect_error(morie_causrddc_rd_bandwidth(rd_x, rd_y, kernel = "x"), "kernel must")
  expect_error(morie_causrddc_rd_bandwidth(rd_x, rd_y, nu = 2), "0 <= nu <= p")
  expect_error(morie_causrddc_rd_bandwidth(c(rd_x[rd_x < 0], 0.1, 0.2), c(rd_y[rd_x < 0], 1, 2)),
               "too few observations")
})

test_that("Rddmanip is the McCrary density test", {
  x <- c(rd_x, rd_x / 2 + 0.03, rd_x^3)
  n <- length(x)
  b <- 0.1
  bw <- 0.5
  r <- Rddmanip(x, cutoff = 0, bw = bw, binsize = b)
  k <- floor(x / b)
  kmin <- floor(min(x) / b)
  j <- floor((max(x) - min(x)) / b) + 2
  pad <- ceiling(bw / b)
  ks <- (kmin - pad):(kmin + j - 1 + pad)
  cnt <- vapply(ks, function(v) sum(k == v), 0)
  inrange <- ks >= kmin & ks <= kmin + j - 1
  cval <- ifelse(inrange, cnt, 0) / n / b
  mp <- ks * b + b / 2
  fit <- function(side) {
    w <- pmax(1 - abs(mp / bw), 0) * (if (side < 0) mp < 0 else mp >= 0)
    unname(coef(lm(cval ~ mp, weights = w))[1])
  }
  fl <- fit(-1)
  fr <- fit(1)
  expect_equal(c(r$fhat_left, r$fhat_right), c(fl, fr), tolerance = 1e-9)
  se <- sqrt(1 / (n * bw) * 24 / 5 * (1 / fr + 1 / fl))
  expect_equal(r$estimate, log(fr) - log(fl), tolerance = 1e-9)
  expect_equal(r$p_value, 2 * pnorm(-abs((log(fr) - log(fl)) / se)), tolerance = 1e-9)
  auto <- Rddmanip(x, cutoff = 0)
  expect_equal(auto$binsize, 2 * sd(x) * n^-0.5, tolerance = 1e-12)
  expect_gt(auto$bw, 0)
  expect_error(Rddmanip(1:5), "at least 20")
  expect_error(Rddmanip(x, cutoff = 5), "strictly within")
})

cp_unit <- rep(c("a", "b", "c", "d", "e", "f"), each = 4)
cp_time <- rep(1:4, 6)
cp_first <- rep(c(3, 3, 4, Inf, Inf, 2), each = 4)
cp_D <- as.numeric(cp_time >= cp_first)
cp_y <- rep(c(1, 1.5, 0.8, 2, 1.2, 0.6), each = 4) + 0.5 * cp_time +
  ifelse(cp_D == 1, 1 + cp_time - cp_first, 0) + 0.1 * cos(seq_along(cp_time))

test_that("Callaway-Sant'Anna group-time ATTs and aggregation", {
  r <- morie_cssant(cp_y, cp_D, cp_unit, cp_time)
  Y <- matrix(cp_y, 6, byrow = TRUE)
  g <- c(2, 2, 3, Inf, Inf, 1)
  gt <- morie_grouptimeatt(Y, g)
  att <- function(gg, t, ctrl) {
    dY <- Y[, t + 1] - Y[, gg]
    mean(dY[g == gg]) - mean(dY[ctrl])
  }
  expect_equal(gt[["2|2"]]$att, att(2, 2, g > 2), tolerance = 1e-12)
  expect_equal(gt[["1|3"]]$att, att(1, 3, g > 3), tolerance = 1e-12)
  expect_equal(gt[["2|0"]]$att, att(2, 0, g > 2), tolerance = 1e-12)
  expect_true(is.null(gt[["3|2"]]))
  expect_true(is.null(gt[["1|0"]]))
  ag <- morie_aggregateatt(gt, g, 6)
  post <- names(gt)[vapply(gt, function(v) isTRUE(v$post), TRUE)]
  sz <- vapply(post, function(k) sum(g == gt[[k]]$gg), 0)
  ov <- sum(sz / sum(sz) * vapply(post, function(k) gt[[k]]$att, 0))
  expect_equal(ag$overall, ov, tolerance = 1e-12)
  infl <- Reduce(`+`, lapply(seq_along(post), function(i) sz[i] / sum(sz) * gt[[post[i]]]$infl))
  expect_equal(ag$overall_se, sqrt(sum(infl^2)) / 6, tolerance = 1e-12)
  expect_equal(unname(ag$cohort[["2"]][1]),
               mean(vapply(post[grepl("^2\\|", post)], function(k) gt[[k]]$att, 0)), tolerance = 1e-12)
  expect_equal(morie_aggregateatt(gt["2|0"], g, 6), list())
  eq <- morie_aggregateatt(gt, g, 6, weights_by = "equal")
  expect_equal(eq$overall, mean(vapply(post, function(k) gt[[k]]$att, 0)), tolerance = 1e-12)
  expect_equal(r$estimate, ov, tolerance = 1e-12)
  expect_equal(r$se, ag$overall_se, tolerance = 1e-12)
  nv <- morie_cssant(cp_y, cp_D, cp_unit, cp_time, control = "never")
  expect_equal(nv$att_gt[["2|2"]], att(2, 2, !is.finite(g)), tolerance = 1e-12)
  ch <- morie_cssant(cp_y, cp_D, cp_unit, cp_time, cohort = cp_first)
  expect_equal(ch$estimate, r$estimate, tolerance = 1e-12)
  expect_equal(morie_causdidcs(cp_y, cp_D, cp_unit, cp_time)$estimate, r$estimate)
  skip_if_not_installed("did")
  dd <- data.frame(y = cp_y, id = match(cp_unit, unique(cp_unit)), t = cp_time,
                   G = ifelse(is.finite(cp_first), cp_first, 0))
  ref <- suppressWarnings(did::att_gt("y", "t", "id", "G", data = dd, control_group = "notyettreated",
                                      base_period = "universal", bstrap = FALSE, cband = FALSE))
  k <- ref$group == 3 & ref$t == 3
  expect_equal(ref$att[k], gt[["2|2"]]$att, tolerance = 1e-10)
  expect_error(morie_cssant(cp_y, cp_D, cp_unit, cp_time, control = "x"), "notyet")
  expect_error(morie_cssant(cp_y, cp_D * 0, cp_unit, cp_time), "ever treated")
  expect_error(morie_cssant(cp_y[-1], cp_D[-1], cp_unit[-1], cp_time[-1]), "unbalanced")
  expect_error(morie_cssant(cp_y, rev(cp_D), cp_unit, cp_time), "absorbing")
})

test_that("Borusyak-Jaravel-Spiess imputation", {
  r <- morie_boryis(cp_y, cp_D, cp_unit, cp_time)
  un <- cp_D == 0
  fe <- lm(cp_y ~ factor(cp_unit) + factor(cp_time), subset = un)
  y0 <- predict(fe, data.frame(cp_unit = cp_unit, cp_time = cp_time))
  Y0 <- matrix(y0, 6, byrow = TRUE)
  expect_equal(r$imputed_y0, Y0, tolerance = 1e-9)
  tau <- matrix(cp_y, 6, byrow = TRUE) - Y0
  tr <- matrix(cp_D, 6, byrow = TRUE) == 1
  expect_equal(r$estimate, mean(tau[tr]), tolerance = 1e-9)
  expect_lt(abs(r$linearity_residual), 1e-9)
  co <- morie_boryis(cp_y, cp_D, cp_unit, cp_time, weights = "cohort")
  W <- tr * c(2, 2, 1, 0, 0, 1)[row(tr)] / 6
  expect_equal(co$estimate, sum(W * tau) / sum(W), tolerance = 1e-9)
  im <- morie_impute_untreated(matrix(cp_y, 6, byrow = TRUE), tr)
  expect_equal(im$Y0, Y0, tolerance = 1e-9)
  xc <- cos(1:24)
  rx <- morie_boryis(cp_y, cp_D, cp_unit, cp_time, X = xc)
  fx <- lm(cp_y ~ factor(cp_unit) + factor(cp_time) + xc, subset = un)
  expect_equal(rx$covariate_coef, unname(coef(fx)["xc"]), tolerance = 1e-8)
  expect_equal(morie_causdidev(cp_y, cp_D, cp_unit, cp_time)$estimate, r$estimate)
  expect_error(morie_boryis(cp_y, cp_D * 0, cp_unit, cp_time), "no observation is treated")
  expect_error(morie_boryis(cp_y, cp_D, cp_unit, cp_time, weights = rep(0, 24)), "no mass")
  expect_error(morie_impute_untreated(matrix(1, 2, 2), matrix(TRUE, 2, 2)), "not identified")
  expect_error(morie_impute_untreated(matrix(1, 3, 3), matrix(c(TRUE, FALSE, FALSE), 3, 3)),
               "treated in every period")
})

test_that("synthetic DiD: DID weighting matches synthdid, SDID weights are optimal", {
  Y <- lapply(1:6, function(i) 1 + 0.3 * i + 0.4 * (1:5) + 0.2 * sin(i * (1:5)) +
                (i >= 5) * c(0, 0, 0, 1, 1.2))
  tr <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  d <- sdid(Y, tr, 3, method = "did")
  M <- do.call(rbind, Y)
  dref <- mean(M[5:6, 4:5]) - mean(M[5:6, 1:3]) - (mean(M[1:4, 4:5]) - mean(M[1:4, 1:3]))
  expect_equal(d$tau, dref, tolerance = 1e-12)
  skip_if_not_installed("synthdid")
  expect_equal(d$tau, as.numeric(synthdid::did_estimate(M, 4, 3)), tolerance = 1e-10)
  s <- morie_causscd(Y, tr, 3)
  om <- s$unit_weights[1:4]
  expect_equal(sum(om), 1, tolerance = 1e-12)
  target <- colMeans(M[5:6, 1:3])
  cols <- M[1:4, 1:3]
  diffs <- as.numeric(t(apply(M[1:4, 1:3], 1, diff)))
  zeta <- (2 * 2)^0.25 * sd(diffs)
  expect_equal(s$zeta, zeta, tolerance = 1e-12)
  fit <- as.numeric(om %*% cols)
  res <- mean(target - fit) + fit - target
  g <- 2 * as.numeric(cols %*% res) + 2 * zeta^2 * 3 * om
  on <- om > 1e-8
  # projected gradient to a 1e-12 step: the active gradients agree to 1e-6
  expect_lt(diff(range(g[on])), 1e-6)
  if (any(!on)) expect_true(all(g[!on] >= min(g[on]) - 1e-6))
  lam <- s$time_weights[1:3]
  tgt <- rowMeans(M[1:4, 4:5])
  fl <- as.numeric(M[1:4, 1:3] %*% lam)
  rl <- mean(tgt - fl) + fl - tgt
  gl <- 2 * as.numeric(crossprod(M[1:4, 1:3], rl))
  ol <- lam > 1e-8
  expect_lt(diff(range(gl[ol])), 1e-6)
  delta <- rowMeans(M[, 4:5]) - as.numeric(M[, 1:3] %*% lam)
  expect_equal(s$tau, mean(delta[5:6]) - sum(s$unit_weights * delta), tolerance = 1e-12)
  expect_equal(s$did, d$tau, tolerance = 1e-12)
  expect_same_function(causal_synthetic_did, unit_weights)
  expect_equal(causscd(Y, tr, 3)$sdid, s$tau)
  expect_error(sdid(Y, tr, 3, method = "x"), "method must be")
  expect_error(sdid(Y[1], tr[1], 3), "two units")
  expect_error(sdid(Y, tr, 5), "t_post must lie")
  expect_error(sdid(Y, rep(FALSE, 6), 3), "no treated")
  expect_error(sdid(Y, rep(TRUE, 6), 3), "no control")
  expect_error(sdid(c(Y[-1], list(1:3)), tr, 3), "ragged")
  expect_error(sdid(Y, tr[-1], 3), "one flag per unit")
})

test_that("morie_causal_forest_predict averages honest-tree leaf effects", {
  set <- 1:80
  X <- cbind(sin(set), cos(set / 3))
  dd <- set %% 2
  yy <- X[, 1] + dd * (1 + X[, 2]) + 0.1 * cos(set)
  f <- morie_causal_forest(yy, dd, X, n_trees = 5L, min_leaf = 5L, max_depth = 3L, seed = 2L)
  walk <- function(nd, x) {
    while (!is.na(nd$feature)) nd <- if (x[nd$feature] <= nd$threshold) nd$left else nd$right
    nd$tau
  }
  nx <- rbind(c(0.2, -0.5), c(-0.8, 0.9))
  p <- morie_causal_forest_predict(f$forest, nx)
  ref <- apply(nx, 1, function(x) mean(vapply(f$forest$trees, walk, 0, x = x)))
  expect_equal(p, ref, tolerance = 1e-12)
  expect_equal(morie_causal_forest_predict(f$forest, X), f$cate, tolerance = 1e-12)
  expect_error(morie_causal_forest_predict(f$forest, matrix(1, 1, 3)), "same number of columns")
})
