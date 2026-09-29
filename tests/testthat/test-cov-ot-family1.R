# Coverage for otbarfree .. otlowrk exports. Every expectation is
# recomputed in the test body: Sinkhorn by plain matrix scaling, exact
# transport by enumerating permutations (uniform marginals of equal size).

cov_ot_scaling <- function(a, b, C, eps, iters) {
  K <- exp(-C / eps)
  v <- rep(1, length(b))
  for (it in seq_len(iters)) {
    u <- a / as.numeric(K %*% v)
    v <- b / as.numeric(crossprod(K, u))
  }
  u * K * rep(v, each = length(a))
}

cov_ot_perms <- function(n) {
  if (n == 1) return(matrix(1L, 1, 1))
  p <- cov_ot_perms(n - 1)
  do.call(rbind, lapply(seq_len(n), function(i) cbind(i, ifelse(p >= i, p + 1L, p))))
}

cov_ot_assign <- function(C) {
  P <- cov_ot_perms(nrow(C))
  costs <- apply(P, 1, function(p) sum(C[cbind(seq_along(p), p)]))
  list(cost = min(costs) / nrow(C), perm = P[which.min(costs), ])
}

test_that("Otemd matches the optimal assignment on uniform marginals", {
  C <- matrix(c(4, 1, 3, 2,
                2, 0, 5, 3,
                3, 2, 2, 6,
                1, 4, 3, 2), 4, byrow = TRUE)
  r <- Otemd(rep(0.25, 4), rep(0.25, 4), C)
  best <- cov_ot_assign(C)
  expect_equal(r$cost, best$cost, tolerance = 1e-12)
  expect_equal(rowSums(r$T), rep(0.25, 4), tolerance = 1e-12)
  expect_equal(colSums(r$T), rep(0.25, 4), tolerance = 1e-12)
  expect_equal(r$cost, sum(r$T * C), tolerance = 1e-12)
  # non-square: a 1 x m problem has a single feasible plan
  r1 <- Otemd(1, c(0.2, 0.5, 0.3), matrix(c(1, 2, 3), 1))
  expect_equal(r1$cost, 0.2 + 1 + 0.9, tolerance = 1e-12)
  expect_error(Otemd(c(0.5, 0.5), c(0.3, 0.3), matrix(1, 2, 2)), "equal total mass")
  expect_error(Otemd(c(-1, 2), c(0.5, 0.5), matrix(1, 2, 2)), "non-negative")
})

test_that("Otbreg and Otdwd are alternating row/column scalings", {
  K <- matrix(c(1, 0.5, 0.2, 0.3, 1, 0.6, 0.8, 0.4, 1), 3, byrow = TRUE)
  a <- c(0.2, 0.5, 0.3)
  b <- c(0.4, 0.4, 0.2)
  r <- Otbreg(K, a, b, max_iter = 300)
  v <- rep(1, 3)
  for (i in 1:300) {
    u <- a / as.numeric(K %*% v)
    v <- b / as.numeric(crossprod(K, u))
  }
  expect_equal(r$T, u * K * rep(v, each = 3), tolerance = 1e-12)
  expect_lt(r$col_err, 1e-15)
  expect_lt(r$row_err, 1e-12)
  expect_error(Otbreg(K, a[-1], b), "does not match")
  expect_error(Otbreg(-K, a, b), "entrywise positive")
  d <- Otdwd(K, max_iter = 500)
  expect_equal(rowSums(d$M), rep(1, 3), tolerance = 1e-12)
  expect_equal(colSums(d$M), rep(1, 3), tolerance = 1e-12)
  expect_equal(d$M, K * d$d1 * rep(d$d2, each = 3), tolerance = 1e-12)
  # the doubly stochastic scaling is unique: same answer as Otbreg with uniform marginals
  expect_equal(d$M / 3, Otbreg(K, rep(1 / 3, 3), rep(1 / 3, 3), max_iter = 500)$T, tolerance = 1e-10)
  expect_error(Otdwd(K[, 1:2]), "square")
  expect_error(Otdwd(K - 0.5), "entrywise positive")
})

test_that("Otcw checks cyclical monotonicity over transpositions", {
  C <- matrix(c(0, 3, 5, 3, 0, 2, 5, 2, 0), 3)
  chk <- function(p) {
    q <- p + 1
    w <- 0
    for (i in 1:2) for (j in (i + 1):3) {
      w <- min(w, C[q[j], i] + C[q[i], j] - C[q[i], i] - C[q[j], j])
    }
    w
  }
  good <- Otcw(NULL, NULL, C, c(0, 1, 2))
  bad <- Otcw(NULL, NULL, C, c(2, 1, 0))
  expect_equal(good$slack, chk(c(0, 1, 2)))
  expect_equal(good$is_cm, 1)
  expect_equal(good$estimate, 0)
  expect_equal(bad$slack, chk(c(2, 1, 0)))
  expect_equal(bad$is_cm, 0)
  expect_equal(bad$estimate, C[3, 1] + C[2, 2] + C[1, 3])
})

test_that("Otdiv is the debiased Sinkhorn divergence", {
  x <- c(0, 1, 2)
  y <- c(0.5, 2.5)
  a <- c(0.2, 0.5, 0.3)
  b <- c(0.6, 0.4)
  Cab <- outer(x, y, "-")^2
  Caa <- outer(x, x, "-")^2
  Cbb <- outer(y, y, "-")^2
  eps <- 0.7
  ot <- function(p, q, C) {
    Tm <- cov_ot_scaling(p, q, C, eps, 300)
    R <- outer(p, q)
    sum(Tm * C) + eps * (sum(Tm * log(Tm / R)) + sum(R) - sum(Tm))
  }
  r <- Otdiv(a, b, Cab, Caa, Cbb, eps, max_iter = 300)
  expect_equal(r$OT_ab, ot(a, b, Cab), tolerance = 1e-10)
  expect_equal(r$OT_aa, ot(a, a, Caa), tolerance = 1e-10)
  expect_equal(r$S_eps, ot(a, b, Cab) - 0.5 * (ot(a, a, Caa) + ot(b, b, Cbb)), tolerance = 1e-10)
  expect_error(Otdiv(a, b, t(Cab), Caa, Cbb, eps), "cross cost")
  expect_error(Otdiv(a, b, Cab, Cbb, Cbb, eps), "self costs")
})

test_that("Otfgw reduces to exact transport at alpha = 0 and reports the fused cost", {
  M <- matrix(c(1, 4, 2, 3, 0, 5, 2, 2, 1), 3, byrow = TRUE)
  Cx <- unname(as.matrix(stats::dist(c(0, 1, 3))))
  Cy <- as.matrix(stats::dist(c(0, 2, 2.5)))
  u <- rep(1 / 3, 3)
  r0 <- Otfgw(M, Cx, Cy, u, u, alpha = 0)
  expect_equal(r0$wass_part, cov_ot_assign(M)$cost, tolerance = 1e-12)
  expect_equal(r0$cost, r0$wass_part, tolerance = 1e-12)
  r <- Otfgw(M, Cx, Cy, u, u, alpha = 0.4, max_iter = 15)
  Tm <- r$T
  gw <- 0
  for (i in 1:3) for (j in 1:3) for (k in 1:3) for (l in 1:3) {
    gw <- gw + (Cx[i, k] - Cy[j, l])^2 * Tm[i, j] * Tm[k, l]
  }
  expect_equal(r$gromov_part, gw, tolerance = 1e-12)
  expect_equal(r$wass_part, sum(Tm * M), tolerance = 1e-12)
  expect_equal(r$cost, 0.6 * sum(Tm * M) + 0.4 * gw, tolerance = 1e-12)
  expect_equal(rowSums(Tm), u, tolerance = 1e-12)
  expect_error(Otfgw(M, Cx, Cy, u, u, alpha = 2), "alpha must lie")
  expect_error(Otfgw(M[, 1:2], Cx, Cy, u, u), "n by m")
  expect_error(Otfgw(M, Cx[1:2, 1:2], Cy, u, u), "structure matrices")
})

test_that("Otgws iterates Sinkhorn on the linearised Gromov cost", {
  Cx <- unname(as.matrix(stats::dist(c(0, 1, 3))))
  Cy <- unname(as.matrix(stats::dist(c(0, 0.5, 2, 4))))
  a <- c(0.3, 0.3, 0.4)
  b <- c(0.25, 0.25, 0.25, 0.25)
  eps <- 0.5
  Tm <- outer(a, b)
  for (t in 1:6) Tm <- cov_ot_scaling(a, b, -Cx %*% Tm %*% Cy, eps, 150)
  r <- Otgws(Cx, Cy, a, b, eps, max_iter = 6, inner_iter = 150)
  expect_equal(r$T, Tm, tolerance = 1e-10)
  gw <- 0
  for (i in 1:3) for (j in 1:4) for (k in 1:3) for (l in 1:4) {
    gw <- gw + (Cx[i, k] - Cy[j, l])^2 * Tm[i, j] * Tm[k, l]
  }
  expect_equal(r$cost, gw, tolerance = 1e-10)
  expect_equal(r$GW, sqrt(gw), tolerance = 1e-10)
  expect_error(Otgws(Cx[, 1:2], Cy, a, b, eps), "Cx must be n by n")
  expect_error(Otgws(Cx, Cy[, 1:2], a, b, eps), "Cy must be m by m")
})

test_that("Otbarfree moves the support to the barycentric projections", {
  X1 <- cbind(c(0, 1, 2, 3), c(0, 0, 1, 1))
  X2 <- cbind(c(4, 5, 6, 7), c(2, 3, 2, 3))
  r <- Otbarfree(list(X1, X2), c(1, 1), n_supp = 4, max_iter = 1)
  # the first support is every second pooled point; one step replaces each
  # support point by the weighted mean of its optimal partners
  Y <- rbind(X1, X2)[c(1, 3, 5, 7), ]
  Z <- matrix(0, 4, 2)
  cst <- 0
  for (X in list(X1, X2)) {
    C <- outer(seq_len(4), seq_len(4), Vectorize(function(i, j) sum((Y[i, ] - X[j, ])^2)))
    best <- cov_ot_assign(C)
    cst <- cst + 0.5 * best$cost
    Z <- Z + 0.5 * X[best$perm, ]
  }
  expect_equal(r$Y, Z, tolerance = 1e-12)
  expect_equal(r$cost, cst, tolerance = 1e-12)
  expect_error(Otbarfree(list(), 1, 1), "no input clouds")
  expect_error(Otbarfree(list(X1, X2[, 1, drop = FALSE]), c(1, 1), 2), "share a dimension")
  expect_error(Otbarfree(list(X1, X2), 1, 2), "one weight per cloud")
  expect_error(Otbarfree(list(X1, X2), c(1, 1), 9), "n_supp must lie")
})

test_that("Otker builds the RKHS transport cost", {
  X <- cbind(c(0, 1, 2), c(1, 0, 1))
  Y <- cbind(c(0.5, 1.5, 3), c(0, 1, 1))
  D2 <- outer(seq_len(3), seq_len(3), Vectorize(function(i, j) sum((X[i, ] - Y[j, ])^2)))
  g <- Otker(X, Y, kernel = "gaussian", epsilon = 0.2, gamma = 0.5, max_iter = 300)
  Cg <- 2 - 2 * exp(-0.5 * D2)
  expect_equal(g$C, Cg, tolerance = 1e-12)
  expect_equal(g$exact_cost, cov_ot_assign(Cg)$cost, tolerance = 1e-12)
  u <- rep(1 / 3, 3)
  expect_equal(g$EMD_approx, sum(cov_ot_scaling(u, u, Cg, 0.2, 300) * Cg), tolerance = 1e-10)
  l <- Otker(X, Y, kernel = "linear")
  expect_equal(l$C, D2, tolerance = 1e-12)
  expect_equal(l$exact_cost, cov_ot_assign(D2)$cost, tolerance = 1e-12)
  expect_error(Otker(X, Y, kernel = "poly"), "gaussian' or 'linear")
  expect_error(Otker(X, Y[, 1, drop = FALSE]), "share a dimension")
})

test_that("Sinkhlowr factorises the plan as Q diag(1/g) R'", {
  a <- c(0.2, 0.3, 0.5)
  b <- c(0.1, 0.4, 0.3, 0.2)
  C <- outer(c(0, 1, 2), c(0.5, 1, 1.5, 3), "-")^2
  r1 <- Sinkhlowr(a, b, C, rank = 1)
  expect_equal(r1$T, outer(a, b), tolerance = 1e-12)
  expect_equal(r1$cost, sum(outer(a, b) * C), tolerance = 1e-12)
  r2 <- Sinkhlowr(a, b, C, rank = 2, epsilon = 0.2, max_iter = 10, inner = 200)
  expect_equal(r2$T, r2$U %*% diag(1 / r2$g) %*% t(r2$V), tolerance = 1e-12)
  expect_equal(r2$cost, sum(r2$T * C), tolerance = 1e-12)
  expect_equal(sum(r2$g), 1, tolerance = 1e-12)
  expect_equal(rowSums(r2$T), a, tolerance = 1e-9)
  expect_equal(colSums(r2$T), b, tolerance = 1e-9)
})

test_that("morie_otis_make_pair_alert_to_volatility_a01 aggregates alert combos per person-year", {
  df <- data.frame(
    UniqueIndividual_ID = c(1, 1, 1, 2, 2, 3),
    EndFiscalYear = c(2020, 2020, 2021, 2020, 2020, 2021),
    Gender = c("M", "M", "M", "F", "F", "M"),
    Age_Category = c("18-24", "18-24", "25-34", "35-49", "35-49", "50+"),
    Region_AtTimeOfPlacement = c("E", "W", "W", "N", "N", "C"),
    Region_MostRecentPlacement = c("E", "E", "W", "N", "S", "C"),
    MentalHealth_Alert = c("Yes", "No", "Yes", "Yes", "Yes", "No"),
    SuicideRisk_Alert = c("No", "No", "Yes", "Yes", "Yes", "No"),
    SuicideWatch_Alert = c("No", "Yes", "No", "No", "No", "No"),
    stringsAsFactors = FALSE)
  r <- morie_otis_make_pair_alert_to_volatility_a01(df)
  yn <- function(v) as.integer(v == "Yes")
  combo <- 4 * yn(df$MentalHealth_Alert) + 2 * yn(df$SuicideRisk_Alert) + yn(df$SuicideWatch_Alert)
  key <- paste(df$UniqueIndividual_ID, df$EndFiscalYear, sep = "|")
  within <- as.integer(df$Region_AtTimeOfPlacement != df$Region_MostRecentPlacement)
  across <- integer(6)
  for (i in 2:6) {
    if (key[i] == key[i - 1]) {
      across[i] <- as.integer(df$Region_AtTimeOfPlacement[i] != df$Region_AtTimeOfPlacement[i - 1])
    }
  }
  keys <- sort(unique(key))
  ac <- vapply(keys, function(k) length(unique(combo[key == k])), 0L)
  vm <- vapply(keys, function(k) sum((within + across)[key == k]), 0L)
  expect_equal(r$data$ac, unname(ac))
  expect_equal(r$data$vm, unname(vm))
  expect_equal(r$data$T_high_ac, as.integer(unname(ac) >= 2))
  expect_equal(r$T, "T_high_ac")
  expect_equal(r$Y, "Y_vm_count")
  expect_error(morie_otis_make_pair_alert_to_volatility_a01(df[, -1]), "missing required OTIS columns")
})

test_that("morie_otis_figures writes one PNG per requested and available panel", {
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  df <- data.frame(
    EndFiscalYear = rep(c(2019, 2020, 2021), each = 4),
    NumberConsecutiveDays_Segregation = c(1, 3, 20, 2, 5, 16, 1, 40, 2, 7, 9, 0),
    MentalHealth_Alert = rep(c("Yes", "No", "Yes", "No"), 3),
    SuicideRisk_Alert = rep(c("No", "No", "Yes", "Yes"), 3),
    SuicideWatch_Alert = rep(c("Yes", "No", "No", "No"), 3),
    Age_Category = rep(c("18-24", "25-34"), 6),
    Region_AtTimeOfPlacement = c("E", "E", "W", "N", "E", "W", "W", "E", "N", "E", "C", "W"),
    stringsAsFactors = FALSE)
  od <- file.path(tempdir(), "otis_fig_cov")
  out <- morie_otis_figures(od, df = df)
  want <- file.path(od, c("otis_powerlaw_days.png", "otis_alert_trend.png", "otis_mortification.png",
                          "otis_age_year.png", "otis_pareto_region.png"))
  expect_equal(out, want)
  expect_true(all(file.exists(want)))
  unlink(od, recursive = TRUE)
  # panels whose columns are absent are skipped
  out2 <- morie_otis_figures(od, df = df[, c("EndFiscalYear", "Age_Category")],
                             which = c("powerlaw", "age_year"))
  expect_equal(out2, file.path(od, "otis_age_year.png"))
  unlink(od, recursive = TRUE)
  expect_error(morie_otis_figures(od, df = df, which = "nope"))
})
