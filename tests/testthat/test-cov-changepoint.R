# Coverage for the change-point files (adwin.R, bocpd.R, chgbcp.R,
# chgseg_native.R, chgsen.R, binseg_native.R, copynm_native.R, chtchg.R,
# cpd_analyze.R): segmentations are checked against an exact optimal-
# partitioning search, statistics against brute-force enumeration and the
# Bayesian recursion against its conjugate update.

cp_x <- c(0.1, -0.2, 0.3, 0.0, 0.2, -0.1, 3.1, 2.8, 3.3, 2.9, 3.0, 3.2,
          -1.0, -1.2, -0.8, -1.1, -0.9, -1.3)

test_that("morie_chgseg is PELT = exact optimal partitioning", {
  r <- morie_chgseg(cp_x, penalty = 2)
  n <- length(cp_x)
  cost <- function(a, b) {
    s <- cp_x[(a + 1):b]
    sum((s - mean(s))^2)
  }
  F <- c(-2, rep(Inf, n))
  last <- integer(n + 1)
  for (t in 1:n) {
    v <- vapply(0:(t - 1), function(s) F[s + 1] + cost(s, t) + 2, 0)
    F[t + 1] <- min(v)
    last[t + 1] <- which.min(v) - 1
  }
  cps <- integer(0)
  t <- n
  while (last[t + 1] > 0) {
    cps <- c(last[t + 1], cps)
    t <- last[t + 1]
  }
  expect_equal(r$changepoints, as.numeric(cps))
  expect_equal(r$segment_means, vapply(seq_along(c(cps, n)), function(i) {
    b <- c(0, cps, n)
    mean(cp_x[(b[i] + 1):b[i + 1]])
  }, 0), tolerance = 1e-12)
  expect_match(r$method, "PELT")
})

test_that("morie_binseg greedily splits by the largest cost reduction", {
  r <- morie_binseg(cp_x, K = 2)
  cost <- function(a, b) {
    s <- cp_x[(a + 1):b]
    sum((s - mean(s))^2)
  }
  split <- function(a, b) {
    taus <- (a + 1):(b - 1)
    g <- vapply(taus, function(t) cost(a, b) - cost(a, t) - cost(t, b), 0)
    c(taus[which.max(g)], max(g))
  }
  s1 <- split(0, 18)
  segs <- rbind(c(0, s1[1]), c(s1[1], 18))
  cand <- rbind(split(segs[1, 1], segs[1, 2]), split(segs[2, 1], segs[2, 2]))
  s2 <- cand[which.max(cand[, 2]), ]
  expect_equal(r$order, c(s1[1], s2[1]))
  expect_equal(r$improvements, c(s1[2], s2[2]), tolerance = 1e-12)
  expect_equal(morie_binseg(cp_x, K = 5, penalty = 50)$n_changepoints, 0L)
  expect_error(morie_binseg(1, K = 1), "too short")
})

test_that("chgsen is Hampel's change-of-variance sensitivity", {
  psi <- c(-1.5, -0.8, -0.2, 0.3, 0.9, 1.4)
  x <- c(-2, -1, -0.3, 0.4, 1.1, 2)
  r <- chgsen(psi, x)
  dp <- c((psi[2] - psi[1]) / (x[2] - x[1]),
          (psi[3:6] - psi[1:4]) / (x[3:6] - x[1:4]),
          (psi[6] - psi[5]) / (x[6] - x[5]))
  a <- mean(psi^2)
  b <- mean(dp)
  v <- a / b^2
  cvf <- v * (1 + psi^2 / a - 2 * dp / b)
  expect_equal(r$V, v, tolerance = 1e-12)
  expect_equal(r$cvf, cvf, tolerance = 1e-12)
  expect_equal(r$kappa_star, max(cvf) / v, tolerance = 1e-12)
  w <- c(1, 2, 1, 2, 1, 2)
  rw <- chgsen(psi, dpsi = rep(1, 6), w = w)
  expect_equal(rw$V, sum(w / sum(w) * psi^2), tolerance = 1e-12)
  expect_same_function(morie_change_of_variance, chgsen)
  expect_true(is.na(chgsen(psi, dpsi = c(1, -1, 1, -1, 1, -1))$V))
})

test_that("Adwin shrinks the window when sub-window means differ", {
  x <- c(rep(0, 8), rep(1, 8))
  r <- Adwin(x, delta = 0.1)
  W <- numeric(0)
  cuts <- integer(0)
  for (pos in seq_along(x)) {
    W <- c(W, x[pos])
    repeat {
      n <- length(W)
      if (n < 2) break
      hit <- FALSE
      for (n0 in 1:(n - 1)) {
        m <- 1 / (1 / n0 + 1 / (n - n0))
        cut <- sqrt(log(4 / (0.1 / n)) / (2 * m))
        if (abs(mean(W[1:n0]) - mean(W[(n0 + 1):n])) >= cut) {
          W <- W[-1]
          cuts <- c(cuts, pos - 1L)
          hit <- TRUE
          break
        }
      }
      if (!hit) break
    }
  }
  expect_equal(r$window, W)
  expect_equal(r$changepoints, cuts)
  expect_equal(r$mean, mean(W), tolerance = 1e-12)
  expect_equal(Adwin(rep(0.5, 10))$ndrops, 0L)
  expect_error(Adwin(x, delta = 1), "\\(0, 1\\)")
})

test_that("Bocpd and Bayesocp run the Normal-Inverse-Gamma run-length recursion", {
  x <- cp_x[1:10]
  r <- Bocpd(x, hazard = 0.1, mu0 = 0.5, kappa0 = 2, alpha0 = 1.5, beta0 = 0.8)
  R <- 1
  mu <- 0.5
  ka <- 2
  al <- 1.5
  be <- 0.8
  cpp <- numeric(10)
  for (t in 1:10) {
    s <- sqrt(be * (ka + 1) / (al * ka))
    pr <- dt((x[t] - mu) / s, 2 * al) / s
    nr <- c(sum(R * pr * 0.1), R * pr * 0.9)
    nr <- nr / sum(nr)
    be <- c(0.8, be + ka * (x[t] - mu)^2 / (2 * (ka + 1)))
    mu <- c(0.5, (ka * mu + x[t]) / (ka + 1))
    ka <- c(2, ka + 1)
    al <- c(1.5, al + 0.5)
    R <- nr
    cpp[t] <- R[2]
  }
  expect_equal(r$cp_prob, cpp, tolerance = 1e-12)
  expect_equal(r$run_length[10], which.max(R) - 1L)
  b <- Bayesocp(x, hazard = 0.1, mu0 = 0.5, kappa0 = 2, alpha0 = 1.5, beta0 = 0.8)
  expect_equal(b$cp_prob, r$cp_prob)
})

test_that("cbs_statistic and copynm (circular binary segmentation)", {
  v <- c(0.1, -0.2, 0.3, 2.9, 3.1, 3.2, 0.0, 0.2)
  r <- cbs_statistic(v)
  best <- -1
  for (i in 0:7) for (j in (i + 1):8) {
    m <- j - i
    k <- 8 - m
    if (k == 0) next
    z <- abs(mean(v[(i + 1):j]) - mean(v[-((i + 1):j)])) / sqrt(1 / m + 1 / k)
    if (z > best) {
      best <- z
      bi <- i
      bj <- j
    }
  }
  expect_equal(r$z, best, tolerance = 1e-12)
  expect_equal(c(r$i, r$j), c(bi, bj))
  expect_error(cbs_statistic(c(1, 2)), "at least 3")
  expect_error(cbs_statistic(numeric(0)), "non-empty")
  y <- c(rep(0, 12), rep(4, 12), rep(0, 12))
  cb <- copynm(y, alpha = 0.05, permutations = 60L, seed = 1L)
  expect_equal(cb$changepoints, c(12L, 24L))
  edges <- c(0, cb$changepoints, 36)
  mm <- vapply(1:3, function(i) mean(y[(edges[i] + 1):edges[i + 1]]), 0)
  expect_equal(vapply(cb$segments, `[[`, 0, "mean"), mm, tolerance = 1e-12)
  expect_equal(cb$fitted, rep(mm, each = 12), tolerance = 1e-12)
  expect_equal(copynm(rep(1, 10))$n_segments, 1L)
  expect_same_function(circular_binary_segmentation, copynm)
  expect_error(copynm(numeric(0)), "non-empty")
  expect_error(copynm(y, alpha = 1), "alpha")
  expect_error(copynm(y, permutations = 0), "permutations")
  expect_error(copynm(y, min_width = 1), "min_width")
})

test_that("Drchange contrasts the two crossover sequences", {
  unit <- rep(letters[1:8], each = 2)
  per <- rep(1:2, 8)
  D <- c(1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1)
  y <- c(5, 3, 2, 4.5, 6, 3.2, 2.2, 5, 5.5, 3.5, 1.8, 4.1, 6.2, 3.1, 2.5, 4.8)
  r <- Drchange(y, D, per, unit)
  ym <- matrix(y, 2)
  dm <- matrix(D, 2)
  con <- ifelse(dm[1, ] > dm[2, ], ym[1, ] - ym[2, ], ym[2, ] - ym[1, ])
  sq <- as.numeric(dm[1, ] > dm[2, ])
  expect_equal(r$tau_naive, 0.5 * (mean(con[sq == 1]) + mean(con[sq == 0])), tolerance = 1e-12)
  expect_equal(r$carryover, mean(con[sq == 1]) - mean(con[sq == 0]), tolerance = 1e-12)
  expect_equal(r$estimate, mean(con[sq == 1]) - mean(con[sq == 0]), tolerance = 1e-9)
  expect_equal(r$n_units, 8L)
  one <- Drchange(y[1:4], D[1:4], per[1:4], unit[1:4])
  expect_true(is.nan(one$se))
})

test_that("morie_cpd_all_analyses tabulates the CPD frames", {
  cr <- data.frame(primary_type = c("THEFT", "BATTERY", "THEFT", "ASSAULT", "THEFT"),
                   community_area = c(1, 2, 1, 3, 2), arrest = c(TRUE, FALSE, FALSE, TRUE, TRUE),
                   year = c(2020, 2021, 2021, 2022, 2022))
  ar <- data.frame(race = c("A", "B", "A", "C", "", "B", "A"))
  od <- tempfile()
  on.exit(unlink(od, recursive = TRUE), add = TRUE)
  r <- morie_cpd_all_analyses(cr, ar, out_dir = od)
  expect_equal(unlist(r$crime_by_type$payload), c(THEFT = 3L, ASSAULT = 1L, BATTERY = 1L))
  ag <- r$arrests_by_area$payload$aggregate
  expect_equal(ag$outcome_rate, c(0.5, 0.5, 1), tolerance = 1e-12)
  expect_equal(ag$n_records, c(2L, 2L, 1L))
  expect_equal(unlist(r$temporal$payload), c(`2020` = 1L, `2021` = 2L, `2022` = 2L))
  expect_equal(unlist(r$arrest_race_disparity$payload$counts), c(A = 3L, B = 2L, C = 1L))
  expect_true(all(file.exists(file.path(od, paste0("cpd_", names(r), ".json")))))
  bad <- morie_cpd_all_analyses(data.frame(x = 1), data.frame(y = 1))
  expect_match(bad$crime_by_type$warnings, "primary_type")
  expect_match(bad$arrests_by_area$warnings, "community_area")
  expect_match(bad$temporal$warnings, "year")
  expect_match(bad$arrest_race_disparity$warnings, "race")
})
