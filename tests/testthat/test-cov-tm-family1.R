# Coverage for tmltsm .. tpsuof exports. Every expectation is recomputed in
# the test body.

test_that("Tmle2stage is sequential regression over two treatment stages", {
  x1 <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7, 0.4, -0.3)
  d1 <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
  d2 <- c(0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1)
  y <- c(2.1, 1.0, 2.9, 1.4, 0.3, 2.2, 1.9, 0.8, 3.0, 1.6, 0.9, 1.5, 2.4, 1.1)
  r <- Tmle2stage(y, d1, d2, X1 = x1)
  f2 <- stats::lm(y ~ d2 + d1 + x1)
  b2 <- stats::coef(f2)
  g1 <- stats::fitted(stats::glm(d1 ~ x1, family = stats::binomial(), control = list(epsilon = 1e-14)))
  g2 <- stats::fitted(stats::glm(d2 ~ d1, family = stats::binomial(), control = list(epsilon = 1e-14)))
  reg <- function(a1, a2) {
    q2 <- b2[1] + b2[2] * a2 + b2[3] * a1 + b2[4] * x1
    b1 <- stats::coef(stats::lm(q2 ~ d1 + x1))
    q1 <- b1[1] + b1[2] * a1 + b1[3] * x1
    p1 <- if (a1 == 1) g1 else 1 - g1
    p2 <- if (a2 == 1) g2 else 1 - g2
    ic <- (d1 == a1) * (d2 == a2) / (p1 * p2) * (y - q2) + (d1 == a1) / p1 * (q2 - q1) + q1 - mean(q1)
    list(m = mean(q1), ic = unname(ic))
  }
  r11 <- reg(1, 1)
  r00 <- reg(0, 0)
  # the package least squares carry a 1e-10 ridge
  expect_equal(r$ey11, r11$m, tolerance = 1e-8)
  expect_equal(r$ey10, reg(1, 0)$m, tolerance = 1e-8)
  expect_equal(r$estimate, r11$m - r00$m, tolerance = 1e-8)
  expect_equal(r$se, sqrt(sum((r11$ic - r00$ic)^2)) / 14, tolerance = 1e-7)
})

test_that("Tmlevar targets the influence-curve variance", {
  ic <- c(0.4, -0.3, 1.1, -0.8, 0.2, 0.5, -0.9, 0.3)
  r <- Tmlevar(ic, level = 0.9)
  s2 <- mean(ic^2)
  se <- stats::sd(ic^2) / sqrt(8)
  z <- stats::qnorm(0.95)
  expect_equal(r$sigma2, s2, tolerance = 1e-12)
  expect_equal(r$ci_lower, s2 * exp(-z * se / s2), tolerance = 1e-12)
  expect_equal(r$se_psi_upper, sqrt(s2 * exp(z * se / s2) / 8), tolerance = 1e-12)
  expect_equal(r$kurtosis, mean(ic^4) / s2^2, tolerance = 1e-12)
  expect_error(Tmlevar(c(1, 2)), "at least three")
  expect_error(Tmlevar(rep(0, 4)), "identically zero")
  expect_error(Tmlevar(ic, level = 0), "strictly between")
})

test_that("Tmscore is the TM-score with the length-dependent d0", {
  A <- cbind(1:20, sin(1:20), cos(1:20))
  B <- A + cbind(0.3 * cos(1:20), 0.2, -0.1 * sin(1:20))
  r <- Tmscore(A, B)
  d0 <- 1.24 * (20 - 15)^(1 / 3) - 1.8
  d2 <- rowSums((A - B)^2)
  expect_equal(r$d0, d0, tolerance = 1e-12)
  expect_equal(r$estimate, mean(1 / (1 + d2 / d0^2)), tolerance = 1e-12)
  expect_equal(r$rmsd, sqrt(mean(d2)), tolerance = 1e-12)
  s <- Tmscore(A[1:10, ], B[1:10, ], l_ref = 12)
  expect_equal(s$estimate, sum(1 / (1 + rowSums((A[1:10, ] - B[1:10, ])^2) / 0.25)) / 12, tolerance = 1e-12)
})

test_that("Tnie is the total natural indirect effect of the VanderWeele model", {
  X <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  M <- c(1.2, 0.4, 1.5, 1.1, 0.2, 0.6, 1.3, 0.5, 1.8, 0.7)
  C <- c(0.3, 0.1, -0.2, 0.5, 0.0, 0.4, -0.1, 0.2, 0.6, -0.3)
  Y <- c(2.1, 1.0, 2.9, 2.2, 0.3, 1.4, 1.9, 0.8, 3.0, 1.2)
  r <- Tnie(X, M, Y, C, a = 1, astar = 0)
  b <- stats::coef(stats::lm(M ~ X + C))
  th <- stats::coef(stats::lm(Y ~ X + M + X:M + C))
  # the package least squares carry a 1e-10 ridge
  expect_equal(r$estimate, unname(th["M"] * b["X"] + th["X:M"] * b["X"]), tolerance = 1e-8)
  cb <- mean(C)
  expect_equal(r$pnde, unname(th["X"] + th["X:M"] * (b[1] + b["C"] * cb)), tolerance = 1e-8)
  expect_equal(r$te, r$pnde + r$tnie, tolerance = 1e-12)
  expect_error(Tnie(X[1:3], M[1:3], Y[1:3]), "too few observations")
})

test_that("morie_tps_figures draws one PNG per requested panel", {
  testthat::skip_on_covr()
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
  dir <- file.path(tempdir(), "tpsfig_cov")
  unlink(dir, recursive = TRUE)
  days <- as.Date("2020-01-01") + cumsum(rep(c(1, 2, 3), 50))
  df <- data.frame(OCC_YEAR = as.integer(format(days, "%Y")), OCC_MONTH = as.integer(format(days, "%m")),
                   OCC_DAY = as.integer(format(days, "%d")))
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(df, csv, row.names = FALSE)
  small <- tempfile(fileext = ".csv")
  utils::write.csv(df[1:20, ], small, row.names = FALSE)
  testthat::local_mocked_bindings(
    morie_fetch_tps = function(cat_name, cache_dir) if (cat_name == "Few") small else csv,
    morie_tps_hawkes_temporal_fit = function(df, ds_name) list(mu = 0.4, kappa = 0.2, omega = 1),
    morie_tps_sarima_forecast = function(df, ds_name) list(forecast = c(10, 11, 12), actual = c(9, 12, 11)),
    morie_tps_langevin_simulate = function(df, ds_name) list(paths = matrix(1:20, 10, 2), theta = 0.3, mu = 5),
    morie_tps_fokker_planck_grid = function(df, ds_name) list(grid = 1:10, density = stats::dnorm(1:10, 5, 2), mu = 5, stationary_var = 4),
    .package = pkg)
  out <- morie_tps_figures(dir, categories = "Homicides")
  expect_setequal(basename(out), paste0(c("hawkes", "sarima", "langevin", "fokker_planck"), "_Homicides.png"))
  expect_true(all(file.exists(out)))
  expect_warning(few <- morie_tps_figures(dir, categories = "Few", which = "sarima"), "only 20 usable timestamps")
  expect_length(few, 0)
})

test_that("morie_tps_use_of_force summarises the rate and type counts", {
  ft <- c("taser", "physical", "taser", NA, "firearm", "taser")
  r <- morie_tps_use_of_force(ft, 200)
  expect_equal(r$rate, 5 / 200)
  expect_equal(unlist(r$type_counts), c(firearm = 1L, physical = 1L, taser = 3L))
  expect_equal(r$summary_lines$`Most common type`, "taser")
  expect_match(r$interpretation, "2.50 per 100 encounters")
  expect_length(r$warnings, 0)
  s <- morie_tps_use_of_force(character(0), 10)
  expect_length(s$warnings, 2)
  expect_equal(s$summary_lines$`Most common type`, "-")
  expect_error(morie_tps_use_of_force(ft, 0), "positive scalar")
  expect_same_function(morie_tpsuof, morie_tps_use_of_force)
})
