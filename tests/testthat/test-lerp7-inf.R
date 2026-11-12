# Type-7 quantile helpers with infinite values, against R's own quantile(type = 7).
# rmoriebricklayer 1.1.1 fixed its quantile, median, IQR and Tukey-fence helpers: written
# lo + g * (hi - lo) they gave NaN beside an infinite value and Inf when the difference
# overflowed. dev/infqsweep.R and dev/lerpscan.R found the same flaw in the helpers below.

.q7_cases <- list(
  list(c(1, 2, Inf), 0.5), list(c(1, 2, Inf), 0.75), list(c(-Inf, 1, 2), 0.25),
  list(c(-Inf, 1), 0.5), list(c(Inf, Inf), 0.5), list(c(-1e308, 1e308), 0.5),
  list(c(1, 2, 3, 4), 0.5), list(c(1, 2, 3, 4), 0.1)
)

test_that("every type-7 quantile helper agrees with quantile(type = 7) beside Inf", {
  helpers <- c(".s03quantile7", ".s4_quantile7", ".bwr_q7", ".quantile7", ".ghc_deseq2_quantile",
               ".flow_an_quantile7", ".mfovsm_quantile7", ".mcst_q7", ".pibmd_quantile",
               ".sxd_q7", ".mor_qm_quantile", ".morie_qm_quant")
  ns <- asNamespace("rmorie")
  for (h in helpers) {
    f <- get(h, envir = ns)
    for (cs in .q7_cases) {
      want <- unname(stats::quantile(cs[[1]], cs[[2]], type = 7))
      expect_identical(f(sort(cs[[1]]), cs[[2]]), want, info = paste(h, toString(cs[[1]]), cs[[2]]))
    }
  }
  # .ot_quantiles goes through .s03quantile7
  expect_identical(.ot_quantiles(c(1, 2, Inf), c(0.5, 0.75)), c(2, Inf))
})

test_that("the BCa order statistic, the trajectory outlier range and the Westenberg quartiles hold beside Inf", {
  sv <- c(1, 2, Inf)
  expect_identical(.btbca_order(sv, 2), 2)
  expect_identical(.btbca_order(sv, 2.5), Inf)
  # 1.5 IQR rule: with Inf the IQR is Inf, so every finite value is kept
  v <- c(1, 2, 3, Inf)
  q <- unname(stats::quantile(v, c(0.25, 0.75), type = 7))
  keep <- v[v >= q[[1]] - 1.5 * (q[[2]] - q[[1]]) & v <= q[[2]] + 1.5 * (q[[2]] - q[[1]])]
  expect_identical(.sctraj_non_outlier_range(v), c(keep[1L], keep[length(keep)]))
  w <- Wstnbrg(c(0.5, 1.5, 2.5), c(1, 2, 3, Inf))
  expect_identical(c(w$q1, w$q3), q)
})

test_that(".morie_lerp7 keeps finite results bit for bit and propagates NA", {
  a <- c(0.1, -3, 1e-300)
  b <- c(0.7, 5, 2e-300)
  g <- c(0.3, 0.25, 0.5)
  expect_identical(.morie_lerp7(a, b, g), a + g * (b - a))
  expect_identical(.morie_lerp7(NA_real_, 1, 0.5), NA_real_)
  expect_true(is.nan(.morie_lerp7(-Inf, Inf, 0.5)))  # as quantile(c(-Inf, Inf), 0.5)
})
