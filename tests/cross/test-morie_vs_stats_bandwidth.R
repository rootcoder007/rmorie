test_that("kbwrt and bwrot reproduce stats::bw.nrd0 and stats::bw.nrd", {
  set.seed(11)
  for (n in c(25, 60, 200)) {
    x <- rgamma(n, shape = 2) + rnorm(n, sd = 0.3)
    expect_equal(kbwrt(x)$bw, stats::bw.nrd0(x), tolerance = 1e-12)
    expect_equal(bwrot(x)$bandwidth, stats::bw.nrd(x), tolerance = 1e-12)
  }
})
