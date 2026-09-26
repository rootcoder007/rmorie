bk_i <- 0:19
bk_x <- 10 * ((bk_i * 0.618034) %% 1)
bk_y <- 10 * ((bk_i * 0.414214 + 0.3) %% 1)
bk_z <- 1 + 0.2 * bk_x + sin(0.8 * bk_x) + cos(0.6 * bk_y) + 0.2 * sin(5.3 * bk_i)
bk_co <- cbind(bk_x, bk_y)
bk_c0 <- rbind(c(2.5, 3.5), c(7, 1), c(5, 8.5))
bk_X <- cbind(1, bk_x)
bk_X0 <- cbind(1, bk_c0[, 1])
bk_v <- exp(-3 * as.matrix(dist(bk_co)) / 4)
bk_v0 <- exp(-3 * sqrt(outer(bk_c0[, 1], bk_x, "-")^2 + outer(bk_c0[, 2], bk_y, "-")^2) / 4)
bk_v00 <- exp(-3 * as.matrix(dist(bk_c0)) / 4)

test_that("bkrnig with a flat prior is gstat's universal kriging (5.29)", {
  f <- bkrnig(bk_X, bk_z, bk_X0, bk_v, bk_v0, bk_v00)
  # krige(z ~ x, model = vgm(1, "Exp", 4 / 3)): var1.pred and var1.var
  expect_equal(f$mean, c(1.7061445705561447, 2.3493210010069245, 1.9095305189366867), tolerance = 1e-12)
  expect_equal(diag(f$variance), c(0.72867075615633337, 0.44013082829933997, 0.76854464567679392) *
                 f$a_star / (f$df - 2), tolerance = 1e-12)
})

test_that("bkrnig NIG mean equals the printed (6.97) and a* the O'Hagan identity", {
  f <- bkrnig(bk_X, bk_z, bk_X0, bk_v, bk_v0, bk_v00, a = 2, d = 4, m = c(0.5, 0.1), Qinv = diag(c(2, 5)))
  expect_equal(f$mean, c(1.6861663842349945, 2.3517380801871717, 1.8853077971494199), tolerance = 1e-12)
  expect_equal(f$a_star, 13.596024854143266, tolerance = 1e-12)
  expect_equal(f$df, 24)
})

test_that("hsbkrg equals geoR::krige.bayes with a discrete uniform prior on phi", {
  h <- hsbkrg(bk_co, bk_z, bk_X, bk_c0, bk_X0, 1 / c(0.5, 1, 1.5, 2, 2.5), 1.5)
  expect_equal(h$mean, c(1.8901783255847411, 2.3797672496277436, 1.6793035375250742), tolerance = 1e-9)
  expect_equal(h$variance, c(0.16557142216248577, 0.039438978803821634, 0.19073490148919969), tolerance = 1e-9)
  expect_equal(as.vector(h$posterior), c(0.0056473900854062961, 0.053295324569191373,
                                         0.19400351523484616, 0.33942697726502258,
                                         0.4076267928455336), tolerance = 1e-9)
})
