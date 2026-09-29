skip_if_not_installed("spdep")

set.seed(3)
n <- 25
xy <- cbind(stats::runif(n), stats::runif(n))

.nb0 <- function(nb) lapply(nb, function(v) sort(as.integer(v[v > 0]) - 1L))

test_that("builders match spdep neighbour lists", {
  expect_equal(lapply(swdist(xy, d = 0.3)$neighbours, sort), .nb0(spdep::dnearneigh(xy, 0, 0.3)))
  expect_equal(lapply(swknn(xy, k = 4)$neighbours, sort), .nb0(spdep::knn2nb(spdep::knearneigh(xy, k = 4))))
  expect_equal(lapply(swgab(xy)$neighbours, sort), .nb0(spdep::graph2nb(spdep::gabrielneigh(xy), sym = TRUE)))
  skip_if_not_installed("deldir")
  expect_equal(lapply(swtri(xy)$neighbours, sort), .nb0(spdep::tri2nb(xy)))
})

test_that("swlag and swpath match lag.listw and nblag", {
  nb <- spdep::knn2nb(spdep::knearneigh(xy, k = 3), sym = TRUE)
  lw <- spdep::nb2listw(nb, style = "W")
  W <- spdep::listw2mat(lw)
  y <- stats::rnorm(n)
  expect_equal(swlag(W, y), spdep::lag.listw(lw, y), tolerance = 1e-14)
  lags <- spdep::nblag(nb, 4)
  for (ord in 1:4) for (j in lags[[ord]][[1]]) expect_equal(swpath(W, 0, j - 1), ord)
  expect_equal(swblk(rep(1:5, 5))$W, spdep::nb2mat(spdep::nb2blocknb(NULL, rep(1:5, 5)), style = "B", zero.policy = TRUE),
               ignore_attr = TRUE)
})
