test_that("cokrg solves the simple cokriging system and matches Python", {
  u <- .morie_random_uniform(80, seed = 4)
  P <- cbind(u[1:20], u[21:40])
  x <- 3 * u[41:60]
  y <- x + 0.3 * u[61:80]
  cv <- function(h, a, b) {
    if (a == 0 && b == 0) return(1.9 * exp(-h / 1.5) + ifelse(h == 0, 0.1, 0))
    if (a == 1 && b == 1) return(1.4 * exp(-h / 1) + ifelse(h == 0, 0.1, 0))
    0.4 * exp(-h / 1.2)
  }
  pts <- rbind(P, P)
  v <- rep(0:1, each = 20)
  M <- matrix(0, 40, 40)
  for (i in 1:40) for (j in 1:40) M[i, j] <- cv(sqrt(sum((pts[i, ] - pts[j, ])^2)), v[i], v[j])
  c0 <- vapply(1:40, function(i) cv(sqrt(sum((pts[i, ] - c(0.5, 0.5))^2)), v[i], 0), 0)
  w <- solve(M, c0)
  r <- cokrg(x, y, P, c(0.5, 0.5), sill_p = 2, range_p = 1.5, sill_s = 1.5, range_s = 1,
             cross_sill = 0.4, cross_range = 1.2, nugget = 0.1)
  expect_equal(r$estimate, sum(w * c(x, y)), tolerance = 1e-10)
  expect_equal(r$se, sqrt(2 - sum(w * c0)), tolerance = 1e-10)
  expect_match(cokrg(x, y, P, c(0.5, 0.5), means = NULL)$method, "Ordinary")
  # Python doctest of morie.fn.cokrg.cokriging
  d <- cokrg(c(1, 2, 1.5, 0.5), c(0.8, 2.2, 1.1, 0.7), rbind(c(0, 0), c(2, 0), c(1, 1), c(0, 2)), c(1, 0))
  expect_equal(round(c(d$estimate, d$se), 10), c(1.1730729181, 0.8439211982))
})
