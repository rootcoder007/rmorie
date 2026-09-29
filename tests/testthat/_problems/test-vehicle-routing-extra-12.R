# Extracted from test-vehicle-routing-extra.R:12

# test -------------------------------------------------------------------------
u <- .morie_random_uniform(200, seed = 41)
P <- rbind(c(50, 50), cbind(100 * u[2:11], 100 * u[22:31]))
D <- as.matrix(dist(P))
q <- c(0, 1 + floor(4 * u[42:51]))
r <- SavingsRoutes(D, 1:10, q, 10)
expect_equal(sort(unlist(r$routes)), 1:10)
expect_true(all(vapply(r$routes, function(x) sum(q[x + 1]), 0) <= 10))
ready <- c(0, 80 * u[62:71])
due <- c(1000, ready[-1] + 70)
s <- c(0, rep(4, 10))
expect_equal(sort(unlist(tw$routes)), 1:10)
