# Extracted from test-research-p268.R:176

# prequel ----------------------------------------------------------------------
set.seed(1)

# test -------------------------------------------------------------------------
set.seed(13)
for (k in 1:100) {
    a <- runif(1, 0.05, 0.95); e <- runif(1, 0.05, 0.95); pb <- runif(1, 0.01, 0.99)
    r <- morie_collider_arrest(a, e, pb)
    expect_equal(r$arrestee_or, pb, tolerance = 1e-12); expect_lt(r$arrestee_or, 1)
  }
n <- 400000
A <- rbinom(n, 1, 0.3)
E <- rbinom(n, 1, 0.2)
U <- rbinom(n, 1, 0.1)
Y <- pmax(A, E, U)
tab <- table(A[Y == 1], E[Y == 1])
or_hat <- (tab[2, 2] * tab[1, 1]) / (tab[2, 1] * tab[1, 2])
expect_equal(unname(or_hat), 0.1, tolerance = 0.08)
