# Extracted from test-causal-estimators.R:47

# prequel ----------------------------------------------------------------------
.ce_data <- function() {
  n <- 40
  i <- 0:(n - 1)
  x <- ((i * 7919) %% 97) / 97 - 0.5
  z <- as.integer((i * 31) %% 5 < 2)
  t <- as.integer(z + x + ((i * 13) %% 7) / 7 > 1)
  y <- 1 + 2 * t + 0.7 * x + ((i * 17) %% 11) / 11 - 0.5
  w <- 1 + ((i * 3) %% 4) / 4
  data.frame(x = x, z = z, t = t, y = y, w = w)
}

# test -------------------------------------------------------------------------
d <- .ce_data()
r <- Late(d, treatment = "t", outcome = "y", instrument = "z", covariates = "x")
Z <- cbind(1, d$z, d$x)
X <- cbind(1, d$t, d$x)
Pz <- Z %*% solve(crossprod(Z), t(Z))
Xh <- Pz %*% X
b <- solve(crossprod(Xh, X), crossprod(Xh, d$y))
e <- d$y - X %*% b
s2 <- sum(e^2) / (nrow(d) - ncol(X))
V <- s2 * solve(crossprod(Xh, X))
expect_equal(r$late, b[2], tolerance = 1e-12)
