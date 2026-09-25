test_that("Gcnemd applies (I + D^-1/2 A D^-1/2) X W, Kipf & Welling eq. (7)", {
  A <- matrix(c(0, 1, 1, 0, 0,  1, 0, 1, 0, 0,  1, 1, 0, 1, 0,
                0, 0, 1, 0, 0,  0, 0, 0, 0, 0), 5, 5, byrow = TRUE)
  X <- matrix(c(1, -0.5, 0.2, 0.8, -1, 0.3, 0.5, 0.5, 2, -1), 5, 2, byrow = TRUE)
  W <- matrix(c(0.7, -0.2, 1, 0.3, 0.9, -0.4), 2, 3, byrow = TRUE)
  d <- rowSums(A)
  s <- ifelse(d == 0, 0, d^-0.5)
  P <- diag(5) + diag(s) %*% A %*% diag(s)
  Z <- P %*% X %*% W
  r <- Gcnemd(A, X, W)
  expect_equal(unname(r$preactivation), Z, tolerance = 1e-14)
  expect_equal(unname(r$H), pmax(Z, 0), tolerance = 1e-14)
  # the isolated node keeps its own features
  expect_equal(unname(r$preactivation[5, ]), as.numeric(X[5, ] %*% W), tolerance = 1e-15)
  # one edge: operator [[1, 1], [1, 1]]
  r2 <- Gcnemd(matrix(c(0, 1, 1, 0), 2), matrix(c(1, 2), 2), matrix(1))
  expect_equal(as.numeric(r2$preactivation), c(3, 3))
})

test_that("Getis-Ord G moments equal the exact randomisation moments", {
  x <- c(3, 1, 4, 1.5, 5)
  W <- matrix(c(0, 1, 0, 0, 1,  1, 0, 1, 0, 0,  0, 1, 0, 1, 0,
                0, 0, 1, 0, 1,  1, 0, 0, 1, 0), 5, 5, byrow = TRUE)
  g <- function(v) {
    num <- 0; den <- 0
    for (i in 1:5) for (j in 1:5) if (i != j) {
      num <- num + W[i, j] * v[i] * v[j]; den <- den + v[i] * v[j]
    }
    num / den
  }
  perms <- function(v) {
    if (length(v) == 1L) return(list(v))
    out <- list()
    for (i in seq_along(v)) for (p in perms(v[-i])) out[[length(out) + 1L]] <- c(v[i], p)
    out
  }
  gs <- vapply(perms(x), g, 0)
  m <- mean(gs)
  v <- mean((gs - m)^2)
  r <- Getisordg(x, W)
  expect_equal(r$estimate, g(x), tolerance = 1e-14)
  expect_equal(r$expected, m, tolerance = 1e-12)
  expect_equal(r$var, v, tolerance = 1e-10)
  expect_error(Getisordg(c(1, 2, 3), W[1:3, 1:3]), "at least 4")
})
