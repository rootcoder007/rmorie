.bbt_data <- function() {
  X <- t(vapply(0:39, function(i) {
    round(4 + sin(1.3 * i) * ((0:6) - 3) * 0.6 + cos(0.7 * i + 0.4) * sin((0:6) + 1) + 0.3 * sin(i * (0:6) + 1))
  }, numeric(7)))
  X[4, 3] <- NA
  X[11, 6] <- NA
  X[18, 1] <- NA
  X
}

.bbt_r2 <- function(a, b) {
  n <- length(a)
  (n * sum(a * b) - sum(a) * sum(b))^2 / ((n * sum(a^2) - sum(a)^2) * (n * sum(b^2) - sum(b)^2))
}

test_that("one-dimensional fit recomputes from the returned parameters", {
  X <- .bbt_data()
  r <- BlackboxTranspose(X, dims = 1)
  s1 <- r$fits[[1]]$singular
  st <- do.call(rbind, r$stimuli[[1]])
  ind <- do.call(rbind, r$individuals[[1]])
  expect_equal(sum(st[, 2]^2), 1, tolerance = 1e-12)
  expect_gt(st[which.max(abs(st[, 2])), 2], 0)
  fit <- ind[, 1] + sqrt(s1) * outer(ind[, 2], st[, 2])
  ok <- !is.na(X)
  for (j in seq_len(nrow(X))) expect_equal(ind[j, 3], .bbt_r2(fit[j, ok[j, ]], X[j, ok[j, ]]), tolerance = 1e-9)
  for (i in seq_len(ncol(X))) {
    expect_equal(st[i, 1], sum(ok[, i]))
    expect_equal(st[i, 3], .bbt_r2(fit[ok[, i], i], X[ok[, i], i]), tolerance = 1e-9)
  }
  expect_equal(r$fits[[1]]$SSE, sum((fit[ok] - X[ok])^2), tolerance = 1e-10)
  v <- X[ok]
  expect_equal(c(r$n_row, r$n_col, r$n_data, r$n_miss), c(7, 40, length(v), 3))
  expect_equal(r$ss_mean, sum(v^2) - sum(v)^2 / length(v), tolerance = 1e-12)
})

test_that("two-dimensional coordinates are orthonormal and fits nest", {
  r <- BlackboxTranspose(.bbt_data(), dims = 2)
  C <- do.call(rbind, r$stimuli[[2]])[, 2:3]
  expect_equal(crossprod(C), diag(2), tolerance = 1e-12)
  expect_lt(r$fits[[2]]$SSE, r$fits[[1]]$SSE)
  expect_equal(r$fits[[1]]$percent + r$fits[[2]]$percent, r$fits[[2]]$cumulative_percent, tolerance = 1e-12)
})

test_that("missing codes, dropped respondents and errors", {
  X <- .bbt_data()
  Y <- X
  Y[is.na(Y)] <- 9
  expect_equal(BlackboxTranspose(X)$stimuli, BlackboxTranspose(Y, missing = 9)$stimuli)
  X[6, 1:5] <- NA
  r <- BlackboxTranspose(X)
  expect_null(r$individuals[[1]][[6]])
  expect_equal(r$n_col, 39)
  expect_error(BlackboxTranspose(X[1:6, ]), "more scaled respondents")
  expect_error(BlackboxTranspose(X, dims = 0), "positive")
})
