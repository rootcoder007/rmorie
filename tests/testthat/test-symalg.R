test_that("JordanCanonical satisfies A P = P J", {
  for (A in list(rbind(c(5, 4, 2, 1), c(0, 1, -1, -1), c(-1, -1, 3, 0), c(1, 1, -1, 2)),
                 rbind(c(1, 2, 0), c(-2, -3, 0), c(4, 4, -1)))) {
    r <- JordanCanonical(A)
    expect_equal(A %*% r$P, r$P %*% r$J)
    expect_gt(abs(det(r$P)), 0.5)
  }
  expect_error(JordanCanonical(rbind(c(0, -1), c(1, 0))))
})

test_that("GaloisGroup classifies known polynomials", {
  polys <- list(c(1, 0, 0, 0, -2), c(1, 0, 0, 0, 1), c(1, 1, 1, 1, 1), c(1, 0, 0, -1, -1), c(1, 0, 0, 8, 12), c(1, 0, 0, -2), c(1, -3, 0, 1))
  expect_equal(vapply(polys, function(p) GaloisGroup(p)$group, ""), c("D4", "V4", "C4", "S4", "A4", "S3", "A3"))
})

test_that("ShuntingYard agrees with R's own parser", {
  for (e in c("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3", "-2^2 + max(1, 3) * sin(0.5)", "2^-3 * -(4 - 7) / 2")) {
    expect_equal(ShuntingYard(e)$value, eval(parse(text = e)), tolerance = 1e-12)
  }
  expect_error(ShuntingYard("(1 + 2"))
})
