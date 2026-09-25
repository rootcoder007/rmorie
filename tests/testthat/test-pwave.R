# P-wave detection (Hengeveld and van Bemmel): synthetic ECG with the P
# wave placed a known lead before each QRS.

.pw_ecg <- function(p_lead, rr = 0.8, fs = 250, n = 2500) {
  q <- as.integer(fs * (0.5 + rr * (0:19)))
  q <- q[q < n - 10]
  x <- numeric(n)
  i <- 0:(n - 1)
  for (r in q) {
    t <- (i - r) / fs
    x <- x + 1.2 * exp(-(t / 0.012)^2) + 0.15 * exp(-((t + p_lead) / 0.025)^2) +
      0.3 * exp(-((t - 0.30) / 0.05)^2)
  }
  list(x = x, q = q)
}

test_that("each P wave is found where it was placed", {
  e <- .pw_ecg(0.16)
  r <- PWaveDet(e$x, e$q, 250)
  lead <- vapply(seq_along(r$p), function(k) (e$q[k + 1] - r$p[[k]]) / 250, numeric(1))
  expect_equal(lead, rep(0.16, length(e$q) - 1L), tolerance = 1e-12)
  expect_identical(r$windows[[1]][1], e$q[1] + as.integer(round((2 / 9 * 0.8 + 0.25) * 250)))
})

test_that("a beat with no interval keeps its place in the output", {
  e <- .pw_ecg(0.16)
  q2 <- c(e$q[1:3], e$q[3] + 60L, e$q[4:length(e$q)])
  r <- PWaveDet(e$x, q2, 250)
  expect_length(r$p, length(q2) - 1L)
  expect_null(r$p[[3]])
  expect_equal((q2[6] - r$p[[5]]) / 250, 0.16, tolerance = 1e-12)
  expect_error(PWaveDet(numeric(64), c(10L, 40L), 20), "22 Hz")
})
