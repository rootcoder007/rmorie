# Kosorok eq. (1.4): U_n(beta) = (1/n) sum over events of z_i - E_bar(t_i).

.cox_u <- function(b, z, t, e) {
  u <- 0
  for (i in seq_along(t)) {
    if (e[i] == 1) {
      r <- t >= t[i]
      w <- exp(b * z[r])
      u <- u + z[i] - sum(w * z[r]) / sum(w)
    }
  }
  u / length(t)
}

test_that("the final score matches the Breslow sum, tied times included", {
  i <- 0:29
  z <- ((i * 7) %% 11) / 5 - 1
  t <- round(1 + ((i * 13) %% 17) / 3 + 0.01 * i)
  e <- as.integer((i * 5) %% 7 != 0)
  r <- morie_cox_score_process(0.3, z, t, e)
  expect_equal(as.numeric(r$U_final), .cox_u(0.3, z, t, e), tolerance = 1e-12)
})
