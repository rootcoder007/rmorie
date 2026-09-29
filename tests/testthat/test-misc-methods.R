test_that("Gdpf trade-off and delta", {
  r <- Gdpf(mu = 1.3, alpha = c(0.05, 0.3), epsilon = 0.8)
  expect_equal(r$trade_off, pnorm(qnorm(1 - c(0.05, 0.3)) - 1.3))
  expect_equal(r$delta, pnorm(-0.8 / 1.3 + 0.65) - exp(0.8) * pnorm(-0.8 / 1.3 - 0.65))
  expect_equal(Gdpf(mech = c(2, 4))$mu, 0.5)
})

test_that("Hmicl assembles the prompt and softmaxes the scores", {
  sc <- function(p, cand) lengths(regmatches(p, gregexpr(cand, p, fixed = TRUE)))
  r <- Hmicl(sc, list(list("a", "pos"), list("b", "pos"), list("c", "neg")), "d")
  expect_equal(r$prompt, "a -> pos\nb -> pos\nc -> neg\nd ->")
  expect_equal(r$prediction, "pos")
  expect_equal(r$posterior, exp(c(2, 1)) / sum(exp(c(2, 1))))
})

test_that("Ksamp standardised statistic", {
  a <- c(0.1, 1.2, 0.5, 2.2, 1.9, 0.7, 1.2)
  b <- c(3.1, 2.4, 4.2, 3.3, 2.9, 1.2)
  r <- Ksamp(a, b)
  expect_equal(r$k, 2)
  expect_equal(Ksamp(a, a + 5)$p_value, 0.001)
})
