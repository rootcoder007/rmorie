# Numerical efficient influence curves (van der Laan & Rose 2018, Chap. 8):
# for the mean, d/de Psi(P_e) along a score s is E_w[(s - m)(x - mu)],
# and the estimated gradient is x - mu. Central differences with h = 1e-5
# are exact to O(h^2) here, hence tolerances of 1e-8.

he_x <- c(0.5, 1.7, -0.3, 2.2, 0.9, 1.1)
he_w <- c(0.1, 0.2, 0.15, 0.25, 0.2, 0.1)
he_psi <- function(w) sum(w * he_x)

test_that("the pathwise derivative of the mean along a tilt", {
  s <- c(1, -1, 0.5, 2, 0, -0.5)
  m <- sum(he_w * s)
  mu <- sum(he_w * he_x)
  ref <- sum(he_w * (s - m) * (he_x - mu))
  expect_equal(numerical_derivative(he_psi, he_w, s), ref, tolerance = 1e-8)
  expect_equal(morie_tlheic(he_psi, weights = he_w, score = s, mode = "deriv"), ref, tolerance = 1e-8)
  expect_error(numerical_derivative(he_psi, he_w, s[-1]), "6 weights but 5")
  expect_error(numerical_derivative(he_psi, he_w, s * 1e6), "left the simplex")
})

test_that("the inner product is the weighted mean of D s", {
  D <- he_x - 1
  s <- seq(-1, 1, length.out = 6)
  expect_equal(gradient_inner_product(D, s, he_w), sum(he_w * D * s) / sum(he_w), tolerance = 1e-15)
  expect_equal(gradient_inner_product(D, s), mean(D * s), tolerance = 1e-15)
  expect_error(gradient_inner_product(D, s[-1]), "6 gradient values but 5")
})

test_that("the estimated EIC of the mean is x - mu and verifies on a held-out path", {
  basis <- cbind(he_x, cos(3 * he_x))
  e <- estimate_eic(he_psi, basis, weights = he_w)
  mu <- sum(he_w * he_x)
  expect_equal(e$D, he_x - mu, tolerance = 1e-7)
  expect_equal(e$mean, 0, tolerance = 1e-12)
  held <- he_x^2
  v <- verify_gradient(he_psi, e$D, held, he_w)
  expect_true(v$verified)
  expect_equal(v$derivative, v$inner_product, tolerance = 1e-7)
  expect_equal(morie_tlheic(he_psi, basis = basis, weights = he_w)$D, e$D)
  expect_identical(morie_tlheic(he_psi, D = e$D, score = held, weights = he_w, mode = "verify")$verified,
                   TRUE)
  expect_equal(morie_tlheic(D = e$D, score = held, weights = he_w, mode = "grad"),
               gradient_inner_product(e$D, held, he_w))
  wrong <- verify_gradient(he_psi, rep(1, 6), held, he_w)
  expect_false(wrong$verified)
})
