# MVSML chapter 10: ANN / backpropagation.

X <- matrix(c(1,0,0, 1,0,1, 1,1,0, 1,1,1), nrow = 4, byrow = TRUE)
Y <- matrix(c(0, 1, 1, 0), ncol = 1)

mknet <- function(seed = 11) {
  set.seed(seed)
  list(matrix(rnorm(12), nrow = 4), matrix(rnorm(4), nrow = 1))
}

test_that("forward pass matches eq (10.1)-(10.3)", {
  W <- mknet()
  f <- morie_ann_forward(X, W, c("logistic", "identity"))
  z <- sum(W[[1]][1, ] * X[1, ])
  expect_equal(f$layers[[2]][1, 1], 1 / (1 + exp(-z)),
               tolerance = 1e-12)
  expect_equal(f$output[1, 1],
               sum(W[[2]][1, ] * f$layers[[2]][1, ]),
               tolerance = 1e-12)
})

test_that("eq (10.5) SSE matches the formula", {
  expect_equal(morie_ann_sse(matrix(c(0.5, 0.2)),
                                   matrix(c(1, 0))),
               0.5 * (0.25 + 0.04), tolerance = 1e-12)
})

test_that("backprop gradients match central differences", {
  W <- mknet()
  for (hid in c("logistic", "tanh")) {
    acts <- c(hid, "identity")
    ana <- morie_ann_gradients(X, Y, W, acts)$gradients
    num <- morie_ann_numeric_gradient(X, Y, W, acts)
    for (li in seq_along(W)) {
      expect_equal(ana[[li]], num[[li]], tolerance = 1e-5)
    }
  }
})

test_that("training decreases the loss for a small learning rate", {
  W <- mknet(7)
  r <- morie_ann_train(X, Y, W, eta = 0.05, n_iter = 3000L,
                             activations = c("logistic", "identity"))
  expect_lt(tail(r$history, 1), r$history[1])
  expect_true(all(diff(r$history) <= 1e-9))
})
