test_that("SymbolicIntegrate antiderivatives differentiate back", {
  for (e in c("x*exp(2*x)", "1/(x^2+1)", "2*x*cos(x^2)", "x^2*sin(x)", "1/(x^2-1)", "(x+1)/(x^2+2*x+5)",
              "exp(x)*sin(2*x)", "atan(x)", "1/(x^3+1)", "x^2*log(3*x)")) {
    r <- SymbolicIntegrate(e)
    expect_true(r$verified)
    f <- function(s, x) eval(parse(text = s), list(x = x))
    for (x in c(0.37, 1.13)) {
      h <- 1e-5
      fd <- suppressWarnings((f(r$antiderivative, x + h) - f(r$antiderivative, x - h)) / (2 * h))
      if (is.finite(fd)) expect_equal(fd, f(e, x), tolerance = 1e-6)
    }
  }
  expect_equal(SymbolicIntegrate("1/(x^2+1)")$antiderivative, "atan(x)")
  expect_null(SymbolicIntegrate("exp(x^2)")$antiderivative)
})

test_that("SymbolicDiff applies the product rule", {
  d <- SymbolicDiff("x^3*sin(x)")$derivative
  x <- 0.4
  expect_equal(eval(parse(text = d)), 3 * x^2 * sin(x) + x^3 * cos(x), tolerance = 1e-12)
})
