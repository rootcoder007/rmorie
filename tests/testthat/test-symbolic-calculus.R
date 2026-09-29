# the returned expression strings are valid R, so R itself is the independent evaluator here
.sc_eval <- function(s, env) eval(parse(text = s), envir = list2env(as.list(env), parent = baseenv()))

.sc_dx <- function(s, v, env = list(), h = 1e-5) {
  (.sc_eval(s, c(list(x = v + h), env)) - .sc_eval(s, c(list(x = v - h), env))) / (2 * h)
}

.det_rec <- function(A) {
  n <- nrow(A)
  if (n == 1) return(A[1, 1])
  sum(vapply(seq_len(n), function(j) (-1)^(j - 1) * A[1, j] * .det_rec(A[-1, -j, drop = FALSE]), 0))
}

test_that("Risch integrals differentiate back to the integrand", {
  for (q in c("(x^2 + 1)/(x^3 - x)", "x/(x^2 + 2*x + 1)", "1/(x^2 + 1)", "x^3/(x^2 - 1)", "1/(x^2*(x + 1))",
              "1/(x^2 + 1)^2", "(x^4 + 1)/(x^3 + x)")) {
    r <- RischIntegration(q)
    expect_true(r$rational)
    expect_null(r$out_of_scope)
    expect_true(r$verified)
    for (v in c(1.63, 2.71, 3.4)) {
      expect_equal(.sc_dx(r$integral, v), .sc_eval(q, list(x = v)), tolerance = 1e-6)
    }
  }
})

test_that("the Risch parts match the known forms", {
  r <- RischIntegration("1/(x^2 + 1)")
  expect_equal(c(r$polynomial_part, r$rational_part, r$log_part), c("0", "0", "atan(x)"))
  r <- RischIntegration("x/(x^2 + 2*x + 1)")
  expect_equal(r$log_part, "log(x + 1)")
  for (v in c(0.4, 2.2)) expect_equal(.sc_eval(r$rational_part, list(x = v)), -v / (v + 1), tolerance = 1e-12)
  r <- RischIntegration("x^3/(x^2 - 1)")
  for (v in c(1.3, 2.5)) {
    expect_equal(.sc_eval(r$polynomial_part, list(x = v)), 0.5 * v * v, tolerance = 1e-12)
    expect_equal(.sc_eval(r$integral, list(x = v)), 0.5 * v * v + 0.5 * log(v + 1) + 0.5 * log(v - 1),
                 tolerance = 1e-9)
  }
})

test_that("Risch reports what is out of scope", {
  r <- RischIntegration("exp(x^2)")
  expect_null(r$integral)
  expect_false(r$rational)
  expect_equal(r$generators, "exp")
  expect_match(r$out_of_scope, "transcendental tower")
  r <- RischIntegration("x^(1/2)")
  expect_equal(r$generators, "algebraic")
  expect_match(r$out_of_scope, "algebraic extensions")
  expect_match(RischIntegration("sin(x)/x")$out_of_scope, "^the transcendental tower")
})

test_that("limits agree with the values of the function nearby", {
  cases <- list(list("sin(x)/x", 0, "both", 1, "series"), list("(1 - cos(x))/x^2", 0, "both", 0.5, "series"),
                list("(exp(x) - 1)/x", 0, "both", 1, "series"), list("tan(x)/x", 0, "both", 1, "series"),
                list("(x^3 - 1)/(x - 1)", 1, "both", 3, "series"),
                list("(x^2 + 3*x)/(2*x^2 - 1)", "inf", "both", 0.5, "degrees"),
                list("x*exp(-x)", "inf", "both", 0, "order"), list("log(x)/x", "inf", "both", 0, "order"),
                list("sin(x)/x", "inf", "both", 0, "squeeze"))
  for (cs in cases) {
    r <- SymbolicLimit(cs[[1]], "x", cs[[2]], side = cs[[3]])
    expect_equal(r$method, cs[[5]])
    expect_equal(r$limit, cs[[4]], tolerance = 1e-12)
    v <- if (identical(cs[[2]], "inf")) 1e6 else as.numeric(cs[[2]]) + 1e-5
    expect_equal(.sc_eval(cs[[1]], list(x = v)), cs[[4]], tolerance = 2e-4)
  }
})

test_that("infinite and one-sided limits", {
  expect_equal(SymbolicLimit("1/x", "x", 0, side = "+")$limit, Inf)
  expect_equal(SymbolicLimit("1/x", "x", 0, side = "-")$limit, -Inf)
  expect_error(SymbolicLimit("1/x", "x", 0), "one-sided")
  expect_equal(SymbolicLimit("exp(x)/x^5", "x", "inf")$limit, Inf)
  expect_equal(SymbolicLimit("(3*x^3 - x)/(x^2 + 1)", "x", "-inf")$limit, -Inf)
  expect_equal(SymbolicLimit("x^2*exp(-x^2)", "x", "inf")$limit, 0)
  r <- SymbolicLimit("3*x + 1", "x", 2)
  expect_equal(c(r$limit, 0), c(7, 0))
  expect_equal(r$method, "substitution")
  expect_error(SymbolicLimit("(1 + 1/x)^x", "x", "inf"), "cannot determine")
  expect_error(SymbolicLimit("sin(x)", "x", "inf"), "cannot determine")
  expect_error(SymbolicLimit("x", "x", 0, side = "up"), "side must be")
})

test_that("symbolic determinant, inverse and characteristic polynomial", {
  r <- MatrixSymbolic(list(c("a", "b"), c("c", "d")))
  env <- list(a = 2, b = 1, c = 1, d = 3)
  A <- matrix(c(2, 1, 1, 3), 2, 2, byrow = TRUE)
  expect_equal(.sc_eval(r$determinant, env), .det_rec(A), tolerance = 1e-12)
  inv <- matrix(vapply(r$inverse, .sc_eval, 0, env = env), 2, 2, byrow = TRUE)
  expect_equal(A %*% inv, diag(2), tolerance = 1e-12)
  for (t in c(0.3, -1.7)) {
    expect_equal(.sc_eval(r$char_poly, c(list(t = t), env)), .det_rec(A - t * diag(2)), tolerance = 1e-12)
  }
  expect_equal(r$trace, "a + d")
  for (lam in r$eigenvalues) {
    v <- .sc_eval(lam, env)
    expect_equal(.sc_eval(r$char_poly, c(list(t = v), env)), 0, tolerance = 1e-9)
  }
})

test_that("numeric eigenvalues and larger matrices", {
  r <- MatrixSymbolic(list(c(2, 1), c(1, 2)))
  expect_equal(r$eigenvalues_numeric, c(1, 3))
  A <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), 3, 3, byrow = TRUE)
  r <- MatrixSymbolic(list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 10)))
  expect_equal(.sc_eval(r$determinant, list()), .det_rec(A), tolerance = 1e-12)
  expect_length(r$eigenvalues_numeric, 3)
  for (lam in r$eigenvalues_numeric) expect_equal(.det_rec(A - lam * diag(3)), 0, tolerance = 1e-9)
  expect_null(r$eigenvalues)
  expect_error(MatrixSymbolic(list(c(1, 2, 3), c(4, 5, 6))), "square")
})

.ode_holds <- function(sol, rhs, xs, cval) {
  for (v in xs) {
    yv <- .sc_eval(sol, list(x = v, C = cval))
    dy <- (.sc_eval(sol, list(x = v + 1e-6, C = cval)) - .sc_eval(sol, list(x = v - 1e-6, C = cval))) / 2e-6
    if (!is.finite(yv) || !is.finite(dy)) return(FALSE)
    if (abs(dy - .sc_eval(rhs, list(x = v, y = yv))) > 1e-5 * max(1, abs(dy))) return(FALSE)
  }
  TRUE
}

test_that("ODE solutions satisfy the equation", {
  cases <- list(list("2*x*y", c("separable", "linear"), c(0.8, 1.4, 2.3), 0.7),
                list("y + x", "linear", c(0.8, 1.4, 2.3), 0.7),
                list("2*y/x + x^2", "linear", c(0.8, 1.4, 2.3), 0.7),
                list("x*y + x*y^3", c("separable", "bernoulli"), c(0.3, 0.6, 0.9), 300),
                list("(x + y)/x", c("linear", "homogeneous"), c(0.8, 1.4, 2.3), 0.7),
                list("exp(x)*y", c("separable", "linear"), c(0.8, 1.4, 2.3), 0.7))
  for (cs in cases) {
    r <- OdeSymbolic(cs[[1]])
    expect_equal(r$classes, cs[[2]])
    expect_equal(r$form, "explicit")
    expect_true(.ode_holds(r$solution, cs[[1]], cs[[3]], cs[[4]]))
  }
})

test_that("ODE classification criteria and implicit solutions", {
  r <- OdeSymbolic("y' = 2*x*y")
  expect_equal(r$solution, "C*exp(x^2)")
  expect_equal(c(r$coefficients$p, r$coefficients$q), c("2*x", "0"))
  expect_equal(OdeSymbolic("x*y + x*y^3")$exponent, 3)
  s <- OdeSymbolic("y^2*x")
  expect_equal(s$form, "implicit")
  expect_equal(s$classes, "separable")
  sides <- strsplit(s$solution, "=")[[1]]
  for (i in 1:2) {
    v <- c(0.9, 1.7)[i]
    cval <- c(0.5, -0.3)[i]
    yv <- -1 / (cval + 0.5 * v * v)
    expect_equal(.sc_eval(sides[1], list(y = yv)), .sc_eval(sides[2], list(x = v, C = cval)), tolerance = 1e-9)
  }
  e <- OdeSymbolic(M = "2*x*y", N = "x^2 + 1")
  expect_equal(e$classes, "exact")
  expect_equal(e$solution, "x^2*y + y = C")
  expect_length(OdeSymbolic(M = "y", N = "x^2")$classes, 0)
  expect_error(OdeSymbolic(M = "y"), "both M and N")
  expect_error(OdeSymbolic("z' = x"), "left-hand side")
})
