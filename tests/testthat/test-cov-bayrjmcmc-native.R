# Coverage for reversible-jump MCMC (Green 1995): the step-function
# Poisson-process likelihood (eq. 9), the change-point move
# probabilities, the birth split / merge of heights and its Jacobian
# (against a finite-difference Jacobian), dimension-matching checks, the
# acceptance ratio (eq. 7), and the generic sampler -- one step
# regenerated from the uniform stream, and the model frequencies of a
# nested pair of Gaussians whose posterior model odds are known exactly.

test_that("step-function log-likelihood is sum log x(y_i) - integral of x", {
  y <- c(0, 0.1, 0.35, 0.5, 0.62, 0.9, 1)
  s <- c(0.3, 0.6)
  h <- c(2, 5, 1.5)
  e <- c(0, s, 1)
  j <- findInterval(y, e, all.inside = TRUE)
  ref <- sum(log(h[j])) - sum(h * diff(e))
  expect_equal(step_function_loglik(y, s, h, 1), ref, tolerance = 1e-12)
  expect_equal(step_function_loglik(y, numeric(0), 3, 1), 7 * log(3) - 3, tolerance = 1e-12)
  expect_identical(step_function_loglik(y, s, c(2, 0, 1), 1), -Inf)
  expect_error(step_function_loglik(y, s, h[-1], 1), "2 heights for 3 intervals")
  expect_error(step_function_loglik(c(y, 1.5), s, h, 1), "outside \\[0, 1\\]")
})

test_that("change-point move probabilities follow Green's b_k, d_k scaling", {
  mp <- changepoint_move_probabilities(3, 5, cap = 0.9)
  rb <- pmin(1, 3 / (1:6))
  rb[6] <- 0
  rd <- c(0, pmin(1, (1:5) / 3))
  cf <- 0.9 / max(rb + rd)
  expect_equal(mp$c, cf, tolerance = 1e-12)
  expect_equal(mp$b, cf * rb, tolerance = 1e-12)
  expect_equal(mp$d, cf * rd, tolerance = 1e-12)
  rest <- 1 - mp$b - mp$d
  expect_equal(mp$eta, c(rest[1], rest[-1] / 2), tolerance = 1e-12)
  expect_equal(mp$pi, c(0, rest[-1] / 2), tolerance = 1e-12)
  # detailed balance of the dimension moves under the Poisson prior
  k <- 0:4
  expect_equal(mp$b[k + 1] * stats::dpois(k, 3), mp$d[k + 2] * stats::dpois(k + 1, 3), tolerance = 1e-12)
  expect_error(changepoint_move_probabilities(0, 5), "lam must be positive")
  expect_error(changepoint_move_probabilities(1, 0), "k_max must be at least 1")
})

test_that("birth splits keep the weighted geometric mean and the Jacobian is (h'+h'')^2/h", {
  hs <- birth_split_heights(2, 0.3, 0.1, 0.4, 0.9)
  w <- 0.3 / 0.8
  expect_equal(w * log(hs[1]) + (1 - w) * log(hs[2]), log(2), tolerance = 1e-12)
  expect_equal(hs[2] / hs[1], 0.3 / 0.7, tolerance = 1e-12)
  expect_equal(birth_log_jacobian(2, hs[1], hs[2]), 2 * log(sum(hs)) - log(2), tolerance = 1e-12)
  fd <- numeric_log_jacobian(function(z) birth_split_heights(z[1], z[2], 0.1, 0.4, 0.9), c(2, 0.3))
  # central differences with h = 1e-6: O(h^2) truncation
  expect_equal(fd, birth_log_jacobian(2, hs[1], hs[2]), tolerance = 1e-8)
  expect_error(birth_split_heights(1, 0.5, 0.4, 0.4, 0.4), "empty interval")
})

test_that("numeric log-Jacobian of a linear map is log|det A|", {
  A <- matrix(c(2, 1, 0, -1, 3, 1, 0.5, 0, 4), 3)
  lj <- numeric_log_jacobian(function(z) as.vector(A %*% z), c(0.2, -1, 3))
  expect_equal(lj, as.numeric(determinant(A)$modulus), tolerance = 1e-8)
  expect_identical(numeric_log_jacobian(function(z) z, numeric(0)), 0)
  expect_identical(numeric_log_jacobian(function(z) c(z[1], z[1]), c(1, 2)), -Inf)
  expect_error(numeric_log_jacobian(function(z) z[1], c(1, 2)), "maps 2 values to 1")
})

test_that("rj_log_acceptance adds the four log ratios", {
  expect_equal(rj_log_acceptance(-3, -1, log(0.5), log(0.25), -0.9, -0.2, 0.7),
               2 + log(0.5) + 0.7 + 0.7, tolerance = 1e-12)
})

.models <- list(A = list(dim = 1, logpost = function(t) log(0.3) + stats::dnorm(t, log = TRUE)),
                B = list(dim = 2, logpost = function(t) log(0.7) + sum(stats::dnorm(t, log = TRUE))))
.moves <- list(
  list(frm = "A", to = "B", n_u = 1, n_u_rev = 0,
       propose = function(t, uni) stats::qnorm(uni()),
       transform = function(t, u) list(c(t, u), numeric(0)),
       logq = function(t, u) stats::dnorm(u, log = TRUE)),
  list(frm = "B", to = "A", n_u = 0, n_u_rev = 1,
       propose = function(t, uni) numeric(0),
       transform = function(t, u) list(t[1], t[2]),
       logq_rev = function(t, u) stats::dnorm(u, log = TRUE)))

test_that("dimension matching and move-set checks", {
  bp <- check_dimension_matching(.models, .moves)
  expect_setequal(names(bp), c("A->B", "B->A"))
  expect_error(check_dimension_matching(list(), .moves), "no models")
  expect_error(check_dimension_matching(list(A = list(dim = 1)), list()), "needs 'dim' and 'logpost'")
  expect_error(check_dimension_matching(.models, .moves[1]), "has no reverse move")
  bad <- .moves
  bad[[1]]$n_u <- 2
  expect_error(check_dimension_matching(.models, bad), "violates dimension matching")
  bad <- .moves
  bad[[2]]$to <- "C"
  expect_error(check_dimension_matching(.models, bad), "unknown model")
  bad <- .moves
  bad[[1]]$to <- "A"
  expect_error(check_dimension_matching(.models, bad), "within-model move")
  expect_error(check_dimension_matching(.models, c(.moves, .moves[1])), "two moves given")
  bad <- .moves
  bad[[1]]$propose <- NULL
  expect_error(check_dimension_matching(.models, bad), "missing propose")
})

test_that("one jump step is regenerated from the uniform stream", {
  u <- .ghc_unif(.ghc_rng(4), 3)
  r <- reversible_jump_mcmc(.models, .moves, "A", 0.4, n_iter = 1, seed = 4, within_weight = 0)
  z <- stats::qnorm(u[2])
  # A has only the jump (weight 1 of 1); B offers within (0) and the reverse
  la <- (log(0.7) + stats::dnorm(0.4, log = TRUE) + stats::dnorm(z, log = TRUE)) -
    (log(0.3) + stats::dnorm(0.4, log = TRUE)) - stats::dnorm(z, log = TRUE)
  jumped <- log(u[3]) < la
  expect_identical(r$chain[[1]]$model, if (jumped) "B" else "A")
  if (jumped) expect_equal(r$chain[[1]]$theta, c(0.4, z), tolerance = 1e-12)
  expect_identical(r$tried[["A->B"]], 1L)
})

test_that("model frequencies converge to the exact posterior odds 0.3 : 0.7", {
  r <- reversible_jump_mcmc(.models, .moves, "A", 0, n_iter = 20000, burn_in = 1000, seed = 2, keep_chain = FALSE)
  # posterior model probability 0.7 for B; the chain is autocorrelated, so
  # the tolerance is ~4 binomial standard errors inflated by an IACT of ~5
  expect_lt(abs(r$model_freq$B - 0.7), 0.03)
  expect_identical(r$n_kept, 19000L)
  expect_length(r$chain, 0L)
  expect_true(all(c("within:A", "within:B", "A->B", "B->A") %in% names(r$tried)))
  # the numeric Jacobian route recovers log 1 = 0 and the same answer
  n <- reversible_jump_mcmc(.models, .moves, "A", 0, n_iter = 3000, burn_in = 100, seed = 2, jacobian = "numeric", thin = 3)
  a <- reversible_jump_mcmc(.models, .moves, "A", 0, n_iter = 3000, burn_in = 100, seed = 2, thin = 3)
  expect_identical(n$visits, a$visits)
  expect_length(a$chain, ceiling(2900 / 3))
  expect_identical(morie_bayrjmcmc, reversible_jump_mcmc)
})

test_that("sampler argument checks", {
  expect_error(reversible_jump_mcmc(.models, .moves, "A", 0, jacobian = "exact"), "jacobian must be one of")
  expect_error(reversible_jump_mcmc(.models, .moves, "C", 0), "not a model")
  expect_error(reversible_jump_mcmc(.models, .moves, "A", c(0, 1)), "init_theta has 2 values")
  expect_error(reversible_jump_mcmc(.models, .moves, "A", 0, n_iter = 5, burn_in = 5), "burn_in must be in")
  expect_error(reversible_jump_mcmc(.models, .moves, "A", 0, thin = 0), "thin must be at least 1")
  bad <- .moves
  bad[[1]]$propose <- function(t, uni) c(1, 2)
  expect_error(reversible_jump_mcmc(.models, bad, "A", 0, n_iter = 1, within_weight = 0), "proposed 2 values")
})

test_that("the change-point sampler reproduces its prior and the exact k = 1 : k = 0 odds", {
  # likelihood off: k ~ Poisson(lam) truncated at k_max and every height
  # ~ Gamma(alpha, beta); 19000 correlated draws, so 0.03 absolute
  r0 <- changepoint_rjmcmc(L = 1, n_iter = 20000, burn_in = 1000, seed = 3, lam = 2, k_max = 10,
                           alpha = 2, beta = 4, use_likelihood = FALSE)
  tp <- stats::dpois(0:10, 2) / sum(stats::dpois(0:10, 2))
  expect_lt(max(abs(r0$k_posterior - tp)), 0.03)
  expect_lt(abs(r0$mean_height - 0.5), 0.03)
  # with data: P(k=1)/P(k=0) = integral of 6 s (1 - s) M(n1, s) M(n2, 1 - s)
  # over s, divided by M(n, 1), with M the Gamma-Poisson marginal (lam = 1
  # makes the Poisson prior ratio 1). MCMC ratio of counts, 10% relative.
  set.seed(5)
  y <- sort(c(stats::runif(8, 0, 0.5), stats::runif(18, 0.5, 1)))
  n <- length(y)
  M <- function(m, l) 2 * log(0.1) + lgamma(m + 2) - (m + 2) * log(l + 0.1)
  f <- function(s) vapply(s, function(v) {
    n1 <- sum(y < v)
    6 * v * (1 - v) * exp(M(n1, v) + M(n - n1, 1 - v) - M(n, 1))
  }, 1)
  exact <- stats::integrate(f, 0, 1, subdivisions = 2000)$value
  r <- changepoint_rjmcmc(y = y, L = 1, n_iter = 40000, burn_in = 2000, seed = 1,
                          alpha = 2, beta = 0.1, lam = 1, k_max = 10)
  expect_equal(r$k_counts[2] / r$k_counts[1], exact, tolerance = 0.1)
  expect_equal(sum(r$k_posterior), 1, tolerance = 1e-12)
  expect_error(changepoint_rjmcmc(y = 2, L = 1), "outside")
  expect_error(changepoint_rjmcmc(L = 1, k_init = 40, k_max = 30), "k_init must be in")
})
