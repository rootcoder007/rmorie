# AlphaFold-3 SampleDiffusion (Abramson et al. 2024, Algorithm 18) on the
# Karras et al. (2022) sigma schedule.
#
# Anchors outside the module: the closed form of the noise schedule, the
# published Table 6 defaults, the centring identity, and the decisive
# behavioural one -- a denoiser that already knows the answer must drive
# the sampler onto that answer, which is what the reverse diffusion is for.

test_that("the noise schedule is the Karras closed form", {
  sd_ <- 16
  smax <- 160
  smin <- 4e-4
  rho <- 7
  for (T in c(1L, 5L, 20L)) {
    g <- .alfbnp_schedule(T, sd_, smax, smin, rho)
    a <- smax^(1 / rho)
    b <- smin^(1 / rho)
    want <- c(vapply(seq_len(T),
                     function(i) sd_ * (a + ((i - 1) / T) * (b - a))^rho,
                     numeric(1)), 0)
    expect_equal(g, want)
    # one entry per step plus the terminal zero
    expect_length(g, T + 1L)
    # the schedule starts at sigma_data times s_max and ends at exactly zero
    expect_equal(g[1], sd_ * smax)
    expect_equal(g[T + 1L], 0)
    # and it decreases all the way down
    expect_true(all(diff(g) < 0))
  }
  # rho = 1 is the linear interpolation in sigma
  lin <- .alfbnp_schedule(4L, 1, 16, 1, 1)
  expect_equal(lin, c(16, 16 + (1:3 / 4) * (1 - 16), 0))
  # a larger rho front-loads the noise more steeply
  s7 <- .alfbnp_schedule(10L, 1, 100, 0.01, 7)
  s2 <- .alfbnp_schedule(10L, 1, 100, 0.01, 2)
  expect_true(s7[6] < s2[6])
})

test_that("centring removes the centroid", {
  X <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), nrow = 3, byrow = TRUE)
  C <- .alfbnp_centre(X)
  expect_equal(colMeans(C), rep(0, 3))
  # centring is exactly the subtraction of the centroid
  expect_equal(C, X - matrix(colMeans(X), nrow(X), 3, byrow = TRUE))
  expect_equal(dim(C), dim(X))
  # an already-centred structure is unchanged
  expect_equal(.alfbnp_centre(C), C)
  # pairwise distances, the only physically meaningful quantity, survive
  expect_equal(as.numeric(dist(C)), as.numeric(dist(X)))
})

test_that("atom coordinates are read from whatever shape they arrive in", {
  m <- matrix(1:6, 2, 3)
  expect_equal(.alfbnp_atoms(m, "x"), matrix(as.double(1:6), 2, 3))
  expect_equal(.alfbnp_atoms(as.data.frame(m), "x"),
               matrix(as.double(1:6), 2, 3), ignore_attr = TRUE)
  # a list of atoms becomes one row each
  expect_equal(.alfbnp_atoms(list(c(1, 2, 3), c(4, 5, 6)), "x"),
               matrix(c(1, 4, 2, 5, 3, 6), 2, 3))
  # a flat vector is filled row-major, three coordinates at a time
  expect_equal(.alfbnp_atoms(c(1, 2, 3, 4, 5, 6), "x"),
               matrix(c(1, 4, 2, 5, 3, 6), 2, 3))
  expect_error(.alfbnp_atoms(matrix(1:4, 2, 2), "coords"),
               "each atom needs exactly x, y, z")
  expect_error(.alfbnp_atoms(matrix(0, 0, 3), "coords"), "no atoms")
})

test_that("a denoiser that knows the answer drives the sampler to it", {
  set.seed(1)
  target <- .alfbnp_centre(matrix(rnorm(15), 5, 3))
  den <- function(X, sigma) target
  r <- morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L, steps = 40L,
                               seed = 2)
  # the reverse diffusion lands on the structure the denoiser reports
  expect_equal(r$coords, target, tolerance = 1e-3)
  expect_true(max(abs(r$coords - target)) < 1e-3)
  # the sample is centred, as every intermediate is
  expect_equal(colMeans(r$coords), rep(0, 3), tolerance = 1e-12)
  expect_equal(dim(r$coords), c(5L, 3L))
  # more steps get closer to the fixed point
  short <- morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L, steps = 4L,
                                   seed = 2)
  expect_true(max(abs(short$coords - target)) >
              max(abs(r$coords - target)))
})

test_that("the run is reproducible and seed-dependent", {
  set.seed(3)
  target <- .alfbnp_centre(matrix(rnorm(15), 5, 3))
  den <- function(X, sigma) target
  a <- morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L, steps = 10L,
                               seed = 3)
  b <- morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L, steps = 10L,
                               seed = 3)
  cc <- morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L, steps = 10L,
                                seed = 9)
  # the noise stream is deterministic, so a run reproduces exactly
  expect_equal(a$coords, b$coords)
  # and a different seed takes a different path
  expect_false(isTRUE(all.equal(a$coords, cc$coords)))
  # an explicit starting structure is honoured
  x0 <- .alfbnp_centre(matrix(rnorm(15), 5, 3))
  d <- morie_alfbnp_af3_sample(denoiser = den, x_init = x0, steps = 10L,
                               seed = 3)
  expect_equal(d$n_atoms, 5L)
  expect_equal(dim(d$coords), c(5L, 3L))
})

test_that("the run reports its schedule, trace and route", {
  set.seed(5)
  target <- .alfbnp_centre(matrix(rnorm(18), 6, 3))
  den <- function(X, sigma) target
  r <- morie_alfbnp_af3_sample(denoiser = den, n_atoms = 6L, steps = 12L,
                               seed = 2)
  # the schedule returned is the one the sampler walked
  expect_equal(r$sigmas,
               .alfbnp_schedule(12L, 16, 160, 4e-4, 7))
  expect_length(r$sigmas, 13L)
  expect_equal(r$steps, 12L)
  expect_equal(r$sigma_data, 16)
  expect_equal(r$n_atoms, 6L)
  # one trace row per step, recording the step index and its noise level
  expect_equal(dim(r$trace), c(12L, 4L))
  expect_equal(r$trace[, 1], 1:12)
  expect_equal(r$trace[, 2], r$sigmas[2:13])
  # with no reference structure there is nothing to compare against, so the
  # summary falls back on the terminal noise level, which is zero
  expect_null(r$rmsd_to_reference)
  expect_equal(r$estimate, 0)
  expect_null(r$denoiser_coefs)
  expect_match(r$route, "supplied denoiser")
  expect_match(r$method, "Abramson")
})

test_that("without a denoiser one is fitted by score matching", {
  set.seed(7)
  cl <- list(.alfbnp_centre(matrix(rnorm(15), 5, 3)),
             .alfbnp_centre(matrix(rnorm(15), 5, 3)))
  r <- morie_alfbnp_af3_sample(clean = cl, steps = 10L, seed = 4)
  expect_match(r$route, "fitted a linear denoiser")
  expect_false(is.null(r$denoiser_coefs))
  expect_true(all(is.finite(r$denoiser_coefs)))
  # a reference set means the deviation from it can be reported
  expect_false(is.null(r$rmsd_to_reference))
  expect_true(r$rmsd_to_reference >= 0)
  expect_equal(r$estimate, r$rmsd_to_reference)
  expect_equal(r$n_atoms, 5L)
  expect_equal(colMeans(r$coords), rep(0, 3), tolerance = 1e-12)
  # sigma_data can be estimated from the reference structures instead of set
  rf <- morie_alfbnp_af3_sample(clean = cl, steps = 10L, seed = 4,
                                sigma_data = "fit")
  expect_true(rf$sigma_data > 0)
  expect_false(isTRUE(all.equal(rf$sigma_data, 16)))
  # it is the root mean square coordinate of the references
  tot <- sum(vapply(cl, function(X) sum(X * X), numeric(1)))
  cnt <- sum(vapply(cl, length, numeric(1)))
  expect_equal(rf$sigma_data, sqrt(tot / cnt))
})

test_that("alfbnp refuses input that leaves the problem undefined", {
  den <- function(X, sigma) matrix(0, 5, 3)
  # the atom count has to come from somewhere
  expect_error(morie_alfbnp_af3_sample(denoiser = den),
               "give n_atoms, x_init, or clean")
  expect_error(morie_alfbnp_af3_sample(denoiser = den, n_atoms = 0L),
               "at least one atom")
  expect_error(morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L,
                                       steps = 0L),
               "steps must be at least 1")
  # references of different sizes are not a reference set
  ragged <- list(matrix(0, 5, 3), matrix(0, 4, 3))
  expect_error(morie_alfbnp_af3_sample(clean = ragged, steps = 5L),
               "different atom counts")
  # fitting sigma_data needs something to fit it to
  expect_error(morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L,
                                       sigma_data = "fit", steps = 5L),
               "needs .clean.")
  expect_error(morie_alfbnp_af3_sample(denoiser = den, n_atoms = 5L,
                                       sigma_data = "auto", steps = 5L),
               "must be a number or 'fit'")
})
