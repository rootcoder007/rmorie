# Anchors for the docking MDP of Wang et al. (2022), BMC Bioinformatics
# 23, 368.
#
# The action space is a rigid motion, so most of this module is checkable
# against geometry rather than against itself: a rotation preserves every
# pairwise distance and the centroid, four quarter turns return the
# original coordinates, and a translation moves exactly one axis. The
# reward is the paper's asymmetric shaping, exp(-rmsd/box) improvement
# with a doubled penalty when the pose gets worse.

pts <- function(seed = 1, n = 7) {
  set.seed(seed)
  matrix(rnorm(n * 3), n, 3)
}
pdist <- function(M) as.numeric(stats::dist(M))

test_that("the centroid is the column mean", {
  P <- pts()
  expect_equal(.agalfsy_centroid(P), colMeans(P), tolerance = 1e-12)
  # a single atom is its own centroid
  one <- matrix(c(1, 2, 3), nrow = 1)
  expect_equal(.agalfsy_centroid(one), c(1, 2, 3), tolerance = 1e-12)
})

test_that("RMSD is the root mean squared deviation over all three axes", {
  P <- pts()
  expect_identical(.agalfsy_rmsd(P, P), 0)
  # a uniform shift of s along one axis gives an RMSD of exactly s
  Q <- P; Q[, 1] <- Q[, 1] + 3
  expect_equal(.agalfsy_rmsd(P, Q), 3, tolerance = 1e-12)
  # shifting all three axes by s gives sqrt(3) s
  Z <- P + 2
  expect_equal(.agalfsy_rmsd(P, Z), sqrt(3) * 2, tolerance = 1e-12)
  # and it agrees with the closed form on arbitrary coordinates
  A <- pts(2); B <- pts(3)
  expect_equal(.agalfsy_rmsd(A, B), sqrt(mean(rowSums((A - B)^2))),
               tolerance = 1e-12)
  # symmetric
  expect_equal(.agalfsy_rmsd(A, B), .agalfsy_rmsd(B, A), tolerance = 1e-12)
})

test_that("a rotation is a rigid motion about the centroid", {
  P <- pts()
  for (axis in 0:2) {
    for (deg in c(7, 37, -90, 180)) {
      R <- .agalfsy_rotate(P, axis, deg)
      # every pairwise distance survives
      expect_equal(pdist(R), pdist(P), tolerance = 1e-12)
      # the centroid does not move
      expect_equal(.agalfsy_centroid(R), .agalfsy_centroid(P), tolerance = 1e-12)
      # the axis turned about keeps its own coordinate
      expect_equal(R[, axis + 1L], P[, axis + 1L], tolerance = 1e-12)
    }
  }
})

test_that("rotations compose the way rotations must", {
  P <- pts()
  # a full turn is the identity
  for (axis in 0:2) {
    expect_equal(.agalfsy_rotate(P, axis, 360), P, tolerance = 1e-10)
    # four quarter turns likewise
    q <- P
    for (k in 1:4) q <- .agalfsy_rotate(q, axis, 90)
    expect_equal(q, P, tolerance = 1e-10)
    # and a turn undone by its negative
    expect_equal(.agalfsy_rotate(.agalfsy_rotate(P, axis, 41), axis, -41), P,
                 tolerance = 1e-10)
    # two halves make a whole
    expect_equal(.agalfsy_rotate(.agalfsy_rotate(P, axis, 20), axis, 20),
                 .agalfsy_rotate(P, axis, 40), tolerance = 1e-10)
  }
})

test_that("a translation moves one axis and leaves the others alone", {
  P <- pts()
  for (axis in 0:2) {
    T1 <- .agalfsy_translate(P, axis, 2.5)
    expect_equal(T1[, axis + 1L], P[, axis + 1L] + 2.5, tolerance = 1e-12)
    others <- setdiff(1:3, axis + 1L)
    expect_equal(T1[, others], P[, others], tolerance = 1e-12)
    # shape is preserved, so every pairwise distance is too
    expect_equal(pdist(T1), pdist(P), tolerance = 1e-12)
    # and the centroid moves by exactly the step
    expect_equal(.agalfsy_centroid(T1)[axis + 1L],
                 .agalfsy_centroid(P)[axis + 1L] + 2.5, tolerance = 1e-12)
  }
})

test_that("the twelve actions are the six translations and six rotations", {
  P <- pts()
  # actions 0..5 translate by +/- the translation step along each axis
  for (a in 0:5) {
    axis <- a %/% 2L
    step <- if (a %% 2L == 0L) 0.1 else -0.1
    expect_equal(.agalfsy_apply(P, a), .agalfsy_translate(P, axis, step),
                 tolerance = 1e-12)
  }
  # actions 6..11 rotate by +/- the rotation step about each axis
  for (a in 6:11) {
    axis <- (a - 6L) %/% 2L
    deg <- if ((a - 6L) %% 2L == 0L) 1 else -1
    expect_equal(.agalfsy_apply(P, a), .agalfsy_rotate(P, axis, deg),
                 tolerance = 1e-12)
  }
  # every action is a rigid motion
  for (a in 0:11) expect_equal(pdist(.agalfsy_apply(P, a)), pdist(P),
                               tolerance = 1e-12)
  # an action and its opposite cancel
  for (a in seq(0, 10, by = 2)) {
    expect_equal(.agalfsy_apply(.agalfsy_apply(P, a), a + 1L), P,
                 tolerance = 1e-10)
  }
})

test_that("the reward rewards improvement and penalises worsening twice over", {
  P <- pts()
  site <- P + 5
  k <- function(M) exp(-.agalfsy_rmsd(site, M) / 18)
  closer <- P + 4.5        # RMSD 0.87 against 8.66
  farther <- P - 5         # RMSD 17.32 against 8.66

  # no movement earns nothing
  expect_identical(.agalfsy_reward(site, P, P), 0)

  # an improvement is the plain difference of the shaped distances
  r_up <- .agalfsy_reward(site, P, closer)
  expect_gt(r_up, 0)
  expect_equal(r_up, k(closer) - k(P), tolerance = 1e-12)

  # a worsening is that difference doubled, which is the paper's asymmetry
  r_dn <- .agalfsy_reward(site, P, farther)
  expect_lt(r_dn, 0)
  expect_equal(r_dn, 2 * (k(farther) - k(P)), tolerance = 1e-12)

  # so the same distance change costs more than it pays
  mid <- P + 5 - 2         # a pose 2 units out
  worse <- P + 5 + 2       # ...and one 2 units out the other way
  expect_equal(.agalfsy_rmsd(site, mid), .agalfsy_rmsd(site, worse),
               tolerance = 1e-12)
  expect_equal(.agalfsy_reward(site, mid, worse), 0, tolerance = 1e-12)
})

test_that("coordinate coercion accepts the shapes it documents", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
  expect_equal(.agalfsy_coords(m, "x"), m)
  expect_equal(.agalfsy_coords(as.data.frame(m), "x"), m, ignore_attr = TRUE)
  expect_equal(.agalfsy_coords(list(c(1, 2, 3), c(4, 5, 6)), "x"), m)
  # three columns are required for a coordinate set
  expect_error(.agalfsy_coords(matrix(1:4, nrow = 2), "x"))
})

test_that("the pose search runs and reports a pose of the ligand's shape", {
  set.seed(4)
  receptor <- pts(5, 12)
  ligand <- pts(6, 5)
  site <- ligand + 3
  res <- morie_agalfsy_rl_pose_search(receptor, ligand, site = site,
                                      max_steps = 60L, min_steps = 20L,
                                      window = 10L, seed = 2)
  expect_true(is.list(res))
  # whatever it reports, the pose is still the same number of atoms in 3D
  pose <- res$pose
  if (!is.null(pose)) {
    expect_identical(dim(pose), dim(ligand))
    # a rigid motion of the ligand: its internal distances are unchanged
    expect_equal(pdist(pose), pdist(ligand), tolerance = 1e-6)
  }
})

test_that("the pose search rejects a site that is not the ligand's shape", {
  receptor <- pts(5, 12)
  ligand <- pts(6, 5)
  expect_error(
    morie_agalfsy_rl_pose_search(receptor, ligand, site = pts(7, 4),
                                 max_steps = 10L),
    "site"
  )
})
