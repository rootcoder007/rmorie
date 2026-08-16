# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of alfrf2 -- RFdiffusion motif scaffolding. Mirrors
# src/morie/fn/alfrf2.py operation for operation, on the shared Jacobi
# eigensolver in R/manfd_native.R and the shared numerics in
# R/aaa_helpers_w3num.R.
#
# The design problem is inverse. A binding site, an active site, a metal
# coordination sphere -- some small set of residues whose geometry is
# the whole point -- is known, and what is missing is a protein that
# holds those residues in exactly that arrangement. RFdiffusion answers
# it by running a denoising diffusion model over backbone coordinates:
# start from noise, walk the noise back down to a structure, and at
# every step overwrite the motif positions with the motif, correctly
# noised for the level you are at.
#
#   THE SCHEDULE. A linear variance schedule and its cumulative
#   products, from Ho, Jain and Abbeel. The forward closed form
#   x_t = sqrt(abar_t) x_0 + sqrt(1 - abar_t) eps and the reverse
#   posterior mean given a predicted x_0 are both exact and both here.
#
#   THE MOTIF CONSTRAINT. At every reverse step the motif rows are
#   replaced by the motif noised to that step's level -- the inpainting
#   rule of Lugmayr et al. At the last step the level is zero, so the
#   motif comes out EXACTLY where it was put in. That is an equality,
#   not a tolerance.
#
#   THE DENOISER. A trained network in the paper; there are no weights
#   here. Two routes stand in, both named for what they are. "prior"
#   predicts the prior mean, zero, which samples the prior conditioned
#   on the motif -- the correct baseline, and not protein design.
#   "ideal" relaxes consecutive alpha carbons toward the 3.8 angstrom
#   spacing a trans peptide bond forces on a backbone, with the motif
#   pinned; that distance is covalent geometry, not a fitted parameter,
#   but the route knows nothing about sequence, packing or secondary
#   structure. A caller with real weights passes denoiser and the routes
#   become irrelevant.
#
#   THE MEASUREMENT. Motif RMSD after optimal superposition, by Kabsch's
#   construction, from the eigendecomposition in morie_manfd_jacobi
#   rather than a second copy of it.
#
# References
#   Watson, J.L. et al. (2023) "De novo design of protein structure and
#     function with RFdiffusion." Nature 620, 1089-1100.
#     doi:10.1038/s41586-023-06415-8.
#   Ho, J., Jain, A. and Abbeel, P. (2020) "Denoising diffusion
#     probabilistic models." NeurIPS 33, 6840-6851.
#   Lugmayr, A. et al. (2022) "RePaint: inpainting using denoising
#     diffusion probabilistic models." CVPR, 11461-11471.
#   Kabsch, W. (1976) "A solution for the best rotation to relate two
#     sets of vectors." Acta Crystallographica A32(5), 922-923.

# The alpha carbon separation a trans peptide bond forces on successive
# residues. A fact of covalent geometry, not a fitted constant.
.alfrf2_ca_spacing <- 3.8

#' The linear variance schedule and its cumulative products
#'
#' Returns betas, alphas and alpha-bars indexed from step one, with the
#' zeroth alpha-bar equal to one standing for the clean structure so the
#' forward and reverse formulas need no special case at the ends. The
#' endpoints are the ones Ho et al. use; RFdiffusion's own schedule is a
#' parameter here rather than a constant, because it is a training
#' choice and not a property of diffusion.
#'
#' @param T The number of steps.
#' @param beta_start The first variance.
#' @param beta_end The last variance.
#' @return A list with betas, alphas and abar.
#' @export
morie_alfrf2_schedule <- function(T, beta_start = 1e-4, beta_end = 0.02) {
  T <- as.integer(T)
  if (T < 1L) stop("a diffusion needs at least one step")
  if (!(beta_start > 0 && beta_start <= beta_end && beta_end < 1))
    stop("the variance schedule must rise through the open unit interval")
  betas <- numeric(T + 1L); alphas <- rep(1, T + 1L); abar <- rep(1, T + 1L)
  for (t in seq_len(T)) {
    b <- beta_start + (beta_end - beta_start) * (t - 1) / max(T - 1L, 1L)
    betas[t + 1L] <- b
    alphas[t + 1L] <- 1 - b
    abar[t + 1L] <- abar[t] * (1 - b)
  }
  list(betas = betas, alphas = alphas, abar = abar)
}

#' The forward process in closed form
#'
#' x_t = sqrt(abar_t) x_0 + sqrt(1 - abar_t) eps. At abar equal to one
#' this is the structure itself, which is what makes the motif exact at
#' the end of the reverse walk.
#'
#' @param x0 The clean structure, a matrix of coordinates.
#' @param abar_t The cumulative alpha at the step wanted.
#' @param eps Standard normal deviates of the same shape.
#' @return The noised structure.
#' @export
morie_alfrf2_noise <- function(x0, abar_t, eps) {
  a <- sqrt(abar_t); b <- sqrt(1 - abar_t)
  a * x0 + b * eps
}

#' .alfrf2_centre
#'
#' A step of the alfrf2_native implementation. Called by \code{morie_alfrf2_kabsch}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P A matrix; indexed by row and column.
#' @return A list with \code{P}, \code{c}.
#' @export
.alfrf2_centre <- function(P) {
  n <- nrow(P)
  cc <- vapply(1:3, function(d) .w3_csum(P[, d]) / n, numeric(1))
  list(P = sweep(P, 2, cc, "-"), c = cc)
}

#' .alfrf2_det3
#'
#' A step of the alfrf2_native implementation. Called by \code{morie_alfrf2_kabsch}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; indexed by row and column.
#' @return A numeric value.
#' @export
.alfrf2_det3 <- function(M)
  M[1, 1] * (M[2, 2] * M[3, 3] - M[2, 3] * M[3, 2]) -
  M[1, 2] * (M[2, 1] * M[3, 3] - M[2, 3] * M[3, 1]) +
  M[1, 3] * (M[2, 1] * M[3, 2] - M[2, 2] * M[3, 1])

#' The rigid motion that best takes P onto Q, and the residual
#'
#' The rotation is the orthogonal polar factor of the cross-covariance
#' C = sum q p', obtained as C (C'C)^{-1/2} from the eigendecomposition
#' of C'C. If that factor comes out with a negative determinant it is a
#' reflection, not a rotation, and the sign is flipped on the direction
#' of least variance -- the standard correction, and the reason a naive
#' superposition can silently report a mirror image as a match.
#'
#' @param P The moving points, one row each.
#' @param Q The reference points.
#' @return A list with the rotation, the translation, the root-mean-
#'   square deviation and the moved points.
#' @export
morie_alfrf2_kabsch <- function(P, Q) {
  P <- as.matrix(P); Q <- as.matrix(Q)
  n <- nrow(P)
  if (n != nrow(Q))
    stop("superposition needs the same number of points on both sides")
  if (n < 3L) stop("three points are the fewest that fix a rotation")
  cp <- .alfrf2_centre(P); cq <- .alfrf2_centre(Q)
  p <- cp$P; q <- cq$P
  C <- matrix(0, 3, 3)
  for (a in 1:3) for (b in 1:3)
    C[a, b] <- .w3_csum(q[, a] * p[, b])
  S <- matrix(0, 3, 3)
  for (a in 1:3) for (b in 1:3)
    S[a, b] <- .w3_csum(vapply(1:3, function(k) C[k, a] * C[k, b],
                               numeric(1)))
  je <- morie_manfd_jacobi(S)
  lam <- je$values; V <- je$vectors
  if (lam[3] <= 1e-12 * (if (lam[1] > 0) lam[1] else 1))
    stop("the points do not span three dimensions, so the polar factor ",
         "does not determine a rotation")
  inv <- 1 / sqrt(lam)
  build <- function(inv) {
    M <- matrix(0, 3, 3)
    for (a in 1:3) for (b in 1:3)
      M[a, b] <- .w3_csum(vapply(1:3, function(k)
        V[a, k] * inv[k] * V[b, k], numeric(1)))
    R <- matrix(0, 3, 3)
    for (a in 1:3) for (b in 1:3)
      R[a, b] <- .w3_csum(vapply(1:3, function(k) C[a, k] * M[k, b],
                                 numeric(1)))
    R
  }
  R <- build(inv)
  if (.alfrf2_det3(R) < 0) { inv[3] <- -inv[3]; R <- build(inv) }
  moved <- matrix(0, n, 3)
  for (i in seq_len(n)) for (a in 1:3)
    moved[i, a] <- .w3_csum(vapply(1:3, function(b) R[a, b] * p[i, b],
                                   numeric(1))) + cq$c[a]
  sq <- .w3_csum(as.numeric(t((moved - Q) * (moved - Q))))
  tr <- vapply(1:3, function(a) cq$c[a] -
                 .w3_csum(vapply(1:3, function(b) R[a, b] * cp$c[b],
                                 numeric(1))), numeric(1))
  list(R = R, t = tr, rmsd = sqrt(sq / n), moved = moved)
}

#' Root-mean-square deviation after optimal superposition
#'
#' @param P The moving points.
#' @param Q The reference points.
#' @return A numeric scalar.
#' @export
morie_alfrf2_rmsd <- function(P, Q) morie_alfrf2_kabsch(P, Q)$rmsd

#' Relax consecutive alpha carbons toward the backbone spacing
#'
#' A symmetric constraint relaxation: each consecutive pair is moved
#' along the line joining it until the pair is the right distance apart,
#' both ends sharing the correction equally unless one of them is
#' pinned, in which case the free end takes all of it. Repeated for a
#' fixed number of passes so the result does not depend on a convergence
#' test. Two points on top of each other have no direction to be
#' separated along, so that pair is left alone rather than moved
#' somewhere arbitrary.
#'
#' @param x The structure, one row per residue.
#' @param fixed The zero-based indices held in place.
#' @param spacing The target separation.
#' @param passes How many relaxation passes.
#' @return The relaxed structure.
#' @export
morie_alfrf2_ideal <- function(x, fixed, spacing = .alfrf2_ca_spacing,
                               passes = 8L) {
  y <- as.matrix(x)
  n <- nrow(y)
  for (it in seq_len(as.integer(passes))) {
    for (i in seq_len(n - 1L)) {
      j <- i + 1L
      d <- y[j, ] - y[i, ]
      L <- sqrt(.w3_csum(d * d))
      if (L == 0) next
      corr <- (L - spacing) / L
      fi <- (i - 1L) %in% fixed
      fj <- (j - 1L) %in% fixed
      if (fi && fj) next
      wi <- if (fi) 0 else if (fj) 1 else 0.5
      wj <- if (fj) 0 else if (fi) 1 else 0.5
      y[i, ] <- y[i, ] + wi * corr * d
      y[j, ] <- y[j, ] - wj * corr * d
    }
  }
  y
}

#' .alfrf2_denoise
#'
#' A step of the alfrf2_native implementation. Called by \code{morie_alfrf2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param route The body requires: the denoiser route is prior or ideal.
#' @param denoiser The body requires: the denoiser route is prior or ideal.
#' @param x A matrix; passed to \code{nrow}.
#' @param t See Usage.
#' @param fixed Passed to \code{morie_alfrf2_ideal}.
#' @param spacing Passed to \code{morie_alfrf2_ideal}.
#' @param passes Passed to \code{morie_alfrf2_ideal}.
#' @return Nothing; this branch always raises.
#' @export
.alfrf2_denoise <- function(route, denoiser, x, t, fixed, spacing,
                            passes) {
  if (!is.null(denoiser)) return(as.matrix(denoiser(x, t)))
  if (identical(route, "prior")) return(matrix(0, nrow(x), 3))
  if (identical(route, "ideal"))
    return(morie_alfrf2_ideal(x, fixed, spacing, passes))
  stop("the denoiser route is prior or ideal")
}

#' Grow a backbone around a fixed motif by reverse diffusion
#'
#' @param target_motif A list of pairs of a zero-based residue index and
#'   the three coordinates that residue must end up at.
#' @param scaffold The number of residues in the design, or a starting
#'   structure to noise from.
#' @param T Diffusion steps.
#' @param denoise Either ideal or prior; see the file header on what
#'   each is and is not.
#' @param denoiser A function of the structure and the step returning
#'   the predicted clean structure. Given, it replaces the routes.
#' @param beta_start,beta_end The variance schedule endpoints.
#' @param spacing The backbone separation the ideal route relaxes to.
#' @param passes Relaxation passes per denoising call.
#' @param noise_scale A multiplier on the reverse-process noise. One is
#'   the sampler as published; zero makes the walk deterministic.
#' @param seed The random stream.
#' @return A list with the designed backbone, the motif it was built
#'   around, the motif RMSD -- which must be zero -- and the chain
#'   geometry it came out with.
#' @export
morie_alfrf2 <- function(target_motif, scaffold, T = 20L,
                         denoise = "ideal", denoiser = NULL,
                         beta_start = 1e-4, beta_end = 0.02,
                         spacing = .alfrf2_ca_spacing, passes = 8L,
                         noise_scale = 1, seed = 0) {
  idx <- vapply(target_motif, function(p) as.integer(p[[1]]), integer(1))
  pos <- lapply(target_motif, function(p) as.numeric(p[[2]]))
  if (length(scaffold) == 1L && is.numeric(scaffold) &&
      scaffold == round(scaffold)) {
    n <- as.integer(scaffold); start <- NULL
  } else {
    start <- as.matrix(scaffold); n <- nrow(start)
  }
  if (n < 3L)
    stop("a backbone of fewer than three residues has no geometry to ",
         "design")
  if (any(idx < 0L | idx >= n))
    stop("a motif residue falls outside the design")
  if (length(unique(idx)) != length(idx))
    stop("a residue cannot be pinned to two places")
  fixed <- idx

  sc <- morie_alfrf2_schedule(T, beta_start, beta_end)
  betas <- sc$betas; alphas <- sc$alphas; abar <- sc$abar
  T <- as.integer(T)
  e <- .ghc_rng(seed)
  drw <- function() matrix(.ghc_norm(e, n * 3L), n, 3L, byrow = TRUE)

  m0 <- matrix(0, n, 3)
  for (k in seq_along(idx)) m0[idx[k] + 1L, ] <- pos[[k]]

  if (is.null(start)) {
    x <- drw()
  } else {
    x <- morie_alfrf2_noise(start, abar[T + 1L], drw())
  }
  mt <- morie_alfrf2_noise(m0, abar[T + 1L], drw())
  for (i in fixed) x[i + 1L, ] <- mt[i + 1L, ]

  traj <- numeric(0)
  # The step index is called tt because t is base R's transpose and the
  # radius of gyration below needs it.
  for (tt in seq(T, 1L)) {
    x0 <- .alfrf2_denoise(denoise, denoiser, x, tt, fixed, spacing, passes)
    for (k in seq_along(idx)) x0[idx[k] + 1L, ] <- pos[[k]]
    c1 <- sqrt(abar[tt]) * betas[tt + 1L] / (1 - abar[tt + 1L])
    c2 <- sqrt(alphas[tt + 1L]) * (1 - abar[tt]) / (1 - abar[tt + 1L])
    sd <- sqrt(betas[tt + 1L] * (1 - abar[tt]) / (1 - abar[tt + 1L]))
    z <- drw()
    nxt <- c1 * x0 + c2 * x
    if (tt > 1L) nxt <- nxt + noise_scale * sd * z
    # The known region is replaced by the motif noised to the level we
    # have arrived at. At tt equal to one that level is zero, so the
    # motif lands exactly.
    mt <- morie_alfrf2_noise(m0, abar[tt], drw())
    for (i in fixed) nxt[i + 1L, ] <- mt[i + 1L, ]
    x <- nxt
    traj <- c(traj, .w3_csum(as.numeric(t(x * x))))
  }

  got <- x[idx + 1L, , drop = FALSE]
  want <- matrix(unlist(pos), length(pos), 3L, byrow = TRUE)
  mdev <- if (length(idx)) max(abs(got - want)) else 0
  spac <- vapply(seq_len(n - 1L), function(i) {
    d <- x[i + 1L, ] - x[i, ]; sqrt(.w3_csum(d * d))
  }, numeric(1))
  # Three or fewer motif residues, or coplanar ones, do not pin a
  # rotation, and the superposition says so rather than returning a
  # number it cannot justify.
  mr <- if (length(idx) >= 3L)
    tryCatch(morie_alfrf2_rmsd(got, want), error = function(e) NaN) else 0
  cen <- vapply(1:3, function(d) .w3_csum(x[, d]) / n, numeric(1))
  ct <- sweep(x, 2, cen, "-")
  rg <- sqrt(.w3_csum(as.numeric(t(ct * ct))) / n)
  list(backbone = x, motif_index = idx, motif_target = want,
       motif_placed = got, motif_max_deviation = mdev,
       motif_rmsd = mr,
       spacing = spac,
       mean_spacing = if (length(spac)) .w3_csum(spac) / length(spac)
                      else 0,
       radius_of_gyration = rg, trace = traj, n = n,
       n_motif = length(idx), T = T,
       denoise = if (is.null(denoiser)) denoise else "callable",
       noise_scale = as.numeric(noise_scale), seed = seed,
       method = "RFdiffusion motif-scaffolding reverse diffusion")
}

#' One-line summary of the alfrf2 module
#'
#' @return A character scalar.
#' @export
morie_alfrf2_cheatsheet <- function()
  paste0("alfrf2: RFdiffusion motif scaffolding. Reverse DDPM over ",
         "backbone coordinates with the motif replaced at every step, ",
         "so it lands exactly; denoiser routes are prior or ideal")
