# R arm of alfbnp -- AlphaFold-3 SampleDiffusion (Abramson et al. 2024).
# Abramson, J. et al. (2024) Nature 630(8016), 493-500,
#   doi:10.1038/s41586-024-07487-w. Algorithm 18 and the Table 6 defaults:
#   sigma_data 16, s_max 160, s_min 4e-4, rho 7, gamma_0 0.8, gamma_min 1.0,
#   noise_scale 1.003, step_scale 1.5.
# Karras, T. et al. (2022) NeurIPS 35, 26565-26577 -- the sigma schedule.
# Vincent, P. (2011) Neural Computation 23(7), 1661-1674 -- the fit objective.
# Mirrors src/morie/fn/alfbnp.py.

.alfbnp_EPS <- 1e-12
.alfbnp_SIGMA_DATA <- 16.0
.alfbnp_S_MAX <- 160.0
.alfbnp_S_MIN <- 4e-4
.alfbnp_RHO <- 7.0

#' .alfbnp_atoms
#'
#' Part of the alfbnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param what See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.alfbnp_atoms <- function(x, what) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 3L, byrow = TRUE)
  storage.mode(m) <- "double"
  if (ncol(m) != 3L) stop(sprintf("%s: each atom needs exactly x, y, z", what))
  if (nrow(m) == 0L) stop(sprintf("%s: no atoms", what))
  m
}

#' .alfbnp_schedule
#'
#' Part of the alfbnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param T See Usage.
#' @param sigma_data See Usage.
#' @param s_max See Usage.
#' @param s_min See Usage.
#' @param rho See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.alfbnp_schedule <- function(T, sigma_data, s_max, s_min, rho) {
  a <- s_max^(1.0 / rho)
  b <- s_min^(1.0 / rho)
  out <- numeric(T + 1L)
  for (i in seq_len(T)) {
    u <- a + ((i - 1L) / T) * (b - a)
    out[i] <- sigma_data * (u^rho)
  }
  out[T + 1L] <- 0.0
  out
}

#' .alfbnp_centre
#'
#' Part of the alfbnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @return The value of \code{X}, as built in the body.
#' @export
.alfbnp_centre <- function(X) {
  cen <- colSums(X) / nrow(X)
  for (a in 1:3) X[, a] <- X[, a] - cen[a]
  X
}

# Denoising score matching, solved exactly. At each noise level the optimal
# linear map is a ridge regression with a closed form; descending to it
# would only add a tolerance for the two arms to disagree about.
#' Denoising score matching, solved exactly. At each noise level the
#' optimal
#'
#' linear map is a ridge regression with a closed form; descending to it
#' would only add a tolerance for the two arms to disagree about.
#'
#' @param clean See Usage.
#' @param sigmas See Usage.
#' @param draws See Usage.
#' @param ridge See Usage.
#' @return The value of \code{coefs}, as built in the body.
#' @export
.alfbnp_fit_linear <- function(clean, sigmas, draws, ridge) {
  n <- nrow(clean[[1]])
  coefs <- numeric(length(sigmas))
  di <- 0L
  for (si in seq_along(sigmas)) {
    s <- sigmas[si]
    num <- 0.0; den <- 0.0
    for (X in clean) {
      for (i in seq_len(n)) for (a in 1:3) {
        xi <- draws[(di %% length(draws)) + 1L]
        di <- di + 1L
        noisy <- X[i, a] + s * xi
        num <- num + noisy * X[i, a]
        den <- den + noisy * noisy
      }
    }
    coefs[si] <- if (den > .alfbnp_EPS) num / (den + ridge) else 1.0
  }
  coefs
}

#  may arrive as a list of n x 3 matrices, or as a single 3-D array
# (k x n x 3) -- which is what jsonlite hands back for a nested list, and
# what a caller who stacked their structures would naturally pass. lapply
# over a 3-D array walks the wrong dimension and silently produces garbage,
# so normalise before anything touches it.
#' May arrive as a list of n x 3 matrices, or as a single 3-D array
#'
#' (k x n x 3) -- which is what jsonlite hands back for a nested list,
#' and what a caller who stacked their structures would naturally pass.
#' lapply over a 3-D array walks the wrong dimension and silently
#' produces garbage, so normalise before anything touches it.
#'
#' @param clean See Usage.
#' @return The value of \code{lapply}.
#' @export
.alfbnp_clean_list <- function(clean) {
  if (is.array(clean) && length(dim(clean)) == 3L) {
    d <- dim(clean)
    return(lapply(seq_len(d[1]), function(i)
      matrix(as.numeric(clean[i, , ]), nrow = d[2], ncol = d[3])))
  }
  if (is.matrix(clean)) return(list(clean))
  lapply(clean, function(c) .alfbnp_atoms(c, "alfbnp clean"))
}

#' morie_alfbnp_af3_sample
#'
#' Part of the alfbnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param n_atoms Defaults to \code{NULL}.
#' @param denoiser Defaults to \code{NULL}.
#' @param clean Defaults to \code{NULL}.
#' @param steps Defaults to \code{20L}.
#' @param sigma_data Defaults to \code{16}.
#' @param s_max Defaults to \code{160}.
#' @param s_min Defaults to \code{4e-04}.
#' @param rho Defaults to \code{7}.
#' @param gamma_0 Defaults to \code{0.8}.
#' @param gamma_min Defaults to \code{1}.
#' @param noise_scale Defaults to \code{1.003}.
#' @param step_scale Defaults to \code{1.5}.
#' @param noise Defaults to \code{NULL}.
#' @param seed Defaults to \code{2}.
#' @param x_init Defaults to \code{NULL}.
#' @param ridge Defaults to \code{1e-06}.
#' @return A list with \code{estimate}, \code{coords}, \code{sigmas}, \code{trace}, \code{denoiser_coefs}, \code{sigma_data}, \code{steps}, \code{rmsd_to_reference}, \code{n_atoms}, \code{route}, \code{method}, \code{note}.
#' @export
morie_alfbnp_af3_sample <- function(n_atoms = NULL, denoiser = NULL,
                                    clean = NULL, steps = 20L,
                                    sigma_data = 16.0, s_max = 160.0,
                                    s_min = 4e-4, rho = 7.0, gamma_0 = 0.8,
                                    gamma_min = 1.0, noise_scale = 1.003,
                                    step_scale = 1.5, noise = NULL, seed = 2,
                                    x_init = NULL, ridge = 1e-6) {
  X0 <- NULL; ref <- NULL
  if (!is.null(x_init)) {
    X0 <- .alfbnp_atoms(x_init, "alfbnp x_init"); n <- nrow(X0)
  } else if (!is.null(clean)) {
    ref <- .alfbnp_clean_list(clean)
    n <- nrow(ref[[1]])
    for (c in ref) if (nrow(c) != n)
      stop("alfbnp: the reference structures have different atom counts")
  } else if (!is.null(n_atoms)) {
    n <- as.integer(n_atoms)
  } else {
    stop(paste0("alfbnp: give n_atoms, x_init, or clean so the number of ",
                "atoms is known"))
  }
  if (n < 1L) stop("alfbnp: need at least one atom")
  T <- as.integer(steps)
  if (T < 1L) stop("alfbnp: steps must be at least 1")
  if (!is.null(clean) && is.null(ref)) ref <- .alfbnp_clean_list(clean)

  if (is.character(sigma_data)) {
    if (sigma_data != "fit")
      stop("alfbnp: sigma_data must be a number or 'fit'")
    if (is.null(ref)) stop("alfbnp: sigma_data='fit' needs `clean`")
    tot <- 0.0; cnt <- 0.0
    for (X in ref) { tot <- tot + sum(X * X); cnt <- cnt + length(X) }
    sd_ <- if (cnt > 0) sqrt(tot / cnt) else .alfbnp_SIGMA_DATA
  } else {
    sd_ <- as.numeric(sigma_data)
  }
  if (!(sd_ > 0.0)) stop("alfbnp: sigma_data must be positive")

  sig <- .alfbnp_schedule(T, sd_, as.numeric(s_max), as.numeric(s_min),
                          as.numeric(rho))

  need <- 3L * n * (T + 2L) +
    (if (!is.null(ref)) 3L * n * length(ref) * (T + 1L) else 0L)
  if (!is.null(noise)) {
    z <- as.numeric(noise)
    if (!length(z)) stop("alfbnp: noise is empty")
  } else {
    z <- .s03normdraws(max(need, 8L), as.integer(seed))
  }
  zi <- 0L

  coefs <- NULL
  if (is.null(denoiser)) {
    if (is.null(ref))
      stop(paste0("alfbnp: no denoiser and no `clean` to fit one from. The ",
                  "network is not bundled and will not be invented: supply ",
                  "a trained denoiser, or reference structures to fit a ",
                  "linear one by denoising score matching."))
    coefs <- .alfbnp_fit_linear(ref, sig[seq_len(T)], z, as.numeric(ridge))
    route <- "fitted a linear denoiser by denoising score matching"
    dcount <- 0L
    denoise <- function(x, s) {
      dcount <<- dcount + 1L
      x * coefs[min(dcount, length(coefs))]
    }
  } else {
    route <- "sampled with a supplied denoiser"
    denoise <- function(x, s) .alfbnp_atoms(denoiser(x, s),
                                            "alfbnp denoiser output")
  }

  if (!is.null(X0)) {
    X <- X0
  } else {
    X <- matrix(0.0, n, 3L)
    for (i in seq_len(n)) {
      for (a in 1:3) X[i, a] <- sig[1] * z[((zi + a - 1L) %% length(z)) + 1L]
      zi <- zi + 3L
    }
  }

  trace <- list()
  for (i in seq_len(T)) {
    # CentreRandomAugmentation: the rotation half is deliberately omitted --
    # it is a training-time augmentation, and at sampling it only picks an
    # arbitrary frame, costing reproducibility for nothing.
    X <- .alfbnp_centre(X)
    prev <- sig[i]
    gamma <- if (sig[i + 1L] > as.numeric(gamma_min)) as.numeric(gamma_0) else 0.0
    t_hat <- prev * (gamma + 1.0)
    var <- t_hat * t_hat - prev * prev
    step_noise <- if (var > 0.0) as.numeric(noise_scale) * sqrt(var) else 0.0
    Xn <- X
    for (j in seq_len(n)) for (a in 1:3) {
      Xn[j, a] <- X[j, a] + step_noise * z[(zi %% length(z)) + 1L]
      zi <- zi + 1L
    }
    Xd <- denoise(Xn, t_hat)
    if (nrow(Xd) != n)
      stop(sprintf("alfbnp: the denoiser returned %d atoms, not %d",
                   nrow(Xd), n))
    dt <- sig[i + 1L] - t_hat
    for (j in seq_len(n)) for (a in 1:3) {
      grad <- if (t_hat > .alfbnp_EPS) (Xn[j, a] - Xd[j, a]) / t_hat else 0.0
      X[j, a] <- Xn[j, a] + as.numeric(step_scale) * dt * grad
    }
    rms <- sqrt(sum(X * X) / (3 * n))
    trace[[length(trace) + 1L]] <- c(i, sig[i + 1L], t_hat, rms)
  }

  rmsd_to_ref <- NULL
  if (!is.null(ref)) {
    best <- NULL
    for (R in ref) {
      d <- sqrt(sum((X - R)^2) / n)
      if (is.null(best) || d < best) best <- d
    }
    rmsd_to_ref <- best
  }

  list(estimate = if (!is.null(rmsd_to_ref)) rmsd_to_ref else sig[T + 1L],
       coords = X, sigmas = sig,
       trace = do.call(rbind, trace),
       denoiser_coefs = coefs,
       sigma_data = sd_, steps = T,
       rmsd_to_reference = rmsd_to_ref,
       n_atoms = as.integer(n),
       route = route,
       method = paste0("AlphaFold-3 SampleDiffusion (Abramson et al. 2024, ",
                       "Algorithm 18) on the Karras sigma schedule, with ",
                       "the Table 6 defaults; the noise stream is the ",
                       "package's deterministic low-discrepancy normal ",
                       "sequence rather than i.i.d. draws, so a run ",
                       "reproduces exactly"),
       note = paste0("route says whether the denoiser was supplied or ",
                     "fitted. The network is not bundled. The rotation half ",
                     "of CentreRandomAugmentation is deliberately not ",
                     "applied: it is a training-time augmentation, and at ",
                     "sampling it only chooses an arbitrary frame, which ",
                     "both costs reproducibility and gains nothing. Pass ",
                     "`noise` to recover genuinely stochastic sampling."))
}

#' .alfbnp_cheatsheet
#'
#' Part of the alfbnp_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.alfbnp_cheatsheet <- function() {
  paste0("alfbnp: morie_alfbnp_af3_sample(n_atoms, denoiser=) or (clean=) ",
         "-> AlphaFold-3 diffusion sampling (Abramson et al. 2024 Nature ",
         "630:493, Algorithm 18)")
}

morie_alfbnp <- morie_alfbnp_af3_sample
