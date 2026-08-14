# morie native arm -- mxetA
# Max-stable process simulation by de Haan's spectral construction.
#
#   Y_t = sup_k f_t(T_k) X_k,   X_k = 1/Gamma_k
#
# with Gamma_k the arrivals of a unit-rate Poisson process and T_k iid
# uniform, giving P(Y_t <= y) = exp(-c_t / y) with c_t = mean_s f_t(s).
#
# The truncation is EXACT, not approximate: once 1/Gamma_k times the
# largest spectral value falls below the running minimum of Y, no
# later point can change any coordinate, so stopping there loses
# nothing.
#
# Draws come from the shared SplitMix64 stream in the same order as
# the Python side, so the realisations are identical field for field.
#
# de Haan, L. (1984) Annals of Probability 12(4), 1194-1204, Sec. 1
# example and Theorem 3.

morie_mxetA <- function(F, n_sim = 1, seed = 0, max_points = 100000L) {
  Fm <- as.matrix(F)
  nt <- nrow(Fm); m <- ncol(Fm)
  if (nt < 1L || m < 1L) {
    stop("F must be a rectangular n_t x m matrix")
  }
  if (any(Fm < 0)) stop("spectral functions must be non-negative")
  fmax <- max(Fm)
  if (fmax <= 0) stop("F must not be identically zero")
  scales <- rowMeans(Fm)

  e <- .ghc_rng(seed)
  unif <- function() .ghc_unif(e, 1L)

  fields <- matrix(0, nrow = as.integer(n_sim), ncol = nt)
  counts <- integer(as.integer(n_sim))
  for (s in seq_len(as.integer(n_sim))) {
    y <- numeric(nt)
    gamma <- 0
    k <- 0L
    while (k < as.integer(max_points)) {
      u <- unif()
      while (u <= 0) u <- unif()
      gamma <- gamma - log(u)
      x <- 1 / gamma
      if (x * fmax <= min(y) && min(y) > 0) break   # exact truncation
      site <- min(as.integer(unif() * m), m - 1L)
      v <- Fm[, site + 1L] * x
      y <- pmax(y, v)
      k <- k + 1L
    }
    fields[s, ] <- y
    counts[s] <- k
  }
  fu <- t(apply(fields, 1L, function(y)
    ifelse(y > 0, exp(-scales / y), 0)))
  if (nt == 1L) fu <- matrix(fu, ncol = 1L)

  list(
    fields = fields,
    scales = scales,
    n_points = counts,
    frechet_uniform = fu,
    seed = seed,
    method = "de Haan (1984) spectral max-stable simulation"
  )
}
