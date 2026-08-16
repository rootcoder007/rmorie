# R arm of krpkrg -- ordinary kriging with a Lagrange unbiasedness
# constraint. Goovaerts, P. (2005) Geostatistics for Natural Resources
# Evaluation, Oxford University Press, Ch. 5.
# Mirrors src/morie/fn/krpkrg.py.

.krpkrg_EPS <- 1e-12

#' .krpkrg_gamma
#'
#' A step of the krpkrg_native implementation. Called by \code{morie_krpkrg_ordinary_kriging}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h Numeric; combined arithmetically in the body.
#' @param model One of \code{"exponential"}, \code{"gaussian"}, \code{"spherical"}.
#' @param nugget Numeric; combined arithmetically in the body.
#' @param sill Numeric; combined arithmetically in the body.
#' @param rng Numeric; combined arithmetically in the body.
#' @return Nothing; this branch always raises.
#' @export
.krpkrg_gamma <- function(h, model, nugget, sill, rng) {
  if (h <= 0.0) return(0.0)
  ps <- sill - nugget
  if (rng <= .krpkrg_EPS) return(sill)
  if (model == "spherical") {
    if (h >= rng) return(sill)
    r <- h / rng
    return(nugget + ps * (1.5 * r - 0.5 * r ^ 3))
  }
  if (model == "exponential")
    return(nugget + ps * (1.0 - exp(-3.0 * h / rng)))
  if (model == "gaussian")
    return(nugget + ps * (1.0 - exp(-3.0 * (h / rng) ^ 2)))
  stop(sprintf(paste0("krpkrg: model must be spherical, exponential or ",
                      "gaussian, got '%s'"), model))
}

#' morie_krpkrg_ordinary_kriging
#'
#' A step of the krpkrg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param values See Usage.
#' @param targets A matrix; passed to \code{as.matrix}.
#' @param model Passed to \code{.krpkrg_gamma}. Defaults to \code{"spherical"}.
#' @param nugget Passed to \code{.krpkrg_gamma}. Defaults to \code{0}.
#' @param sill Passed to \code{.krpkrg_gamma}. Defaults to \code{1}.
#' @param rng Passed to \code{.krpkrg_gamma}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{prediction}, \code{variance}, \code{std_error}, \code{weights}, \code{n}, \code{n_targets}, \code{model}, \code{nugget}, \code{sill}, \code{range}, \code{method}, \code{note}.
#' @export
morie_krpkrg_ordinary_kriging <- function(coords, values, targets,
                                          model = "spherical", nugget = 0.0,
                                          sill = 1.0, rng = 1.0) {
  C <- as.matrix(coords); storage.mode(C) <- "double"
  z <- as.numeric(values)
  Tg <- as.matrix(targets); storage.mode(Tg) <- "double"
  n <- nrow(C)
  if (n == 0L) stop("krpkrg: no data locations")
  if (length(z) != n)
    stop(sprintf("krpkrg: %d locations but %d values", n, length(z)))
  if (sill < nugget) stop("krpkrg: the sill cannot be below the nugget")

  dist2 <- function(a, b) sqrt(sum((a - b) ^ 2))
  G <- matrix(0.0, n + 1L, n + 1L)
  for (i in seq_len(n)) {
    for (j in seq_len(n))
      G[i, j] <- .krpkrg_gamma(dist2(C[i, ], C[j, ]), model, nugget, sill, rng)
    G[i, n + 1L] <- 1.0; G[n + 1L, i] <- 1.0
  }
  G[n + 1L, n + 1L] <- 0.0
  A <- crossprod(G)
  Lc <- chol(A)

  preds <- numeric(nrow(Tg)); variances <- numeric(nrow(Tg))
  weightsets <- vector("list", nrow(Tg))
  for (q in seq_len(nrow(Tg))) {
    g0 <- c(vapply(seq_len(n), function(i)
      .krpkrg_gamma(dist2(C[i, ], Tg[q, ]), model, nugget, sill, rng),
      numeric(1)), 1.0)
    b <- as.numeric(crossprod(G, g0))
    sol <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
    lam <- sol[seq_len(n)]; mu <- sol[n + 1L]
    preds[q] <- sum(lam * z)
    v <- sum(lam * g0[seq_len(n)]) + mu
    # exactly 0 at a data location; below the floor it is rounding, and sqrt
    # would turn that rounding into a spurious standard error
    floor <- 1e-12 * (if (sill > .krpkrg_EPS) sill else 1.0)
    variances[q] <- if (v < floor) 0.0 else v
    weightsets[[q]] <- lam
  }

  list(estimate = preds, prediction = preds, variance = variances,
       std_error = sqrt(variances), weights = weightsets,
       n = as.integer(n), n_targets = as.integer(nrow(Tg)),
       model = model, nugget = as.numeric(nugget), sill = as.numeric(sill),
       range = as.numeric(rng),
       method = paste0("ordinary kriging with a Lagrange unbiasedness ",
                       "constraint (Goovaerts 2005 Ch. 5)"),
       note = paste0("the kriging variance depends on the configuration ",
                     "and the variogram, never on the observed values -- ",
                     "which is what makes it usable as a design criterion"))
}

#' .krpkrg_cheatsheet
#'
#' A step of the krpkrg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.krpkrg_cheatsheet <- function() {
  paste0("krpkrg: morie_krpkrg_ordinary_kriging(coords, values, targets, ",
         "model, nugget, sill, range) -> BLUP and kriging variance ",
         "(Goovaerts 2005 Ch. 5)")
}

morie_krpkrg <- morie_krpkrg_ordinary_kriging
