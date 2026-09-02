# SPDX-License-Identifier: AGPL-3.0-or-later
#' Couple more than two histograms at once
#'
#' Barycentres, Wasserstein-Bures interpolation and incompressible fluid
#' paths are all the same object seen from different angles: a single
#' coupling of \code{S} measures, not a chain of pairwise ones. Pairwise
#' plans cannot be glued into a consistent joint law in general, so the
#' multimarginal problem has to be solved as one problem. Entropic
#' smoothing keeps the scalings one-dimensional, which is the only reason
#' it is tractable at all.
#'
#' Formula: \code{min_P <C, P> - eps H(P)} over tensors whose \code{S}
#' marginals are the given histograms; solved by
#' \code{u_s <- u_s a_s / marg_s(P)} -- Peyre and Cuturi (2019)
#' eq. (10.1)-(10.2), p. 159; Benamou et al. (2015) Section 5.
#'
#' @param margins List of the prescribed marginals.
#' @param C_tensor Cost tensor, flattened row-major (last index varying
#'   fastest), of length \code{prod_s length(margins[\[s\]])}.
#' @param epsilon Entropic strength, positive.
#' @param max_iter Sweeps over the marginals.
#' @return List with \code{T}, \code{cost}, \code{mass}, \code{marg_err},
#'   \code{dims}, \code{S}, \code{iters}.
#' @references Benamou, J.-D., Carlier, G., Cuturi, M., Nenna, L. and
#'   Peyre, G. (2015). SIAM Journal on Scientific Computing
#'   37(2):A1111-A1138. \doi{10.1137/141000439}.
#' @export
Otmot <- function(margins, C_tensor, epsilon, max_iter = 200) {
  ms <- lapply(margins, .ot_hist)
  S <- length(ms)
  if (S < 2L) stop("a multimarginal problem needs at least two margins")
  dims <- vapply(ms, length, 0L)
  total <- prod(dims)
  C <- as.numeric(C_tensor)
  if (length(C) != total)
    stop("the cost tensor does not match the marginal sizes")
  eps <- as.numeric(epsilon)
  if (eps <= 0) stop("epsilon must be positive")
  strides <- rep(1L, S)
  if (S >= 2L) for (s in seq(S - 1L, 1L)) strides[s] <- strides[s + 1L] * dims[s + 1L]
  tt <- seq_len(total) - 1L
  idx <- vapply(seq_len(S), function(s) (tt %/% strides[s]) %% dims[s] + 1L,
                integer(total))
  idx <- matrix(idx, nrow = total, ncol = S)
  K <- exp(-C / eps)
  u <- lapply(seq_len(S), function(s) rep(1, dims[s]))
  it <- as.integer(max_iter)
  scaling <- function() {
    v <- K
    for (s in seq_len(S)) v <- v * u[[s]][idx[, s]]
    v
  }
  for (t in seq_len(it)) {
    for (s in seq_len(S)) {
      P <- scaling()
      marg <- as.numeric(tapply(P, factor(idx[, s], levels = seq_len(dims[s])),
                                sum))
      marg[is.na(marg)] <- 0
      u[[s]] <- ifelse(marg > 0, u[[s]] * ms[[s]] / marg, u[[s]])
    }
  }
  P <- scaling()
  err <- 0
  for (s in seq_len(S)) {
    marg <- as.numeric(tapply(P, factor(idx[, s], levels = seq_len(dims[s])),
                              sum))
    marg[is.na(marg)] <- 0
    err <- max(err, max(abs(marg - ms[[s]])))
  }
  .t1_result(T = P, cost = sum(P * C), mass = sum(P), marg_err = err,
             dims = dims, S = S, iters = it,
             method = "Multimarginal entropic optimal transport")
}
