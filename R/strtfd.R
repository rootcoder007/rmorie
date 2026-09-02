# SPDX-License-Identifier: AGPL-3.0-or-later
#' Allocate a stratified sample and report the variance it achieves
#'
#' Formula: proportional n_h prop. N_h; Neyman n_h prop. N_h S_h;
#'   cost-optimal n_h prop. N_h S_h / sqrt(C_h);
#'   V(ybar_st) = sum_h W_h^2 (1 - f_h) S_h^2 / n_h
#'
#' @param Nh Population size of each stratum.
#' @param Sh Stratum standard deviations (advance estimates).
#' @param n Total sample size, at least 1 per stratum.
#' @param Ch Cost per unit in each stratum; required for kind = "cost".
#' @param kind One of "prop", "neyman", "cost".
#' @return List with \code{nh}, \code{nh_exact}, \code{weights},
#'   \code{variance}, \code{se}, \code{Wh}, \code{N}, \code{n}, \code{L},
#'   \code{kind}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Sections
#'   5.3-5.5: Theorem 5.3 for the variance, Corollary 2 for proportional
#'   allocation, and the Neyman/cost-optimal rules n_h prop. N_h S_h and
#'   n_h prop. N_h S_h/sqrt(c_h) under the linear cost function (5.17).
#'   Chapter 5 read from the scanned original. Cross-checked against the
#'   CRAN package samplingbook 1.2.4, whose stratasamp uses exactly these
#'   three weight rules.
#' @export
#' @examples
#' Stratdes(Nh = 5L, Sh = 5L, n = 5L)
Stratdes <- function(Nh, Sh, n, Ch = NULL, kind = "neyman") {
  Nh <- .t1_vec(Nh); Sh <- .t1_vec(Sh); n <- as.integer(n)
  L <- length(Nh)
  if (length(Sh) != L) stop("Nh and Sh must have the same length")
  if (any(Nh <= 0)) stop("stratum sizes must be positive")
  if (any(Sh < 0)) stop("stratum standard deviations must be non-negative")
  if (n < L) stop("n must be at least the number of strata")
  kind <- tolower(kind)
  w <- if (kind == "prop") Nh
       else if (kind == "neyman") Nh * Sh
       else if (kind == "cost") {
         if (is.null(Ch)) stop("kind='cost' needs the per-unit costs Ch")
         Cv <- .t1_vec(Ch)
         if (length(Cv) != L || any(Cv <= 0))
           stop("Ch must be positive and of length L")
         Nh * Sh / sqrt(Cv)
       } else stop("kind must be 'prop', 'neyman' or 'cost'")
  sw <- sum(w)
  if (sw <= 0) stop("the allocation weights are all zero")
  w <- w / sw
  exact <- n * w
  base <- pmax(1L, as.integer(floor(exact)))
  while (sum(base) > n) {
    cand <- which(base > 1L)
    i <- cand[order(-(base[cand] - exact[cand]), cand)][1]
    base[i] <- base[i] - 1L
  }
  ord <- order(-(exact - base), seq_len(L))
  j <- 0L
  while (sum(base) < n) {
    k <- ord[(j %% L) + 1L]
    base[k] <- base[k] + 1L
    j <- j + 1L
  }
  N <- sum(Nh); W <- Nh / N
  var <- sum(W^2 * (1 - base / Nh) * Sh^2 / base)
  .t1_result(nh = base, nh_exact = exact, weights = w, variance = var,
             se = sqrt(var), Wh = W, N = N, n = n, L = L,
             kind = c(prop = 1, neyman = 2, cost = 3)[[kind]],
             method = "Stratified allocation and achieved variance")
}
