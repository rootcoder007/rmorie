# SPDX-License-Identifier: AGPL-3.0-or-later
#' Profile hidden Markov model: forward likelihood and Viterbi alignment
#'
#' Each profile position has a match state with its own emission
#' distribution, an insert state emitting from a background
#' distribution, and a silent delete state.  The forward recursion sums
#' over alignments and Viterbi takes the maximum; the delete states are
#' silent, so their recursion consumes no residue.
#'
#' Formula: seven transitions mm, mi, md, im, ii, dm, dd; forward sums
#'   and Viterbi maximises over the three state types.
#'
#' @param seq Residue indices, 0-based.
#' @param profile Named list with \code{match} (L x A emissions),
#'   \code{insert} (background emissions) and \code{trans} (named list
#'   with mm, mi, md, im, ii, dm, dd).
#' @return List with \code{estimate}, \code{forward_logprob},
#'   \code{viterbi_logprob}, \code{log_odds},
#'   \code{background_logprob}, \code{n}, \code{method}.
#' @references Eddy (1998), Profile hidden Markov models,
#'   Bioinformatics 14(9):755-763,
#'   \doi{10.1093/bioinformatics/14.9.755}; Krogh, Brown, Mian,
#'   Sjolander and Haussler (1994), Journal of Molecular Biology
#'   235(5):1501-1531. \doi{10.1006/jmbi.1994.1104}
#' @export
Hmmprf <- function(seq, profile) {
  xs <- as.integer(.s03vec(seq))
  T <- length(xs)
  if (T == 0L) stop("hmm_profile: seq is empty")
  Mm <- .s03mat(profile$match)
  ins <- .s03vec(profile$insert)
  tr <- profile$trans
  L <- nrow(Mm); A <- ncol(Mm)
  if (L == 0L) stop("hmm_profile: profile has no positions")
  if (length(ins) != A) stop("hmm_profile: insert distribution has the wrong alphabet size")
  if (any(xs < 0L | xs >= A)) stop("hmm_profile: residue index out of range")
  lg <- function(p) if (p > 0) log(p) else -Inf
  lse <- function(a, b) {
    if (a == -Inf) return(b)
    if (b == -Inf) return(a)
    m <- max(a, b)
    m + log(exp(a - m) + exp(b - m))
  }
  lmm <- lg(tr$mm); lmi <- lg(tr$mi); lmd <- lg(tr$md)
  lim <- lg(tr$im); lii <- lg(tr$ii); ldm <- lg(tr$dm); ldd <- lg(tr$dd)
  fM <- matrix(-Inf, T + 1L, L + 1L); fI <- fM; fD <- fM
  vM <- fM; vI <- fM; vD <- fM
  fM[1, 1] <- 0; vM[1, 1] <- 0
  for (i in seq_len(T + 1L)) for (j in seq_len(L + 1L)) {
    if (i == 1L && j == 1L) next
    if (i > 1L && j > 1L) {
      e <- lg(Mm[j - 1L, xs[i - 1L] + 1L])
      acc <- lse(lse(fM[i - 1L, j - 1L] + lmm, fI[i - 1L, j - 1L] + lim), fD[i - 1L, j - 1L] + ldm)
      fM[i, j] <- acc + e
      vM[i, j] <- max(vM[i - 1L, j - 1L] + lmm, vI[i - 1L, j - 1L] + lim, vD[i - 1L, j - 1L] + ldm) + e
    }
    if (i > 1L) {
      e <- lg(ins[xs[i - 1L] + 1L])
      fI[i, j] <- lse(fM[i - 1L, j] + lmi, fI[i - 1L, j] + lii) + e
      vI[i, j] <- max(vM[i - 1L, j] + lmi, vI[i - 1L, j] + lii) + e
    }
    if (j > 1L) {
      fD[i, j] <- lse(fM[i, j - 1L] + lmd, fD[i, j - 1L] + ldd)
      vD[i, j] <- max(vM[i, j - 1L] + lmd, vD[i, j - 1L] + ldd)
    }
  }
  fwd <- lse(lse(fM[T + 1L, L + 1L], fI[T + 1L, L + 1L]), fD[T + 1L, L + 1L])
  vit <- max(vM[T + 1L, L + 1L], vI[T + 1L, L + 1L], vD[T + 1L, L + 1L])
  bg <- sum(vapply(xs, function(v) lg(ins[v + 1L]), 0))
  .t1_result(estimate = fwd, forward_logprob = fwd, viterbi_logprob = vit,
             log_odds = fwd - bg, background_logprob = bg, n = T,
             method = "forward and Viterbi over match/insert/delete states, Eddy (1998); Krogh et al. (1994)")
}
