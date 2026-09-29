#' Hotelling-Downs competition of n candidates over uniform voters
#'
#' Voters are n_voters Philox uniforms; each votes for the nearest candidate.
#' For a uniform electorate the pure-strategy equilibria of vote-share
#' maximisers are: two candidates at the median; none for three; for four or
#' more, the outer candidates paired at a and 1 - a with the others evenly
#' spaced at 3a, 5a, and so on, a = 1 / (2n - 4) (Eaton and Lipsey 1975;
#' unique for n = 4, 5). The fractions are mapped to sample quantiles and
#' \code{\link{PluralityCompetition}} gives the largest finite-electorate
#' deviation gain. Identical to the Python arm
#' \code{morie.fn.hotlg.hotelling_model}.
#'
#' @param n_voters Number of voters.
#' @param n_candidates Number of candidates.
#' @param seed Philox seed.
#' @return List: value (voter median), equilibrium_positions (NULL for three
#'   candidates), fractions, has_pure_equilibrium, shares, max_gain,
#'   voter_median, n_voters, n_candidates.
#' @references Hotelling, H. (1929). Stability in competition. Economic
#'   Journal 39, 41-57.
#'
#'   Eaton, B. C. and Lipsey, R. G. (1975). The principle of minimum
#'   differentiation reconsidered. Review of Economic Studies 42, 27-49.
#'
#'   Shaked, A. (1975). Non-existence of equilibrium for the two-dimensional
#'   three-firms location problem. Review of Economic Studies 42, 51-55.
#' @examples
#' HotellingModel(n_voters = 9, n_candidates = 2, seed = 1)$equilibrium_positions
#' HotellingModel(n_candidates = 5)$fractions
#' @export
HotellingModel <- function(n_voters = 100, n_candidates = 2, seed = 42) {
  nv <- as.integer(n_voters)
  nc <- as.integer(n_candidates)
  if (nv < 1 || nc < 1) stop("need at least one voter and one candidate")
  voters <- .morie_random_uniform(nv, seed = seed)
  s <- sort(voters)
  q7 <- function(p) {
    idx <- (nv - 1) * p
    lo <- floor(idx)
    h <- idx - lo
    if (h == 0) s[lo + 1] else (1 - h) * s[lo + 1] + h * s[lo + 2]
  }
  med <- q7(0.5)
  fr <- if (nc <= 2) {
    rep(0.5, nc)
  } else if (nc == 3) {
    NULL
  } else {
    a <- 1 / (2 * nc - 4)
    c(a, a, (2 * seq_len(nc - 4) + 1) * a, 1 - a, 1 - a)
  }
  pos <- NULL
  shares <- NULL
  gain <- NULL
  if (!is.null(fr)) {
    pos <- vapply(fr, q7, 0)
    pc <- PluralityCompetition(voters, pos)
    shares <- pc$shares
    gain <- max(pc$gain)
  }
  list(
    value = med, equilibrium_positions = pos, fractions = fr, has_pure_equilibrium = !is.null(fr),
    shares = shares, max_gain = gain, voter_median = med, n_voters = nv, n_candidates = nc
  )
}
