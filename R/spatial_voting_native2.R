# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Batch-2 parity gap-fillers. Everything else on the Armstrong shelf
# already exists in R/spatial_voting.R (35 functions: MDS, SMACOF,
# NOMINATE, Bayesian AM/MDS/unfolding, CJR/EM/dynamic IRT, wordfish,
# anchoring vignettes, OC) and R/algnm.R (Rice cohesion). Only the two
# genuinely missing mirrors are added here -- collision-scanned first.

#' Party unity score per legislator
#'
#' Share of votes each legislator casts with their own party's
#' majority. With `unity_votes_only = TRUE` only roll calls where the
#' two largest parties' majorities oppose each other count (the CQ
#' convention). Ties within a party leave that roll call undefined for
#' its members. Mirrors `morie.fn.agpar`.
#'
#' @param vote_matrix Binary vote matrix (1 = yea, 0 = nay, NA =
#'   missing), legislators in rows.
#' @param party_id Party label per legislator.
#' @param unity_votes_only Restrict to party-unity votes.
#' @return List with `unity`, `by_party`, `n_votes_scored`, `n`.
#' @references Poole KT, Rosenthal H (1997). \emph{Congress: A
#'   Political-Economic History of Roll Call Voting}. Oxford University
#'   Press. Rice SA (1925). The behavior of legislative groups: a
#'   method of measurement. \emph{Political Science Quarterly} 40(1),
#'   60-72.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_party_unity(V, V)
morie_party_unity <- function(vote_matrix, party_id, unity_votes_only = FALSE) {
  V <- as.matrix(vote_matrix)
  storage.mode(V) <- "double"
  pid <- as.character(party_id)
  n <- nrow(V)
  q <- ncol(V)
  if (length(pid) != n) {
    stop("party_id must have one entry per legislator.", call. = FALSE)
  }
  parties <- unique(pid)
  maj <- lapply(parties, function(p) {
    rows <- V[pid == p, , drop = FALSE]
    vapply(seq_len(q), function(j) {
      col <- rows[, j]
      col <- col[!is.na(col)]
      if (!length(col)) return(NA_real_)
      yea <- sum(col)
      nay <- length(col) - yea
      if (yea > nay) 1 else if (nay > yea) 0 else NA_real_
    }, numeric(1))
  })
  names(maj) <- parties

  scored <- rep(TRUE, q)
  if (unity_votes_only) {
    if (length(parties) < 2L) {
      stop("unity_votes_only needs at least two parties.", call. = FALSE)
    }
    sizes <- vapply(parties, function(p) sum(pid == p), numeric(1))
    big2 <- parties[order(-sizes)][1:2]
    a <- maj[[big2[1]]]
    b <- maj[[big2[2]]]
    scored <- !is.na(a) & !is.na(b) & a != b
  }

  unity <- rep(NA_real_, n)
  counts <- integer(n)
  for (i in seq_len(n)) {
    m <- maj[[pid[i]]]
    ok <- scored & !is.na(V[i, ]) & !is.na(m)
    if (any(ok)) {
      unity[i] <- mean(V[i, ok] == m[ok])
      counts[i] <- sum(ok)
    }
  }
  by_party <- vapply(parties, function(p) {
    u <- unity[pid == p]
    if (all(is.na(u))) NA_real_ else mean(u, na.rm = TRUE)
  }, numeric(1))
  names(by_party) <- parties
  list(unity = unity, by_party = as.list(by_party),
       n_votes_scored = counts, n = n)
}

#' Heteroskedastic IRT scales given ideal points (Lauderdale 2010)
#'
#' Per-legislator noise scales for the probit ideal-point model
#' `P(yea) = pnorm((beta_j x_i - alpha_j) / psi_i)`; a large `psi_i`
#' marks an unpredictable voter. Ideal points and item parameters are
#' taken as given; each `psi_i` is fitted by one-dimensional ML and the
#' vector is normalised to geometric mean 1 for identification.
#' Mirrors `morie.fn.hsirt` (with fixed items).
#'
#' @param votes Binary vote matrix (NA = missing).
#' @param ideal_points Fixed ideal points, one per row of `votes`.
#' @param alpha,beta Fixed item parameters (one per column).
#' @return List with `psi`, `loglik`, `n`, `q`.
#' @references Lauderdale BE (2010). Unpredictable voters in ideal
#'   point estimation. \emph{Political Analysis} 18(2), 151-171.
#' @export
#' @examples
#' morie_heteroskedastic_scales(votes = c("a", "b", "c"), ideal_points = c("a", "b",
#' "c"), alpha = 0.5, beta = 0.5)
morie_heteroskedastic_scales <- function(votes, ideal_points, alpha, beta) {
  V <- as.matrix(votes)
  storage.mode(V) <- "double"
  x <- as.numeric(ideal_points)
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  n <- nrow(V)
  q <- ncol(V)
  if (length(x) != n) {
    stop("ideal_points must have one entry per legislator.", call. = FALSE)
  }
  if (length(alpha) != q || length(beta) != q) {
    stop("alpha and beta must have one entry per roll call.", call. = FALSE)
  }
  ok <- V[!is.na(V)]
  if (!all(ok %in% c(0, 1))) {
    stop("votes must be binary 0/1 (NA for missing).", call. = FALSE)
  }

  psi <- rep(1, n)
  for (i in seq_len(n)) {
    m <- !is.na(V[i, ])
    y <- V[i, m]
    if (sum(m) < 3L || min(y) == max(y)) next
    idx <- beta[m] * x[i] - alpha[m]
    nll <- function(logp) {
      pr <- pmin(pmax(stats::pnorm(idx / exp(logp)), 1e-9), 1 - 1e-9)
      -sum(y * log(pr) + (1 - y) * log(1 - pr))
    }
    psi[i] <- exp(stats::optimize(nll, c(-3, 3))$minimum)
  }
  psi <- psi / exp(mean(log(psi)))

  z <- sweep(outer(x, beta) , 2, alpha) / psi
  pr <- pmin(pmax(stats::pnorm(z), 1e-9), 1 - 1e-9)
  llmat <- ifelse(is.na(V), 0, V * log(pr) + (1 - V) * log(1 - pr))
  list(psi = psi, loglik = sum(llmat), n = n, q = q)
}
