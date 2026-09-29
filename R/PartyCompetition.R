# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial party competition and legislative bargaining.
# Identical to the Python arm morie.fn.polcomp.

#' Party competition: vote shares, Hotelling-Downs equilibria, strategic voting, entry, bargaining, committees
#'
#' \code{VoteShares}: proximity (ties split) or logit shares with
#' salience-weighted Euclidean distances. \code{BestResponseEquilibrium}:
#' sequential best responses on a grid (a move needs a gain above 1e-12; the
#' lowest improving index wins). \code{StrategicVoteShares}: Cox's M + 1
#' viable parties and wasted-vote switching. \code{EntryGame}: incumbents
#' best-respond anticipating the entrant's best response (Palfrey 1984).
#' \code{EffectiveNumberParties}: Laakso-Taagepera and Golosov.
#' \code{RubinsteinBargaining}: \code{(1 - delta2) / (1 - delta1 delta2)}.
#' \code{MedianParty}: the party of the seat-weighted median legislator.
#' \code{LaverDynamics}: sticker, aggregator, hunter and predator rules
#' (Laver 2005; hunter headings from Philox stream \code{step * P + party}).
#' \code{MergeSplitParties}: shares before and after a merger or split.
#' \code{SetterOutcome}: Romer-Rosenthal setter proposal.
#' \code{CommitteeOutcome}: Denzau-Mackay gatekeeping under open or closed rules.
#' Party indices are 0-based, as the Python arm.
#'
#' @param positions Party positions (vector for 1-D, or matrix rows).
#' @param voters Voter ideal points (vector or matrix rows).
#' @param weights Voter weights, or NULL.
#' @param rule "proximity" or "logit"; for \code{CommitteeOutcome} "open" or "closed".
#' @param beta Logit sensitivity.
#' @param salience Dimension weights, or NULL.
#' @param n_parties Number of parties.
#' @param grid Candidate positions.
#' @param start Starting grid indices (0-based), or NULL.
#' @param max_rounds Maximum best-response rounds.
#' @param magnitude District magnitude.
#' @param n_incumbents Number of incumbents.
#' @param shares Party vote or seat shares.
#' @param delta1,delta2 Discount factors.
#' @param seats Seat counts.
#' @param rules Decision rule of each party.
#' @param n_steps Number of steps.
#' @param speed Step length.
#' @param seed Philox seed.
#' @param merge Pair of 0-based party indices to merge, or NULL.
#' @param split c(index, delta) of a party to split, or NULL.
#' @param setter,median,status_quo Setter ideal, floor median and status quo.
#' @param committee,floor Committee and floor ideal points.
#' @return A list or numeric vector.
#' @references Downs, A. (1957). An Economic Theory of Democracy. Eaton, B. C.
#'   and Lipsey, R. G. (1975). Rev. Econ. Stud. 42, 27-49. Cox, G. W. (1997).
#'   Making Votes Count. Palfrey, T. R. (1984). Rev. Econ. Stud. 51, 139-156.
#'   Laakso, M. and Taagepera, R. (1979). Comparative Political Studies 12,
#'   3-27. Golosov, G. V. (2010). Party Politics 16, 171-192. Rubinstein, A.
#'   (1982). Econometrica 50, 97-109. Laver, M. and Schofield, N. (1990).
#'   Multiparty Government. Laver, M. (2005). APSR 99, 263-281. Romer, T. and
#'   Rosenthal, H. (1978). Public Choice 33, 27-43. Denzau, A. T. and Mackay,
#'   R. J. (1983). AJPS 27, 740-761.
#' @examples
#' VoteShares(c(0, 1), c(0.1, 0.4, 0.5, 0.9))
#' EffectiveNumberParties(c(0.5, 0.3, 0.2))
#' SetterOutcome(0.9, 0.5, 0.3)$outcome
#' @export
VoteShares <- function(positions, voters, weights = NULL, rule = "proximity", beta = 1, salience = NULL) {
  P <- .pc_pts(positions)
  V <- .pc_pts(voters)
  w <- if (is.null(weights)) rep(1, nrow(V)) else as.numeric(weights)
  sal <- if (is.null(salience)) rep(1, ncol(P)) else as.numeric(salience)
  sh <- numeric(nrow(P))
  for (i in seq_len(nrow(V))) {
    d <- colSums(sal * (V[i, ] - t(P))^2)
    m <- min(d)
    if (rule == "logit") {
      e <- exp(-beta * (d - m))
      sh <- sh + w[i] * e / sum(e)
    } else {
      win <- d == m
      sh <- sh + w[i] * win / sum(win)
    }
  }
  sh / sum(w)
}

.pc_pts <- function(x) {
  if (is.matrix(x)) return(unname(x) * 1)
  if (is.list(x)) return(do.call(rbind, lapply(x, as.numeric)))
  matrix(as.numeric(x), ncol = 1)
}

#' @rdname VoteShares
#' @export
BestResponseEquilibrium <- function(voters, n_parties, grid, weights = NULL, start = NULL, rule = "proximity", beta = 1,
                                    max_rounds = 200) {
  G <- .pc_pts(grid)
  idx <- if (is.null(start)) seq_len(n_parties) - 1 else start
  idx <- pmin(idx, nrow(G) - 1)
  rounds <- 0
  converged <- FALSE
  for (r in seq_len(max_rounds)) {
    rounds <- rounds + 1
    moved <- FALSE
    for (j in seq_len(n_parties)) {
      best <- idx[j]
      bs <- VoteShares(G[idx + 1, , drop = FALSE], voters, weights, rule, beta)[j]
      for (g in seq_len(nrow(G)) - 1) {
        trial <- idx
        trial[j] <- g
        s <- VoteShares(G[trial + 1, , drop = FALSE], voters, weights, rule, beta)[j]
        if (s > bs + 1e-12) {
          best <- g
          bs <- s
        }
      }
      if (best != idx[j]) {
        idx[j] <- best
        moved <- TRUE
      }
    }
    if (!moved) {
      converged <- TRUE
      break
    }
  }
  pos <- G[idx + 1, , drop = FALSE]
  list(positions = pos, shares = VoteShares(pos, voters, weights, rule, beta), grid_index = idx, converged = converged,
       rounds = rounds)
}

#' @rdname VoteShares
#' @export
StrategicVoteShares <- function(positions, voters, weights = NULL, magnitude = 1) {
  P <- .pc_pts(positions)
  V <- .pc_pts(voters)
  w <- if (is.null(weights)) rep(1, nrow(V)) else as.numeric(weights)
  sinc <- VoteShares(P, V, w)
  viable <- sort(order(-sinc, seq_along(sinc))[seq_len(magnitude + 1)])
  sh <- numeric(nrow(P))
  for (i in seq_len(nrow(V))) {
    d <- colSums((V[i, ] - t(P))^2)
    m <- min(d[viable])
    win <- viable[d[viable] == m]
    sh[win] <- sh[win] + w[i] / length(win)
  }
  list(sincere = sinc, strategic = sh / sum(w), viable = viable - 1)
}

#' @rdname VoteShares
#' @export
EntryGame <- function(voters, grid, weights = NULL, n_incumbents = 2, max_rounds = 100) {
  G <- .pc_pts(grid)
  ng <- nrow(G)
  entrant <- function(inc) {
    best <- 0
    bs <- -Inf
    for (g in seq_len(ng) - 1) {
      s <- VoteShares(G[c(inc, g) + 1, , drop = FALSE], voters, weights)[length(inc) + 1]
      if (s > bs + 1e-12) {
        best <- g
        bs <- s
      }
    }
    best
  }
  payoff <- function(inc, j) VoteShares(G[c(inc, entrant(inc)) + 1, , drop = FALSE], voters, weights)[j]
  mid <- ng %/% 2
  inc <- vapply(seq_len(n_incumbents) - 1, function(q) if (q %% 2 == 0) max(0, mid - 1 - q) else min(ng - 1, mid + q), 0)
  for (r in seq_len(max_rounds)) {
    moved <- FALSE
    for (j in seq_len(n_incumbents)) {
      best <- inc[j]
      bs <- payoff(inc, j)
      for (g in seq_len(ng) - 1) {
        trial <- inc
        trial[j] <- g
        s <- payoff(trial, j)
        if (s > bs + 1e-12) {
          best <- g
          bs <- s
        }
      }
      if (best != inc[j]) {
        inc[j] <- best
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  e <- entrant(inc)
  list(incumbents = G[inc + 1, , drop = FALSE], entrant = G[e + 1, ], shares = VoteShares(G[c(inc, e) + 1, , drop = FALSE],
                                                                                           voters, weights))
}

#' @rdname VoteShares
#' @export
EffectiveNumberParties <- function(shares) {
  p <- as.numeric(shares) / sum(shares)
  p1 <- max(p)
  list(laakso_taagepera = 1 / sum(p^2), golosov = sum(p / (p + p1^2 - p^2)))
}

#' @rdname VoteShares
#' @export
RubinsteinBargaining <- function(delta1, delta2) {
  x1 <- (1 - delta2) / (1 - delta1 * delta2)
  r1 <- -log(delta1)
  r2 <- -log(delta2)
  list(proposer = x1, responder = 1 - x1, nash_weight = r2 / (r1 + r2))
}

#' @rdname VoteShares
#' @export
MedianParty <- function(positions, seats) {
  pos <- as.numeric(positions)
  s <- as.numeric(seats)
  tot <- sum(s)
  o <- order(pos, seq_along(pos))
  acc <- cumsum(s[o])
  med <- o[which(acc > tot / 2)[1]]
  list(index = med - 1, position = pos[med], seat_share = s[med] / tot, majority = s[med] > tot / 2)
}

#' @rdname VoteShares
#' @export
LaverDynamics <- function(voters, positions, rules, n_steps, speed = 0.1, seed = 0) {
  V <- .pc_pts(voters)
  P <- .pc_pts(positions)
  k <- nrow(P)
  heading <- matrix(0, k, 2)
  path <- list(P)
  shares <- list(VoteShares(P, V))
  prev <- shares[[1]]
  for (step in seq_len(n_steps) - 1) {
    sh <- VoteShares(P, V)
    nw <- P
    big <- which.max(sh)
    near <- apply(V, 1, function(v) which.min(colSums((v - t(P))^2)))
    for (j in seq_len(k)) {
      rule <- rules[j]
      if (rule == "aggregator") {
        if (any(near == j)) nw[j, ] <- colMeans(V[near == j, , drop = FALSE])
      } else if (rule == "predator" && big != j) {
        dxy <- P[big, ] - P[j, ]
        dd <- sqrt(sum(dxy^2))
        if (dd > 0) nw[j, ] <- P[j, ] + min(speed, dd) * dxy / dd
      } else if (rule == "hunter") {
        if (step == 0 || sh[j] <= prev[j]) {
          u <- .morie_random_uniform(1, seed = seed, stream = step * k + j - 1)
          ang <- if (step > 0) atan2(-heading[j, 2], -heading[j, 1]) + (u - 0.5) * pi else 2 * pi * u
          heading[j, ] <- c(cos(ang), sin(ang))
        }
        nw[j, ] <- P[j, ] + speed * heading[j, ]
      }
    }
    prev <- sh
    P <- nw
    path[[length(path) + 1]] <- P
    shares[[length(shares) + 1]] <- VoteShares(P, V)
  }
  list(path = path, shares = shares)
}

#' @rdname VoteShares
#' @export
MergeSplitParties <- function(positions, voters, weights = NULL, merge = NULL, split = NULL) {
  P <- .pc_pts(positions)
  old <- VoteShares(P, voters, weights)
  if (!is.null(merge)) {
    i <- merge[1] + 1
    j <- merge[2] + 1
    wi <- old[i]
    wj <- old[j]
    mp <- if (wi + wj > 0) (wi * P[i, ] + wj * P[j, ]) / (wi + wj) else (P[i, ] + P[j, ]) / 2
    nP <- rbind(P[-c(i, j), , drop = FALSE], mp)
    nw <- VoteShares(nP, voters, weights)
    gain <- nw[length(nw)] - (wi + wj)
  } else {
    i <- split[1] + 1
    nP <- rbind(P[-i, , drop = FALSE], P[i, ] - split[2], P[i, ] + split[2])
    nw <- VoteShares(nP, voters, weights)
    gain <- sum(nw[length(nw) - 0:1]) - old[i]
  }
  list(old = old, new = nw, positions = unname(nP), gain = gain)
}

#' @rdname VoteShares
#' @export
SetterOutcome <- function(setter, median, status_quo) {
  r <- abs(status_quo - median)
  x <- min(max(setter, median - r), median + r)
  list(outcome = x, advantage = abs(x - median), accepted_interval = c(median - r, median + r))
}

#' @rdname VoteShares
#' @export
CommitteeOutcome <- function(committee, floor, status_quo, rule = "closed") {
  c_ <- stats::median(committee)
  m <- stats::median(floor)
  prop <- if (rule == "open") m else SetterOutcome(c_, m, status_quo)$outcome
  opened <- abs(prop - c_) < abs(status_quo - c_)
  list(outcome = if (opened) prop else status_quo, gate_opened = opened, committee_median = c_, floor_median = m)
}
