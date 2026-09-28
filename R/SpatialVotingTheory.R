.sv_W <- function(A, d) {
  if (is.null(A)) return(diag(d))
  if (is.matrix(A)) return(A)
  diag(A, d)
}

.sv_d2 <- function(p, x, Wm) {
  h <- p - x
  sum(h * (Wm %*% h))
}

.sv_votes <- function(P, x, y, A, w) {
  M <- .sv_W(A, ncol(P))
  dx <- apply(P, 1, .sv_d2, x = x, Wm = M)
  dy <- apply(P, 1, .sv_d2, x = y, Wm = M)
  c(sum(w[dx < dy]), sum(w[dy < dx]))
}

.sv_wq <- function(v, w, q) {
  o <- order(v, seq_along(v))
  acc <- cumsum(w[o])
  v[o][which(acc >= q * sum(w) - 1e-12)[1]]
}

#' Spatial voting: majority rule, pivots, Pareto sets and win sets
#'
#' \code{MajorityCompare}: weighted sincere votes between two alternatives
#' under (weighted) Euclidean preferences. \code{PivotPoints}: weighted
#' median and supermajority, veto and bicameral pivots (Krehbiel 1998).
#' \code{ParetoSet}: convex-hull Pareto set. \code{WinSet}: majority win set
#' of a status quo on a grid (Plott 1967). Identical to the Python arm
#' \code{morie.fn.spatialvote}.
#'
#' @param ideals Ideal-point matrix (or vector in one dimension).
#' @param x,y Alternatives.
#' @param weights Voter weights.
#' @param quota Winning share.
#' @param A Salience vector or positive definite matrix.
#' @param supermajority,veto Supermajority and veto-override quotas.
#' @param chambers List of member index vectors (1-based).
#' @param points Points to test.
#' @param status_quo Status quo.
#' @param grid Candidate points (matrix).
#' @return List.
#' @references Krehbiel, K. (1998). Pivotal Politics. University of Chicago
#'   Press.
#'
#'   Plott, C. R. (1967). A notion of equilibrium and its possibility under
#'   majority rule. American Economic Review 57, 787-806.
#' @examples
#' MajorityCompare(matrix(c(0, 2, 3)), 1, 2.5)$winner
#' PivotPoints(1:5, supermajority = 0.6)$gridlock
#' @export
MajorityCompare <- function(ideals, x, y, weights = NULL, quota = 0.5, A = NULL) {
  P <- as.matrix(ideals)
  w <- if (is.null(weights)) rep(1, nrow(P)) else weights
  v <- .sv_votes(P, x, y, A, w)
  list(votes_x = v[1], votes_y = v[2], winner = if (v[1] > quota * sum(w)) "x" else if (v[2] > quota * sum(w)) "y" else "none")
}

#' @rdname MajorityCompare
#' @export
PivotPoints <- function(ideals, weights = NULL, supermajority = NULL, veto = NULL, chambers = NULL) {
  x <- as.numeric(ideals)
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  med <- .sv_wq(x, w, 0.5)
  out <- list(median = med)
  lo <- med
  hi <- med
  if (!is.null(supermajority)) {
    pv <- c(.sv_wq(x, w, 1 - supermajority), .sv_wq(x, w, supermajority))
    out$supermajority_pivots <- pv
    lo <- min(lo, pv[1])
    hi <- max(hi, pv[2])
  }
  if (!is.null(veto)) {
    out$veto_pivot <- .sv_wq(x, w, 1 - veto)
    lo <- min(lo, out$veto_pivot)
  }
  if (!is.null(chambers)) {
    meds <- vapply(chambers, function(cc) .sv_wq(x[cc], w[cc], 0.5), 0)
    out$chamber_medians <- meds
    out$bicameral_core <- range(meds)
    lo <- min(lo, meds)
    hi <- max(hi, meds)
  }
  out$gridlock <- c(lo, hi)
  out
}

#' @rdname MajorityCompare
#' @export
ParetoSet <- function(ideals, points = NULL) {
  P <- as.matrix(ideals)
  Q <- if (is.null(points)) matrix(0, 0, ncol(P)) else as.matrix(points)
  if (ncol(P) == 1) {
    r <- range(P)
    return(list(hull = r, inside = Q[, 1] >= r[1] & Q[, 1] <= r[2]))
  }
  H <- .cmp_hull(P)
  ins <- vapply(seq_len(nrow(Q)), function(k) {
    all(vapply(seq_len(nrow(H)), function(i) {
      j <- if (i == nrow(H)) 1 else i + 1
      (H[j, 1] - H[i, 1]) * (Q[k, 2] - H[i, 2]) - (H[j, 2] - H[i, 2]) * (Q[k, 1] - H[i, 1]) >= -1e-12
    }, TRUE))
  }, TRUE)
  list(hull = unname(H), inside = ins)
}

#' @rdname MajorityCompare
#' @export
WinSet <- function(ideals, status_quo, grid, weights = NULL, quota = 0.5, A = NULL) {
  P <- as.matrix(ideals)
  G <- as.matrix(grid)
  w <- if (is.null(weights)) rep(1, nrow(P)) else weights
  flags <- vapply(seq_len(nrow(G)), function(k) .sv_votes(P, G[k, ], status_quo, A, w)[1] > quota * sum(w), TRUE)
  list(in_win_set = flags, members = G[flags, , drop = FALSE], share = mean(flags))
}

.sv_cdf <- function(z, error) {
  switch(error,
    logit = stats::plogis(z), probit = stats::pnorm(z), cauchy = 0.5 + atan(z) / pi,
    laplace = ifelse(z < 0, 0.5 * exp(z), 1 - 0.5 * exp(-z)), uniform = pmin(1, pmax(0, (z + 1) / 2)),
    stop("error must be logit, probit, cauchy, laplace or uniform")
  )
}

#' Probabilistic voting and candidate competition
#'
#' \code{ProbabilisticVote}: choice probabilities, expected vote shares and
#' (two candidates) win probability under logit, probit, Cauchy, Laplace or
#' uniform utility-difference errors with valence. \code{CandidateEquilibrium}:
#' alternating best responses of two vote-share maximisers on a grid.
#' \code{HotellingPriceLocation} and \code{SalopCircle}: spatial price
#' competition (d'Aspremont et al. 1979; Salop 1979). \code{KalaiSmorodinsky}:
#' Nash and Kalai-Smorodinsky solutions on a piecewise-linear frontier.
#' \code{BaronFerejohn}: closed-rule legislative bargaining.
#' \code{VoteTradingRikerBrams}: logrolling and its welfare effect.
#' \code{PcaIdealPoints}: principal-component ideal points.
#'
#' @param ideals Ideal-point matrix.
#' @param positions Candidate positions (matrix).
#' @param valence Candidate valences.
#' @param beta Spatial sensitivity.
#' @param error Error distribution.
#' @param weights Voter weights.
#' @param A Salience vector or matrix.
#' @param grid Candidate grid (matrix).
#' @param start Starting positions (matrix, two rows).
#' @param maxit Maximum iterations.
#' @param a,b Firm locations from the left and right ends.
#' @param t Transport cost.
#' @param c Marginal cost.
#' @param n Number of firms or legislators.
#' @param fixed_cost Entry cost.
#' @param length Market circumference.
#' @param frontier Matrix of frontier points (increasing first utility).
#' @param disagreement Disagreement point.
#' @param delta Discount factor.
#' @param quota Votes needed.
#' @param valuations Voters x issues signed intensities.
#' @param votes Legislator x roll-call matrix.
#' @param dims Number of dimensions.
#' @return List.
#' @references Enelow, J. M. and Hinich, M. J. (1984). The Spatial Theory of
#'   Voting. Cambridge University Press.
#'
#'   Groseclose, T. (2001). A model of candidate location when one candidate
#'   has a valence advantage. American Journal of Political Science 45,
#'   862-886.
#'
#'   Baron, D. P. and Ferejohn, J. A. (1989). Bargaining in legislatures.
#'   American Political Science Review 83, 1181-1206.
#'
#'   Riker, W. H. and Brams, S. J. (1973). The paradox of vote trading.
#'   American Political Science Review 67, 1235-1247.
#' @examples
#' ProbabilisticVote(matrix(c(0, 1)), matrix(c(0, 1)))$shares
#' BaronFerejohn(5, 1)$proposer_share
#' @export
ProbabilisticVote <- function(ideals, positions, valence = NULL, beta = 1, error = "logit", weights = NULL, A = NULL) {
  P <- as.matrix(ideals)
  X <- as.matrix(positions)
  J <- nrow(X)
  val <- if (is.null(valence)) numeric(J) else valence
  w <- if (is.null(weights)) rep(1, nrow(P)) else weights
  M <- .sv_W(A, ncol(P))
  U <- t(apply(P, 1, function(p) val - beta * apply(X, 1, function(x) .sv_d2(p, x, M))))
  if (J == 2) {
    q <- .sv_cdf(U[, 1] - U[, 2], error)
    pr <- cbind(q, 1 - q)
  } else {
    if (error != "logit") stop("more than two candidates needs error='logit'")
    e <- exp(U - apply(U, 1, max))
    pr <- e / rowSums(e)
  }
  out <- list(probabilities = unname(pr), shares = as.vector(colSums(w * pr) / sum(w)))
  if (J == 2) {
    m <- sum(pr[, 1])
    v <- sum(pr[, 1] * (1 - pr[, 1]))
    out$win_probability <- if (v > 0) 1 - stats::pnorm((nrow(P) / 2 - m) / sqrt(v)) else as.numeric(m > nrow(P) / 2)
  }
  out
}

#' @rdname ProbabilisticVote
#' @export
CandidateEquilibrium <- function(ideals, grid, valence = c(0, 0), beta = 1, error = "logit", start = NULL,
                                 maxit = 100, A = NULL) {
  G <- as.matrix(grid)
  pos <- if (is.null(start)) G[c(1, nrow(G)), , drop = FALSE] else as.matrix(start)
  ok <- FALSE
  for (it in seq_len(maxit)) {
    moved <- FALSE
    for (cc in 1:2) {
      other <- pos[3 - cc, ]
      best <- -Inf
      arg <- pos[cc, ]
      for (k in seq_len(nrow(G))) {
        pp <- if (cc == 1) rbind(G[k, ], other) else rbind(other, G[k, ])
        sh <- ProbabilisticVote(ideals, pp, valence, beta, error, A = A)$shares[cc]
        if (sh > best + 1e-12) {
          best <- sh
          arg <- G[k, ]
        }
      }
      if (any(arg != pos[cc, ])) {
        pos[cc, ] <- arg
        moved <- TRUE
      }
    }
    if (!moved) {
      ok <- TRUE
      break
    }
  }
  list(positions = unname(pos), distance = sqrt(sum((pos[1, ] - pos[2, ])^2)),
       shares = ProbabilisticVote(ideals, pos, valence, beta, error, A = A)$shares, converged = ok)
}

#' @rdname ProbabilisticVote
#' @export
HotellingPriceLocation <- function(a, b, t = 1, c = 0) {
  L <- 1 - a - b
  p1 <- c + t * L * (1 + (a - b) / 3)
  p2 <- c + t * L * (1 + (b - a) / 3)
  xh <- a + L / 2 + (p2 - p1) / (2 * t * L)
  list(prices = c(p1, p2), demand = c(xh, 1 - xh), profits = c((p1 - c) * xh, (p2 - c) * (1 - xh)), indifferent = xh)
}

#' @rdname ProbabilisticVote
#' @export
SalopCircle <- function(n = NULL, t = 1, c = 0, fixed_cost = NULL, length = 1) {
  out <- list()
  if (!is.null(fixed_cost)) out$free_entry_firms <- sqrt(t * length^2 / fixed_cost)
  if (!is.null(n)) {
    out$price <- c + t * length / n
    out$profit <- t * length^2 / n^2 - (if (is.null(fixed_cost)) 0 else fixed_cost)
  }
  out
}

#' @rdname ProbabilisticVote
#' @export
KalaiSmorodinsky <- function(frontier, disagreement = c(0, 0)) {
  F <- as.matrix(frontier)
  d <- disagreement
  best <- -Inf
  nash <- NULL
  for (k in seq_len(nrow(F) - 1)) {
    dx <- F[k + 1, 1] - F[k, 1]
    dy <- F[k + 1, 2] - F[k, 2]
    cand <- c(0, 1)
    if (dx * dy != 0) {
      s <- -((F[k, 1] - d[1]) * dy + (F[k, 2] - d[2]) * dx) / (2 * dx * dy)
      if (s >= 0 && s <= 1) cand <- c(cand, s)
    }
    for (s in cand) {
      u <- c(F[k, 1] + s * dx, F[k, 2] + s * dy)
      if (u[1] >= d[1] && u[2] >= d[2] && (u[1] - d[1]) * (u[2] - d[2]) > best) {
        best <- (u[1] - d[1]) * (u[2] - d[2])
        nash <- u
      }
    }
  }
  I <- c(max(F[, 1]), max(F[, 2]))
  ks <- NULL
  for (k in seq_len(nrow(F) - 1)) {
    ax <- I[1] - d[1]
    ay <- I[2] - d[2]
    den <- (F[k + 1, 1] - F[k, 1]) * ay - (F[k + 1, 2] - F[k, 2]) * ax
    if (abs(den) < 1e-15) next
    s <- ((d[1] - F[k, 1]) * ay - (d[2] - F[k, 2]) * ax) / den
    if (s >= -1e-12 && s <= 1 + 1e-12) {
      ks <- F[k, ] + s * (F[k + 1, ] - F[k, ])
      break
    }
  }
  list(nash = nash, nash_product = best, kalai_smorodinsky = unname(ks), ideal = I)
}

#' @rdname ProbabilisticVote
#' @export
BaronFerejohn <- function(n, delta = 1, quota = NULL) {
  q <- if (is.null(quota)) (n + 1) %/% 2 else quota
  list(proposer_share = 1 - delta / n * (q - 1), coalition_share = delta / n, coalition_size = q, continuation_value = 1 / n)
}

#' @rdname ProbabilisticVote
#' @export
VoteTradingRikerBrams <- function(valuations) {
  V <- as.matrix(valuations)
  n <- nrow(V)
  K <- ncol(V)
  votes <- V > 0
  outcome <- function(vt) colSums(vt) > n / 2
  welfare <- function(o) sum(V[, o, drop = FALSE])
  sincere <- outcome(votes)
  trades <- list()
  repeat {
    cur <- outcome(votes)
    best <- NULL
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        for (a in seq_len(K)) {
          for (b in seq_len(K)) {
            if (a == b) next
            if (!(abs(V[i, a]) > abs(V[i, b]) && abs(V[j, b]) > abs(V[j, a]))) next
            wb <- V[j, b] > 0
            wa <- V[i, a] > 0
            if (votes[i, b] == wb || votes[j, a] == wa) next
            tr <- votes
            tr[i, b] <- wb
            tr[j, a] <- wa
            nw <- outcome(tr)
            if (all(nw == cur)) next
            gi <- sum(V[i, ] * (nw - cur))
            gj <- sum(V[j, ] * (nw - cur))
            if (gi > 0 && gj > 0 && (is.null(best) || gi + gj > best$g + 1e-12)) {
              best <- list(g = gi + gj, tr = tr, rec = c(i, j, a, b) - 1)
            }
          }
        }
      }
    }
    if (is.null(best)) break
    votes <- best$tr
    trades[[length(trades) + 1]] <- best$rec
  }
  after <- outcome(votes)
  list(sincere = unname(sincere), after_trade = unname(after), trades = trades, welfare_before = welfare(sincere),
       welfare_after = welfare(after))
}

#' @rdname ProbabilisticVote
#' @export
PcaIdealPoints <- function(votes, dims = 1) {
  M <- as.matrix(votes) + 0
  for (j in seq_len(ncol(M))) M[is.na(M[, j]), j] <- mean(M[, j], na.rm = TRUE)
  X <- sweep(M, 2, colMeans(M))
  e <- eigen(tcrossprod(X), symmetric = TRUE)
  S <- vapply(seq_len(dims), function(k) {
    s <- e$vectors[, k] * sqrt(max(e$values[k], 0))
    if (s[1] > 0) -s else s
  }, numeric(nrow(M)))
  list(scores = matrix(S, nrow(M)), variance_share = pmax(e$values[seq_len(dims)], 0) / sum(e$values[e$values > 0]))
}
