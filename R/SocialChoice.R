#' Majority tournament solutions
#'
#' Pairwise majority relation among spatial alternatives (voter i prefers a to b
#' when U_i(a) > U_i(b)), with the Condorcet winner and loser, cycle detection,
#' the top cycle (Smith set), Miller's uncovered set, the Banks set (tops of
#' maximal transitive chains), Copeland and Borda scores.
#'
#' @param voters,alternatives Points.
#' @param model Utility model (see \code{VoterUtility}).
#' @param ... Passed to \code{VoterUtility}.
#' @return list(margin, beats, condorcet_winner, condorcet_loser, cyclic,
#'   top_cycle, uncovered, banks, copeland, borda); indices are 1-based.
#' @references Miller, N. R. (1980). A new solution set for tournaments and
#'   majority voting. American Journal of Political Science 24, 68-96. Banks, J.
#'   S. (1985). Sophisticated voting outcomes and agenda control. Social Choice
#'   and Welfare 1, 295-306.
#' @examples
#' MajorityTournament(c(0, 1, 2), c(0, 1, 2))$condorcet_winner
#' @export
MajorityTournament <- function(voters, alternatives, model = "quadratic", ...) {
  U <- VoterUtility(voters, alternatives, model = model, ...)$utility
  m <- ncol(U)
  M <- matrix(0, m, m)
  for (i in seq_len(nrow(U))) for (a in seq_len(m)) for (b in seq_len(m)) if (U[i, a] > U[i, b]) M[a, b] <- M[a, b] + 1
  margin <- M - t(M)
  beats <- margin > 0
  reach <- function(B) {
    R <- B
    for (k in seq_len(m)) for (i in seq_len(m)) if (R[i, k]) R[i, ] <- R[i, ] | R[k, ]
    R
  }
  others <- function(a) setdiff(seq_len(m), a)
  winner <- Filter(function(a) all(beats[a, others(a)]), seq_len(m))
  loser <- Filter(function(a) all(beats[others(a), a]), seq_len(m))
  R <- reach(beats)
  weak <- margin >= 0
  diag(weak) <- FALSE
  W <- reach(weak)
  top <- Filter(function(a) all(W[a, others(a)]), seq_len(m))
  uncovered <- Filter(function(b) !any(vapply(others(b), function(a) beats[a, b] && all(beats[a, beats[b, ]]), TRUE)), seq_len(m))
  banks <- integer(0)
  extend <- function(chain) {
    grew <- FALSE
    for (cc in seq_len(m)) {
      if (!(cc %in% chain) && all(beats[cc, chain])) {
        grew <- TRUE
        extend(c(chain, cc))
      }
    }
    if (!grew) banks <<- union(banks, chain[length(chain)])
  }
  for (a in seq_len(m)) extend(a)
  list(margin = margin, beats = beats, condorcet_winner = if (length(winner)) winner[1] else NULL,
       condorcet_loser = if (length(loser)) loser[1] else NULL, cyclic = any(diag(R)), top_cycle = top,
       uncovered = uncovered, banks = sort(banks), copeland = rowSums(beats) - colSums(beats),
       borda = vapply(seq_len(m), function(a) sum(vapply(seq_len(nrow(U)), function(i) sum(U[i, a] > U[i, ]), 0)), 0))
}

.yk_angles <- function(P) {
  a <- numeric(0)
  n <- nrow(P)
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    dx <- P[j, 1] - P[i, 1]
    dy <- P[j, 2] - P[i, 2]
    if (dx != 0 || dy != 0) a <- c(a, atan2(dx, -dy) %% pi)
  }
  sort(unique(a))
}

.yk_radius <- function(cc, P, angles) {
  arcs <- if (length(angles)) c(angles, angles[1] + pi) else c(0, pi)
  n <- nrow(P)
  best <- 0
  for (q in seq_len(length(arcs) - 1)) {
    a <- arcs[q]
    b <- arcs[q + 1]
    mid <- 0.5 * (a + b)
    pr <- P[, 1] * cos(mid) + P[, 2] * sin(mid)
    k <- order(pr, seq_len(n))[(n + 1) / 2]
    v <- cc - P[k, ]
    for (t in c(a, b)) best <- max(best, abs(v[1] * cos(t) + v[2] * sin(t)))
    nv <- sqrt(sum(v^2))
    if (nv > 0) {
      phi <- atan2(v[2], v[1]) %% pi
      for (cand in c(phi, phi + pi, phi - pi)) if (a <= cand && cand <= b) best <- max(best, nv)
    }
  }
  best
}

#' Yolk of a spatial majority game
#'
#' Smallest disc meeting every median line of an odd set of ideal points in the
#' plane, from the exact farthest-median-line distance (convex in the centre; the
#' interior-arc case where limiting median lines do not suffice is included)
#' minimised by Nelder-Mead from the centroid and each ideal point.
#'
#' @param ideals Two-column matrix of ideal points (odd count).
#' @param tol Radius below which the core is declared to exist.
#' @return list(center, radius, core).
#' @references McKelvey, R. D. (1986). Covering, dominance, and institution-free
#'   properties of social choice. American Journal of Political Science 30,
#'   283-314. Stone, R. E. and Tovey, C. A. (1992). Limiting median lines do not
#'   suffice to determine the yolk. Social Choice and Welfare 9, 33-35.
#' @examples
#' Yolk(rbind(c(0, 0), c(1, 0), c(-1, 0), c(0, 1), c(0, -1)))$radius
#' @export
Yolk <- function(ideals, tol = 1e-12) {
  P <- matrix(as.numeric(as.matrix(ideals)), ncol = 2)
  n <- nrow(P)
  if (n %% 2 == 0 || n < 3) stop("the yolk here needs an odd number (>= 3) of ideal points", call. = FALSE)
  angles <- .yk_angles(P)
  starts <- rbind(colMeans(P), P)
  best <- NULL
  for (s in seq_len(nrow(starts))) {
    r <- NelderMead(function(cc) .yk_radius(cc, P, angles), starts[s, ], step = 0.25, xtol = 1e-13, ftol = 1e-15, max_iter = 4000)
    if (is.null(best) || r$fun < best$fun) best <- r
  }
  list(center = best$x, radius = best$fun, core = best$fun <= max(tol, 1e-9))
}

#' Amendment agenda outcomes
#'
#' The amendment procedure over an ordered agenda: sincere voting compares the
#' two alternatives on the floor; sophisticated voting compares the outcomes each
#' branch leads to (backward induction on the voting tree).
#'
#' @param voters Points.
#' @param agenda Points in voting order.
#' @param model Utility model.
#' @param ... Passed to \code{VoterUtility}.
#' @return list(sincere, sophisticated, sincere_path) as 1-based agenda indices.
#' @references McKelvey, R. D. and Niemi, R. G. (1978). A multistage game
#'   representation of sophisticated voting for binary procedures. Journal of
#'   Economic Theory 18, 1-22.
#' @examples
#' AmendmentAgenda(c(0, 0.4, 1), c(1, 0, 0.5))$sincere
#' @export
AmendmentAgenda <- function(voters, agenda, model = "quadratic", ...) {
  U <- VoterUtility(voters, agenda, model = model, ...)$utility
  m <- ncol(U)
  maj <- function(a, b) if (sum(U[, a] > U[, b]) >= sum(U[, b] > U[, a])) a else b
  path <- 1L
  w <- 1L
  if (m > 1) for (k in 2:m) {
    w <- maj(w, k)
    path <- c(path, w)
  }
  memo <- new.env()
  soph <- function(cur, k) {
    if (k > m) return(cur)
    key <- paste(cur, k)
    if (is.null(memo[[key]])) {
      keep <- soph(cur, k + 1L)
      take <- soph(as.integer(k), k + 1L)
      memo[[key]] <- if (maj(keep, take) == keep) keep else take
    }
    memo[[key]]
  }
  list(sincere = as.integer(path[length(path)]), sophisticated = as.integer(soph(1L, 2L)), sincere_path = as.integer(path))
}

#' Agenda-setter equilibrium in one dimension
#'
#' A monopoly setter makes a take-it-or-leave-it proposal to a median voter who
#' accepts x iff |x - m| <= |q - m|; the setter proposes the acceptable point (or
#' option) closest to its ideal, keeping q when none is acceptable.
#'
#' @param setter,median,status_quo Ideal points and status quo.
#' @param options Optional discrete proposals.
#' @return list(outcome, acceptance_set, power, setter_gain).
#' @references Romer, T. and Rosenthal, H. (1978). Political resource
#'   allocation, controlled agendas, and the status quo. Public Choice 33(4),
#'   27-43.
#' @examples
#' AgendaSetterEquilibrium(10, 4, 1)$outcome
#' @export
AgendaSetterEquilibrium <- function(setter, median, status_quo, options = NULL) {
  lo <- median - abs(status_quo - median)
  hi <- median + abs(status_quo - median)
  if (is.null(options)) {
    out <- min(max(setter, lo), hi)
  } else {
    acc <- options[options >= lo & options <= hi]
    out <- if (length(acc)) acc[order(abs(acc - setter), acc)[1]] else status_quo
  }
  list(outcome = out, acceptance_set = c(lo, hi), power = abs(out - status_quo), setter_gain = abs(status_quo - setter) - abs(out - setter))
}

#' Voting power indices
#'
#' Banzhaf (absolute and normalised), Shapley-Shubik, Deegan-Packel, Johnston and
#' Holler (public good) indices of the weighted game [q; w] by exact enumeration,
#' with the minimal winning coalitions.
#'
#' @param weights Player weights (at most 20).
#' @param quota Quota (default: simple majority of the total weight).
#' @return list(banzhaf, banzhaf_absolute, shapley_shubik, deegan_packel,
#'   johnston, holler, minimal_winning (1-based), min_winning_size, quota).
#' @references Banzhaf, J. F. (1965). Weighted voting doesn't work. Rutgers Law
#'   Review 19, 317-343. Shapley, L. S. and Shubik, M. (1954). A method for
#'   evaluating the distribution of power in a committee system. American
#'   Political Science Review 48, 787-792. Deegan, J. and Packel, E. W. (1978). A
#'   new index of power for simple n-person games. International Journal of Game
#'   Theory 7, 113-123. Johnston, R. J. (1978). On the measurement of power.
#'   Environment and Planning A 10, 907-914.
#' @examples
#' PowerIndices(c(3, 2, 2), 4)$banzhaf
#' @export
PowerIndices <- function(weights, quota = NULL) {
  w <- as.numeric(weights)
  n <- length(w)
  if (n == 0 || n > 20 || any(w < 0)) stop("need 1 to 20 non-negative weights", call. = FALSE)
  tot <- sum(w)
  q <- if (!is.null(quota)) quota else if (all(w == round(w))) floor(tot / 2) + 1 else tot / 2 + 1e-12
  N <- 2^n
  Wt <- numeric(N)
  for (mask in seq_len(N - 1)) {
    low <- bitwAnd(mask, -mask)
    Wt[mask + 1] <- Wt[bitwXor(mask, low) + 1] + w[log2(low) + 1]
  }
  win <- Wt >= q - 1e-12
  swings <- numeric(n)
  ss <- numeric(n)
  jo <- numeric(n)
  vul <- 0
  minimal <- list()
  for (mask in 0:(N - 1)) {
    if (!win[mask + 1]) next
    mem <- which(bitwAnd(mask, 2^(0:(n - 1))) > 0)
    size <- length(mem)
    crit <- mem[!win[bitwXor(mask, 2^(mem - 1)) + 1]]
    swings[crit] <- swings[crit] + 1
    ss[crit] <- ss[crit] + factorial(size - 1) * factorial(n - size) / factorial(n)
    if (length(crit)) {
      vul <- vul + 1
      jo[crit] <- jo[crit] + 1 / length(crit)
    }
    if (length(crit) == size) minimal[[length(minimal) + 1]] <- mem
  }
  dp <- numeric(n)
  hol <- numeric(n)
  for (S in minimal) {
    dp[S] <- dp[S] + 1 / (length(S) * length(minimal))
    hol[S] <- hol[S] + 1
  }
  minimal <- minimal[order(vapply(minimal, function(S) paste(sprintf("%03d", S), collapse = ""), ""))]
  list(banzhaf = if (sum(swings)) swings / sum(swings) else numeric(n), banzhaf_absolute = swings / 2^(n - 1),
       shapley_shubik = ss, deegan_packel = dp, johnston = if (vul) jo / vul else numeric(n),
       holler = if (sum(hol)) hol / sum(hol) else numeric(n), minimal_winning = minimal,
       min_winning_size = if (length(minimal)) min(lengths(minimal)) else NULL, quota = q)
}

#' Shapley-Owen value
#'
#' Share of directions (uniform on the half circle) in which each voter is the
#' median of the projected ideal points; exact from the critical directions
#' perpendicular to pairs of ideal points.
#'
#' @param ideals Two-column matrix of ideal points (odd count).
#' @return list(value, strongest (1-based)).
#' @references Owen, G. and Shapley, L. S. (1989). Optimal location of candidates
#'   in ideological space. International Journal of Game Theory 18, 339-356.
#' @examples
#' ShapleyOwen(rbind(c(0, 0), c(1, 0), c(-1, 0), c(0, 1), c(0, -1)))$value
#' @export
ShapleyOwen <- function(ideals) {
  P <- matrix(as.numeric(as.matrix(ideals)), ncol = 2)
  n <- nrow(P)
  if (n %% 2 == 0 || n < 3) stop("needs an odd number (>= 3) of voters", call. = FALSE)
  ang <- .yk_angles(P)
  arcs <- if (length(ang)) c(ang, ang[1] + pi) else c(0, pi)
  val <- numeric(n)
  for (q in seq_len(length(arcs) - 1)) {
    mid <- 0.5 * (arcs[q] + arcs[q + 1])
    pr <- P[, 1] * cos(mid) + P[, 2] * sin(mid)
    k <- order(pr, seq_len(n))[(n + 1) / 2]
    val[k] <- val[k] + (arcs[q + 1] - arcs[q]) / pi
  }
  list(value = val, strongest = which(val == max(val))[1])
}
