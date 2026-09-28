#' Electoral systems and legislative polarization
#'
#' \code{GelmanKingIncumbency}: OLS incumbency advantage of Gelman and King.
#' \code{ProportionalSeats}: highest-averages (D'Hondt, Sainte-Lague,
#' modified Sainte-Lague) and largest-remainder (Hare, Droop) seat
#' allocation. \code{RunoffWinner}: two-round runoff from ranked ballots.
#' \code{NetworkPolarization}: modularity of the party partition of the
#' roll-call agreement network. Identical to the Python arm
#' \code{morie.fn.electoral}.
#'
#' @param v,v_lag Current and previous Democratic vote shares.
#' @param party Previous winner's party (+1/-1), or party labels for
#'   \code{NetworkPolarization}.
#' @param incumbency Incumbency indicator (+1, -1, 0).
#' @param votes Party votes, or a legislators by roll-calls matrix (1, 0, NA).
#' @param seats Number of seats.
#' @param method Allocation method.
#' @param threshold Legal threshold (vote fraction).
#' @param ballots List of rankings (0-based candidate indices).
#' @param n_candidates Number of candidates (default from the ballots).
#' @return List.
#' @references Gelman, A. and King, G. (1990). Estimating incumbency
#'   advantage without bias. American Journal of Political Science 34,
#'   1142-1164.
#'
#'   Balinski, M. L. and Young, H. P. (2001). Fair Representation, 2nd edn.
#'   Brookings Institution Press.
#'
#'   Waugh, A. S., Pei, L., Fowler, J. H., Mucha, P. J. and Porter, M. A.
#'   (2009). Party polarization in Congress: a network science approach.
#'   arXiv:0907.3509.
#' @examples
#' ProportionalSeats(c(100000, 80000, 30000, 20000), 8)$seats
#' RunoffWinner(c(rep(list(c(0, 1, 2)), 4), rep(list(c(1, 2, 0)), 3), rep(list(c(2, 1, 0)), 2)))$winner
#' @export
GelmanKingIncumbency <- function(v, v_lag, party, incumbency) {
  X <- cbind(1, v_lag, party, incumbency)
  n <- length(v)
  XtX <- crossprod(X)
  Ai <- solve(XtX)
  b <- as.vector(Ai %*% crossprod(X, v))
  res <- v - as.vector(X %*% b)
  s2 <- sum(res^2) / (n - 4)
  list(coefficients = b, se = unname(sqrt(s2 * diag(Ai))), psi = b[4], sigma = sqrt(s2))
}

#' @rdname GelmanKingIncumbency
#' @export
ProportionalSeats <- function(votes, seats, method = "dhondt", threshold = 0) {
  V <- as.numeric(votes)
  tot <- 0
  for (a in V) tot <- tot + a
  ok <- V / tot >= threshold
  k <- length(V)
  s <- integer(k)
  if (method %in% c("dhondt", "sainte_lague", "modified_sainte_lague")) {
    div <- function(n) {
      if (method == "dhondt") return(n + 1)
      if (method == "modified_sainte_lague" && n == 0) return(1.4)
      2 * n + 1
    }
    for (t in seq_len(seats)) {
      cand <- which(ok)
      q <- V[cand] / vapply(s[cand], div, 0)
      o <- order(-q, -V[cand], cand)
      s[cand[o[1]]] <- s[cand[o[1]]] + 1L
    }
  } else if (method %in% c("hare", "droop")) {
    tv <- sum(V[ok])
    q <- if (method == "hare") tv / seats else floor(tv / (seats + 1)) + 1
    s[ok] <- as.integer(floor(V[ok] / q))
    cand <- which(ok)
    o <- cand[order(-(V[cand] / q - s[cand]), -V[cand], cand)]
    left <- seats - sum(s)
    if (left > 0) s[o[seq_len(left)]] <- s[o[seq_len(left)]] + 1L
  } else {
    stop("unknown method")
  }
  list(seats = s, method = method)
}

#' @rdname GelmanKingIncumbency
#' @export
RunoffWinner <- function(ballots, n_candidates = NULL) {
  m <- if (is.null(n_candidates)) 1 + max(unlist(ballots)) else n_candidates
  first <- integer(m)
  for (b in ballots) if (length(b)) first[b[1] + 1] <- first[b[1] + 1] + 1L
  nv <- sum(first)
  top <- order(-first, seq_len(m))[1]
  if (first[top] > nv / 2) return(list(winner = top - 1, first_round = first, second_round = NULL))
  o <- order(-first, seq_len(m))
  a <- o[1] - 1
  b2 <- o[2] - 1
  va <- 0
  vb <- 0
  for (b in ballots) {
    ra <- if (a %in% b) match(a, b) else Inf
    rb <- if (b2 %in% b) match(b2, b) else Inf
    if (ra < rb) va <- va + 1 else if (rb < ra) vb <- vb + 1
  }
  win <- if (va > vb || (va == vb && (first[a + 1] > first[b2 + 1] || (first[a + 1] == first[b2 + 1] && a < b2)))) a else b2
  sec <- c(va, vb)
  names(sec) <- c(a, b2)
  list(winner = win, first_round = first, second_round = sec)
}

#' @rdname GelmanKingIncumbency
#' @export
NetworkPolarization <- function(votes, party) {
  Vm <- as.matrix(votes)
  n <- nrow(Vm)
  A <- matrix(0, n, n)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      ok <- !is.na(Vm[i, ]) & !is.na(Vm[j, ])
      A[i, j] <- A[j, i] <- if (any(ok)) sum(Vm[i, ok] == Vm[j, ok]) / sum(ok) else 0
    }
  }
  list(modularity = GraphModularity(A, party), agreement = A)
}
