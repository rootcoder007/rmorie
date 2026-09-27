.fl_dist <- function(demand = NULL, sites = NULL, dist = NULL) {
  if (!is.null(dist)) {
    D <- as.matrix(dist)
  } else {
    if (is.null(demand)) stop("give dist or demand coordinates", call. = FALSE)
    A <- as.matrix(demand)
    S <- if (is.null(sites)) A else as.matrix(sites)
    D <- matrix(0, nrow(A), nrow(S))
    for (i in seq_len(nrow(A))) for (j in seq_len(nrow(S))) D[i, j] <- sqrt(sum((A[i, ] - S[j, ])^2))
  }
  storage.mode(D) <- "double"
  if (!length(D)) stop("distance matrix must be non-empty", call. = FALSE)
  D
}

.fl_search <- function(objective, m, p, max_enum) {
  if (p < 1 || p > m) stop("need 1 <= p <= number of candidate sites", call. = FALSE)
  if (choose(m, p) <= max_enum) {
    best <- Inf
    bs <- NULL
    cmb <- utils::combn(m, p)
    for (k in seq_len(ncol(cmb))) {
      v <- objective(cmb[, k])
      if (v < best - 1e-12) {
        best <- v
        bs <- cmb[, k]
      }
    }
    return(list(sites = bs, value = best, method = "exact enumeration"))
  }
  S <- integer(0)
  for (r in seq_len(p)) {
    cand <- setdiff(seq_len(m), S)
    vals <- vapply(cand, function(j) objective(sort(c(S, j))), 0)
    S <- c(S, cand[order(vals, cand)[1]])
  }
  S <- sort(S)
  best <- objective(S)
  improved <- TRUE
  while (improved) {
    improved <- FALSE
    for (out in S) {
      for (j in setdiff(seq_len(m), S)) {
        Tn <- sort(c(setdiff(S, out), j))
        v <- objective(Tn)
        if (v < best - 1e-12) {
          S <- Tn
          best <- v
          improved <- TRUE
          break
        }
      }
      if (improved) break
    }
  }
  list(sites = S, value = best, method = "greedy + vertex substitution (Teitz and Bart 1968)")
}

.fl_nearest <- function(D, S) vapply(seq_len(nrow(D)), function(i) S[order(D[i, S], S)[1]], 0)

#' Linear sum assignment (Hungarian method)
#'
#' Rows are added one at a time; each finds a shortest augmenting path in the
#' reduced costs c_ij - u_i - v_j with dual potentials (Jonker and Volgenant
#' 1987), so the assignment is optimal. A matrix with more rows than columns is
#' solved on its transpose.
#'
#' @param cost Finite cost matrix.
#' @param maximize Maximise instead of minimise.
#' @return list(rows, cols (1-based matched pairs, rows ascending), total,
#'   row_potential, col_potential).
#' @references Kuhn, H. W. (1955). The Hungarian method for the assignment
#'   problem. Naval Research Logistics Quarterly 2, 83-97.
#'   Jonker, R. and Volgenant, A. (1987). A shortest augmenting path algorithm
#'   for dense and sparse linear assignment problems. Computing 38, 325-340.
#' @examples
#' LinearAssignment(rbind(c(4, 1, 3), c(2, 0, 5), c(3, 2, 2)))$total
#' @export
LinearAssignment <- function(cost, maximize = FALSE) {
  C <- as.matrix(cost)
  storage.mode(C) <- "double"
  if (!length(C) || any(!is.finite(C))) stop("cost must be a non-empty finite matrix", call. = FALSE)
  sg <- if (maximize) -1 else 1
  tr <- nrow(C) > ncol(C)
  A <- sg * (if (tr) t(C) else C)
  n <- nrow(A)
  m <- ncol(A)
  u <- numeric(n + 1)
  v <- numeric(m + 1)
  p <- integer(m + 1)
  way <- integer(m + 1)
  for (i in seq_len(n)) {
    p[1] <- i
    j0 <- 0
    minv <- rep(Inf, m + 1)
    used <- rep(FALSE, m + 1)
    repeat {
      used[j0 + 1] <- TRUE
      i0 <- p[j0 + 1]
      delta <- Inf
      j1 <- 0
      for (j in seq_len(m)) {
        if (!used[j + 1]) {
          cur <- A[i0, j] - u[i0 + 1] - v[j + 1]
          if (cur < minv[j + 1]) {
            minv[j + 1] <- cur
            way[j + 1] <- j0
          }
          if (minv[j + 1] < delta) {
            delta <- minv[j + 1]
            j1 <- j
          }
        }
      }
      for (j in 0:m) {
        if (used[j + 1]) {
          u[p[j + 1] + 1] <- u[p[j + 1] + 1] + delta
          v[j + 1] <- v[j + 1] - delta
        } else {
          minv[j + 1] <- minv[j + 1] - delta
        }
      }
      j0 <- j1
      if (p[j0 + 1] == 0) break
    }
    while (j0 != 0) {
      j1 <- way[j0 + 1]
      p[j0 + 1] <- p[j1 + 1]
      j0 <- j1
    }
  }
  jj <- which(p[-1] > 0)
  rr <- p[-1][jj]
  if (tr) {
    tmp <- rr
    rr <- jj
    jj <- tmp
  }
  o <- order(rr)
  rows <- rr[o]
  cols <- jj[o]
  total <- 0
  for (k in seq_along(rows)) total <- total + C[rows[k], cols[k]]
  up <- sg * u[-1]
  vp <- sg * v[-1]
  list(rows = rows, cols = cols, total = total,
       row_potential = if (tr) vp else up, col_potential = if (tr) up else vp)
}

#' Travelling salesman tour
#'
#' Exact Held-Karp dynamic programme (O(2^n n^2)) or a nearest-neighbour tour
#' from node 1 improved by first-improvement 2-opt; "auto" is exact for
#' n <= max_exact. Tours start at node 1 with the smaller neighbour second.
#'
#' @param points Coordinate matrix (rows are nodes), or NULL.
#' @param dist Symmetric distance matrix, or NULL.
#' @param method "auto", "exact" or "heuristic".
#' @param max_exact Largest n solved exactly under "auto".
#' @return list(tour (1-based), length, method).
#' @references Held, M. and Karp, R. M. (1962). A dynamic programming approach
#'   to sequencing problems. Journal of SIAM 10, 196-210.
#'   Croes, G. A. (1958). A method for solving traveling-salesman problems.
#'   Operations Research 6, 791-812.
#' @examples
#' TravellingSalesman(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))$length
#' @export
TravellingSalesman <- function(points = NULL, dist = NULL, method = c("auto", "exact", "heuristic"), max_exact = 13) {
  method <- match.arg(method)
  if (is.null(points) == is.null(dist)) stop("give exactly one of points and dist", call. = FALSE)
  D <- if (!is.null(dist)) as.matrix(dist) else .fl_dist(points)
  n <- nrow(D)
  tl <- function(t) {
    s <- 0
    for (k in seq_along(t)) s <- s + D[t[k], t[k %% length(t) + 1]]
    s
  }
  exact <- method == "exact" || (method == "auto" && n <= max_exact)
  if (n <= 3) {
    tour <- seq_len(n)
  } else if (exact) {
    full <- bitwShiftL(1L, n - 1L)
    C <- matrix(Inf, full, n)
    P <- matrix(0L, full, n)
    for (j in 2:n) C[bitwShiftL(1L, j - 2L) + 1, j] <- D[1, j]
    for (S in 1:(full - 1)) {
      for (j in 2:n) {
        bj <- bitwShiftL(1L, j - 2L)
        if (bitwAnd(S, bj) == 0 || C[S + 1, j] == Inf) next
        for (k in 2:n) {
          bk <- bitwShiftL(1L, k - 2L)
          if (bitwAnd(S, bk) != 0) next
          cc <- C[S + 1, j] + D[j, k]
          T2 <- bitwOr(S, bk)
          if (cc < C[T2 + 1, k]) {
            C[T2 + 1, k] <- cc
            P[T2 + 1, k] <- j
          }
        }
      }
    }
    S <- full - 1
    best <- Inf
    last <- 0
    for (j in 2:n) {
      cc <- C[S + 1, j] + D[j, 1]
      if (cc < best) {
        best <- cc
        last <- j
      }
    }
    rev_t <- integer(0)
    while (last != 0) {
      rev_t <- c(rev_t, last)
      prev <- P[S + 1, last]
      S <- bitwAnd(S, bitwNot(bitwShiftL(1L, last - 2L)))
      last <- prev
    }
    tour <- c(1L, rev(rev_t))
  } else {
    tour <- 1L
    while (length(tour) < n) {
      cur <- tour[length(tour)]
      cand <- setdiff(seq_len(n), tour)
      tour <- c(tour, cand[order(D[cur, cand], cand)[1]])
    }
    improved <- TRUE
    while (improved) {
      improved <- FALSE
      for (a in 1:(n - 1)) {
        bmax <- if (a > 1) n else n - 1
        if (a + 2 > bmax) next
        for (b in (a + 2):bmax) {
          i <- tour[a]
          i1 <- tour[a + 1]
          j <- tour[b]
          j1 <- tour[b %% n + 1]
          if (D[i, j] + D[i1, j1] < D[i, i1] + D[j, j1] - 1e-12) {
            tour[(a + 1):b] <- rev(tour[(a + 1):b])
            improved <- TRUE
          }
        }
      }
    }
  }
  if (n > 2 && tour[n] < tour[2]) tour <- c(1L, rev(tour[-1]))
  list(tour = as.integer(tour), length = tl(tour), method = if (exact) "Held-Karp" else "nearest neighbour + 2-opt")
}

#' p-median location
#'
#' Chooses p sites minimising sum_i w_i min_j d_ij: exact enumeration when
#' choose(m, p) <= max_enum, otherwise greedy addition plus Teitz-Bart vertex
#' substitution.
#'
#' @param p Number of sites.
#' @param demand,sites Coordinate matrices (sites default to demand).
#' @param dist Demand-by-site distance matrix (instead of coordinates).
#' @param weights Demand weights.
#' @param max_enum Enumeration limit.
#' @return list(sites (1-based), assignment, objective, method).
#' @references Hakimi, S. L. (1964). Optimum locations of switching centers and
#'   the absolute centers and medians of a graph. Operations Research 12,
#'   450-459. Teitz, M. B. and Bart, P. (1968). Operations Research 16, 955-961.
#' @examples
#' PMedian(2, rbind(c(0, 0), c(1, 0), c(10, 0), c(11, 0)))$objective
#' @export
PMedian <- function(p, demand = NULL, sites = NULL, dist = NULL, weights = NULL, max_enum = 200000) {
  D <- .fl_dist(demand, sites, dist)
  w <- if (is.null(weights)) rep(1, nrow(D)) else as.numeric(weights)
  obj <- function(S) {
    s <- 0
    for (i in seq_len(nrow(D))) s <- s + w[i] * min(D[i, S])
    s
  }
  r <- .fl_search(obj, ncol(D), as.integer(p), max_enum)
  list(sites = r$sites, assignment = .fl_nearest(D, r$sites), objective = r$value, method = r$method)
}

#' p-center location
#'
#' Chooses p sites minimising max_i w_i min_j d_ij (vertex p-center), exactly by
#' enumeration or by greedy addition and substitution.
#'
#' @inheritParams PMedian
#' @return list(sites, assignment, radius, method).
#' @references Hakimi, S. L. (1965). Optimum distribution of switching centers
#'   in a communication network and some related graph theoretic problems.
#'   Operations Research 13, 462-475.
#' @examples
#' PCenter(1, rbind(c(0, 0), c(1, 0), c(4, 0)))$radius
#' @export
PCenter <- function(p, demand = NULL, sites = NULL, dist = NULL, weights = NULL, max_enum = 200000) {
  D <- .fl_dist(demand, sites, dist)
  w <- if (is.null(weights)) rep(1, nrow(D)) else as.numeric(weights)
  obj <- function(S) max(vapply(seq_len(nrow(D)), function(i) w[i] * min(D[i, S]), 0))
  r <- .fl_search(obj, ncol(D), as.integer(p), max_enum)
  list(sites = r$sites, assignment = .fl_nearest(D, r$sites), radius = r$value, method = r$method)
}

#' Maximal covering location
#'
#' Chooses p sites maximising the demand weight within \code{radius} of an open
#' site, exactly by enumeration or by greedy addition and substitution.
#'
#' @inheritParams PMedian
#' @param radius Service distance.
#' @return list(sites, covered, covered_weight, coverage_share, method).
#' @references Church, R. and ReVelle, C. (1974). The maximal covering location
#'   problem. Papers of the Regional Science Association 32, 101-118.
#' @examples
#' MaximalCovering(1, 1.5, rbind(c(0, 0), c(1, 0), c(2, 0), c(9, 0)))$covered_weight
#' @export
MaximalCovering <- function(p, radius, demand = NULL, sites = NULL, dist = NULL, weights = NULL, max_enum = 200000) {
  D <- .fl_dist(demand, sites, dist)
  w <- if (is.null(weights)) rep(1, nrow(D)) else as.numeric(weights)
  obj <- function(S) {
    s <- 0
    for (i in seq_len(nrow(D))) if (any(D[i, S] <= radius)) s <- s + w[i]
    -s
  }
  r <- .fl_search(obj, ncol(D), as.integer(p), max_enum)
  cov <- vapply(seq_len(nrow(D)), function(i) any(D[i, r$sites] <= radius), TRUE)
  tot <- 0
  for (x in w) tot <- tot + x
  list(sites = r$sites, covered = cov, covered_weight = -r$value, coverage_share = -r$value / tot, method = r$method)
}

#' Location set covering
#'
#' Fewest (or cheapest) sites so that every demand point lies within
#' \code{radius} of one; exact depth-first branch and bound on the uncovered
#' point with fewest covering sites.
#'
#' @inheritParams PMedian
#' @param radius Service distance.
#' @param costs Site costs (default 1).
#' @return list(sites, cost, n_sites).
#' @references Toregas, C., Swain, R., ReVelle, C. and Bergman, L. (1971). The
#'   location of emergency service facilities. Operations Research 19,
#'   1363-1373.
#' @examples
#' SetCoveringLocation(1, cbind(0:4, 0))$sites
#' @export
SetCoveringLocation <- function(radius, demand = NULL, sites = NULL, dist = NULL, costs = NULL) {
  D <- .fl_dist(demand, sites, dist)
  n <- nrow(D)
  m <- ncol(D)
  cst <- if (is.null(costs)) rep(1, m) else as.numeric(costs)
  cover <- lapply(seq_len(n), function(i) which(D[i, ] <= radius))
  if (any(lengths(cover) == 0)) stop("some demand point is farther than radius from every site", call. = FALSE)
  cmin <- min(cst)
  best <- new.env()
  best$cost <- Inf
  best$sites <- NULL
  rec <- function(chosen, cost, covered) {
    if (cost + (if (length(covered) < n) cmin else 0) >= best$cost - 1e-12) return(invisible())
    if (length(covered) == n) {
      best$cost <- cost
      best$sites <- sort(chosen)
      return(invisible())
    }
    unc <- setdiff(seq_len(n), covered)
    i <- unc[order(lengths(cover[unc]), unc)[1]]
    cand <- cover[[i]]
    for (j in cand[order(cst[cand], cand)]) rec(c(chosen, j), cost + cst[j], union(covered, which(D[, j] <= radius)))
  }
  rec(integer(0), 0, integer(0))
  list(sites = best$sites, cost = best$cost, n_sites = length(best$sites))
}

#' Uncapacitated facility location
#'
#' Opens sites S minimising sum_S f_j + sum_i w_i min_S d_ij: exact over all
#' subsets when 2^m - 1 <= max_enum, otherwise the Kuehn-Hamburger greedy add
#' followed by drop and swap moves.
#'
#' @inheritParams PMedian
#' @param fixed_costs Fixed cost per site.
#' @return list(sites, assignment, total, fixed, transport, method).
#' @references Kuehn, A. A. and Hamburger, M. J. (1963). A heuristic program
#'   for locating warehouses. Management Science 9, 643-666.
#' @examples
#' FacilityLocation(c(1, 100, 1), rbind(c(0, 0), c(1, 0), c(10, 0)))$sites
#' @export
FacilityLocation <- function(fixed_costs, demand = NULL, sites = NULL, dist = NULL, weights = NULL, max_enum = 200000) {
  D <- .fl_dist(demand, sites, dist)
  n <- nrow(D)
  m <- ncol(D)
  f <- as.numeric(fixed_costs)
  if (length(f) != m) stop("one fixed cost per site", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  total <- function(S) {
    t <- 0
    for (j in S) t <- t + f[j]
    for (i in seq_len(n)) t <- t + w[i] * min(D[i, S])
    t
  }
  if (2^m - 1 <= max_enum) {
    best <- Inf
    bs <- NULL
    for (k in seq_len(m)) {
      cmb <- utils::combn(m, k)
      for (q in seq_len(ncol(cmb))) {
        v <- total(cmb[, q])
        if (v < best - 1e-12) {
          best <- v
          bs <- cmb[, q]
        }
      }
    }
    S <- bs
    how <- "exact enumeration"
  } else {
    v1 <- vapply(seq_len(m), function(j) total(j), 0)
    S <- order(v1, seq_len(m))[1]
    best <- total(S)
    repeat {
      cand <- setdiff(seq_len(m), S)
      if (!length(cand)) break
      vals <- vapply(cand, function(j) total(sort(c(S, j))), 0)
      k <- order(vals, cand)[1]
      if (vals[k] >= best - 1e-12) break
      best <- vals[k]
      S <- sort(c(S, cand[k]))
    }
    improved <- TRUE
    while (improved) {
      improved <- FALSE
      moves <- if (length(S) > 1) lapply(S, function(o) setdiff(S, o)) else list()
      for (o in S) for (j in setdiff(seq_len(m), S)) moves[[length(moves) + 1]] <- sort(c(setdiff(S, o), j))
      for (Tn in moves) {
        v <- total(Tn)
        if (v < best - 1e-12) {
          S <- Tn
          best <- v
          improved <- TRUE
          break
        }
      }
    }
    how <- "greedy add + drop/swap (Kuehn and Hamburger 1963)"
  }
  fx <- 0
  for (j in S) fx <- fx + f[j]
  list(sites = S, assignment = .fl_nearest(D, S), total = best, fixed = fx, transport = best - fx, method = how)
}

#' Transportation problem
#'
#' Minimises sum c_ij x_ij subject to row sums = supply and column sums =
#' demand by successive shortest augmenting paths (Bellman-Ford on the residual
#' network); unbalanced problems get a zero-cost dummy source or sink.
#'
#' @param cost Cost matrix (sources by sinks).
#' @param supply,demand Non-negative amounts.
#' @return list(flow, total_cost, balanced).
#' @references Hitchcock, F. L. (1941). The distribution of a product from
#'   several sources to numerous localities. Journal of Mathematics and Physics
#'   20, 224-230.
#' @examples
#' TransportationProblem(rbind(c(4, 6), c(5, 3)), c(10, 10), c(8, 12))$total_cost
#' @export
TransportationProblem <- function(cost, supply, demand) {
  C0 <- as.matrix(cost)
  s <- as.numeric(supply)
  d <- as.numeric(demand)
  if (nrow(C0) != length(s) || ncol(C0) != length(d) || min(c(s, d)) < 0) {
    stop("cost must be length(supply) x length(demand) with non-negative amounts", call. = FALSE)
  }
  ns <- length(s)
  nd <- length(d)
  S <- .mh_sum_loop(s)
  Tt <- .mh_sum_loop(d)
  bal <- abs(S - Tt) <= 1e-9 * max(1, S, Tt)
  C <- C0
  if (!bal && S > Tt) {
    C <- cbind(C, 0)
    d <- c(d, S - Tt)
  } else if (!bal) {
    C <- rbind(C, 0)
    s <- c(s, Tt - S)
  }
  a <- length(s)
  b <- length(d)
  N <- a + b + 2
  src <- a + b + 1
  snk <- a + b + 2
  cap <- matrix(0, N, N)
  cst <- matrix(0, N, N)
  adj <- vector("list", N)
  add <- function(u, v, cc, w) {
    cap[u, v] <<- cc
    cst[u, v] <<- w
    cst[v, u] <<- -w
    adj[[u]] <<- c(adj[[u]], v)
    adj[[v]] <<- c(adj[[v]], u)
  }
  for (i in seq_len(a)) {
    add(src, i, s[i], 0)
    for (j in seq_len(b)) add(i, a + j, Inf, C[i, j])
  }
  for (j in seq_len(b)) add(a + j, snk, d[j], 0)
  need <- .mh_sum_loop(s)
  sent <- 0
  while (need - sent > 1e-12 * max(1, need)) {
    dist <- rep(Inf, N)
    prev <- integer(N)
    dist[src] <- 0
    for (it in seq_len(N - 1)) {
      ch <- FALSE
      for (u in seq_len(N)) {
        if (dist[u] == Inf) next
        for (v in adj[[u]]) {
          if (cap[u, v] > 1e-15 && dist[u] + cst[u, v] < dist[v] - 1e-15) {
            dist[v] <- dist[u] + cst[u, v]
            prev[v] <- u
            ch <- TRUE
          }
        }
      }
      if (!ch) break
    }
    if (dist[snk] == Inf) break
    fl <- Inf
    v <- snk
    while (v != src) {
      fl <- min(fl, cap[prev[v], v])
      v <- prev[v]
    }
    v <- snk
    while (v != src) {
      u <- prev[v]
      cap[u, v] <- cap[u, v] - fl
      cap[v, u] <- cap[v, u] + fl
      v <- u
    }
    sent <- sent + fl
  }
  X <- matrix(0, ns, nd)
  for (i in seq_len(ns)) for (j in seq_len(nd)) X[i, j] <- cap[a + j, i]
  tot <- 0
  for (i in seq_len(ns)) for (j in seq_len(nd)) tot <- tot + X[i, j] * C0[i, j]
  list(flow = X, total_cost = tot, balanced = bal)
}

.mh_sum_loop <- function(x) {
  s <- 0
  for (v in x) s <- s + v
  s
}

#' Vehicle routing by Clarke-Wright savings
#'
#' Parallel savings heuristic from depot 1: pairs are merged in decreasing
#' s_ij = d_1i + d_1j - d_ij when both are route ends and the load fits.
#'
#' @param demand Demand per node (entry 1, the depot, ignored).
#' @param capacity Vehicle capacity.
#' @param points Coordinate matrix, or NULL.
#' @param dist Distance matrix, or NULL.
#' @return list(routes (1-based customer vectors), loads, length, route_lengths).
#' @references Clarke, G. and Wright, J. W. (1964). Scheduling of vehicles from
#'   a central depot to a number of delivery points. Operations Research 12,
#'   568-581.
#' @examples
#' VehicleRoutingSavings(c(0, 1, 1, 1), 2, rbind(c(0, 0), c(1, 0), c(2, 0), c(0, 5)))$routes
#' @export
VehicleRoutingSavings <- function(demand, capacity, points = NULL, dist = NULL) {
  if (is.null(points) == is.null(dist)) stop("give exactly one of points and dist", call. = FALSE)
  D <- if (!is.null(dist)) as.matrix(dist) else .fl_dist(points)
  n <- nrow(D)
  q <- as.numeric(demand)
  if (length(q) != n || any(q[-1] > capacity)) stop("one demand per node, each at most the capacity", call. = FALSE)
  route <- lapply(seq_len(n), function(i) i)
  load <- q
  of <- seq_len(n)
  alive <- c(FALSE, rep(TRUE, n - 1))
  pr <- which(upper.tri(D) & row(D) > 1, arr.ind = TRUE)
  pr <- pr[pr[, 1] > 1, , drop = FALSE]
  sv <- D[1, pr[, 1]] + D[1, pr[, 2]] - D[pr]
  o <- order(-sv, pr[, 1], pr[, 2])
  for (k in o) {
    if (sv[k] <= 0) break
    i <- pr[k, 1]
    j <- pr[k, 2]
    ri <- of[i]
    rj <- of[j]
    if (ri == rj || load[ri] + load[rj] > capacity + 1e-12) next
    A <- route[[ri]]
    B <- route[[rj]]
    if (A[length(A)] == i && B[1] == j) {
      nw <- c(A, B)
    } else if (A[1] == i && B[length(B)] == j) {
      nw <- c(B, A)
    } else if (A[1] == i && B[1] == j) {
      nw <- c(rev(A), B)
    } else if (A[length(A)] == i && B[length(B)] == j) {
      nw <- c(A, rev(B))
    } else {
      next
    }
    route[[ri]] <- nw
    load[ri] <- load[ri] + load[rj]
    alive[rj] <- FALSE
    of[nw] <- ri
  }
  routes <- lapply(route[alive], function(r) if (r[1] <= r[length(r)]) r else rev(r))
  routes <- routes[order(vapply(routes, function(r) r[1], 0))]
  lens <- vapply(routes, function(r) {
    L <- D[1, r[1]] + D[r[length(r)], 1]
    if (length(r) > 1) for (k in 2:length(r)) L <- L + D[r[k - 1], r[k]]
    L
  }, 0)
  list(routes = routes, loads = vapply(routes, function(r) .mh_sum_loop(q[r]), 0), length = .mh_sum_loop(lens),
       route_lengths = lens)
}

#' Flow-capturing location model
#'
#' Chooses p nodes maximising the volume of flows whose path passes an open node,
#' exactly by enumeration or by greedy addition and substitution.
#'
#' @param p Number of sites.
#' @param paths List of integer node vectors (1-based), one per flow.
#' @param volumes Flow volumes (default 1).
#' @param n_nodes Number of candidate nodes (default the largest node index).
#' @param max_enum Enumeration limit.
#' @return list(sites, captured, captured_volume, share, method).
#' @references Hodgson, M. J. (1990). A flow-capturing location-allocation
#'   model. Geographical Analysis 22, 270-279.
#' @examples
#' FlowCapturingLocation(1, list(c(1, 2, 3), c(4, 2, 5), c(6, 7)))$captured_volume
#' @export
FlowCapturingLocation <- function(p, paths, volumes = NULL, n_nodes = NULL, max_enum = 200000) {
  q <- if (is.null(volumes)) rep(1, length(paths)) else as.numeric(volumes)
  m <- if (is.null(n_nodes)) max(unlist(paths)) else as.integer(n_nodes)
  obj <- function(S) {
    s <- 0
    for (k in seq_along(paths)) if (any(paths[[k]] %in% S)) s <- s + q[k]
    -s
  }
  r <- .fl_search(obj, m, as.integer(p), max_enum)
  cap <- vapply(paths, function(pt) any(pt %in% r$sites), TRUE)
  list(sites = r$sites, captured = cap, captured_volume = -r$value, share = -r$value / .mh_sum_loop(q), method = r$method)
}

.pc_area_per <- function(P) {
  a <- 0
  per <- 0
  k <- nrow(P)
  for (i in seq_len(k)) {
    j <- i %% k + 1
    a <- a + P[i, 1] * P[j, 2] - P[j, 1] * P[i, 2]
    per <- per + sqrt((P[j, 1] - P[i, 1])^2 + (P[j, 2] - P[i, 2])^2)
  }
  c(abs(a) / 2, per)
}

.pc_hull <- function(P) {
  P <- unique(P)
  P <- P[order(P[, 1], P[, 2]), , drop = FALSE]
  if (nrow(P) < 3) return(P)
  cross <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  lower <- matrix(0, 0, 2)
  for (i in seq_len(nrow(P))) {
    while (nrow(lower) >= 2 && cross(lower[nrow(lower) - 1, ], lower[nrow(lower), ], P[i, ]) <= 0) lower <- lower[-nrow(lower), , drop = FALSE]
    lower <- rbind(lower, P[i, ])
  }
  upper <- matrix(0, 0, 2)
  for (i in rev(seq_len(nrow(P)))) {
    while (nrow(upper) >= 2 && cross(upper[nrow(upper) - 1, ], upper[nrow(upper), ], P[i, ]) <= 0) upper <- upper[-nrow(upper), , drop = FALSE]
    upper <- rbind(upper, P[i, ])
  }
  rbind(lower[-nrow(lower), , drop = FALSE], upper[-nrow(upper), , drop = FALSE])
}

.pc_mec <- function(P) {
  H <- .pc_hull(P)
  inside <- function(cc, r) all(sqrt((H[, 1] - cc[1])^2 + (H[, 2] - cc[2])^2) <= r * (1 + 1e-12) + 1e-12)
  best <- list(c(0, 0), Inf)
  h <- nrow(H)
  if (h == 1) return(list(H[1, ], 0))
  for (i in 1:(h - 1)) for (j in (i + 1):h) {
    cc <- (H[i, ] + H[j, ]) / 2
    r <- sqrt(sum((H[i, ] - cc)^2))
    if (r < best[[2]] && inside(cc, r)) best <- list(cc, r)
  }
  if (h >= 3) {
    for (i in 1:(h - 2)) for (j in (i + 1):(h - 1)) for (k in (j + 1):h) {
      a <- H[i, ]
      b <- H[j, ]
      cc <- H[k, ]
      dd <- 2 * (a[1] * (b[2] - cc[2]) + b[1] * (cc[2] - a[2]) + cc[1] * (a[2] - b[2]))
      if (abs(dd) < 1e-300) next
      ux <- ((a[1]^2 + a[2]^2) * (b[2] - cc[2]) + (b[1]^2 + b[2]^2) * (cc[2] - a[2]) + (cc[1]^2 + cc[2]^2) * (a[2] - b[2])) / dd
      uy <- ((a[1]^2 + a[2]^2) * (cc[1] - b[1]) + (b[1]^2 + b[2]^2) * (a[1] - cc[1]) + (cc[1]^2 + cc[2]^2) * (b[1] - a[1])) / dd
      r <- sqrt((a[1] - ux)^2 + (a[2] - uy)^2)
      if (r < best[[2]] && inside(c(ux, uy), r)) best <- list(c(ux, uy), r)
    }
  }
  best
}

#' Polygon compactness
#'
#' Polsby-Popper 4 pi A / P^2, Schwartzberg 2 sqrt(pi A) / P, Reock A over the
#' area of the minimum enclosing circle, and A over the convex-hull area; all
#' equal 1 for a disc.
#'
#' @param vertices Two-column matrix of polygon vertices (either orientation).
#' @return list(area, perimeter, polsby_popper, schwartzberg, reock, convex_hull,
#'   enclosing_circle).
#' @references Polsby, D. D. and Popper, R. D. (1991). The third criterion:
#'   compactness as a procedural safeguard against partisan gerrymandering. Yale
#'   Law and Policy Review 9, 301-353. Reock, E. C. (1961). A note: measuring
#'   compactness as a requirement of legislative apportionment. Midwest Journal
#'   of Political Science 5, 70-74. Schwartzberg, J. E. (1966).
#'   Reapportionment, gerrymanders, and the notion of compactness. Minnesota Law
#'   Review 50, 443-452.
#' @examples
#' PolygonCompactness(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))$polsby_popper
#' @export
PolygonCompactness <- function(vertices) {
  P <- as.matrix(vertices)
  storage.mode(P) <- "double"
  if (nrow(P) < 3) stop("a polygon needs at least 3 vertices", call. = FALSE)
  ap <- .pc_area_per(P)
  if (ap[1] <= 0) stop("polygon has zero area", call. = FALSE)
  Ah <- .pc_area_per(.pc_hull(P))[1]
  mc <- .pc_mec(P)
  list(area = ap[1], perimeter = ap[2], polsby_popper = 4 * pi * ap[1] / ap[2]^2,
       schwartzberg = 2 * sqrt(pi * ap[1]) / ap[2], reock = ap[1] / (pi * mc[[2]]^2),
       convex_hull = ap[1] / Ah, enclosing_circle = list(centre = mc[[1]], radius = mc[[2]]))
}

#' Morphological opening
#'
#' Dilation of the erosion with a flat structuring element; positions outside
#' the image are ignored (+Inf for erosion, -Inf for dilation). Binary 0/1
#' images give the binary opening; the result is anti-extensive and idempotent.
#'
#' @param image Numeric matrix.
#' @param structure 0/1 matrix (default 3 x 3 square).
#' @param origin c(row, col), 1-based (default the centre).
#' @return list(opened, eroded).
#' @references Serra, J. (1982). Image Analysis and Mathematical Morphology.
#'   Academic Press. Soille, P. (2003). Morphological Image Analysis, 2nd ed.
#'   Springer.
#' @examples
#' img <- matrix(0, 5, 5)
#' img[2:3, 2:3] <- 1
#' img[4, 4] <- 1
#' MorphologicalOpening(img, matrix(1, 2, 2), c(1, 1))$opened
#' @export
MorphologicalOpening <- function(image, structure = matrix(1, 3, 3), origin = NULL) {
  img <- as.matrix(image)
  storage.mode(img) <- "double"
  se <- as.matrix(structure) != 0
  if (!any(se)) stop("structuring element is empty", call. = FALSE)
  o <- if (is.null(origin)) c(nrow(se) %/% 2 + 1, ncol(se) %/% 2 + 1) else as.integer(origin)
  offs <- which(se, arr.ind = TRUE)
  offs <- offs[order(offs[, 1], offs[, 2]), , drop = FALSE]
  dy <- offs[, 1] - o[1]
  dx <- offs[, 2] - o[2]
  pass <- function(M, op) {
    h <- nrow(M)
    w <- ncol(M)
    out <- M
    for (y in seq_len(h)) for (x in seq_len(w)) {
      yy <- if (op == "erode") y + dy else y - dy
      xx <- if (op == "erode") x + dx else x - dx
      ok <- yy >= 1 & yy <= h & xx >= 1 & xx <= w
      if (any(ok)) {
        vals <- M[cbind(yy[ok], xx[ok])]
        out[y, x] <- if (op == "erode") min(vals) else max(vals)
      }
    }
    out
  }
  er <- pass(img, "erode")
  list(opened = pass(er, "dilate"), eroded = er)
}
