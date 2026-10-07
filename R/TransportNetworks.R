.tn_edges <- function(edges) {
  E <- matrix(as.numeric(as.matrix(edges)), ncol = 3)
  E
}

.tn_dijkstra <- function(n, E, s, ban_nodes = integer(0), ban_edges = integer(0)) {
  keep <- !(seq_len(nrow(E)) %in% ban_edges) & !(E[, 1] %in% ban_nodes) & !(E[, 2] %in% ban_nodes)
  E <- E[keep, , drop = FALSE]
  dist <- rep(Inf, n)
  pred <- rep(0L, n)
  done <- rep(FALSE, n)
  dist[s] <- 0
  for (it in seq_len(n)) {
    cand <- which(!done & is.finite(dist))
    if (length(cand) == 0) break
    u <- cand[which.min(dist[cand])]
    done[u] <- TRUE
    for (k in which(E[, 1] == u)) {
      v <- E[k, 2]
      if (dist[u] + E[k, 3] < dist[v]) {
        dist[v] <- dist[u] + E[k, 3]
        pred[v] <- u
      }
    }
  }
  list(dist = dist, pred = pred)
}

.tn_path <- function(pred, s, t) {
  if (s == t) return(s)
  if (pred[t] == 0) return(integer(0))
  p <- t
  while (p[1] != s) p <- c(pred[p[1]], p)
  p
}

#' Shortest paths, k-shortest paths and travelling salesman tours
#'
#' \code{ShortestPath}: Dijkstra (1959) on a directed graph given as
#' \code{(from, to, cost)} rows with nodes \code{1..n}; the unsettled node of
#' least distance (lowest index on ties) is settled next. \code{KShortestPaths}:
#' Yen's (1971) loopless k shortest paths. \code{TspTour}: Held-Karp exact
#' tour (up to 12 nodes under \code{"auto"}) or nearest neighbour plus 2-opt,
#' starting at node 1. Identical to the Python arm \code{morie.fn.transnet}
#' (which numbers nodes from 0).
#'
#' @param n Number of nodes.
#' @param edges Matrix of \code{from}, \code{to}, \code{cost}.
#' @param source,target Nodes.
#' @param k Number of paths.
#' @param D Distance matrix.
#' @param method \code{"auto"}, \code{"exact"} or \code{"heuristic"}.
#' @return List.
#' @references Dijkstra, E. W. (1959). A note on two problems in connexion
#'   with graphs. Numerische Mathematik 1, 269-271.
#'
#'   Yen, J. Y. (1971). Finding the k shortest loopless paths in a network.
#'   Management Science 17, 712-716.
#'
#'   Held, M. and Karp, R. M. (1962). A dynamic programming approach to
#'   sequencing problems. Journal of SIAM 10, 196-210.
#' @examples
#' ShortestPath(4, rbind(c(1, 2, 1), c(2, 3, 2), c(1, 3, 4), c(3, 4, 1)), 1, 4)$path
#' TspTour(rbind(c(0, 2, 9, 10), c(1, 0, 6, 4), c(15, 7, 0, 8), c(6, 3, 12, 0)))$length
#' @export
ShortestPath <- function(n, edges, source, target = NULL) {
  E <- .tn_edges(edges)
  if (any(E[, 3] < 0)) stop("edge costs must be nonnegative")
  d <- .tn_dijkstra(n, E, source)
  out <- list(distance = d$dist, predecessor = d$pred)
  if (!is.null(target)) out$path <- .tn_path(d$pred, source, target)
  out
}

.tn_cost <- function(E, p) {
  sum(vapply(seq_len(length(p) - 1), function(i) min(E[E[, 1] == p[i] & E[, 2] == p[i + 1], 3]), 0))
}

#' @rdname ShortestPath
#' @export
KShortestPaths <- function(n, edges, source, target, k) {
  E <- .tn_edges(edges)
  first <- .tn_path(.tn_dijkstra(n, E, source)$pred, source, target)
  if (length(first) == 0) return(list(paths = list(), costs = numeric(0)))
  A <- list(first)
  B <- list()
  same <- function(a, b) length(a) == length(b) && all(a == b)
  while (length(A) < k) {
    last <- A[[length(A)]]
    for (i in seq_len(length(last) - 1)) {
      spur <- last[i]
      root <- last[seq_len(i)]
      ban_e <- integer(0)
      for (p in A) {
        if (length(p) > i && same(p[seq_len(i)], root)) {
          ban_e <- c(ban_e, which(E[, 1] == p[i] & E[, 2] == p[i + 1]))
        }
      }
      tail <- .tn_path(.tn_dijkstra(n, E, spur, root[-i], ban_e)$pred, spur, target)
      if (length(tail) > 0) {
        cand <- c(root[-i], tail)
        if (!any(vapply(A, same, TRUE, cand)) && !any(vapply(B, function(bb) same(bb$p, cand), TRUE))) {
          B[[length(B) + 1]] <- list(c = .tn_cost(E, cand), p = cand)
        }
      }
    }
    if (length(B) == 0) break
    lex_less <- function(a, b) {
      m <- min(length(a), length(b))
      d <- which(a[seq_len(m)] != b[seq_len(m)])
      if (length(d)) a[d[1]] < b[d[1]] else length(a) < length(b)
    }
    best <- 1
    for (j in seq_along(B)[-1]) {
      if (B[[j]]$c < B[[best]]$c || (B[[j]]$c == B[[best]]$c && lex_less(B[[j]]$p, B[[best]]$p))) best <- j
    }
    A[[length(A) + 1]] <- B[[best]]$p
    B <- B[-best]
  }
  list(paths = A, costs = vapply(A, function(p) .tn_cost(E, p), 0))
}

.tn_tour_len <- function(M, t) sum(M[cbind(t, c(t[-1], t[1]))])

#' @rdname ShortestPath
#' @export
TspTour <- function(D, method = "auto") {
  M <- as.matrix(D)
  n <- nrow(M)
  if (method == "auto") method <- if (n <= 12) "exact" else "heuristic"
  if (n <= 2) return(list(tour = seq_len(n), length = if (n) .tn_tour_len(M, seq_len(n)) else 0))
  if (method == "exact") {
    nm <- 2^n
    C <- matrix(Inf, nm, n)
    P <- matrix(0L, nm, n)
    C[2, 1] <- 0
    for (mask in seq(1, nm - 1, by = 2)) {
      for (j in seq_len(n)) {
        cj <- C[mask + 1, j]
        if (!is.finite(cj)) next
        for (m in 2:n) {
          if (bitwAnd(mask, 2^(m - 1)) != 0) next
          key <- mask + 2^(m - 1)
          v <- cj + M[j, m]
          if (v < C[key + 1, m]) {
            C[key + 1, m] <- v
            P[key + 1, m] <- j
          }
        }
      }
    }
    full <- nm - 1
    tot <- C[full + 1, 2:n] + M[2:n, 1]
    last <- which(tot == min(tot))[1] + 1
    tour <- integer(0)
    mask <- full
    while (last > 1) {
      tour <- c(last, tour)
      prev <- P[mask + 1, last]
      mask <- mask - 2^(last - 1)
      last <- prev
    }
    tour <- c(1L, tour)
    return(list(tour = tour, length = .tn_tour_len(M, tour)))
  }
  if (method != "heuristic") stop("method must be auto, exact or heuristic")
  tour <- 1L
  left <- 2:n
  while (length(left)) {
    cc <- tour[length(tour)]
    nx <- left[which.min(M[cc, left])]
    tour <- c(tour, nx)
    left <- setdiff(left, nx)
  }
  improved <- TRUE
  while (improved) {
    improved <- FALSE
    for (i in 2:(n - 1)) {
      for (j in (i + 1):n) {
        a <- tour[i - 1]
        b <- tour[i]
        cc <- tour[j]
        d <- tour[if (j == n) 1 else j + 1]
        if (M[a, cc] + M[b, d] < M[a, b] + M[cc, d] - 1e-12) {
          tour[i:j] <- rev(tour[i:j])
          improved <- TRUE
        }
      }
    }
  }
  list(tour = tour, length = .tn_tour_len(M, tour))
}

#' Vehicle routing, trip distribution, mode choice, assignment and signals
#'
#' \code{ClarkeWrightVrp}: Clarke and Wright (1964) parallel savings for the
#' capacitated VRP. \code{GravityDistribution}: doubly constrained gravity
#' model balanced by Furness. \code{LogitModeShares}: multinomial logit
#' shares and logsums. \code{TrafficAssignment}: all-or-nothing, Frank-Wolfe
#' user equilibrium or system optimum with BPR costs. \code{WebsterSignal}:
#' Webster (1958) cycle, greens and delay.
#'
#' @param D Symmetric distance matrix (depot included).
#' @param demand Demands (0 at the depot).
#' @param capacity Vehicle capacity.
#' @param depot Depot node.
#' @param productions,attractions Trip ends.
#' @param cost Cost matrix.
#' @param beta Deterrence parameter or BPR exponent.
#' @param deterrence \code{"exp"} or \code{"power"}.
#' @param tol,max_iter Convergence controls.
#' @param utilities Zones x modes systematic utilities.
#' @param n Number of nodes.
#' @param links Matrix of \code{from}, \code{to}, \code{t0}, \code{capacity}.
#' @param od Matrix of \code{origin}, \code{destination}, \code{flow}.
#' @param method \code{"aon"}, \code{"ue"} or \code{"so"}.
#' @param alpha BPR coefficient.
#' @param gap Relative-gap tolerance.
#' @param flows,saturation_flows Critical flows and saturation flows (veh/s).
#' @param lost_time Total lost time per cycle (s).
#' @return List.
#' @references Clarke, G. and Wright, J. W. (1964). Scheduling of vehicles
#'   from a central depot to a number of delivery points. Operations Research
#'   12, 568-581.
#'
#'   LeBlanc, L. J., Morlok, E. K. and Pierskalla, W. P. (1975). An efficient
#'   approach to solving the road network equilibrium traffic assignment
#'   problem. Transportation Research 9, 309-318.
#'
#'   Webster, F. V. (1958). Traffic Signal Settings. Road Research Technical
#'   Paper 39, HMSO.
#' @examples
#' GravityDistribution(c(100, 50), c(60, 90), rbind(c(1, 2), c(2, 1)), 0.5)$trips
#' WebsterSignal(c(0.25, 0.15), c(0.5, 0.5), 10)$cycle
#' @export
ClarkeWrightVrp <- function(D, demand, capacity, depot = 1) {
  M <- as.matrix(D)
  n <- nrow(M)
  custs <- setdiff(seq_len(n), depot)
  if (any(demand[custs] > capacity)) stop("a single demand exceeds capacity")
  rid <- setNames(seq_along(custs), custs)
  routes <- lapply(custs, function(i) i)
  loads <- demand[custs]
  pr <- t(utils::combn(custs, 2))
  sv <- M[depot, pr[, 1]] + M[depot, pr[, 2]] - M[pr]
  ord <- order(-sv, pr[, 1], pr[, 2])
  for (o in ord) {
    s <- sv[o]
    if (s <= 0) break
    i <- pr[o, 1]
    j <- pr[o, 2]
    a <- rid[[as.character(i)]]
    b <- rid[[as.character(j)]]
    if (a == b || loads[a] + loads[b] > capacity) next
    ri <- routes[[a]]
    rj <- routes[[b]]
    if (!(i %in% c(ri[1], ri[length(ri)])) || !(j %in% c(rj[1], rj[length(rj)]))) next
    if (ri[length(ri)] != i) ri <- rev(ri)
    if (rj[1] != j) rj <- rev(rj)
    routes[[a]] <- c(ri, rj)
    loads[a] <- loads[a] + loads[b]
    routes[[b]] <- NULL
    loads <- loads[-b]
    rid <- setNames(rep(seq_along(routes), lengths(routes)), unlist(routes))
  }
  routes <- lapply(routes, function(r) if (r[1] <= r[length(r)]) r else rev(r))
  routes <- routes[order(vapply(routes, `[`, 0, 1))]
  cost <- sum(vapply(routes, function(r) M[depot, r[1]] + sum(M[cbind(r[-length(r)], r[-1])]) + M[r[length(r)], depot], 0))
  list(routes = routes, cost = cost, loads = vapply(routes, function(r) sum(demand[r]), 0))
}

#' @rdname ClarkeWrightVrp
#' @export
GravityDistribution <- function(productions, attractions, cost, beta, deterrence = "exp", tol = 1e-12,
                                max_iter = 10000) {
  if (abs(sum(productions) - sum(attractions)) > 1e-9 * sum(productions)) {
    stop("productions and attractions must have equal totals")
  }
  C <- as.matrix(cost)
  Fm <- if (deterrence == "exp") exp(-beta * C) else C^(-beta)
  B <- rep(1, length(attractions))
  for (it in seq_len(max_iter)) {
    A <- 1 / as.vector(Fm %*% (B * attractions))
    B <- 1 / as.vector(t(Fm) %*% (A * productions))
    Tm <- outer(A * productions, B * attractions) * Fm
    if (max(abs(rowSums(Tm) - productions) / productions) < tol) break
  }
  list(trips = Tm, A = A, B = B, iterations = it)
}

#' @rdname ClarkeWrightVrp
#' @export
LogitModeShares <- function(utilities) {
  .morie_arg(utilities, "m")
  V <- as.matrix(utilities)
  mx <- apply(V, 1, max)
  e <- exp(V - mx)
  s <- rowSums(e)
  list(shares = e / s, logsum = mx + log(s))
}

#' @rdname ClarkeWrightVrp
#' @export
TrafficAssignment <- function(n, links, od, method = "ue", alpha = 0.15, beta = 4, max_iter = 1000, gap = 1e-10) {
  L <- matrix(as.numeric(as.matrix(links)), ncol = 4)
  OD <- matrix(as.numeric(as.matrix(od)), ncol = 3)
  marginal <- method == "so"
  cst <- function(x, mg) {
    t <- L[, 3] * (1 + alpha * (x / L[, 4])^beta)
    if (mg) t <- t + ifelse(x > 0, x * L[, 3] * alpha * beta * x^(beta - 1) / L[, 4]^beta, 0)
    t
  }
  aon <- function(t) {
    y <- numeric(nrow(L))
    E <- cbind(L[, 1:2, drop = FALSE], t)
    for (o in sort(unique(OD[, 1]))) {
      pred <- .tn_dijkstra(n, E, o)$pred
      for (r in which(OD[, 1] == o & OD[, 3] != 0)) {
        v <- OD[r, 2]
        while (v != o) {
          u <- pred[v]
          ks <- which(L[, 1] == u & L[, 2] == v)
          k <- ks[order(t[ks], ks)][1]
          y[k] <- y[k] + OD[r, 3]
          v <- u
        }
      }
    }
    y
  }
  x <- aon(cst(numeric(nrow(L)), FALSE))
  if (method == "aon") return(list(flow = x, time = cst(x, FALSE), iterations = 0, gap = NaN))
  if (!method %in% c("ue", "so")) stop("method must be aon, ue or so")
  rg <- Inf
  for (it in seq_len(max_iter)) {
    t <- cst(x, marginal)
    y <- aon(t)
    rg <- (sum(t * x) - sum(t * y)) / sum(t * x)
    if (rg < gap) break
    dv <- y - x
    lo <- 0
    hi <- 1
    for (b in 1:200) {
      mid <- (lo + hi) / 2
      if (sum(cst(x + mid * dv, marginal) * dv) > 0) hi <- mid else lo <- mid
      if (hi - lo < 1e-16) break
    }
    x <- x + (lo + hi) / 2 * dv
  }
  tt <- cst(x, FALSE)
  list(flow = x, time = tt, total_time = sum(tt * x), iterations = it, gap = rg)
}

#' @rdname ClarkeWrightVrp
#' @export
WebsterSignal <- function(flows, saturation_flows, lost_time) {
  y <- flows / saturation_flows
  Y <- sum(y)
  if (Y >= 1) stop("sum of critical flow ratios must be below 1")
  C <- (1.5 * lost_time + 5) / (1 - Y)
  g <- (C - lost_time) * y / Y
  lam <- g / C
  x <- flows / (lam * saturation_flows)
  d <- C * (1 - lam)^2 / (2 * (1 - lam * x)) + x^2 / (2 * flows * (1 - x)) -
    0.65 * (C / flows^2)^(1 / 3) * x^(2 + 5 * lam)
  list(cycle = C, green = g, Y = Y, delay = d)
}
