#' Arc and vehicle routing
#'
#' \code{ChinesePostman}: shortest closed walk covering every edge (odd
#' vertices matched exactly, Hierholzer circuit). \code{MultiDepotVrp}:
#' nearest-depot clustering then Clarke-Wright savings. \code{SolomonVrptw}:
#' Solomon's I1 insertion with time windows. \code{PickupDeliveryInsertion}:
#' pickup-and-delivery / dial-a-ride cheapest insertion.
#' \code{PeriodicVrp}: evenly spaced visit patterns, then savings per day.
#' \code{StochasticRouteCost}: expected length of an a-priori route with
#' stochastic demands and restocking. Node and customer indices are 0-based
#' as in the Python arm \code{morie.fn.routing}.
#'
#' @param n Number of vertices.
#' @param edges Matrix or list of (u, v, w) with 0-based vertices.
#' @param start Start vertex.
#' @param depots,customers Two-column matrices of coordinates.
#' @param demand Demands (entry 1 is the depot where nodes include it).
#' @param capacity Vehicle capacity.
#' @param points Two-column matrix of node coordinates (row 1 = depot).
#' @param ready,due,service Time windows and service times per node.
#' @param requests Matrix of (pickup, delivery, load).
#' @param max_ride Maximum ride length (dial-a-ride) or NULL.
#' @param frequency Visits per horizon for each node.
#' @param horizon Number of days.
#' @param route Customer visiting order (0-based node indices).
#' @param demand_pmfs List of named probability vectors (names are demands).
#' @return List.
#' @references Edmonds, J. and Johnson, E. L. (1973). Matching, Euler tours
#'   and the Chinese postman. Mathematical Programming 5, 88-124.
#'
#'   Solomon, M. M. (1987). Algorithms for the vehicle routing and scheduling
#'   problems with time window constraints. Operations Research 35, 254-265.
#'
#'   Savelsbergh, M. W. P. and Sol, M. (1995). The general pickup and
#'   delivery problem. Transportation Science 29, 17-29.
#'
#'   Dror, M., Laporte, G. and Trudeau, P. (1989). Vehicle routing with
#'   stochastic demands. Transportation Science 23, 166-176.
#' @examples
#' ChinesePostman(4, rbind(c(0, 1, 1), c(1, 2, 1), c(2, 3, 1), c(3, 0, 1), c(0, 2, 2)))$length
#' @export
ChinesePostman <- function(n, edges, start = 0L) {
  E <- matrix(as.numeric(unlist(edges)), ncol = 3, byrow = !is.matrix(edges))
  if (is.matrix(edges)) E <- as.matrix(edges) + 0
  D <- matrix(Inf, n, n)
  diag(D) <- 0
  nxt <- matrix(rep(seq_len(n), each = n), n, n)
  for (k in seq_len(nrow(E))) {
    u <- E[k, 1] + 1
    v <- E[k, 2] + 1
    if (E[k, 3] < D[u, v]) D[u, v] <- D[v, u] <- E[k, 3]
  }
  for (k in seq_len(n)) for (i in seq_len(n)) for (j in seq_len(n)) {
    if (D[i, k] + D[k, j] < D[i, j]) {
      D[i, j] <- D[i, k] + D[k, j]
      nxt[i, j] <- nxt[i, k]
    }
  }
  deg <- tabulate(c(E[, 1], E[, 2]) + 1, n)
  odd <- which(deg %% 2 == 1)
  m <- length(odd)
  memo <- new.env()
  best <- function(mask) {
    key <- as.character(mask)
    if (!is.null(memo[[key]])) return(memo[[key]])
    if (mask == 0) return(list(c = 0, pr = list()))
    i <- which(bitwAnd(mask, bitwShiftL(1L, 0:(m - 1))) > 0)[1] - 1
    res <- list(c = Inf, pr = list())
    if (i + 1 < m) {
      for (j in (i + 1):(m - 1)) {
        if (bitwAnd(mask, bitwShiftL(1L, j)) > 0) {
          sub <- best(bitwAnd(bitwAnd(mask, bitwNot(bitwShiftL(1L, i))), bitwNot(bitwShiftL(1L, j))))
          cc <- sub$c + D[odd[i + 1], odd[j + 1]]
          if (cc < res$c) res <- list(c = cc, pr = c(list(c(odd[i + 1], odd[j + 1]) - 1), sub$pr))
        }
      }
    }
    memo[[key]] <- res
    res
  }
  bm <- if (m > 0) best(bitwShiftL(1L, m) - 1L) else list(c = 0, pr = list())
  multi <- E
  for (pr in bm$pr) {
    x <- pr[1] + 1
    b <- pr[2] + 1
    while (x != b) {
      y <- nxt[x, b]
      w <- min(E[(E[, 1] == x - 1 & E[, 2] == y - 1) | (E[, 1] == y - 1 & E[, 2] == x - 1), 3])
      multi <- rbind(multi, c(x - 1, y - 1, w))
      x <- y
    }
  }
  adj <- vector("list", n)
  for (k in seq_len(nrow(multi))) {
    u <- multi[k, 1] + 1
    v <- multi[k, 2] + 1
    adj[[u]] <- rbind(adj[[u]], c(v, k))
    adj[[v]] <- rbind(adj[[v]], c(u, k))
  }
  for (v in seq_len(n)) if (!is.null(adj[[v]])) adj[[v]] <- adj[[v]][order(adj[[v]][, 1], adj[[v]][, 2]), , drop = FALSE]
  used <- rep(FALSE, nrow(multi))
  ptr <- rep(1L, n)
  stack <- start + 1
  circ <- integer(0)
  while (length(stack)) {
    v <- stack[length(stack)]
    a <- adj[[v]]
    na <- if (is.null(a)) 0 else nrow(a)
    while (ptr[v] <= na && used[a[ptr[v], 2]]) ptr[v] <- ptr[v] + 1L
    if (ptr[v] > na) {
      circ <- c(circ, v)
      stack <- stack[-length(stack)]
    } else {
      used[a[ptr[v], 2]] <- TRUE
      stack <- c(stack, a[ptr[v], 1])
    }
  }
  list(length = .rt_ss(multi[, 3]), circuit = rev(circ) - 1L, matching = bm$pr, added_length = bm$c)
}

.rt_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.rt_dist <- function(P) {
  P <- as.matrix(P)
  n <- nrow(P)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) D[i, j] <- sqrt(.rt_ss((P[i, ] - P[j, ])^2))
  D
}

#' @rdname ChinesePostman
#' @export
MultiDepotVrp <- function(depots, customers, demand, capacity) {
  Dp <- as.matrix(depots)
  C <- as.matrix(customers)
  assign <- vapply(seq_len(nrow(C)), function(i) {
    d <- sqrt(colSums((t(Dp) - C[i, ])^2))
    order(d, seq_along(d))[1]
  }, 1L)
  routes <- list()
  total <- 0
  for (d in seq_len(nrow(Dp))) {
    idx <- which(assign == d)
    if (!length(idx)) {
      routes[[d]] <- list()
      next
    }
    r <- VehicleRoutingSavings(c(0, demand[idx]), capacity, rbind(Dp[d, ], C[idx, , drop = FALSE]))
    routes[[d]] <- lapply(r$routes, function(rt) idx[rt - 1] - 1L)
    total <- total + r$length
  }
  list(routes = routes, assignment = assign - 1L, length = total)
}

.rt_sched <- function(route, D, e, l, s) {
  t <- 0
  sched <- numeric(0)
  prev <- 1
  for (u in route + 1) {
    t <- max(e[u], t + D[prev, u])
    if (t > l[u] + 1e-9) return(NULL)
    sched <- c(sched, t)
    t <- t + s[u]
    prev <- u
  }
  if (t + D[prev, 1] > l[1] + 1e-9) return(NULL)
  sched
}

#' @rdname ChinesePostman
#' @export
SolomonVrptw <- function(points, demand, ready, due, service, capacity) {
  D <- .rt_dist(points)
  n <- nrow(D)
  un <- seq_len(n - 1)
  routes <- list()
  ins <- function(r, pos, u) append(r, u, after = pos)
  while (length(un)) {
    seed <- un[order(-D[1, un + 1], un)[1]]
    if (is.null(.rt_sched(seed, D, ready, due, service)) || demand[seed + 1] > capacity) {
      stop(sprintf("customer %d cannot be served on its own", seed))
    }
    route <- seed
    un <- setdiff(un, seed)
    repeat {
      best <- NULL
      load <- .rt_ss(demand[route + 1])
      for (u in un) {
        if (load + demand[u + 1] > capacity + 1e-12) next
        bu <- NULL
        for (pos in 0:length(route)) {
          i <- if (pos > 0) route[pos] else 0
          j <- if (pos < length(route)) route[pos + 1] else 0
          cand <- ins(route, pos, u)
          if (is.null(.rt_sched(cand, D, ready, due, service))) next
          c1 <- D[i + 1, u + 1] + D[u + 1, j + 1] - D[i + 1, j + 1]
          if (is.null(bu) || c1 < bu[1] - 1e-12) bu <- c(c1, pos)
        }
        if (!is.null(bu)) {
          c2 <- D[1, u + 1] - bu[1]
          if (is.null(best) || c2 > best[1] + 1e-12) best <- c(c2, u, bu[2])
        }
      }
      if (is.null(best)) break
      route <- ins(route, best[3], best[2])
      un <- setdiff(un, best[2])
    }
    routes[[length(routes) + 1]] <- route
  }
  len <- .rt_ss(vapply(routes, function(r) {
    L <- D[1, r[1] + 1] + D[r[length(r)] + 1, 1]
    if (length(r) > 1) for (k in 2:length(r)) L <- L + D[r[k - 1] + 1, r[k] + 1]
    L
  }, 0))
  list(routes = routes, length = len, schedules = lapply(routes, .rt_sched, D = D, e = ready, l = due, s = service))
}

#' @rdname ChinesePostman
#' @export
PickupDeliveryInsertion <- function(points, requests, capacity, max_ride = NULL) {
  D <- .rt_dist(points)
  R <- matrix(as.numeric(unlist(requests)), ncol = 3, byrow = !is.matrix(requests))
  if (is.matrix(requests)) R <- as.matrix(requests) + 0
  lo <- numeric(nrow(D))
  for (k in seq_len(nrow(R))) {
    lo[R[k, 1] + 1] <- R[k, 3]
    lo[R[k, 2] + 1] <- -R[k, 3]
  }
  feasible <- function(r) {
    onb <- 0
    for (v in r) {
      onb <- onb + lo[v + 1]
      if (onb > capacity + 1e-12) return(FALSE)
    }
    if (!is.null(max_ride)) {
      for (k in seq_len(nrow(R))) {
        pp <- match(R[k, 1], r)
        dd <- match(R[k, 2], r)
        if (!is.na(pp) && !is.na(dd)) {
          ride <- 0
          if (dd > pp) for (h in pp:(dd - 1)) ride <- ride + D[r[h] + 1, r[h + 1] + 1]
          if (ride > max_ride + 1e-12) return(FALSE)
        }
      }
    }
    TRUE
  }
  len <- function(r) {
    if (!length(r)) return(0)
    L <- D[1, r[1] + 1]
    if (length(r) > 1) for (k in 2:length(r)) L <- L + D[r[k - 1] + 1, r[k] + 1]
    L + D[r[length(r)] + 1, 1]
  }
  routes <- list()
  for (k in seq_len(nrow(R))) {
    p <- R[k, 1]
    d <- R[k, 2]
    best <- NULL
    for (ri in seq_along(routes)) {
      r <- routes[[ri]]
      base <- len(r)
      for (i in 0:length(r)) {
        for (j in (i + 1):(length(r) + 1)) {
          cand <- append(r, p, after = i)
          cand <- append(cand, d, after = j)
          if (!feasible(cand)) next
          add <- len(cand) - base
          if (is.null(best) || add < best$add - 1e-12) best <- list(add = add, ri = ri, cand = cand)
        }
      }
    }
    if (is.null(best)) {
      if (!feasible(c(p, d))) stop(sprintf("request (%d, %d) is infeasible on its own", p, d))
      routes[[length(routes) + 1]] <- c(p, d)
    } else {
      routes[[best$ri]] <- best$cand
    }
  }
  list(routes = routes, length = .rt_ss(vapply(routes, len, 0)))
}

#' @rdname ChinesePostman
#' @export
PeriodicVrp <- function(points, demand, frequency, horizon, capacity) {
  P <- as.matrix(points)
  n <- nrow(P)
  dayload <- numeric(horizon)
  days_of <- vector("list", n)
  ord <- order(-(demand[-1] * frequency[-1]), seq_len(n - 1)) + 1
  for (i in ord) {
    f <- frequency[i]
    if (f <= 0 || horizon %% f != 0) stop("frequencies must be positive divisors of the horizon")
    step <- horizon %/% f
    score <- vapply(0:(step - 1), function(o) max(dayload[seq(o, horizon - 1, by = step) + 1]), 0)
    o <- order(score, seq_along(score))[1] - 1
    days_of[[i]] <- seq(o, horizon - 1, by = step)
    dayload[days_of[[i]] + 1] <- dayload[days_of[[i]] + 1] + demand[i]
  }
  routes <- list()
  total <- 0
  for (d in 0:(horizon - 1)) {
    idx <- which(vapply(seq_len(n), function(i) i > 1 && d %in% days_of[[i]], TRUE))
    if (!length(idx)) {
      routes[[d + 1]] <- list()
      next
    }
    r <- VehicleRoutingSavings(c(0, demand[idx]), capacity, rbind(P[1, ], P[idx, , drop = FALSE]))
    routes[[d + 1]] <- lapply(r$routes, function(rt) idx[rt - 1] - 1L)
    total <- total + r$length
  }
  list(routes = routes, days = days_of[-1], length = total, daily_load = dayload)
}

#' @rdname ChinesePostman
#' @export
StochasticRouteCost <- function(route, points, demand_pmfs, capacity) {
  D <- .rt_dist(points)
  Q <- capacity
  r1 <- route + 1
  base <- D[1, r1[1]] + D[r1[length(r1)], 1]
  if (length(r1) > 1) for (k in 2:length(r1)) base <- base + D[r1[k - 1], r1[k]]
  S <- c("0" = 1)
  extra <- 0
  fails <- numeric(length(route))
  for (i in seq_along(route)) {
    pmf <- demand_pmfs[[i]]
    xv <- as.numeric(names(pmf))
    new <- numeric(0)
    pf <- 0
    for (a in seq_along(S)) {
      s <- as.numeric(names(S)[a])
      m <- max(Q, ceiling(s / Q) * Q)
      for (b in seq_along(pmf)) {
        key <- as.character(s + xv[b])
        new[key] <- (if (is.na(new[key])) 0 else new[key]) + S[a] * pmf[b]
        if (m < s + xv[b]) pf <- pf + S[a] * pmf[b]
      }
    }
    fails[i] <- unname(pf)
    extra <- extra + pf * 2 * D[1, r1[i]]
    S <- new
  }
  list(expected_length = unname(base + extra), route_length = base, failure_probability = fails)
}
