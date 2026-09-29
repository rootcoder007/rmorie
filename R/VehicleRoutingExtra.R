.vr_len <- function(D, r, depot) {
  if (!length(r)) return(0)
  D[depot + 1, r[1] + 1] + sum(D[cbind(r[-length(r)] + 1, r[-1] + 1)]) + D[r[length(r)] + 1, depot + 1]
}

#' Vehicle routing extensions and trip generation
#'
#' \code{SavingsRoutes}: Clarke-Wright savings for a customer subset.
#' \code{VrptwInsertion}: Solomon I1 insertion with time windows.
#' \code{MultiDepotVrp}: nearest-depot assignment then savings.
#' \code{PickupDeliveryRoutes}: paired cheapest insertion.
#' \code{PeriodicVrp}: pattern assignment then savings per day.
#' \code{TripGeneration}: cross-classification. Locations are zero-based as
#' in the Python arm \code{morie.fn.vrpext}.
#'
#' @param D Distance (travel time) matrix.
#' @param customers Customer indices (zero-based).
#' @param demand Demands by location.
#' @param capacity Vehicle capacity.
#' @param depot Depot index (zero-based).
#' @param ready,due,service Time windows and service times by location.
#' @param mu,lam,alpha1 Solomon I1 parameters.
#' @param depots Depot indices.
#' @param pairs List of c(pickup, delivery) pairs.
#' @param load Load of each request.
#' @param frequency Visit frequency by location.
#' @param patterns Named list: for each frequency, a list of day vectors.
#' @param households Zones by classes matrix.
#' @param rates Trip rate per class.
#' @return A list, or a vector for \code{TripGeneration}.
#' @references Clarke, G. and Wright, J. W. (1964). Operations Research 12,
#'   568-581.
#'
#'   Solomon, M. M. (1987). Algorithms for the vehicle routing and scheduling
#'   problems with time window constraints. Operations Research 35, 254-265.
#'
#'   Savelsbergh, M. W. P. and Sol, M. (1995). The general pickup and
#'   delivery problem. Transportation Science 29, 17-29.
#'
#'   Beltrami, E. J. and Bodin, L. D. (1974). Networks and vehicle routing for
#'   municipal waste collection. Networks 4, 65-94.
#' @examples
#' D <- rbind(c(0, 2, 2, 3), c(2, 0, 1, 4), c(2, 1, 0, 3), c(3, 4, 3, 0))
#' SavingsRoutes(D, 1:3, c(0, 1, 1, 1), 2)$routes
#' TripGeneration(rbind(c(10, 5), c(0, 20)), c(2, 4.5))
#' @export
SavingsRoutes <- function(D, customers, demand, capacity, depot = 0) {
  cs <- customers
  routes <- lapply(cs, function(c) c)
  names(routes) <- as.character(cs)
  where <- stats::setNames(cs, as.character(cs))
  load <- stats::setNames(demand[cs + 1], as.character(cs))
  sv <- NULL
  for (a in seq_along(cs)) {
    for (b in seq_along(cs)) {
      if (b <= a) next
      i <- cs[a]
      j <- cs[b]
      sv <- rbind(sv, c(D[depot + 1, i + 1] + D[depot + 1, j + 1] - D[i + 1, j + 1], i, j))
    }
  }
  if (!is.null(sv)) sv <- sv[order(-sv[, 1], sv[, 2], sv[, 3]), , drop = FALSE]
  for (k in seq_len(NROW(sv))) {
    s <- sv[k, 1]
    i <- sv[k, 2]
    j <- sv[k, 3]
    if (s <= 0) next
    ri <- as.character(where[as.character(i)])
    rj <- as.character(where[as.character(j)])
    if (ri == rj || load[[ri]] + load[[rj]] > capacity) next
    a <- routes[[ri]]
    b <- routes[[rj]]
    new <- if (a[length(a)] == i && b[1] == j) c(a, b) else if (a[1] == i && b[length(b)] == j) c(b, a) else
      if (a[1] == i && b[1] == j) c(rev(a), b) else if (a[length(a)] == i && b[length(b)] == j) c(a, rev(b)) else NULL
    if (is.null(new)) next
    routes[[ri]] <- new
    load[[ri]] <- load[[ri]] + load[[rj]]
    routes[[rj]] <- NULL
    load <- load[names(load) != rj]
    where[as.character(new)] <- as.numeric(ri)
  }
  rl <- unname(routes[order(vapply(routes, min, 0))])
  list(routes = rl, length = sum(vapply(rl, function(r) .vr_len(D, r, depot), 0)))
}

.vr_schedule <- function(D, r, ready, due, service, depot) {
  t <- 0
  prev <- depot
  starts <- numeric(0)
  for (c in r) {
    t <- max(t + D[prev + 1, c + 1], ready[c + 1])
    if (t > due[c + 1] + 1e-12) return(NULL)
    starts <- c(starts, t)
    t <- t + service[c + 1]
    prev <- c
  }
  if (t + D[prev + 1, depot + 1] > due[depot + 1] + 1e-12) return(NULL)
  starts
}

.vr_insert <- function(r, pos, u) if (pos == 0) c(u, r) else if (pos == length(r)) c(r, u) else c(r[1:pos], u, r[(pos + 1):length(r)])

#' @rdname SavingsRoutes
#' @export
VrptwInsertion <- function(D, demand, capacity, ready, due, service, mu = 1, lam = 1, alpha1 = 1, depot = 0) {
  n <- nrow(D)
  un <- setdiff(seq_len(n) - 1, depot)
  routes <- list()
  while (length(un)) {
    seed <- un[order(-D[depot + 1, un + 1], un)[1]]
    r <- seed
    un <- setdiff(un, seed)
    if (is.null(.vr_schedule(D, r, ready, due, service, depot))) {
      routes[[length(routes) + 1]] <- r
      next
    }
    repeat {
      best <- NULL
      load <- sum(demand[r + 1])
      base <- .vr_schedule(D, r, ready, due, service, depot)
      for (u in un) {
        if (load + demand[u + 1] > capacity) next
        bu <- NULL
        for (pos in 0:length(r)) {
          cand <- .vr_insert(r, pos, u)
          st <- .vr_schedule(D, cand, ready, due, service, depot)
          if (is.null(st)) next
          i <- if (pos == 0) depot else r[pos]
          j <- if (pos == length(r)) depot else r[pos + 1]
          c11 <- D[i + 1, u + 1] + D[u + 1, j + 1] - mu * D[i + 1, j + 1]
          c12 <- if (j == depot) 0 else st[pos + 2] - base[pos + 1]
          c1 <- alpha1 * c11 + (1 - alpha1) * c12
          if (is.null(bu) || c1 < bu[1] - 1e-12) bu <- c(c1, pos)
        }
        if (is.null(bu)) next
        c2 <- lam * D[depot + 1, u + 1] - bu[1]
        if (is.null(best) || c2 > best[1] + 1e-12) best <- c(c2, u, bu[2])
      }
      if (is.null(best)) break
      r <- .vr_insert(r, best[3], best[2])
      un <- setdiff(un, best[2])
    }
    routes[[length(routes) + 1]] <- r
  }
  list(routes = routes, length = sum(vapply(routes, function(r) .vr_len(D, r, depot), 0)))
}

#' @rdname SavingsRoutes
#' @export
MultiDepotVrp <- function(D, depots, demand, capacity) {
  cust <- setdiff(seq_len(nrow(D)) - 1, depots)
  near <- vapply(cust, function(c) depots[order(D[depots + 1, c + 1], depots)[1]], 0)
  routes <- list()
  assign <- list()
  total <- 0
  for (d in depots) {
    cs <- cust[near == d]
    assign[[as.character(d)]] <- cs
    if (length(cs)) {
      r <- SavingsRoutes(D, cs, demand, capacity, depot = d)
      routes[[as.character(d)]] <- r$routes
      total <- total + r$length
    } else {
      routes[[as.character(d)]] <- list()
    }
  }
  list(routes = routes, assignment = assign, length = total)
}

#' @rdname SavingsRoutes
#' @export
PickupDeliveryRoutes <- function(D, pairs, load, capacity, depot = 0) {
  key <- vapply(seq_along(pairs), function(k) {
    p <- pairs[[k]][1]
    d <- pairs[[k]][2]
    D[depot + 1, p + 1] + D[p + 1, d + 1] + D[d + 1, depot + 1]
  }, 0)
  ord <- order(-key, seq_along(pairs))
  qq <- numeric(nrow(D))
  for (k in seq_along(pairs)) {
    qq[pairs[[k]][1] + 1] <- load[k]
    qq[pairs[[k]][2] + 1] <- -load[k]
  }
  ok <- function(r) all(cumsum(qq[r + 1]) <= capacity + 1e-12)
  routes <- list()
  for (k in ord) {
    p <- pairs[[k]][1]
    d <- pairs[[k]][2]
    best <- NULL
    for (ri in seq_along(routes)) {
      r <- routes[[ri]]
      base <- .vr_len(D, r, depot)
      for (a in 0:length(r)) {
        for (b in a:length(r)) {
          cand <- c(r[seq_len(a)], p, r[seq_len(b - a) + a], d, r[seq_len(length(r) - b) + b])
          if (!ok(cand)) next
          inc <- .vr_len(D, cand, depot) - base
          if (is.null(best) || inc < best$inc - 1e-12) best <- list(inc = inc, ri = ri, cand = cand)
        }
      }
    }
    if (is.null(best) || load[k] > capacity) routes[[length(routes) + 1]] <- c(p, d) else routes[[best$ri]] <- best$cand
  }
  list(routes = routes, length = sum(vapply(routes, function(r) .vr_len(D, r, depot), 0)))
}

#' @rdname SavingsRoutes
#' @export
PeriodicVrp <- function(D, demand, capacity, frequency, patterns, depot = 0) {
  cust <- setdiff(seq_len(nrow(D)) - 1, depot)
  cust <- cust[order(-demand[cust + 1], cust)]
  days <- sort(unique(unlist(patterns)))
  loadd <- stats::setNames(numeric(length(days)), days)
  visits <- stats::setNames(vector("list", length(days)), days)
  for (c in cust) {
    best <- NULL
    for (pat in patterns[[as.character(frequency[c + 1])]]) {
      peak <- max(loadd + ifelse(days %in% pat, demand[c + 1], 0))
      if (is.null(best) || peak < best$peak - 1e-12) best <- list(peak = peak, pat = pat)
    }
    for (d in best$pat) {
      loadd[as.character(d)] <- loadd[as.character(d)] + demand[c + 1]
      visits[[as.character(d)]] <- c(visits[[as.character(d)]], c)
    }
  }
  out <- list()
  total <- 0
  for (d in as.character(days)) {
    r <- SavingsRoutes(D, sort(visits[[d]]), demand, capacity, depot)
    out[[d]] <- r$routes
    total <- total + r$length
  }
  list(days = lapply(visits, sort), routes = out, length = total)
}

#' @rdname SavingsRoutes
#' @export
TripGeneration <- function(households, rates) as.vector(as.matrix(households) %*% rates)
