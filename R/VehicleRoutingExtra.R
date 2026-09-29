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
TripGeneration <- function(households, rates) as.vector(as.matrix(households) %*% rates)
