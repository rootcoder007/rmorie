.me_plogp <- function(p) ifelse(p > 0, p * log2(pmax(p, 1e-300)), 0)

.me_flow <- function(A, flow) {
  n <- nrow(A)
  s <- rowSums(A)
  if (flow == "undirected") return(list(p = s / sum(s), F = A / sum(s), tele = NULL))
  if (flow != "igraph") stop("flow must be 'undirected' or 'igraph'")
  alpha <- 0.15
  beta <- 1 - alpha
  P <- A / ifelse(s > 0, s, 1)
  dang <- s <= 0
  tmp <- rep(1 / n, n)
  sq <- 1
  it <- 0
  repeat {
    dsz <- sum(tmp[dang])
    size <- rep((alpha + beta * dsz) / n, n) + beta * as.vector(crossprod(P, tmp))
    size <- size / sum(size)
    old <- sq
    sq <- sum(abs(size - tmp))
    tmp <- size
    it <- it + 1
    if (sq == old) {
      alpha <- alpha + 1e-10
      beta <- 1 - alpha
    }
    if (!(it < 200 && (sq > 1e-15 || it < 50))) break
  }
  list(p = tmp, F = beta * tmp * P, tele = list(alpha = alpha, beta = beta, d = ifelse(dang, tmp, 0)))
}

.me_codelength <- function(fl, member) {
  n <- length(fl$p)
  mods <- sort(unique(member))
  pin <- vapply(mods, function(m) sum(fl$p[member == m]), 0)
  ex <- vapply(seq_along(mods), function(k) {
    m <- mods[k]
    inm <- member == m
    if (is.null(fl$tele)) return(sum(fl$F[inm, !inm]))
    pin[k] - (fl$tele$alpha * pin[k] + fl$tele$beta * sum(fl$tele$d[inm])) * sum(inm) / n - sum(fl$F[inm, inm])
  }, 0)
  q <- sum(ex)
  .me_plogp(q) - 2 * sum(.me_plogp(ex)) - sum(.me_plogp(fl$p)) + sum(.me_plogp(ex + pin))
}

#' Community detection by the map equation
#'
#' \code{MapEquation}: two-level description length of a partition of an
#' undirected (weighted) network. \code{InfomapPartition}: deterministic
#' local moving and aggregation minimising it. Identical to the Python arm
#' \code{morie.fn.mapeq}; modules are zero-based.
#'
#' @param A Adjacency (weight) matrix; it is symmetrised.
#' @param membership Module label of each node.
#' @param flow \code{"undirected"} (stationary random walk) or \code{"igraph"}
#'   (PageRank flow with teleportation 0.15, as \code{igraph::cluster_infomap}).
#' @param max_passes Maximum number of aggregation levels.
#' @return A number or a list.
#' @references Rosvall, M. and Bergstrom, C. T. (2008). Maps of random walks
#'   on complex networks reveal community structure. PNAS 105, 1118-1123.
#'
#'   Rosvall, M., Axelsson, D. and Bergstrom, C. T. (2009). The map equation.
#'   European Physical Journal Special Topics 178, 13-23.
#' @examples
#' A <- matrix(0, 6, 6)
#' A[rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(4, 6), c(5, 6))] <- 1
#' A <- A + t(A)
#' MapEquation(A, c(0, 0, 0, 1, 1, 1))
#' InfomapPartition(A)$membership
#' @export
MapEquation <- function(A, membership, flow = "undirected") {
  .me_codelength(.me_flow((A + t(A)) / 2, flow), membership)
}

#' @rdname MapEquation
#' @export
InfomapPartition <- function(A, flow = "undirected", max_passes = 100) {
  A <- (A + t(A)) / 2
  n <- nrow(A)
  fl <- .me_flow(A, flow)
  member <- seq_len(n) - 1
  groups <- as.list(seq_len(n))
  L <- .me_codelength(fl, member)
  for (pass in seq_len(max_passes)) {
    moved_any <- FALSE
    improved <- TRUE
    while (improved) {
      improved <- FALSE
      for (g in groups) {
        cur <- member[g[1]]
        nbr <- which(colSums(A[g, , drop = FALSE] != 0) > 0)
        nb <- sort(unique(member[nbr][member[nbr] != cur]))
        best <- L
        best_m <- cur
        for (m in nb) {
          trial <- member
          trial[g] <- m
          lt <- .me_codelength(fl, trial)
          if (lt < best - 1e-12) {
            best <- lt
            best_m <- m
          }
        }
        if (best_m != cur) {
          member[g] <- best_m
          L <- best
          improved <- TRUE
          moved_any <- TRUE
        }
      }
    }
    labels <- sort(unique(member))
    new_groups <- lapply(labels, function(m) which(member == m))
    if (!moved_any || length(new_groups) == length(groups)) break
    groups <- new_groups
  }
  out <- match(member, unique(member)) - 1
  list(membership = out, codelength = L, n_modules = length(unique(out)))
}
