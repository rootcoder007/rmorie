#' Movement trajectory analysis
#'
#' \code{TrajectoryMetrics}: steps, headings, turning angles (wrapped to
#' \eqn{(-\pi, \pi]}), straightness (Batschelet 1981) and Benhamou (2004)
#' sinuosity. \code{MeanSquaredDisplacement}: time-averaged MSD by lag.
#' \code{FirstPassageTime}: Fauchald and Tveraa (2003) FPT with linear
#' interpolation of the circle crossings. \code{SimulateWalk}: correlated
#' (wrapped Cauchy), biased and Levy walks from Philox uniforms (streams 0
#' and 1). \code{OdMatrix}: origin-destination counts. Identical to the
#' Python arm \code{morie.fn.movetraj}.
#'
#' @param track Two-column matrix of positions.
#' @param max_lag Maximum lag.
#' @param times Fix times.
#' @param radius Circle radius.
#' @param n_steps Number of steps.
#' @param kind \code{"correlated"}, \code{"biased"} or \code{"levy"}.
#' @param step Step length (Levy minimum step).
#' @param rho Wrapped Cauchy concentration.
#' @param mu Levy tail exponent.
#' @param target Target of the biased walk.
#' @param bias Weight of the target heading.
#' @param start Starting position.
#' @param seed Philox seed.
#' @param origins,destinations Zone labels of each trip.
#' @param zones Zone order.
#' @param weights Trip weights.
#' @return List or vector.
#' @references Benhamou, S. (2004). How to reliably estimate the tortuosity
#'   of an animal's path: straightness, sinuosity, or fractal dimension?
#'   Journal of Theoretical Biology 229, 209-220.
#'
#'   Fauchald, P. and Tveraa, T. (2003). Using first-passage time in the
#'   analysis of area-restricted search and habitat selection. Ecology 84,
#'   282-288.
#'
#'   Codling, E. A., Plank, M. J. and Benhamou, S. (2008). Random walk models
#'   in biology. Journal of the Royal Society Interface 5, 813-834.
#' @examples
#' TrajectoryMetrics(rbind(c(0, 0), c(1, 0), c(1, 1), c(2, 1)))$straightness
#' MeanSquaredDisplacement(cbind(0:3, 0))
#' @export
TrajectoryMetrics <- function(track) {
  P <- as.matrix(track)
  if (nrow(P) < 2) stop("a trajectory needs at least two points")
  d <- diff(P)
  steps <- sqrt(rowSums(d^2))
  head <- atan2(d[, 2], d[, 1])
  turn <- diff(head)
  turn <- ifelse(turn <= -pi, turn + 2 * pi, ifelse(turn > pi, turn - 2 * pi, turn))
  L <- sum(steps)
  net <- sqrt(sum((P[nrow(P), ] - P[1, ])^2))
  p <- mean(steps)
  b <- sqrt(mean((steps - p)^2)) / p
  cc <- if (length(turn)) mean(cos(turn)) else NaN
  sinu <- if (length(turn) && cc < 1) 2 / sqrt(p * ((1 + cc) / (1 - cc) + b^2)) else NaN
  list(steps = steps, headings = head, turning = turn, path_length = L, net_displacement = net,
       straightness = if (L > 0) net / L else NaN, sinuosity = sinu)
}

#' @rdname TrajectoryMetrics
#' @export
MeanSquaredDisplacement <- function(track, max_lag = NULL) {
  P <- as.matrix(track)
  n <- nrow(P)
  K <- if (is.null(max_lag)) n - 1 else min(max_lag, n - 1)
  vapply(seq_len(K), function(k) mean(rowSums((P[(1 + k):n, , drop = FALSE] - P[1:(n - k), , drop = FALSE])^2)), 0)
}

#' @rdname TrajectoryMetrics
#' @export
FirstPassageTime <- function(track, times, radius) {
  P <- as.matrix(track)
  n <- nrow(P)
  vapply(seq_len(n), function(i) {
    dd <- sqrt(colSums((t(P) - P[i, ])^2))
    ex <- function(idx) {
      prev <- i
      for (j in idx) {
        if (dd[j] > radius) return(times[prev] + (radius - dd[prev]) / (dd[j] - dd[prev]) * (times[j] - times[prev]))
        prev <- j
      }
      NA_real_
    }
    fw <- if (i < n) ex((i + 1):n) else NA_real_
    bw <- if (i > 1) ex((i - 1):1) else NA_real_
    if (is.na(fw) || is.na(bw)) NaN else fw - bw
  }, 0)
}

#' @rdname TrajectoryMetrics
#' @export
SimulateWalk <- function(n_steps, kind = "correlated", step = 1, rho = 0.8, mu = 2, target = c(0, 0), bias = 0.5,
                         start = c(0, 0), seed = 1L) {
  if (!kind %in% c("correlated", "biased", "levy")) stop("kind must be correlated, biased or levy")
  U1 <- .morie_random_uniform(n_steps, seed = seed, stream = 0)
  U2 <- .morie_random_uniform(n_steps, seed = seed, stream = 1)
  x <- start[1]
  y <- start[2]
  track <- matrix(c(x, y), 1)
  h <- if (kind != "levy") 2 * pi * U2[1] else 0
  steps <- numeric(n_steps)
  for (k in seq_len(n_steps)) {
    if (kind == "levy") {
      h <- 2 * pi * U2[k]
      s <- step * U1[k]^(-1 / (mu - 1))
    } else {
      turn <- 2 * atan((1 - rho) / (1 + rho) * tan(pi * (U1[k] - 0.5)))
      hc <- if (k > 1) h + turn else h
      if (kind == "biased") {
        ht <- atan2(target[2] - y, target[1] - x)
        hc <- atan2((1 - bias) * sin(hc) + bias * sin(ht), (1 - bias) * cos(hc) + bias * cos(ht))
      }
      h <- hc
      s <- step
    }
    x <- x + s * cos(h)
    y <- y + s * sin(h)
    track <- rbind(track, c(x, y))
    steps[k] <- s
  }
  list(track = track, steps = steps)
}

#' @rdname TrajectoryMetrics
#' @export
OdMatrix <- function(origins, destinations, zones = NULL, weights = NULL) {
  if (length(origins) != length(destinations)) stop("origins and destinations must have equal length")
  Z <- if (is.null(zones)) sort(unique(c(as.character(origins), as.character(destinations)))) else as.character(zones)
  w <- if (is.null(weights)) rep(1, length(origins)) else weights
  M <- matrix(0, length(Z), length(Z))
  for (k in seq_along(origins)) {
    i <- match(as.character(origins[k]), Z)
    j <- match(as.character(destinations[k]), Z)
    M[i, j] <- M[i, j] + w[k]
  }
  list(zones = Z, matrix = M, production = rowSums(M), attraction = colSums(M))
}
