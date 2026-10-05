#' Partisan symmetry, swing, competitiveness and disproportionality
#'
#' \code{PartisanGerrymanderMeasures}: efficiency gap (Stephanopoulos and
#' McGhee 2015; winners waste votes above half the district total),
#' mean-median difference (McDonald and Best 2015) and uniform-swing partisan
#' bias. \code{ElectoralSwing}: Butler and Steed swing.
#' \code{DistrictCompetitiveness}: two-party margins and districts within
#' \code{threshold} of a tie. \code{Disproportionality}: Loosemore-Hanby,
#' Gallagher least squares and Laakso-Taagepera effective numbers of
#' parties. \code{Malapportionment}: Samuels and Snyder (2001) MAL.
#' Identical to the Python arm \code{morie.fn.electgeo}.
#'
#' @param votes_a,votes_b Two-party votes by district.
#' @param prev_a,prev_b,cur_a,cur_b Vote shares in two elections.
#' @param threshold Distance from 0.5 counted as competitive.
#' @param votes,seats Party votes and seats.
#' @param population Unit populations.
#' @return List.
#' @references Stephanopoulos, N. O. and McGhee, E. M. (2015). Partisan
#'   gerrymandering and the efficiency gap. University of Chicago Law Review
#'   82, 831-900.
#'
#'   Gallagher, M. (1991). Proportionality, disproportionality and electoral
#'   systems. Electoral Studies 10, 33-51.
#'
#'   Samuels, D. and Snyder, R. (2001). The value of a vote: malapportionment
#'   in comparative perspective. British Journal of Political Science 31,
#'   651-671.
#' @examples
#' PartisanGerrymanderMeasures(c(70, 70, 40, 40, 40), c(30, 30, 60, 60, 60))$efficiency_gap
#' Disproportionality(c(40, 35, 25), c(55, 40, 5))$gallagher
#' @export
PartisanGerrymanderMeasures <- function(votes_a, votes_b) {
  if (length(votes_a) != length(votes_b)) stop("votes_a and votes_b must have equal length")
  half <- (votes_a + votes_b) / 2
  win <- votes_a > votes_b
  wa <- sum(ifelse(win, votes_a - half, votes_a))
  wb <- sum(ifelse(win, votes_b, votes_b - half))
  sh <- votes_a / (votes_a + votes_b)
  m <- mean(sh)
  list(efficiency_gap = (wa - wb) / sum(votes_a + votes_b), wasted_a = wa, wasted_b = wb,
       mean_median = m - stats::median(sh), partisan_bias = mean(sh + 0.5 - m > 0.5) - 0.5, shares = sh)
}

#' @rdname PartisanGerrymanderMeasures
#' @export
ElectoralSwing <- function(prev_a, prev_b, cur_a, cur_b) {
  list(butler = ((cur_a - prev_a) - (cur_b - prev_b)) / 2,
       steed = cur_a / (cur_a + cur_b) - prev_a / (prev_a + prev_b))
}

#' @rdname PartisanGerrymanderMeasures
#' @export
DistrictCompetitiveness <- function(votes_a, votes_b, threshold = 0.05) {
  sh <- votes_a / (votes_a + votes_b)
  mg <- abs(2 * sh - 1)
  comp <- abs(sh - 0.5) < threshold
  list(margin = mg, competitive = comp, n_competitive = sum(comp), mean_margin = mean(mg))
}

#' @rdname PartisanGerrymanderMeasures
#' @export
Disproportionality <- function(votes, seats) {
  .morie_arg(votes, "n")
  pv <- 100 * votes / sum(votes)
  ps <- 100 * seats / sum(seats)
  list(loosemore_hanby = sum(abs(pv - ps)) / 2, gallagher = sqrt(sum((pv - ps)^2) / 2),
       enp_votes = 1 / sum((pv / 100)^2), enp_seats = 1 / sum((ps / 100)^2))
}

#' @rdname PartisanGerrymanderMeasures
#' @export
Malapportionment <- function(seats, population) {
  ss <- seats / sum(seats)
  pp <- population / sum(population)
  list(mal = sum(abs(ss - pp)) / 2, ratio = ss / pp)
}

.cmp_area <- function(P) {
  n <- nrow(P)
  j <- c(2:n, 1)
  abs(sum(P[, 1] * P[j, 2] - P[j, 1] * P[, 2])) / 2
}

.cmp_circ3 <- function(a, b, c) {
  d <- 2 * (a[1] * (b[2] - c[2]) + b[1] * (c[2] - a[2]) + c[1] * (a[2] - b[2]))
  if (d == 0) {
    pr <- list(list(a, b), list(a, c), list(b, c))
    k <- which.max(vapply(pr, function(t) sqrt(sum((t[[1]] - t[[2]])^2)), 0))
    return(list(c = (pr[[k]][[1]] + pr[[k]][[2]]) / 2, r = sqrt(sum((pr[[k]][[1]] - pr[[k]][[2]])^2)) / 2))
  }
  sa <- sum(a^2)
  sb <- sum(b^2)
  sc <- sum(c^2)
  u <- c((sa * (b[2] - c[2]) + sb * (c[2] - a[2]) + sc * (a[2] - b[2])) / d,
         (sa * (c[1] - b[1]) + sb * (a[1] - c[1]) + sc * (b[1] - a[1])) / d)
  list(c = u, r = sqrt(sum((u - a)^2)))
}

.cmp_min_circle <- function(P) {
  inside <- function(cc, p) sqrt(sum((cc$c - p)^2)) <= cc$r * (1 + 1e-12) + 1e-12
  cc <- list(c = P[1, ], r = 0)
  if (nrow(P) < 2) return(cc)
  for (i in 2:nrow(P)) {
    if (inside(cc, P[i, ])) next
    cc <- list(c = P[i, ], r = 0)
    for (j in seq_len(i - 1)) {
      if (inside(cc, P[j, ])) next
      cc <- list(c = (P[i, ] + P[j, ]) / 2, r = sqrt(sum((P[i, ] - P[j, ])^2)) / 2)
      for (k in seq_len(j - 1)) {
        if (!inside(cc, P[k, ])) cc <- .cmp_circ3(P[i, ], P[j, ], P[k, ])
      }
    }
  }
  cc
}

.cmp_hull <- function(P) {
  P <- unique(P)
  P <- P[order(P[, 1], P[, 2]), , drop = FALSE]
  if (nrow(P) < 3) return(P)
  cross <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  chain <- function(ix) {
    h <- integer(0)
    for (i in ix) {
      while (length(h) >= 2 && cross(P[h[length(h) - 1], ], P[h[length(h)], ], P[i, ]) <= 0) h <- h[-length(h)]
      h <- c(h, i)
    }
    h
  }
  lo <- chain(seq_len(nrow(P)))
  hi <- chain(rev(seq_len(nrow(P))))
  P[c(lo[-length(lo)], hi[-length(hi)]), , drop = FALSE]
}

#' District compactness
#'
#' Polsby-Popper \code{4 pi A / P^2}, Schwartzberg \code{P / (2 sqrt(pi A))},
#' convex-hull ratio and Reock (area over the minimum enclosing circle, by
#' Welzl's incremental algorithm on the hull vertices) of a simple polygon.
#'
#' @param polygon Two-column vertex matrix (no repeated closing vertex).
#' @return List.
#' @references Polsby, D. D. and Popper, R. D. (1991). The third criterion:
#'   compactness as a procedural safeguard against partisan gerrymandering.
#'   Yale Law and Policy Review 9, 301-353.
#'
#'   Reock, E. C. (1961). A note: measuring compactness as a requirement of
#'   legislative apportionment. Midwest Journal of Political Science 5, 70-74.
#' @examples
#' DistrictCompactness(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))$reock
#' @export
DistrictCompactness <- function(polygon) {
  P <- matrix(as.numeric(as.matrix(polygon)), ncol = 2)
  A <- .cmp_area(P)
  j <- c(2:nrow(P), 1)
  per <- sum(sqrt(rowSums((P[j, , drop = FALSE] - P)^2)))
  H <- .cmp_hull(P)
  R <- .cmp_min_circle(H)$r
  list(polsby_popper = 4 * pi * A / per^2, schwartzberg = per / (2 * sqrt(pi * A)), convex_hull = A / .cmp_area(H),
       reock = A / (pi * R^2), area = A, perimeter = per)
}
