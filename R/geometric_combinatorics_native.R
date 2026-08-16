# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geometric combinatorics. Mirrors morie.fn.geocmb.
#
# Everything runs in exact integer arithmetic on lattice input --
# orientation tests are integer cross products, areas integer shoelace
# doubles, boundary counts gcds -- so a theorem check means the
# theorem, not the rounding. Coordinates here stay far below 2^26, so
# products fit doubles exactly and no bigint is needed; the tests
# enforce that assumption rather than trusting it.
#
# This is distinct from morie_convex_hull in reds_native.R, which is a
# floating hull over grDevices::chull for general numeric input; this
# one is the exact lattice version, and their names differ because
# their contracts do.
#
# Pick GA (1899); Erdos P, Szekeres G (1935) Compositio Math 2:463-470;
# Helly E (1923); Matousek J (2002) Lectures on Discrete Geometry.

#' .morie_cross3
#'
#' Part of the geometric_combinatorics_native implementation; see the
#' file header for the source it follows.
#'
#' @param o See Usage.
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.morie_cross3 <- function(o, a, b) {
  (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
}

#' Convex hull of lattice points by Andrew's monotone chain
#'
#' Orientation tests are exact integer cross products, so no hull
#' vertex is ever mis-classified by rounding -- the failure mode of
#' floating hulls on nearly-collinear input. Collinear boundary points
#' are dropped; the hull is the minimal vertex set, counter-clockwise
#' from the lexicographic minimum.
#'
#' @param points Two-column matrix of integer (x, y).
#' @return A list with `hull` (matrix), `n_vertices`, `twice_area`
#'   (exact integer), `area`.
#' @export
morie_lattice_convex_hull <- function(points) {
  p <- unique(round(as.matrix(points)))
  if (ncol(p) != 2L) stop("points must be an (n, 2) matrix.", call. = FALSE)
  if (nrow(p) < 3L) {
    stop(sprintf("need at least 3 distinct points; got %d.", nrow(p)),
      call. = FALSE
    )
  }
  ord <- order(p[, 1], p[, 2])
  p <- p[ord, , drop = FALSE]
  half <- function(rows) {
    h <- list()
    for (i in rows) {
      pt <- p[i, ]
      while (length(h) >= 2L &&
        .morie_cross3(h[[length(h) - 1L]], h[[length(h)]], pt) <= 0) {
        h[[length(h)]] <- NULL
      }
      h[[length(h) + 1L]] <- pt
    }
    h
  }
  lower <- half(seq_len(nrow(p)))
  upper <- half(rev(seq_len(nrow(p))))
  hull <- c(lower[-length(lower)], upper[-length(upper)])
  if (length(hull) < 3L) {
    stop("all points are collinear; the hull is a segment and has no interior.",
      call. = FALSE
    )
  }
  hm <- do.call(rbind, hull)
  nh <- nrow(hm)
  nxt <- c(2:nh, 1L)
  twice <- sum(hm[, 1] * hm[nxt, 2] - hm[nxt, 1] * hm[, 2])
  list(
    hull = hm, n_vertices = nh, twice_area = twice, area = twice / 2,
    estimate = twice / 2, n = nrow(p),
    method = "Andrew's monotone chain, exact integer orientation"
  )
}

#' Pick's theorem, checked by counting rather than assumed
#'
#' \eqn{A = I + B/2 - 1} for a simple lattice polygon. The three
#' quantities are computed independently -- area by integer shoelace,
#' boundary by \eqn{\sum \gcd(|\Delta x|, |\Delta y|)}, interior by
#' testing every lattice point of the bounding box with exact integer
#' ray casting -- and the theorem is then CHECKED. A check that derives
#' one side from the other two would be a tautology.
#'
#' Enumeration over a huge bounding box is refused rather than silently
#' skipped: `verified` comes back `NA` with a warning, so a cap never
#' masquerades as a pass. A self-intersecting polygon shows up as a
#' disagreement between the formula and the count, which is the
#' detection, not a defect.
#'
#' @param vertices Two-column matrix of integer (x, y) in traversal
#'   order.
#' @param verify_by_enumeration Count interior points directly.
#' @param enumeration_cap Largest bounding box enumerated.
#' @return A list with `area`, `twice_area`, `boundary`, `interior`,
#'   `interior_enumerated`, `verified`, `pick_holds`, `warnings`.
#' @references Pick G (1899) \emph{Lotos} 19:311-319.
#' @export
morie_pick_theorem <- function(vertices, verify_by_enumeration = TRUE,
                               enumeration_cap = 1e6) {
  v <- round(as.matrix(vertices))
  if (ncol(v) != 2L) stop("vertices must be an (n, 2) matrix.", call. = FALSE)
  n <- nrow(v)
  if (n < 3L) {
    stop(sprintf("a polygon needs at least 3 vertices; got %d.", n),
      call. = FALSE
    )
  }
  nxt <- c(2:n, 1L)
  twice <- sum(v[, 1] * v[nxt, 2] - v[nxt, 1] * v[, 2])
  gcd2 <- function(a, b) {
    while (b != 0) {
      t <- b
      b <- a %% b
      a <- t
    }
    a
  }
  boundary <- sum(vapply(seq_len(n), function(i) {
    gcd2(abs(v[nxt[i], 1] - v[i, 1]), abs(v[nxt[i], 2] - v[i, 2]))
  }, numeric(1)))
  if (twice == 0) {
    stop("the polygon is degenerate: its signed area is 0.", call. = FALSE)
  }
  area2 <- abs(twice)
  interior_pick <- (area2 - boundary + 2) %/% 2
  parity_ok <- (area2 - boundary) %% 2 == 0

  strictly_inside <- function(px, py) {
    cnt <- 0L
    for (i in seq_len(n)) {
      ax <- v[i, 1]
      ay <- v[i, 2]
      bx <- v[nxt[i], 1]
      by <- v[nxt[i], 2]
      cr <- (bx - ax) * (py - ay) - (by - ay) * (px - ax)
      if (cr == 0 &&
        px >= min(ax, bx) && px <= max(ax, bx) &&
        py >= min(ay, by) && py <= max(ay, by)) {
        return(FALSE) # on the boundary
      }
      if ((ay > py) != (by > py)) {
        lhs <- (px - ax) * (by - ay)
        rhs <- (py - ay) * (bx - ax)
        if (by > ay) {
          if (lhs < rhs) cnt <- cnt + 1L
        } else {
          if (lhs > rhs) cnt <- cnt + 1L
        }
      }
    }
    cnt %% 2L == 1L
  }

  interior_direct <- NA_real_
  verified <- NA
  warns <- character(0)
  if (isTRUE(verify_by_enumeration)) {
    xr <- range(v[, 1])
    yr <- range(v[, 2])
    box <- (xr[2] - xr[1] + 1) * (yr[2] - yr[1] + 1)
    if (box > enumeration_cap) {
      warns <- c(warns, sprintf(paste(
        "The bounding box holds %g lattice points, above the enumeration",
        "cap of %g, so the theorem was NOT verified by direct counting",
        "here. The Pick value is still returned; 'verified' is NA, not",
        "TRUE."
      ), box, enumeration_cap))
    } else {
      cnt <- 0L
      for (px in xr[1]:xr[2]) {
        for (py in yr[1]:yr[2]) {
          if (strictly_inside(px, py)) cnt <- cnt + 1L
        }
      }
      interior_direct <- cnt
      verified <- cnt == interior_pick
    }
  }
  if (isFALSE(verified)) {
    warns <- c(warns, paste(
      "Direct counting disagrees with Pick's formula. For a SIMPLE",
      "polygon that is impossible, so the input is self-intersecting or",
      "traversed with repeated vertices."
    ))
  }
  list(
    area = area2 / 2, twice_area = area2, boundary = boundary,
    interior = interior_pick, interior_enumerated = interior_direct,
    verified = verified,
    pick_holds = if (!is.na(verified)) verified else parity_ok,
    estimate = area2 / 2, n = n, warnings = warns,
    method = "Pick's theorem (Pick 1899), checked by enumeration"
  )
}

#' The Erdos-Szekeres monotone subsequence theorem
#'
#' Any sequence of \eqn{(r-1)(s-1) + 1} distinct reals contains an
#' increasing subsequence of length \eqn{r} or a decreasing one of
#' length \eqn{s}. The bound is tight: \eqn{s-1} descending blocks of
#' \eqn{r-1} ascending values escape at length \eqn{(r-1)(s-1)}, and
#' the tests build that sequence.
#'
#' @param sequence Distinct numeric values.
#' @param r,s Target lengths, both at least 2; omit both to just get
#'   the longest monotone lengths.
#' @return A list with `longest_increasing`, `longest_decreasing`,
#'   `threshold`, `applies`, `guarantee_met`.
#' @references Erdos P, Szekeres G (1935) \emph{Compositio Math}
#'   2:463-470.
#' @export
morie_erdos_szekeres_check <- function(sequence, r = NULL, s = NULL) {
  w <- as.numeric(sequence)
  n <- length(w)
  if (n == 0L) stop("the sequence is empty.", call. = FALSE)
  if (anyDuplicated(w)) {
    stop("the theorem needs distinct values; ties were supplied.",
      call. = FALSE
    )
  }
  inc <- rep(1L, n)
  dec <- rep(1L, n)
  for (i in seq_len(n)) {
    if (i > 1L) {
      for (j in seq_len(i - 1L)) {
        if (w[j] < w[i]) {
          inc[i] <- max(inc[i], inc[j] + 1L)
        } else {
          dec[i] <- max(dec[i], dec[j] + 1L)
        }
      }
    }
  }
  li <- max(inc)
  ld <- max(dec)
  threshold <- NULL
  applies <- NULL
  met <- NULL
  if (!is.null(r) && !is.null(s)) {
    r <- as.integer(r)
    s <- as.integer(s)
    if (is.na(r) || is.na(s) || r < 2L || s < 2L) {
      stop("r and s must be at least 2.", call. = FALSE)
    }
    threshold <- (r - 1L) * (s - 1L) + 1L
    applies <- n >= threshold
    met <- if (applies) li >= r || ld >= s else NULL
  }
  list(
    longest_increasing = li, longest_decreasing = ld,
    threshold = threshold, applies = applies, guarantee_met = met,
    estimate = as.numeric(li), n = n,
    method = "Erdos-Szekeres theorem (1935)"
  )
}

#' The happy ending problem
#'
#' Any 5 points in general position contain 4 in convex position.
#' General position (no 3 collinear) is tested with exact integer cross
#' products, and the convex quadrilateral is FOUND -- its hull has 4
#' vertices -- rather than merely asserted. With more than 5 points
#' every 5-subset is checked, which is the theorem's statement, not a
#' sample of it.
#'
#' @param points Two-column matrix of integer (x, y), distinct, no 3
#'   collinear.
#' @return A list with `found`, `witness` (matrix), `every_five_subset`.
#' @references Erdos P, Szekeres G (1935). A combinatorial problem in
#'   geometry. \emph{Compositio Mathematica}, 2, 463-470.
#' @export
morie_happy_ending_quadrilateral <- function(points) {
  p <- round(as.matrix(points))
  if (ncol(p) != 2L) stop("points must be an (n, 2) matrix.", call. = FALSE)
  if (nrow(p) < 5L) {
    stop(sprintf("the theorem is about 5 or more points; got %d.", nrow(p)),
      call. = FALSE
    )
  }
  if (anyDuplicated(p)) stop("points must be distinct.", call. = FALSE)
  np <- nrow(p)
  trip <- utils::combn(np, 3L)
  for (t in seq_len(ncol(trip))) {
    a <- p[trip[1, t], ]
    b <- p[trip[2, t], ]
    cc <- p[trip[3, t], ]
    if (.morie_cross3(a, b, cc) == 0) {
      stop(sprintf(
        "points (%d, %d), (%d, %d), (%d, %d) are collinear; the theorem assumes general position.",
        a[1], a[2], b[1], b[2], cc[1], cc[2]
      ), call. = FALSE)
    }
  }
  hull4 <- function(idx) {
    q <- p[idx, , drop = FALSE]
    ord <- order(q[, 1], q[, 2])
    q <- q[ord, , drop = FALSE]
    half <- function(rows) {
      h <- list()
      for (i in rows) {
        pt <- q[i, ]
        while (length(h) >= 2L &&
          .morie_cross3(h[[length(h) - 1L]], h[[length(h)]], pt) <= 0) {
          h[[length(h)]] <- NULL
        }
        h[[length(h) + 1L]] <- pt
      }
      h
    }
    lower <- half(seq_len(nrow(q)))
    upper <- half(rev(seq_len(nrow(q))))
    hull <- c(lower[-length(lower)], upper[-length(upper)])
    if (length(hull) == 4L) do.call(rbind, hull) else NULL
  }
  witness <- NULL
  all_five_ok <- TRUE
  fives <- utils::combn(np, 5L)
  for (f in seq_len(ncol(fives))) {
    found_here <- FALSE
    fours <- utils::combn(fives[, f], 4L)
    for (g in seq_len(ncol(fours))) {
      h <- hull4(fours[, g])
      if (!is.null(h)) {
        found_here <- TRUE
        if (is.null(witness)) witness <- h
        break
      }
    }
    if (!found_here) {
      all_five_ok <- FALSE
      break
    }
  }
  list(
    found = !is.null(witness), witness = witness,
    every_five_subset = all_five_ok,
    estimate = as.numeric(!is.null(witness)), n = np,
    method = "Happy ending theorem (Erdos and Szekeres 1935)"
  )
}

#' Helly's theorem on the line
#'
#' In one dimension the Helly number is 2: intervals meeting pairwise
#' have a common point, \eqn{\max_i a_i \le \min_i b_i}, and the
#' witness interval is returned. Both facts are computed independently
#' -- every pair tested, then the global intersection formed -- and the
#' payload reports whether the implication held. When some pair is
#' disjoint the theorem is silent and the pair is named (0-based, to
#' match the Python mirror).
#'
#' @param intervals Two-column matrix of (a, b) with a <= b.
#' @return A list with `pairwise_intersecting`, `disjoint_pair`,
#'   `common_point_exists`, `witness`, `helly_holds`.
#' @references Helly E (1923) \emph{Jahresbericht DMV} 32:175-176.
#' @export
morie_helly_intervals <- function(intervals) {
  iv <- as.matrix(intervals)
  if (nrow(iv) < 1L) stop("no intervals supplied.", call. = FALSE)
  if (ncol(iv) != 2L) {
    stop("intervals must be an (n, 2) matrix.",
      call. = FALSE
    )
  }
  if (any(iv[, 1] > iv[, 2])) {
    stop("every interval must have a <= b.", call. = FALSE)
  }
  disjoint <- NULL
  if (nrow(iv) >= 2L) {
    pr <- utils::combn(nrow(iv), 2L)
    for (k in seq_len(ncol(pr))) {
      i <- pr[1, k]
      j <- pr[2, k]
      if (iv[i, 2] < iv[j, 1] || iv[j, 2] < iv[i, 1]) {
        disjoint <- c(i, j) - 1L # 0-based, matching Python
        break
      }
    }
  }
  lo <- max(iv[, 1])
  hi <- min(iv[, 2])
  common <- lo <= hi
  holds <- common || !is.null(disjoint)
  list(
    pairwise_intersecting = is.null(disjoint), disjoint_pair = disjoint,
    common_point_exists = common,
    witness = if (common) c(lo, hi) else NULL,
    helly_holds = holds, estimate = as.numeric(common), n = nrow(iv),
    method = "Helly's theorem, d = 1 (Helly 1923)"
  )
}
