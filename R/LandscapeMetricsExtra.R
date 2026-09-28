.lsx_hull <- function(P) {
  P <- unique(P[order(P[, 1], P[, 2]), , drop = FALSE])
  n <- nrow(P)
  if (n <= 2) return(P)
  cross <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  build <- function(idx) {
    h <- integer(0)
    for (k in idx) {
      while (length(h) >= 2 && cross(P[h[length(h) - 1], ], P[h[length(h)], ], P[k, ]) <= 0) h <- h[-length(h)]
      h <- c(h, k)
    }
    h
  }
  lo <- build(seq_len(n))
  up <- build(rev(seq_len(n)))
  P[c(lo[-length(lo)], up[-length(up)]), , drop = FALSE]
}

.lsx_c2 <- function(a, b) {
  c0 <- (a + b) / 2
  list(c = c0, r = sqrt(sum((a - c0)^2)))
}

.lsx_c3 <- function(a, b, c) {
  d <- 2 * (a[1] * (b[2] - c[2]) + b[1] * (c[2] - a[2]) + c[1] * (a[2] - b[2]))
  if (d == 0) return(NULL)
  ux <- ((a[1]^2 + a[2]^2) * (b[2] - c[2]) + (b[1]^2 + b[2]^2) * (c[2] - a[2]) + (c[1]^2 + c[2]^2) * (a[2] - b[2])) / d
  uy <- ((a[1]^2 + a[2]^2) * (c[1] - b[1]) + (b[1]^2 + b[2]^2) * (a[1] - c[1]) + (c[1]^2 + c[2]^2) * (b[1] - a[1])) / d
  list(c = c(ux, uy), r = sqrt((a[1] - ux)^2 + (a[2] - uy)^2))
}

.lsx_in <- function(cc, p) sqrt(sum((p - cc$c)^2)) <= cc$r * (1 + 1e-12) + 1e-12

.lsx_mec_radius <- function(P) {
  H <- .lsx_hull(P)
  cc <- list(c = H[1, ], r = 0)
  if (nrow(H) > 1) for (i in 2:nrow(H)) {
    if (.lsx_in(cc, H[i, ])) next
    cc <- list(c = H[i, ], r = 0)
    for (j in seq_len(i - 1)) {
      if (.lsx_in(cc, H[j, ])) next
      cc <- .lsx_c2(H[i, ], H[j, ])
      for (k in seq_len(j - 1)) if (!.lsx_in(cc, H[k, ])) {
        c3 <- .lsx_c3(H[i, ], H[j, ], H[k, ])
        if (!is.null(c3)) cc <- c3
      }
    }
  }
  cc$r
}

.lsx_core <- function(lab, p, edge_depth, consider_boundary) {
  nr <- nrow(lab)
  nc <- ncol(lab)
  alive <- lab == p
  for (step in seq_len(edge_depth)) {
    edge <- matrix(FALSE, nr, nc)
    for (d in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
      sh <- matrix(!consider_boundary, nr, nc)
      xi <- seq_len(nr) + d[1]
      yi <- seq_len(nc) + d[2]
      okx <- xi >= 1 & xi <= nr
      oky <- yi >= 1 & yi <= nc
      nbr <- matrix(TRUE, nr, nc)
      nbr[okx, oky] <- !alive[xi[okx], yi[oky]]
      nbr[!okx, ] <- sh[!okx, ]
      nbr[, !oky] <- sh[, !oky]
      edge <- edge | (alive & nbr)
    }
    alive <- alive & !edge
  }
  sum(alive)
}

#' Further FRAGSTATS landscape metrics: shape, core, proximity and information theory
#'
#' \code{PatchStructure}: per patch \code{frac} \eqn{2\ln(0.25p)/\ln a}
#' (1 for single cells), \code{gyrate} (mean distance of cell centres from
#' the centroid, m), \code{circle} \eqn{1 - a/a_c} (smallest circle enclosing
#' the cell corners), \code{contig} (3x3 template: self 1, rook 2, diagonal
#' 1; \eqn{((\sum t)/n - 1)/12}), \code{core} (ha; cells more than
#' \code{edge_depth} rook steps from outside the patch, landscape boundary
#' counted unless \code{consider_boundary}) and \code{cai}.
#' \code{ClassStructure}: class means and area-weighted means (frac, shape,
#' para, gyrate), circle_mn, contig_mn, core_mn, cai_mn, tca, cpland and
#' \code{pafrac} \eqn{2/b} from the regression of \eqn{\ln a} on \eqn{\ln p}
#' (NA below 10 patches). \code{ProximityMetrics}: PROX
#' \eqn{\sum a_j/h_{ij}^2} over same-class patches within
#' \code{search_radius} (edge-to-edge cell-centre distance \eqn{h}), SIMI with
#' a class similarity matrix, and CONNECT (percentage of patch pairs within
#' \code{threshold}). \code{LandscapeInformation}: marginal entropy, joint
#' entropy, conditional entropy, mutual information and relative mutual
#' information of the rook co-occurrence matrix (McGarigal and Marks 1995;
#' Gustafson and Parker 1994; Nowosad and Stepinski 2019; Hesselbarth et al.
#' 2019). Identical to the Python arm \code{morie.fn.lsmextra}.
#'
#' @param landscape Integer matrix of classes (\code{NA} allowed).
#' @param res Cell size (m).
#' @param directions 4 or 8 (patch connectivity).
#' @param edge_depth Edge depth in cells for core area.
#' @param consider_boundary Logical; if \code{TRUE} the landscape boundary is
#'   not an edge.
#' @param circle_method \code{"exact"} (smallest circle enclosing every cell
#'   corner) or \code{"landscapemetrics"} (corners of the cells in the extreme
#'   rows and columns only, as \code{landscapemetrics}; that circle can miss
#'   other corners).
#' @param search_radius PROX/SIMI search radius (m).
#' @param similarity Class-by-class similarity matrix (ascending classes);
#'   default identity.
#' @param threshold CONNECT distance threshold (m); default
#'   \code{search_radius}.
#' @param base Logarithm base: \code{"log2"}, \code{"log"} or \code{"log10"}.
#' @return List of metrics.
#' @references McGarigal, K. and Marks, B. J. (1995). FRAGSTATS. USDA Forest
#'   Service General Technical Report PNW-351.
#'   Gustafson, E. J. and Parker, G. R. (1994). Using an index of habitat patch
#'   proximity for landscape design. Landscape and Urban Planning, 29, 117-130.
#'   Nowosad, J. and Stepinski, T. F. (2019). Information theory as a consistent
#'   framework for quantification and classification of landscape patterns.
#'   Landscape Ecology, 34(9), 2091-2101.
#' @examples
#' m <- matrix(c(1,1,1,1,1, 1,1,1,1,2, 1,1,1,2,2, 2,1,1,1,1), 4, byrow = TRUE)
#' round(PatchStructure(m)$contig, 6)
#' round(LandscapeInformation(m)$condent, 6)
#' @export
PatchStructure <- function(landscape, res = 1, directions = 8, edge_depth = 1, consider_boundary = FALSE,
                           circle_method = "exact") {
  circle_method <- match.arg(circle_method, c("exact", "landscapemetrics"))
  s <- .ls_setup(landscape, directions)
  ids <- seq_along(s$pcls)
  out <- lapply(ids, function(p) {
    cl <- which(s$lab == p, arr.ind = TRUE)
    n <- nrow(cl)
    a <- n * res^2
    pm <- s$per[p] * res
    fr <- if (a != 1) 2 * log(0.25 * pm) / log(a) else NaN
    if (is.nan(fr)) fr <- 1
    cx <- mean(cl[, 2])
    cy <- mean(cl[, 1])
    gy <- mean(sqrt((cl[, 2] - cx)^2 + (cl[, 1] - cy)^2)) * res
    x <- cl[, 2] - 1
    y <- cl[, 1] - 1
    corners <- if (circle_method == "exact") {
      do.call(rbind, lapply(list(c(0, 0), c(0, 1), c(1, 0), c(1, 1)), function(d) cbind(x + d[1], y + d[2])))
    } else {
      xa <- x == max(x)
      xi <- x == min(x)
      ya <- y == max(y)
      yi <- y == min(y)
      rbind(cbind(x + 1, y)[xa, , drop = FALSE], cbind(x + 1, y + 1)[xa, , drop = FALSE],
            cbind(x, y)[xi, , drop = FALSE], cbind(x, y + 1)[xi, , drop = FALSE],
            cbind(x, y + 1)[ya, , drop = FALSE], cbind(x + 1, y + 1)[ya, , drop = FALSE],
            cbind(x, y)[yi, , drop = FALSE], cbind(x + 1, y)[yi, , drop = FALSE])
    }
    r <- .lsx_mec_radius(corners) * res
    key <- paste(cl[, 1], cl[, 2])
    t <- 0
    for (di in -1:1) for (dj in -1:1) if (di != 0 || dj != 0)
      t <- t + sum(paste(cl[, 1] + di, cl[, 2] + dj) %in% key) * (if (di != 0 && dj != 0) 1 else 2)
    core <- .lsx_core(s$lab, p, as.integer(edge_depth), consider_boundary)
    c(fr, gy, 1 - a / (pi * r^2), ((t + n) / n - 1) / 12, core * res^2 / 10000, 100 * core / n)
  })
  M <- do.call(rbind, out)
  list(id = ids, patch_class = s$pcls, frac = M[, 1], gyrate = M[, 2], circle = M[, 3], contig = M[, 4],
       core = M[, 5], cai = M[, 6])
}

#' @rdname PatchStructure
#' @export
ClassStructure <- function(landscape, res = 1, directions = 8, edge_depth = 1, consider_boundary = FALSE,
                           circle_method = "exact") {
  P <- PatchStructure(landscape, res, directions, edge_depth, consider_boundary, circle_method)
  B <- PatchMetrics(landscape, res, directions)
  total <- sum(!is.na(as.matrix(landscape))) * res^2 / 10000
  classes <- sort(unique(P$patch_class))
  rows <- lapply(classes, function(cc) {
    ix <- which(P$patch_class == cc)
    W <- B$area[ix] / sum(B$area[ix])
    tca <- sum(P$core[ix])
    pafrac <- if (length(ix) < 10) NaN else
      2 / unname(stats::coef(stats::lm(log(B$area[ix] * 10000) ~ log(B$perim[ix])))[2])
    c(mean(P$frac[ix]), sum(W * P$frac[ix]), sum(W * B$shape[ix]), sum(W * B$para[ix]), mean(P$gyrate[ix]),
      sum(W * P$gyrate[ix]), mean(P$circle[ix]), mean(P$contig[ix]), mean(P$core[ix]), mean(P$cai[ix]), tca,
      100 * tca / total, pafrac)
  })
  M <- do.call(rbind, rows)
  nm <- c("frac_mn", "frac_am", "shape_am", "para_am", "gyrate_mn", "gyrate_am", "circle_mn", "contig_mn",
          "core_mn", "cai_mn", "tca", "cpland", "pafrac")
  c(list(class = classes), stats::setNames(lapply(seq_along(nm), function(k) M[, k]), nm))
}

#' @rdname PatchStructure
#' @export
ProximityMetrics <- function(landscape, search_radius, res = 1, directions = 8, similarity = NULL,
                             threshold = NULL) {
  s <- .ls_setup(landscape, directions)
  ids <- seq_along(s$pcls)
  n <- length(ids)
  classes <- sort(unique(s$pcls))
  S <- if (is.null(similarity)) diag(length(classes)) else as.matrix(similarity)
  thr <- if (is.null(threshold)) search_radius else threshold
  cells <- lapply(ids, function(p) which(s$lab == p, arr.ind = TRUE))
  H <- matrix(0, n, n)
  for (a in seq_len(n - 1)) for (b in (a + 1):n) {
    H[a, b] <- H[b, a] <- sqrt(min(outer(cells[[a]][, 1], cells[[b]][, 1], `-`)^2 +
                                   outer(cells[[a]][, 2], cells[[b]][, 2], `-`)^2)) * res
  }
  area <- s$ncell * res^2
  cls <- s$pcls
  ci <- match(cls, classes)
  prox <- simi <- numeric(n)
  for (a in seq_len(n)) {
    near <- setdiff(which(H[a, ] <= search_radius), a)
    same <- near[cls[near] == cls[a]]
    prox[a] <- sum(area[same] / H[a, same]^2)
    simi[a] <- sum(area[near] * S[ci[a], ci[near]] / H[a, near]^2)
  }
  jp <- t(vapply(classes, function(cc) {
    ix <- which(cls == cc)
    m <- length(ix)
    j <- if (m > 1) sum(H[ix, ix][upper.tri(H[ix, ix])] <= thr) else 0
    c(j, m * (m - 1) / 2)
  }, numeric(2)))
  list(id = ids, patch_class = cls, prox = prox, simi = simi, class = classes,
       connect = ifelse(jp[, 2] > 0, 100 * jp[, 1] / jp[, 2], NaN),
       connect_landscape = if (sum(jp[, 2]) > 0) 100 * sum(jp[, 1]) / sum(jp[, 2]) else NaN)
}

#' @rdname PatchStructure
#' @export
LandscapeInformation <- function(landscape, base = "log2") {
  G <- as.matrix(landscape)
  nr <- nrow(G)
  nc <- ncol(G)
  from <- to <- integer(0)
  for (d in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
    xi <- seq_len(nr) + d[1]
    yi <- seq_len(nc) + d[2]
    okx <- which(xi >= 1 & xi <= nr)
    oky <- which(yi >= 1 & yi <= nc)
    a <- G[okx, oky]
    b <- G[xi[okx], yi[oky]]
    keep <- !is.na(a) & !is.na(b)
    from <- c(from, a[keep])
    to <- c(to, b[keep])
  }
  lg <- switch(base, log2 = log2, log = log, log10 = log10)
  H <- function(cn) {
    p <- cn[cn > 0] / sum(cn)
    -sum(p * lg(p))
  }
  ent <- H(as.numeric(table(to)))
  joinent <- H(as.numeric(table(paste(from, to))))
  condent <- joinent - ent
  mutinf <- ent - condent
  list(ent = ent, joinent = joinent, condent = condent, mutinf = mutinf,
       relmutinf = if (mutinf == 0) 1 else mutinf / ent)
}
