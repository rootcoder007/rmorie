#' Patches of a categorical raster
#'
#' Connected components of equal-class cells (4 or 8 neighbours), numbered
#' from 1 by class (ascending) and, within a class, in column-major order of
#' their first cell, as \code{landscapemetrics::get_patches}.
#'
#' @param landscape Integer matrix of classes (\code{NA} allowed).
#' @param directions 4 or 8.
#' @return List with \code{labels} (matrix, 0 for NA) and \code{patch_class}.
#' @examples
#' LabelPatches(rbind(c(1, 1, 2), c(2, 1, 2)), directions = 4)$labels
#' @export
LabelPatches <- function(landscape, directions = 8) {
  if (!directions %in% c(4, 8)) stop("directions must be 4 or 8")
  G <- as.matrix(landscape)
  nr <- nrow(G)
  nc <- ncol(G)
  nb <- rbind(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  if (directions == 8) nb <- rbind(nb, c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))
  lab <- matrix(0L, nr, nc)
  cls <- integer(0)
  nxt <- 0L
  for (k in sort(unique(G[!is.na(G)]))) {
    for (j in seq_len(nc)) {
      for (i in seq_len(nr)) {
        if (is.na(G[i, j]) || G[i, j] != k || lab[i, j] > 0) next
        nxt <- nxt + 1L
        cls <- c(cls, k)
        lab[i, j] <- nxt
        stack <- list(c(i, j))
        while (length(stack)) {
          cur <- stack[[length(stack)]]
          stack[[length(stack)]] <- NULL
          for (t in seq_len(nrow(nb))) {
            x <- cur[1] + nb[t, 1]
            y <- cur[2] + nb[t, 2]
            if (x >= 1 && x <= nr && y >= 1 && y <= nc && lab[x, y] == 0 && !is.na(G[x, y]) && G[x, y] == k) {
              lab[x, y] <- nxt
              stack[[length(stack) + 1]] <- c(x, y)
            }
          }
        }
      }
    }
  }
  list(labels = lab, patch_class = cls)
}

.ls_min_edges <- function(n) {
  m <- floor(sqrt(n))
  ifelse(m * m == n, 4 * m, ifelse(n <= m * (m + 1), 4 * m + 2, 4 * m + 4))
}

.ls_max_like <- function(n) {
  m <- floor(sqrt(n))
  r <- n - m * m
  ifelse(r == 0, 2 * m * (m - 1), ifelse(r <= m, 2 * m * (m - 1) + 2 * r - 1, 2 * m * (m - 1) + 2 * r - 2))
}

.ls_setup <- function(landscape, directions) {
  G <- as.matrix(landscape)
  L <- LabelPatches(G, directions)
  nr <- nrow(G)
  nc <- ncol(G)
  np <- length(L$patch_class)
  per <- numeric(np)
  classes <- sort(unique(G[!is.na(G)]))
  adj <- matrix(0, length(classes), length(classes), dimnames = list(classes, classes))
  outer <- setNames(numeric(length(classes)), classes)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (is.na(G[i, j])) next
      ci <- as.character(G[i, j])
      for (d in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
        x <- i + d[1]
        y <- j + d[2]
        inside <- x >= 1 && x <= nr && y >= 1 && y <= nc
        other <- if (inside) G[x, y] else NA
        if (is.na(other) || other != G[i, j]) per[L$labels[i, j]] <- per[L$labels[i, j]] + 1
        if (is.na(other)) outer[ci] <- outer[ci] + 1 else adj[ci, as.character(other)] <- adj[ci, as.character(other)] + 1
      }
    }
  }
  list(G = G, lab = L$labels, pcls = L$patch_class, ncell = tabulate(L$labels[L$labels > 0], np), per = per,
       adj = adj, outer = outer, classes = classes)
}

#' Landscape metrics of categorical rasters (FRAGSTATS, landscapemetrics conventions)
#'
#' \code{PatchMetrics}: area (ha), perim (m, boundary included), shape
#' \eqn{0.25\,p/\sqrt{a}}, para, enn (nearest cell-centre distance to the
#' class's other patches). \code{ClassMetrics}: ca, pland, np, pd, lpi, ed
#' (boundary excluded), lsi, ai, clumpy (FRAGSTATS definition), cohesion, iji,
#' area_mn, shape_mn, enn_mn. \code{LandscapeMetrics}: ta, np, pd, lpi, ed,
#' lsi, ai, contag (adjacency proportions), iji, pr, shdi, shei, sidi, siei,
#' dominance \eqn{\ln m - SHDI}, shape_mn, area_mn, enn_mn (McGarigal and
#' Marks 1995; Li and Reynolds 1993; O'Neill et al. 1988; Hesselbarth et al.
#' 2019). Identical to the Python arm \code{morie.fn.lsmets}.
#'
#' @param landscape Integer matrix of classes (\code{NA} allowed).
#' @param res Cell size (m).
#' @param directions 4 or 8 (patch connectivity).
#' @return List of metrics.
#' @references McGarigal, K. and Marks, B. J. (1995). FRAGSTATS. USDA Forest
#'   Service General Technical Report PNW-351.
#'
#'   Hesselbarth, M. H. K., Sciaini, M., With, K. A., Wiegand, K. and
#'   Nowosad, J. (2019). landscapemetrics: an open-source R tool to calculate
#'   landscape metrics. Ecography 42, 1648-1657.
#' @examples
#' g <- rbind(c(1, 1, 2), c(2, 1, 2))
#' PatchMetrics(g, directions = 4)$shape
#' ClassMetrics(g, directions = 4)$pland
#' LandscapeMetrics(g, directions = 4)$shdi
#' @export
PatchMetrics <- function(landscape, res = 1, directions = 8) {
  s <- .ls_setup(landscape, directions)
  ids <- seq_along(s$pcls)
  n <- s$ncell
  enn <- vapply(ids, function(p) {
    same <- ids[ids != p & s$pcls == s$pcls[p]]
    if (!length(same)) return(NaN)
    a <- which(s$lab == p, arr.ind = TRUE)
    b <- which(matrix(s$lab %in% same, nrow(s$lab)), arr.ind = TRUE)
    sqrt(min(outer(a[, 1], b[, 1], `-`)^2 + outer(a[, 2], b[, 2], `-`)^2)) * res
  }, 0)
  list(id = ids, patch_class = s$pcls, area = n * res^2 / 10000, perim = s$per * res,
       shape = 0.25 * s$per / sqrt(n), para = s$per * res / (n * res^2), enn = enn)
}

#' @rdname PatchMetrics
#' @export
ClassMetrics <- function(landscape, res = 1, directions = 8) {
  s <- .ls_setup(landscape, directions)
  P <- PatchMetrics(landscape, res, directions)
  Z <- sum(!is.na(s$G))
  ta <- Z * res^2 / 10000
  m <- length(s$classes)
  out <- lapply(seq_along(s$classes), function(ci) {
    k <- s$classes[ci]
    idx <- which(P$patch_class == k)
    n <- sum(s$ncell[idx])
    like <- s$adj[ci, ci]
    unlike <- sum(s$adj[ci, -ci])
    pk <- n / Z
    den <- like + unlike + s$outer[ci] - .ls_min_edges(n)
    Gi <- if (den != 0) like / den else NaN
    clumpy <- if (is.nan(Gi) || pk == 1) NaN else if (Gi < pk && pk < 0.5) (Gi - pk) / pk else (Gi - pk) / (1 - pk)
    gm <- .ls_max_like(n)
    pp <- s$per[idx]
    aa <- s$ncell[idx]
    e_ik <- s$adj[ci, -ci]
    E <- sum(e_ik)
    iji <- if (m < 3 || E == 0) NaN else -100 * sum((e_ik[e_ik > 0] / E) * log(e_ik[e_ik > 0] / E)) / log(m - 1)
    en <- P$enn[idx][!is.nan(P$enn[idx])]
    c(class = k, ca = n * res^2 / 10000, pland = 100 * pk, np = length(idx), pd = length(idx) / ta * 100,
      lpi = 100 * max(P$area[idx]) / ta, ed = unlike * res / ta, lsi = sum(pp) / .ls_min_edges(n),
      ai = if (gm > 0) 100 * (like / 2) / gm else NaN, clumpy = unname(clumpy),
      cohesion = 100 * (1 - sum(pp) / sum(pp * sqrt(aa))) / (1 - 1 / sqrt(Z)), iji = iji,
      area_mn = mean(P$area[idx]), shape_mn = mean(P$shape[idx]), enn_mn = if (length(en)) mean(en) else NaN)
  })
  out <- do.call(rbind, out)
  lapply(setNames(colnames(out), colnames(out)), function(nm) unname(out[, nm]))
}

#' @rdname PatchMetrics
#' @export
LandscapeMetrics <- function(landscape, res = 1, directions = 8) {
  s <- .ls_setup(landscape, directions)
  P <- PatchMetrics(landscape, res, directions)
  C <- ClassMetrics(landscape, res, directions)
  m <- length(s$classes)
  Z <- sum(!is.na(s$G))
  ta <- Z * res^2 / 10000
  p <- C$pland / 100
  shdi <- -sum(p[p > 0] * log(p[p > 0]))
  sidi <- 1 - sum(p^2)
  unlike_total <- (sum(s$adj) - sum(diag(s$adj))) / 2
  like <- diag(s$adj) / 2
  gmax <- .ls_max_like(round(p * Z))
  q <- s$adj[s$adj > 0] / sum(s$adj)
  e_pairs <- s$adj[upper.tri(s$adj)]
  E <- sum(e_pairs)
  iji <- if (m >= 3 && E > 0) {
    -100 * sum((e_pairs[e_pairs > 0] / E) * log(e_pairs[e_pairs > 0] / E)) / log(m * (m - 1) / 2)
  } else {
    NaN
  }
  en <- P$enn[!is.nan(P$enn)]
  list(ta = ta, np = length(P$id), pd = length(P$id) / ta * 100, lpi = 100 * max(P$area) / ta,
       ed = unlike_total * res / ta, lsi = (unlike_total + sum(s$outer)) / .ls_min_edges(Z),
       ai = 100 * sum((p * like / gmax)[gmax > 0]), contag = if (m > 1) 100 * (1 + sum(q * log(q)) / (2 * log(m))) else NaN,
       iji = iji, pr = m, shdi = shdi, shei = if (m > 1) shdi / log(m) else NaN, sidi = sidi,
       siei = if (m > 1) sidi / (1 - 1 / m) else NaN, dominance = log(m) - shdi, shape_mn = mean(P$shape),
       area_mn = mean(P$area), enn_mn = if (length(en)) mean(en) else NaN)
}
