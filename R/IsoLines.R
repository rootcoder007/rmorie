#' Contour lines by marching squares
#'
#' Iso-lines of a grid \code{z} (\code{length(x)} rows by \code{length(y)}
#' columns, \code{z[i, j]} at \code{(x[i], y[j])}): corners count as above when
#' \code{z > level}, crossings are linearly interpolated along cell edges (as
#' \code{grDevices::contourLines}), shared edge crossings are computed once,
#' segments are chained into polylines (closed rings repeat their first
#' vertex), saddles are resolved by the cell mean and cells with a missing
#' corner are skipped. Identical to the Python arm \code{morie.fn.isolines}.
#'
#' @param x,y Grid coordinates.
#' @param z Matrix of values.
#' @param levels Contour levels.
#' @return List with \code{lines} (each \code{level}, \code{x}, \code{y}),
#'   \code{length} per level and \code{levels}.
#' @references Lorensen, W. E. and Cline, H. E. (1987). Marching cubes: a high
#'   resolution 3D surface construction algorithm. Computer Graphics 21,
#'   163-169.
#' @examples
#' IsoLines(0:2, 0:2, rbind(c(0, 0, 0), c(0, 2, 0), c(0, 0, 0)), 1)$length
#' @export
IsoLines <- function(x, y, z, levels) {
  Z <- as.matrix(z)
  nx <- length(x)
  ny <- length(y)
  if (nrow(Z) != nx || ncol(Z) != ny) stop("z must be length(x) x length(y)")
  seg_tab <- list(`1` = list(c(3, 0)), `2` = list(c(0, 1)), `3` = list(c(3, 1)), `4` = list(c(1, 2)),
                  `6` = list(c(0, 2)), `7` = list(c(3, 2)), `8` = list(c(2, 3)), `9` = list(c(0, 2)),
                  `11` = list(c(1, 2)), `12` = list(c(1, 3)), `13` = list(c(0, 1)), `14` = list(c(3, 0)))
  lines <- list()
  lens <- numeric(0)
  for (L in levels) {
    cache <- new.env()
    pt <- function(a, b) {
      if (a[1] > b[1] || (a[1] == b[1] && a[2] > b[2])) {
        tmp <- a
        a <- b
        b <- tmp
      }
      key <- paste(a[1], a[2], b[1], b[2])
      if (is.null(cache[[key]])) {
        t <- (L - Z[a[1], a[2]]) / (Z[b[1], b[2]] - Z[a[1], a[2]])
        cache[[key]] <- c(x[a[1]] + t * (x[b[1]] - x[a[1]]), y[a[2]] + t * (y[b[2]] - y[a[2]]))
      }
      key
    }
    segs <- list()
    for (i in seq_len(nx - 1)) {
      for (j in seq_len(ny - 1)) {
        cc <- list(c(i, j), c(i + 1, j), c(i + 1, j + 1), c(i, j + 1))
        v <- c(Z[i, j], Z[i + 1, j], Z[i + 1, j + 1], Z[i, j + 1])
        if (anyNA(v)) next
        code <- sum(2^(0:3)[v > L])
        edges <- list(list(cc[[1]], cc[[2]]), list(cc[[2]], cc[[3]]), list(cc[[4]], cc[[3]]), list(cc[[1]], cc[[4]]))
        pairs <- if (code %in% c(5, 10)) {
          if (mean(v) > L) {
            if (code == 5) list(c(3, 2), c(0, 1)) else list(c(0, 3), c(2, 1))
          } else {
            if (code == 5) list(c(3, 0), c(1, 2)) else list(c(0, 1), c(2, 3))
          }
        } else {
          seg_tab[[as.character(code)]]
        }
        for (p in pairs) {
          e1 <- edges[[p[1] + 1]]
          e2 <- edges[[p[2] + 1]]
          segs[[length(segs) + 1]] <- c(pt(e1[[1]], e1[[2]]), pt(e2[[1]], e2[[2]]))
        }
      }
    }
    adj <- list()
    for (k in seq_along(segs)) for (q in segs[[k]]) adj[[q]] <- c(adj[[q]], k)
    used <- rep(FALSE, length(segs))
    level_lines <- list()
    for (k0 in seq_along(segs)) {
      if (used[k0]) next
      used[k0] <- TRUE
      chain <- segs[[k0]]
      for (fwd in c(TRUE, FALSE)) {
        repeat {
          tip <- if (fwd) chain[length(chain)] else chain[1]
          cand <- adj[[tip]][!used[adj[[tip]]]]
          if (length(cand) == 0) break
          nxt <- cand[1]
          used[nxt] <- TRUE
          other <- if (segs[[nxt]][1] == tip) segs[[nxt]][2] else segs[[nxt]][1]
          chain <- if (fwd) c(chain, other) else c(other, chain)
        }
      }
      P <- do.call(rbind, lapply(chain, function(q) cache[[q]]))
      level_lines[[length(level_lines) + 1]] <- list(level = L, x = P[, 1], y = P[, 2])
    }
    lines <- c(lines, level_lines)
    lens <- c(lens, sum(vapply(level_lines, function(ln) sum(sqrt(diff(ln$x)^2 + diff(ln$y)^2)), 0)))
  }
  list(lines = lines, length = lens, levels = levels)
}
