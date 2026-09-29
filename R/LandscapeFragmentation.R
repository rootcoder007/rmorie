#' Landscape shape and fragmentation indices
#'
#' \code{DissectionIndex}: perimeter over the circumference of the circle of
#' equal area. \code{ForestFragmentation}: Riitters et al. (2000) moving-window
#' Pf, Pff and fragmentation classes (character matrix, NA for non-forest).
#' Identical to the Python arm \code{morie.fn.landfrag}.
#'
#' @param area,perimeter Patch area and perimeter.
#' @param grid Binary forest matrix (1 forest, 0 other, NA missing).
#' @param window Odd window size.
#' @return A number, vector or list.
#' @references Patton, D. R. (1975). A diversity index for quantifying habitat
#'   edge. Wildlife Society Bulletin 3, 171-173.
#'
#'   Riitters, K., Wickham, J., O'Neill, R., Jones, B. and Smith, E. (2000).
#'   Global-scale patterns of forest fragmentation. Conservation Ecology 4, 3.
#' @examples
#' DissectionIndex(100, 40)
#' ForestFragmentation(rbind(c(1, 1, 1), c(1, 1, 1), c(1, 1, 0)))$classes
#' @export
DissectionIndex <- function(area, perimeter) perimeter / (2 * sqrt(pi * area))

#' @rdname DissectionIndex
#' @export
ForestFragmentation <- function(grid, window = 3) {
  nr <- nrow(grid)
  nc <- ncol(grid)
  h <- window %/% 2
  cls <- matrix(NA_character_, nr, nc)
  pfg <- matrix(NaN, nr, nc)
  pffg <- matrix(NaN, nr, nc)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (is.na(grid[i, j]) || grid[i, j] != 1) next
      rr <- max(1, i - h):min(nr, i + h)
      cc <- max(1, j - h):min(nc, j + h)
      sub <- grid[rr, cc, drop = FALSE]
      ok <- !is.na(sub)
      f <- ok & sub == 1
      pf <- sum(f) / sum(ok)
      pv <- ok[-nrow(sub), , drop = FALSE] & ok[-1, , drop = FALSE]
      ph <- ok[, -ncol(sub), drop = FALSE] & ok[, -1, drop = FALSE]
      anyv <- pv & (f[-nrow(sub), , drop = FALSE] | f[-1, , drop = FALSE])
      anyh <- ph & (f[, -ncol(sub), drop = FALSE] | f[, -1, drop = FALSE])
      bothv <- pv & f[-nrow(sub), , drop = FALSE] & f[-1, , drop = FALSE]
      bothh <- ph & f[, -ncol(sub), drop = FALSE] & f[, -1, drop = FALSE]
      na <- sum(anyv) + sum(anyh)
      pff <- if (na > 0) (sum(bothv) + sum(bothh)) / na else NaN
      cls[i, j] <- if (pf == 1) "interior" else if (pf < 0.4) "patch" else if (pf < 0.6) "transitional" else
        if (pf - pff < 0) "edge" else if (pf - pff > 0) "perforated" else "undetermined"
      pfg[i, j] <- pf
      pffg[i, j] <- pff
    }
  }
  list(classes = cls, pf = pfg, pff = pffg, counts = table(cls))
}
