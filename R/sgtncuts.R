# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalised cut of a partition
#'
#' Shi and Malik (2000), Normalized cuts and image segmentation, IEEE
#' TPAMI 22(8), 888-905, equation (2): Ncut(A, B) = cut(A, B)/assoc(A, V)
#' + cut(A, B)/assoc(B, V), the cut weight charged against the total
#' volume of each side -- which is what stops the criterion preferring to
#' shave off a single vertex.  The PAMI paper is paywalled; the equation
#' is quoted in its standard published form.  The normalised association
#' satisfies Ncut = 2 - Nassoc for a two-way cut and is returned so the
#' identity can be checked.
#'
#' @param A symmetric weight matrix.
#' @param labels partition label per node.
#' @return list: ncut, estimate, cut, vol, nassoc, n_groups, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' Ncut(A, c(1, 1, 2))$ncut
#' @export
Ncut <- function(A, labels) {
  W <- .s03mat(A); n <- nrow(W)
  lab <- as.character(labels)
  ids <- character(0)
  for (cc in lab) if (!(cc %in% ids)) ids <- c(ids, cc)
  vol <- numeric(length(ids))
  for (gi in seq_along(ids)) {
    s <- 0
    for (i in seq_len(n)) if (lab[i] == ids[gi]) for (j in seq_len(n)) s <- s + W[i, j]
    vol[gi] <- s
  }
  ncut <- 0; nassoc <- 0; cut_total <- 0
  for (gi in seq_along(ids)) {
    cut <- 0; assoc <- 0
    for (i in seq_len(n)) {
      if (lab[i] != ids[gi]) next
      for (j in seq_len(n)) {
        if (lab[j] != ids[gi]) cut <- cut + W[i, j] else assoc <- assoc + W[i, j]
      }
    }
    cut_total <- cut_total + cut
    if (vol[gi] > 0) { ncut <- ncut + cut / vol[gi]; nassoc <- nassoc + assoc / vol[gi] }
  }
  list(ncut = ncut, estimate = ncut, cut = cut_total / 2, vol = vol,
       nassoc = nassoc, n_groups = length(ids),
       method = "Normalised cut (Shi and Malik 2000, eq. 2)")
}
