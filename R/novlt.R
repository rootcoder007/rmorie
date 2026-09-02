# SPDX-License-Identifier: AGPL-3.0-or-later
#' How surprising a recommendation is, in bits
#'
#' Accuracy metrics reward recommending what everyone already likes, so a
#' system optimised for them converges on the head of the catalogue.
#' Novelty is the counterweight: self-information, large exactly when few
#' users have seen the item. In bits, so an item half the users know
#' scores 1.
#'
#' Formula: \code{nov(i) = -log2 P(i)}.
#'
#' @param item Zero-based item indices.
#' @param popularity Interaction counts or probabilities per item.
#' @return List with \code{estimate}, \code{nov}, \code{p}, \code{n_items}.
#' @references Vargas, S. & Castells, P. (2011). RecSys 2011, 109-116.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Novlt(V, V)
Novlt <- function(item, popularity) {
  pop <- as.numeric(popularity)
  p <- pop / sum(pop)
  idx <- as.integer(round(as.numeric(item))) + 1L
  nov <- ifelse(p[idx] > 0, -log(p[idx]) / log(2), Inf)
  .t1_result(estimate = sum(nov) / length(nov), nov = nov, p = p,
             n_items = length(p), method = "Novelty, self-information in bits")
}
