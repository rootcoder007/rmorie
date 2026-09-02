# SPDX-License-Identifier: AGPL-3.0-or-later
#' Strata bookkeeping shared by the post-stratification pair (internal)
#'
#' @param y Observed values.
#' @param stratum Stratum label per observation.
#' @param Nh Named vector or plain vector of population sizes.
#' @param who Caller name for error messages.
#' @return List with \code{y}, \code{s}, \code{sizes}, \code{order}.
#' @keywords internal
.ps_strata <- function(y, stratum, Nh, who) {
  y <- as.numeric(unlist(y))
  s <- as.character(unlist(stratum))
  if (length(y) == 0L) stop(paste0(who, ": y is empty"))
  if (length(s) != length(y))
    stop(paste0(who, ": stratum must have one entry per observation"))
  ord <- unique(s)
  if (!is.null(names(Nh))) {
    sizes <- as.numeric(Nh); names(sizes) <- names(Nh)
  } else {
    v <- as.numeric(unlist(Nh))
    if (length(v) != length(ord))
      stop(paste0(who, ": Nh must give one size per stratum"))
    sizes <- v; names(sizes) <- ord
  }
  if (!all(s %in% names(sizes)))
    stop(paste0(who, ": a stratum has no population size"))
  if (any(sizes <= 0)) stop(paste0(who, ": stratum sizes must be positive"))
  list(y = y, s = s, sizes = sizes, order = ord)
}

#' Post-stratified estimator of a population mean
#'
#' Post-stratification uses stratum sizes that were not used to draw the
#' sample. When the sample happens to be proportionally allocated the
#' estimator collapses to the plain sample mean, which is the anchor used
#' for this module.
#'
#' Formula: \code{ybar_post = sum_h (N_h / N) ybar_h} with
#' \code{var = sum_h (N_h / N)^2 s_h^2 / n_h}.
#'
#' @param y Observed values.
#' @param stratum Stratum label per observation.
#' @param Nh Population size per stratum; an unnamed vector is matched to
#'   the strata in order of first appearance.
#' @return List with \code{estimate}, \code{se}, \code{variance},
#'   \code{strata}, \code{N}, \code{n}.
#' @references Holt, D. & Smith, T. M. F. (1979). Post stratification.
#'   Journal of the Royal Statistical Society Series A 142(1):33-46.
#'   \doi{10.2307/2344652}. Standard form; the paper is paywalled and not
#'   held locally.
#' @export
#' @examples
#' Poststs(y = c(1, 2, 3, 4, 5, 6, 7, 8), stratum = c(1, 2, 3, 4, 5, 6, 7, 8), Nh = c(1, 2, 3, 4, 5, 6, 7, 8))
Poststs <- function(y, stratum, Nh) {
  d <- .ps_strata(y, stratum, Nh, "Poststs")
  N <- sum(d$sizes[d$order])
  est <- 0; v <- 0
  for (k in d$order) {
    vals <- d$y[d$s == k]
    nh <- length(vals)
    if (nh == 0L) stop(paste0("Poststs: stratum ", k, " has no observations"))
    mh <- sum(vals) / nh
    w <- d$sizes[[k]] / N
    est <- est + w * mh
    if (nh > 1L) v <- v + w * w * (sum((vals - mh)^2) / (nh - 1)) / nh
  }
  .t1_result(estimate = est, se = sqrt(v), variance = v,
             strata = length(d$order), N = N, n = length(d$y),
             method = "Post-stratified mean, sum_h (N_h/N) ybar_h")
}
