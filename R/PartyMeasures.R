.pm_get <- function(per, code) {
  for (k in c(as.character(code), paste0("per", code))) {
    if (!is.null(per[[k]])) {
      v <- per[[k]]
      return(if (is.na(v)) 0 else as.numeric(v))
    }
  }
  0
}

#' Left-right scales from Manifesto Project category percentages
#'
#' With R and L the summed percentages of the \code{pos} and \code{neg}
#' categories (by default the right and left RILE categories of Laver and
#' Budge 1992): \code{rile = R - L}; the Kim and Fording (1998) ratio scale
#' \eqn{(R - L)/(R + L)}; the ratio \eqn{R/L}; and, when the quasi-sentence
#' count \code{total} is given, the logit scale of Lowe et al. (2011)
#' \eqn{\log((R\,total/100 + 0.5)/(L\,total/100 + 0.5))}. These are
#' \code{manifestoR::rile}, \code{scale_ratio_1}, \code{scale_ratio_2} and
#' \code{logit_rile}.
#'
#' @param per A named list or vector (names like \code{"per104"} or
#'   \code{"104"}) of category percentages, or a data frame with one
#'   document per row.
#' @param total Quasi-sentence count(s) for the logit scale.
#' @param pos,neg Categories scored positive and negative.
#' @param zero_offset Added to each count in the logit scale.
#' @return List with \code{rile}, \code{ratio_1}, \code{ratio_2},
#'   \code{logit}.
#' @references Laver, M. and Budge, I. (1992). Party Policy and Government
#'   Coalitions. Macmillan.
#'
#'   Kim, H. and Fording, R. C. (1998). Voter ideology in Western
#'   democracies, 1946-1989. European Journal of Political Research 33,
#'   73-97.
#'
#'   Lowe, W., Benoit, K., Mikhaylov, S. and Laver, M. (2011). Scaling
#'   policy preferences from coded political texts. Legislative Studies
#'   Quarterly 36, 123-155.
#' @examples
#' ManifestoScales(list(per104 = 5, per401 = 7.5, per403 = 2, per504 = 10), total = 200)
#' @export
ManifestoScales <- function(per, total = NULL,
                            pos = c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606),
                            neg = c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701),
                            zero_offset = 0.5) {
  docs <- if (is.data.frame(per)) lapply(seq_len(nrow(per)), function(i) as.list(per[i, , drop = FALSE])) else list(as.list(per))
  tot <- if (is.null(total)) rep(NA_real_, length(docs)) else as.numeric(total)
  R <- vapply(docs, function(d) sum(vapply(pos, function(c) .pm_get(as.list(d), c), 0)), 0)
  L <- vapply(docs, function(d) sum(vapply(neg, function(c) .pm_get(as.list(d), c), 0)), 0)
  list(rile = R - L, ratio_1 = (R - L) / (R + L), ratio_2 = R / L,
       logit = log((R * tot / 100 + zero_offset) / (L * tot / 100 + zero_offset)))
}

#' Meyer and Miller (2015) nicheness of the parties in one election
#'
#' For every policy dimension a party's squared deviation from the
#' (weighted) mean of its rivals is taken; its nicheness is the root mean
#' square over the dimensions (dimensions nobody mentions are dropped when
#' \code{only_non_zero}), minus, with \code{normalize}, the weighted rival
#' mean of that quantity. \code{transform = "bischof"} applies
#' \eqn{\log(x + 1)} first. This is manifestoR's single-election
#' Meyer-Miller computation.
#'
#' @param emphases Parties x dimensions attention matrix.
#' @param weights Party weights (e.g. vote shares); default 1.
#' @param normalize Subtract the weighted rival mean.
#' @param only_non_zero Drop dimensions with zero total attention.
#' @param transform \code{NULL} or \code{"bischof"}.
#' @return Numeric vector of nicheness scores.
#' @references Meyer, T. M. and Miller, B. (2015). The niche party concept
#'   and its measurement. Party Politics 21, 259-271.
#'
#'   Bischof, D. (2017). Towards a renewal of the niche party concept.
#'   Party Politics 23, 220-235.
#' @examples
#' PartyNicheness(rbind(c(10, 0), c(4, 6), c(2, 8)), normalize = FALSE)
#' @export
PartyNicheness <- function(emphases, weights = NULL, normalize = TRUE, only_non_zero = TRUE, transform = NULL) {
  E <- as.matrix(emphases)
  if (identical(transform, "bischof")) E <- log(E + 1)
  w <- if (is.null(weights)) rep(1, nrow(E)) else as.numeric(weights)
  rival <- function(x) (sum(x * w) - x * w) / (sum(w) - w)
  cols <- if (only_non_zero) which(colSums(E) > 0) else seq_len(ncol(E))
  dev <- vapply(cols, function(c) (E[, c] - rival(E[, c]))^2, numeric(nrow(E)))
  dev <- matrix(dev, nrow(E))
  nic <- sqrt(rowSums(dev) / length(cols))
  if (normalize) nic <- nic - rival(nic)
  nic
}

#' Kim and Fording (1998) median voter from party positions and votes
#'
#' Parties are ordered on the scale; each party's voters are spread
#' uniformly between the midpoints to its neighbours (the scale ends for the
#' outermost parties, or, with \code{adjusted}, the reflections of the
#' neighbouring midpoints; Kim and Fording 2003). The median voter lies
#' where the cumulative vote share crosses one half. Parties at the same
#' position are merged. This is \code{manifestoR::median_voter}.
#'
#' @param positions Party positions.
#' @param voteshares Party vote shares.
#' @param adjusted Use the adjusted outer bounds.
#' @param scalemin,scalemax Scale ends.
#' @return The median voter position.
#' @references Kim, H. and Fording, R. C. (1998). Voter ideology in Western
#'   democracies, 1946-1989. European Journal of Political Research 33,
#'   73-97.
#'
#'   Kim, H. and Fording, R. C. (2003). Voter ideology in Western
#'   democracies: an update. European Journal of Political Research 42,
#'   95-105.
#' @examples
#' KimFordingMedian(c(-20, 5, 30), c(30, 25, 45))
#' @export
KimFordingMedian <- function(positions, voteshares, adjusted = FALSE, scalemin = -100, scalemax = 100) {
  agg <- tapply(as.numeric(voteshares), as.numeric(positions), sum)
  xs <- as.numeric(names(agg))
  sh <- as.numeric(agg) / sum(agg)
  m <- length(xs)
  mid <- (xs[-1] + xs[-m]) / 2
  if (adjusted && m >= 2) {
    left <- c((2 * xs[1] - xs[2] + xs[1]) / 2, mid)
    right <- c(mid, (xs[m] + 2 * xs[m] - xs[m - 1]) / 2)
  } else {
    left <- c(scalemin, mid)
    right <- c(mid, scalemax)
  }
  cum <- cumsum(sh)
  k <- which(cum >= 0.5)[1]
  before <- if (k > 1) cum[k - 1] else 0
  left[k] + (0.5 - before) / sh[k] * (right[k] - left[k])
}
