# SPDX-License-Identifier: AGPL-3.0-or-later

#' Recycling training objective of AlphaFold
#'
#' Algorithm 31 and equation (48) of the Supplementary Information to
#' Jumper et al. (2021), pp. 42-43.  The objective is the average loss over
#' all recycling iterations.  Training never evaluates that average; it
#' samples one iteration \code{N'} uniformly and trains only that one,
#' stopping gradients into the earlier iterations and skipping the later
#' ones entirely.
#'
#' The sampled estimate is unbiased, which is the whole justification for
#' the scheme: averaging the single-iteration estimate over every possible
#' \code{N'} returns equation (48) exactly.  Nothing is sampled here --
#' pass \code{nprime} for one iteration's estimate, or omit it for the
#' average.
#'
#' @param losses Loss at each recycling iteration.
#' @param nprime Optional one-based iteration selected by training.
#' @return A list with \code{estimate}, the equation (48) \code{average},
#'   \code{expected}, \code{ncycle}, \code{nprime} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. Algorithm 31,
#'   equation (48)
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Alfrcyl(V)
Alfrcyl <- function(losses, nprime = NULL) {
  nc <- length(losses)
  if (nc == 0L) stop("losses must not be empty")
  avg <- sum(losses) / nc
  if (!is.null(nprime) && (nprime < 1 || nprime > nc)) {
    stop("nprime ", nprime, " outside 1..", nc)
  }
  est <- if (is.null(nprime)) avg else losses[nprime]
  list(
    estimate = est, average = avg, expected = sum(losses) / nc,
    ncycle = nc, nprime = nprime,
    method = "AlphaFold recycling training objective"
  )
}
