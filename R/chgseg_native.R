# Mean-change segmentation of a series, solved exactly by PELT.
# Source: Killick, R., Fearnhead, P. and Eckley, I. A. (2012), JASA
# 107(500), 1590-1598, eq (3) (the penalised minimisation) solved by
# their Algorithm 2.  Native implementation mirroring Python
# morie.fn.chgseg, which is the same call onto morie.fn.pelt with
# cost="mean".

#' Mean-change segmentation (PELT)
#'
#' Exact minimisation of eq (3) of Killick et al. (2012) with the
#' Normal change-in-mean cost, i.e. \code{\link{morie_pelt}} with
#' \code{cost = "mean"}.
#'
#' @param y Numeric series.
#' @param penalty Penalty per changepoint; \code{NULL} (default) uses
#'   the BIC value \code{log(n)}.
#' @return The list returned by \code{\link{morie_pelt}}, with
#'   \code{method} naming the segmentation.
#' @references Killick, R., Fearnhead, P. and Eckley, I. A. (2012).
#'   Optimal detection of changepoints with a linear computational
#'   cost. Journal of the American Statistical Association, 107(500),
#'   1590-1598.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_chgseg(V)
morie_chgseg <- function(y, penalty = NULL) {
  out <- morie_pelt(y, cost = "mean", penalty = penalty)
  out$method <- paste("PELT mean-change segmentation",
                      "(Killick et al. 2012, eq 3)")
  out
}
