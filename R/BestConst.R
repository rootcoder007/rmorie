# SPDX-License-Identifier: AGPL-3.0-or-later

#' Best constant predictor under squared error
#'
#' Two probe constants either side of the mean are evaluated; if either
#' beats the mean the routine stops rather than returning a wrong
#' optimum.
#'
#' @param y outcomes, non-empty.
#' @return list(best_prediction, mse).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (6.22)-(6.23).
#' @examples
#' BestConst(c(1, 1, 3, 4, 6))$best_prediction
#' @export
BestConst <- function(y) {
  y <- as.numeric(y)
  if (length(y) == 0L || any(is.na(y))) {
    stop("y must be a non-empty numeric vector.", call. = FALSE)
  }
  mu <- mean(y)
  mse_mu <- mean((y - mu)^2)
  for (cand in c(mu - 0.1 * (1 + abs(mu)), mu + 0.1 * (1 + abs(mu)))) {
    if (mean((y - cand)^2) < mse_mu) {
      stop("a constant beat the mean; impossible.", call. = FALSE)
    }
  }
  list(best_prediction = mu, mse = mse_mu)
}
