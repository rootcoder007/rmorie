# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviation from a variance
#'
#' sigma_X = sqrt(Var(X)).
#'
#' @param var_x a variance, >= 0.
#' @return list(variance, sd).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.39).
#' @examples
#' SdFromVar(9)$sd
#' @export
SdFromVar <- function(var_x) {
  v <- as.numeric(var_x)
  if (length(v) != 1L || is.na(v) || v < 0) {
    stop("var_x must be a single value >= 0.", call. = FALSE)
  }
  list(variance = v, sd = sqrt(v))
}
