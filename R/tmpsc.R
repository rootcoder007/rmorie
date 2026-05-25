# SPDX-License-Identifier: AGPL-3.0-or-later

#' Temperature-scaled softmax (Hinton 2015)
#'
#' @param x Numeric vector of logits.
#' @param temperature Numeric softmax temperature > 0 (default 1).
#' @return Named list with tensor, entropy, temperature, method.
#' @keywords internal
temperature_scaling <- function(x, temperature = 1) {
  if (temperature <= 0) stop("Temperature must be > 0")
  z <- as.numeric(x) / temperature
  z <- z - max(z)
  p <- exp(z)
  p <- p / sum(p)
  H <- -sum(ifelse(p > 0, p * log(p), 0))
  list(tensor = p, entropy = H, temperature = temperature, method = "temperature-softmax")
}
