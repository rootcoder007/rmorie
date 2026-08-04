# SPDX-License-Identifier: AGPL-3.0-or-later
#' One SGD step with momentum and L2 weight decay
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED): the parameters are
#' "updated by stochastic gradient descent with momentum", and the loss of
#' Silver et al. (2017), Nature 550, 354-359 -- reproduced in the
#' AlphaZero paper as l = (z - v)^2 - pi' log p + c ||theta||^2 -- carries
#' an explicit L2 term.  Because that penalty sits inside the loss its
#' gradient is 2 c theta, so the update is v <- mu v + (g + lambda theta),
#' theta <- theta - lr v with lambda = 2c.  This is the L2-regularisation
#' form, in which decay passes through the momentum buffer, not the
#' decoupled AdamW form; the two differ whenever mu is nonzero.
#'
#' @param theta current parameters, flattened.
#' @param grad gradient of the data term.
#' @param momentum momentum coefficient mu.
#' @param weight_decay lambda = 2c.
#' @param lr learning rate.
#' @param buf previous momentum buffer; zeros by default.
#' @return list: estimate, theta_new, buf, step_norm, method.
#' @keywords internal
#' @examples
#' Sgdmomstep(c(1, 2), c(0.1, -0.2))$theta_new
#' @export
Sgdmomstep <- function(theta, grad, momentum = 0.9, weight_decay = 1e-4,
                       lr = 0.2, buf = NULL) {
  th <- .s03vec(theta); g <- .s03vec(grad)
  b <- if (!is.null(buf)) .s03vec(buf) else numeric(length(th))
  mu <- as.numeric(momentum); wd <- as.numeric(weight_decay); a <- as.numeric(lr)
  nb <- numeric(length(th)); nt <- numeric(length(th))
  s2 <- 0
  for (i in seq_along(th)) {
    nb[i] <- mu * b[i] + (g[i] + wd * th[i])
    step <- a * nb[i]
    nt[i] <- th[i] - step
    s2 <- s2 + step * step
  }
  list(estimate = if (length(nt)) nt[1] else NaN, theta_new = nt, buf = nb,
       step_norm = sqrt(s2),
       method = "SGD with momentum and L2 weight decay (AlphaZero training)")
}
