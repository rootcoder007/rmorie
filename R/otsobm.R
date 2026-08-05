# SPDX-License-Identifier: AGPL-3.0-or-later
#' Replace W_1 by a Hilbert norm on the difference of the measures
#'
#' \eqn{_1} linearised around a common reference measure is exactly the
# prime \code{H^{-1}} norm of the difference, and unlike \eqn{_1} that norm
# prime is quadratic: it needs one linear solve, not a linear program, and it
# prime embeds into a Hilbert space so kernel methods apply directly. The price
# prime is that it is only a first-order approximation.
# prime
# prime Formula: \code{W_1(mu,nu) ~ ||mu - nu||_{H^{-1}} = sqrt((mu-nu)'
#' (-Delta)^{-1} (mu-nu))} -- Peyre (2018), Section 2.
#'
#' @param mu,nu Two measures on the same support.
#' @param Laplace_inv Inverse (or pseudo-inverse) of the Laplacian,
#'   symmetric positive semi-definite.
#' @return List with \code{W1_sob}, \code{quad_form}, \code{mass_gap},
#'   \code{n}.
#' @references Peyre, G. (2018). ESAIM: Control, Optimisation and Calculus
#'   of Variations 24(4):1489-1501. \doi{10.1051/cocv/2017050}.
#' @export
Otsobm <- function(mu, nu, Laplace_inv) {
  a <- as.numeric(mu); b <- as.numeric(nu); L <- as.matrix(Laplace_inv)
  n <- length(a)
  if (length(b) != n || nrow(L) != n || ncol(L) != n)
    stop("Laplace_inv must be n by n and match both measures")
  r <- a - b
  q <- as.numeric(t(r) %*% L %*% r)
  if (q < 0) q <- 0
  .t1_result(W1_sob = sqrt(q), quad_form = q, mass_gap = sum(a) - sum(b),
             n = n, method = "Sobolev H^-1 approximation to W_1")
}
