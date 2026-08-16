% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Chernoff}
\alias{Chernoff}
\title{Rho_a = sum_l p1^a p2^(1-a); C = -ln min_a rho_a.  The Bhattacharyya}
\usage{
Chernoff(p1, p2, alpha = NULL, n_grid = 201)
}
\arguments{
\item{p1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{alpha}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_grid}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{201}.}
}
\description{
coefficient is exactly this at a = 1/2, which is the relationship the whole family turns on.  Leaving alpha unset searches for the minimizing a, which is what makes the Chernoff bound TIGHTER than the Bhattacharyya bound -- the latter is the same bound fixed at a = 1/2 rather than the best a.  Every a gives a valid bound P_e <= P1^a P2^(1-a) rho_a; the minimum gives the best of them. NOT FROM RANGAYYAN (2024).
}
\value{
A list with \code{coefficient}, \code{alpha}, \code{information}, \code{bhattacharyya_coefficient}, \code{bhattacharyya_is_alpha_one_half}, \code{alpha_searched}, \code{at_least_as_tight_as_bhattacharyya}, \code{reference}, \code{not_from_this_book}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Chernoff(V, V)
}
