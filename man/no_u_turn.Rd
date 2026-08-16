% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{no_u_turn}
\alias{no_u_turn}
\title{The NUTS stopping rule: has the trajectory begun to double back?}
\usage{
no_u_turn(theta_minus, theta_plus, r_minus, r_plus)
}
\arguments{
\item{theta_minus}{See Usage.}
\item{theta_plus}{See Usage.}
\item{r_minus}{See Usage.}
\item{r_plus}{See Usage.}
}
\description{
True (no U-turn) while (theta+ - theta-).r is non-negative at both ends.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
no_u_turn(theta_minus = c(1, 2, 3, 4, 5, 6, 7, 8), theta_plus = c(1, 2, 3, 4, 5, 6, 7, 8), r_minus = c(1, 2, 3, 4, 5, 6, 7, 8), r_plus = c(1, 2, 3, 4, 5, 6, 7, 8))
}
