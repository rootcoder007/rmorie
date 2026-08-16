% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{second_choice}
\alias{second_choice}
\title{second_choice}
\usage{
second_choice(i1, alpha, y, E, C, rng, tol = 0.001)
}
\arguments{
\item{i1}{Coerced to integer by the body, with \code{as.integer}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{See Usage.}
\item{E}{A vector; indexed elementwise.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rng}{A list; the body reads \code{$uniform} from it.}
\item{tol}{Defaults to \code{0.001}.}
}
\description{
A step of the smoopt_native implementation. Called by \code{smo_platt}. See the file header for the source the module follows.
}
\value{
A list with \code{index}, \code{level}, \code{note}.
}
\keyword{internal}
\examples{
second_choice(i1 = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), alpha = 0.5, y = c(1, 2, 3, 4, 5, 6, 7, 8), E = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4, 5, 6, 7, 8), rng = c(1, 2, 3, 4, 5, 6, 7, 8))
}
