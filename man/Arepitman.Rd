% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Arepitman}
\alias{Arepitman}
\title{ARE from derivatives and variances -- Theorem 13.2.2, eq. (13.2.1), p. 485}
\usage{
Arepitman(deriv, var, deriv_star, var_star) 
}
\description{
ARE from derivatives and variances -- Theorem 13.2.2, eq. (13.2.1), p. 485
}
\arguments{
\item{deriv}{Argument \code{deriv}; see Usage.}
\item{var}{Argument \code{var}; see Usage.}
\item{deriv_star}{Argument \code{deriv_star}; see Usage.}
\item{var_star}{Argument \code{var_star}; see Usage.}
}
\value{
A list with \code{are}, \code{check}, \code{efficacy}, \code{efficacy_star}.
}
\keyword{internal}
\examples{
rmorie:::Arepitman(deriv = 0.399, var = 1, deriv_star = 0.318, var_star = 1.05)
}
