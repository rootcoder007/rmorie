% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_brr_hyper}
\alias{morie_brr_hyper}
\title{morie_brr_hyper}
\usage{
morie_brr_hyper(y, R2 = 0.5, nu = 5, nu_beta = 5, sum_var_x = NULL) 
}
\description{
morie_brr_hyper
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{R2}{Argument \code{R2}; see Usage.}
\item{nu}{Argument \code{nu}; see Usage.}
\item{nu_beta}{Argument \code{nu_beta}; see Usage.}
\item{sum_var_x}{Argument \code{sum_var_x}; see Usage.}
}
\value{
A list with \code{S}, \code{S_beta}, \code{nu}, \code{nu_beta}, \code{var_y}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_brr_hyper(V)
}
