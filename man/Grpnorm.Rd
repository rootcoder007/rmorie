% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Grpnorm}
\alias{Grpnorm}
\title{Group normalisation (Wu & He 2018)}
\usage{
Grpnorm(x, n_groups, eps = 1e-05) 
}
\description{
Group normalisation (Wu & He 2018)
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{n_groups}{Argument \code{n_groups}; see Usage.}
\item{eps}{Argument \code{eps}; see Usage.}
}
\value{
A list with \code{x}, \code{mean}, \code{sd}, \code{n_groups}, \code{group_size}.
}
\keyword{internal}
\examples{
set.seed(4)
Grpnorm(rnorm(12), n_groups = 3)
}
