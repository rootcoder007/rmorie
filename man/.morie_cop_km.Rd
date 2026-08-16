% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cop_km}
\alias{.morie_cop_km}
\title{Kaplan-Meier survival evaluated at the observed times}
\usage{
.morie_cop_km(time, event)
}
\arguments{
\item{time}{A vector; indexed elementwise.}
\item{event}{A vector; indexed elementwise.}
}
\description{
A step of the copula_native implementation. Called by \code{morie_copula_survival}. See the file header for the source the module follows.
}
\value{
A list with \code{grid}, \code{vals}.
}
\keyword{internal}
