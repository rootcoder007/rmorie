% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{box_cox}
\alias{box_cox}
\title{box_cox}
\usage{
box_cox(y, omega) 
}
\arguments{
\item{y}{Numeric; passed to \code{log}.}
\item{omega}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}, \code{morie_bats}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
box_cox(y = c(1, 2, 3, 4, 5, 6, 7, 8), omega = 5L)
}
