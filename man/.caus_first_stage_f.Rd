% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.caus_first_stage_f}
\alias{.caus_first_stage_f}
\title{.caus_first_stage_f}
\usage{
.caus_first_stage_f(D, Z)
}
\arguments{
\item{D}{A vector; its length is taken.}
\item{Z}{Passed to \code{.caus_intercept}.}
}
\description{
A step of the caus_native implementation. Called by \code{morie_caus_iv_2sls}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.caus_first_stage_f(V, V)
}
