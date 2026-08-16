% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.smoopt_make_rng}
\alias{.smoopt_make_rng}
\title{.smoopt_make_rng}
\usage{
.smoopt_make_rng(seed)
}
\arguments{
\item{seed}{Passed to \code{.ghc_rng}.}
}
\description{
A step of the smoopt_native implementation. Called by \code{smo_platt}. See the file header for the source the module follows.
}
\value{
A list with \code{uniform}.
}
\keyword{internal}
\examples{
.smoopt_make_rng(seed = 5L)
}
