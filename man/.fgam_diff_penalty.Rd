% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fgam_diff_penalty}
\alias{.fgam_diff_penalty}
\title{.fgam_diff_penalty}
\usage{
.fgam_diff_penalty(n, order = 2L)
}
\arguments{
\item{n}{A count; the body uses it as \code{matrix(...)}.}
\item{order}{Numeric; combined arithmetically in the body. Defaults to \code{2L}.}
}
\description{
A step of the fgam_native implementation. Called by \code{morie_fgam_functional_gam}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{crossprod}.
}
\keyword{internal}
\examples{
.fgam_diff_penalty(n = 5L)
}
