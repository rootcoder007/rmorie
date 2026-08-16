% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ddim_alpha_bar}
\alias{.ddim_alpha_bar}
\title{Nichol-Dhariwal cosine schedule for the cumulative alpha}
\usage{
.ddim_alpha_bar(t, T)
}
\arguments{
\item{t}{Passed to \code{f}.}
\item{T}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ddimst implementation. Called by \code{Ddimst}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ddim_alpha_bar(V, V)
}
