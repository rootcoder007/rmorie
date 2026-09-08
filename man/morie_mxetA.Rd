% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mxetA}
\alias{morie_mxetA}
\title{morie_mxetA}
\usage{
morie_mxetA(F, n_sim = 1, seed = 0, max_points = 100000L) 
}
\arguments{
\item{F}{A matrix; passed to \code{as.matrix}.}
\item{n_sim}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{max_points}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100000L}.}
}
\description{
Part of the mxetA_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{fields}, \code{scales}, \code{n_points}, \code{frechet_uniform}, \code{seed}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_mxetA(V)
}
