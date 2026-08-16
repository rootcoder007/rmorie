% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_chronos}
\alias{morie_chronos}
\title{morie_chronos}
\usage{
morie_chronos(x, bins, context = NULL, add_eos = TRUE, pad_to = NULL)
}
\arguments{
\item{x}{Passed to \code{tokenize}.}
\item{bins}{Passed to \code{tokenize}.}
\item{context}{Passed to \code{tokenize}.}
\item{add_eos}{Passed to \code{tokenize}. Defaults to \code{TRUE}.}
\item{pad_to}{Passed to \code{tokenize}.}
}
\description{
A step of the chronos_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{tokenize}.
}
\keyword{internal}
\examples{
set.seed(3)
r <- morie_chronos(rnorm(20), uniform_bins(-3, 3, n_bins = 16L))
str(r, max.level = 1)
}
