% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpmll}
\alias{.gpmll}
\title{.gpmll}
\usage{
.gpmll(probs, y_true, n_classes = NULL)
}
\arguments{
\item{probs}{Passed to \code{.gpmat}.}
\item{y_true}{Passed to \code{.gpflat}.}
\item{n_classes}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm009}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.gpmll(probs = c(1, 2, 3, 4, 5, 6, 7, 8), y_true = 5L)
}
