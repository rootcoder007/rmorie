% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.med_fit}
\alias{.med_fit}
\title{The two regressions of the regression-based mediation formulas}
\usage{
.med_fit(X, M, Y, C, who)
}
\arguments{
\item{X}{Passed to \code{.s03vec}.}
\item{M}{Passed to \code{.s03vec}.}
\item{Y}{Passed to \code{.s03vec}.}
\item{C}{Optional; may be \code{NULL}. Passed to \code{.s03mat}.}
\item{who}{Passed to \code{paste0}.}
}
\description{
A step of the ctde implementation. Called by \code{Ctde}, \code{Pnie}, \code{Tnie}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{theta}, \code{cbar}, \code{n}.
}
\keyword{internal}
\examples{
.med_fit(X = c(1, 2, 3, 4, 5, 6, 7, 8), M = c(1, 2, 3, 4, 5, 6, 7, 8), Y = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4, 5, 6, 7, 8), who = c(1, 2, 3, 4, 5, 6, 7, 8))
}
