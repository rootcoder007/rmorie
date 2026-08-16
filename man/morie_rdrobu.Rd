% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rdrobu}
\alias{morie_rdrobu}
\title{morie_rdrobu}
\usage{
morie_rdrobu(y, x, cutoff = 0, alpha = 0.05, ...)
}
\arguments{
\item{y}{Passed to \code{morie_causrddc}.}
\item{x}{Passed to \code{morie_causrddc}.}
\item{cutoff}{Passed to \code{morie_causrddc}. Defaults to \code{0}.}
\item{alpha}{Passed to \code{morie_causrddc}. Defaults to \code{0.05}.}
\item{...}{Passed through.}
}
\description{
A step of the rdrobu_native implementation. Called by \code{morie_calonico_cattaneo_titiunik}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
