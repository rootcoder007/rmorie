% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FecgNmf}
\alias{FecgNmf}
\title{FecgNmf}
\usage{
FecgNmf(x, fs, nwin = 64, hop = NULL, rank = 4, lam = 0, maxiter = 150, taum = 0.6, tauf = 0.45, seed = 1)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{nwin}{Defaults to \code{64}.}
\item{hop}{Defaults to \code{NULL}.}
\item{rank}{Defaults to \code{4}.}
\item{lam}{Defaults to \code{0}.}
\item{maxiter}{Defaults to \code{150}.}
\item{taum}{Defaults to \code{0.6}.}
\item{tauf}{Defaults to \code{0.45}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{fetal}, \code{maternal}, \code{fetalrow}, \code{maternalrow}, \code{peaks}, \code{W}, \code{H}, \code{error}, \code{method}.
}
\keyword{internal}
