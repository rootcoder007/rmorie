% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_tse}
\alias{.morie_rg_tse}
\title{Bsaadapt}
\usage{
.morie_rg_tse(seg, p)
}
\arguments{
\item{seg}{A vector; its length is taken and its elements indexed.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the rangayyan_ccf implementation. Called by \code{.morie_rg_H}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_rg_tse(seg = c(1, 2, 3, 4, 5, 6, 7, 8), p = 0.5)
}
