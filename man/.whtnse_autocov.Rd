% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.whtnse_autocov}
\alias{.whtnse_autocov}
\title{Internal: Gamma_l = n^-1 sum_{t=l+1}^{n} e_t e_{t-l}\\'}
\usage{
.whtnse_autocov(E, lag)
}
\arguments{
\item{E}{A matrix; indexed by row and column.}
\item{lag}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the whtnse implementation. Called by \code{morie_portmanteau_hosking}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
