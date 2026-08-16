% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rrblpr_reml_at}
\alias{.rrblpr_reml_at}
\title{Restricted log likelihood at lambda, profiled over sigma_e^2}
\usage{
.rrblpr_reml_at(loglam, G, y, X)
}
\arguments{
\item{loglam}{Numeric; passed to \code{exp}.}
\item{G}{Numeric; combined arithmetically in the body.}
\item{y}{A vector; its length is taken.}
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the rrblpr_native implementation. Called by \code{morie_rrblpr_rr_blup}. See the file header for the source the module follows.
}
\value{
A list with \code{ll}, \code{lam}, \code{beta}, \code{s2e}, \code{L}.
}
\keyword{internal}
