% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cqtmpl_wls}
\alias{.cqtmpl_wls}
\title{Weighted least squares with an intercept already in X}
\usage{
.cqtmpl_wls(X, y, w)
}
\arguments{
\item{X}{Numeric matrix (n by p).}
\item{y}{Numeric vector of length n.}
\item{w}{Numeric weight vector of length n.}
}
\description{
Solves the normal equations for a weighted regression \code{X b = y} with diagonal weights \code{w}. The Python arm keeps this internal; it is exposed here only so the EM step is auditable.
}
\value{
Numeric coefficient vector of length p.
}
\keyword{internal}
