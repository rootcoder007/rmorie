% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.anm_resid}
\alias{.anm_resid}
\title{Internal: leave-one-out Nadaraya-Watson residuals}
\usage{
.anm_resid(x, y)
}
\arguments{
\item{x}{See Usage.}
\item{y}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the causal_native implementation. Called by \code{morie_anm_direction}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
