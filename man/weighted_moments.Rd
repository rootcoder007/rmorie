% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{weighted_moments}
\alias{weighted_moments}
\title{weighted_moments}
\usage{
weighted_moments(m, g) 
}
\arguments{
\item{m}{A matrix; the body checks with \code{is.matrix}.}
\item{g}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{cvm_statistic}, \code{ks_critical_value}, \code{ks_statistic}. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{sd}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
weighted_moments(V, V)
}
