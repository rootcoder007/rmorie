% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MdlOrder}
\alias{MdlOrder}
\title{Rissanen (1978): MDL(p) = N log(sigma_p^2) + p log(N).  The penalty}
\usage{
MdlOrder(errors, n_samples)
}
\arguments{
\item{errors}{See Usage.}
\item{n_samples}{See Usage.}
}
\description{
per parameter is log(N), larger than AIC\\'s 2 for any N > 7, so MDL picks the same order or a lower one -- and unlike AIC it is consistent.  AIC is returned alongside for comparison.
}
\value{
A list with \code{order}, \code{criterion}, \code{aic}, \code{aic_order}, \code{n}, \code{start_order}, \code{penalty_per_parameter}, \code{stricter_than_aic}, \code{method}.
}
\keyword{internal}
