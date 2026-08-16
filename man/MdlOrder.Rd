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
\item{errors}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_samples}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
per parameter is log(N), larger than AIC\'s 2 for any N > 7, so MDL picks the same order or a lower one -- and unlike AIC it is consistent.  AIC is returned alongside for comparison.
}
\value{
A list with \code{order}, \code{criterion}, \code{aic}, \code{aic_order}, \code{n}, \code{start_order}, \code{penalty_per_parameter}, \code{stricter_than_aic}, \code{method}.
}
\keyword{internal}
\examples{
MdlOrder(errors = c(1, 2, 3, 4, 5, 6, 7, 8), n_samples = 5L)
}
