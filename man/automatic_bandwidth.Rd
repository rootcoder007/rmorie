% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{automatic_bandwidth}
\alias{automatic_bandwidth}
\title{automatic_bandwidth}
\usage{
automatic_bandwidth(v, kernel = "qs", weights = NULL, n = NULL) 
}
\arguments{
\item{v}{A matrix; passed to \code{as.matrix}.}
\item{kernel}{Passed to \code{.check_kernel}. Defaults to \code{"qs"}.}
\item{weights}{Passed to \code{alpha_ar1}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{andrews_monahan_hac}. See the file header for the source the module follows.
}
\value{
A list with \code{bandwidth}, \code{alpha}, \code{fits}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
automatic_bandwidth(V)
}
