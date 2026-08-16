% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{andrews_monahan_hac}
\alias{andrews_monahan_hac}
\title{andrews_monahan_hac}
\usage{
andrews_monahan_hac(e, X = NULL, prewhiten = TRUE, var_order = 1, kernel = "qs", bandwidth = NULL, weights = NULL, n_params = NULL, cap = 0.97, adjust = TRUE)
}
\arguments{
\item{e}{A matrix; passed to \code{as.matrix}.}
\item{X}{Defaults to \code{NULL}.}
\item{prewhiten}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{var_order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{kernel}{Carried through into a list the body builds. Defaults to \code{"qs"}.}
\item{bandwidth}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{weights}{Defaults to \code{NULL}.}
\item{n_params}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{cap}{Numeric; combined arithmetically in the body. Defaults to \code{0.97}.}
\item{adjust}{Defaults to \code{TRUE}.}
}
\description{
A step of the andmnh_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{structure}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
andrews_monahan_hac(V)
}
