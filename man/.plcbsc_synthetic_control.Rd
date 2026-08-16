% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_synthetic_control}
\alias{.plcbsc_synthetic_control}
\title{.plcbsc_synthetic_control}
\usage{
.plcbsc_synthetic_control(x_treated, x_donors, v = NULL, max_iter = 5000, tol = 1e-12, step = NULL)
}
\arguments{
\item{x_treated}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x_donors}{Iterated over elementwise, with \code{lapply}.}
\item{v}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5000}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.}
\item{step}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the plcbsc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{loss}, \code{fitted}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
\examples{
.plcbsc_synthetic_control(x_treated = 5L, x_donors = c(1, 2, 3, 4, 5, 6, 7, 8))
}
