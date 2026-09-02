% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_svdd}
\alias{morie_svdd}
\title{morie_svdd}
\usage{
morie_svdd(X, C = NULL, nu = NULL, kernel = "rbf", gamma = NULL, degree = 3, 
    coef0 = 1, tol = 1e-10, max_iter = 20000) 
}
\arguments{
\item{X}{Passed to \code{.morie_svdd_mat}.}
\item{C}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{nu}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{kernel}{One of \code{"linear"}, \code{"poly"}, \code{"rbf"}. Defaults to \code{"rbf"}.}
\item{gamma}{Optional; may be \code{NULL}. Passed to \code{.morie_svdd_kernel_matrix}.}
\item{degree}{Passed to \code{.morie_svdd_kernel_matrix}. Defaults to \code{3}.}
\item{coef0}{Passed to \code{.morie_svdd_kernel_matrix}. Defaults to \code{1}.}
\item{tol}{Passed to \code{.morie_svdd_solve_dual}. Defaults to \code{1e-10}.}
\item{max_iter}{Passed to \code{.morie_svdd_solve_dual}. Defaults to \code{20000}.}
}
\description{
A step of the svdd_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{R2}, \code{radius}, \code{center}, \code{support_}, \code{boundary_}, \code{bounded_}, \code{n_support}, \code{degenerate}, \code{distance2}, \code{outlier_fraction}, \code{outlier_bound}, \code{decision}, \code{predict}, \code{C}, \code{kernel}, \code{gamma}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_svdd(V)
}
