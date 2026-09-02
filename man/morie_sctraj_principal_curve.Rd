% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_principal_curve}
\alias{morie_sctraj_principal_curve}
\title{morie_sctraj_principal_curve}
\usage{
morie_sctraj_principal_curve(X, init, weights = NULL, max_iter = 15, tol = 0.001, span = 0.4, 
    n_knots = NULL) 
}
\arguments{
\item{X}{Passed to \code{.sctraj_matrix}.}
\item{init}{A matrix; passed to \code{as.matrix}.}
\item{weights}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{15}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{0.001}.}
\item{span}{Passed to \code{.sctraj_smooth}. Defaults to \code{0.4}.}
\item{n_knots}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}. See the file header for the source the module follows.
}
\value{
A list with \code{pseudotime}, \code{curve}, \code{distance}, \code{sse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sctraj_principal_curve(V, V)
}
